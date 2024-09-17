use std::path::PathBuf;

use fxhash::FxHashMap;
use silver_oxide::{ast, walk::AstWalker};

use crate::{function::{Function, FunctionVerified}, method::{Method, MethodVerified}};


#[derive(Default, Debug)]
pub struct Declarations<'a, F, M> {
    pub callable: FxHashMap<&'a ast::Ident, CallableDecl<'a, F, M>>,
    pub field: FxHashMap<&'a ast::Ident, &'a ast::Field>,
    pub ty: FxHashMap<&'a ast::Ident, TypeDecl<'a>>,
    pub axiom: Vec<&'a ast::Axiom>,
}

pub type Declarations0<'a> = Declarations<'a, (), ()>;
pub type Declarations1<'a> = Declarations<'a, FunctionVerified<'a>, ()>;
pub type Declarations2<'a> = Declarations<'a, FunctionVerified<'a>, MethodVerified>;

impl<'a> Declarations0<'a> {
    pub fn new(program: &'a ast::Program) -> Self {
        let mut decls = Declarations::<(), ()>::default();
        decls.walk_program(program);
        decls
    }

    pub fn verify_functions(self, log_dir: &PathBuf) -> Declarations1<'a> {
        let callable = self.callable.iter().map(
            |(&k, &v)| (k, v.verify_functions(&self, log_dir))
        ).collect();
        Declarations1 {
            callable,
            field: self.field,
            ty: self.ty,
            axiom: self.axiom,
        }
    }
}

impl<'a> Declarations1<'a> {
    pub fn verify_methods(self, log_dir: &PathBuf) -> Declarations2<'a> {
        let mut callable: FxHashMap<_, _> = self.callable.iter().flat_map(
            |(&k, v)| v.verify_methods(&self, log_dir).map(|v| (k, v))
        ).collect();
        callable.extend(self.callable.into_iter().filter_map(
            |(k, v)| v.move_non_methods().map(|v| (k, v))
        ));
        Declarations2 {
            callable,
            field: self.field,
            ty: self.ty,
            axiom: self.axiom,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CallableDecl<'a, F, M> {
    DomainFunction(&'a ast::DomainFunction),
    Function(&'a ast::Function, F),
    Predicate(&'a ast::Predicate),
    Method(&'a ast::Method, M),
}

impl<'a> CallableDecl<'a, (), ()> {
    fn verify_functions(self, decls: &Declarations0<'a>, log_dir: &PathBuf) -> CallableDecl<'a, FunctionVerified<'a>, ()> {
        match self {
            CallableDecl::Function(f, ()) => {
                let d = Function::verify(f, decls, log_dir).unwrap();
                CallableDecl::Function(f, d)
            }
            CallableDecl::DomainFunction(f) => CallableDecl::DomainFunction(f),
            CallableDecl::Method(m, ()) => CallableDecl::Method(m, ()),
            CallableDecl::Predicate(p) => CallableDecl::Predicate(p),
        }
    }
}

impl<'a> CallableDecl<'a, FunctionVerified<'a>, ()> {
    fn verify_methods(&self, decls: &Declarations1<'a>, log_dir: &PathBuf) -> Option<CallableDecl<'a, FunctionVerified<'a>, MethodVerified>> {
        match self {
            CallableDecl::Method(m, ()) => {
                let d = Method::verify(m, decls, log_dir).unwrap();
                Some(CallableDecl::Method(m, d))
            }
            _ => None,
        }
    }
    fn move_non_methods(self) -> Option<CallableDecl<'a, FunctionVerified<'a>, MethodVerified>> {
        let self_ = match self {
            CallableDecl::Method(..) => return None,
            CallableDecl::Function(f, v) => CallableDecl::Function(f, v),
            CallableDecl::DomainFunction(f) => CallableDecl::DomainFunction(f),
            CallableDecl::Predicate(p) => CallableDecl::Predicate(p),
        };
        Some(self_)
    }
}

#[derive(Debug)]
pub enum TypeDecl<'a> {
    Domain(&'a ast::Domain),
    Adt(&'a ast::Adt),
}

impl<'a> AstWalker<'a> for Declarations0<'a> {
    fn walk_domain_function(&mut self, f: &'a ast::DomainFunction) {
        self.callable.insert(&f.signature.name, CallableDecl::DomainFunction(f));
    }
    fn walk_axiom(&mut self, ast: &'a ast::Axiom) {
        self.axiom.push(ast);
    }
    fn walk_field(&mut self, ast: &'a ast::Field) {
        self.field.insert(&ast.fields[0].0, ast);
    }
    fn walk_function(&mut self, ast: &'a ast::Function) {
        self.callable.insert(&ast.signature.name, CallableDecl::Function(ast, ()));
    }
    fn walk_predicate(&mut self, ast: &'a ast::Predicate) {
        self.callable.insert(&ast.signature.name, CallableDecl::Predicate(ast));
    }
    fn walk_method(&mut self, ast: &'a ast::Method) {
        self.callable.insert(&ast.signature.name, CallableDecl::Method(ast, ()));
    }
    fn walk_domain(&mut self, ast: &'a ast::Domain) {
        self.ty.insert(&ast.name, TypeDecl::Domain(ast));
    }
    fn walk_adt(&mut self, ast: &'a ast::Adt) {
        self.ty.insert(&ast.name, TypeDecl::Adt(ast));
    }
}
