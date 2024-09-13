use fxhash::FxHashMap;
use silver_oxide::{ast, walk::AstWalker};


#[derive(Default, Debug)]
pub struct Declarations<'a> {
    pub callable: FxHashMap<&'a ast::Ident, CallableDecl<'a>>,
    pub field: FxHashMap<&'a ast::Ident, &'a ast::Field>,
    pub ty: FxHashMap<&'a ast::Ident, TypeDecl<'a>>,
    pub axiom: Vec<&'a ast::Axiom>,
}

impl<'a> Declarations<'a> {
    pub fn new(program: &'a ast::Program) -> Self {
        let mut decls = Self::default();
        decls.walk_program(program);
        decls
    }
}

#[derive(Debug)]
pub enum CallableDecl<'a> {
    DomainFunction(&'a ast::DomainFunction),
    Function(&'a ast::Function),
    Predicate(&'a ast::Predicate),
    Method(&'a ast::Method),
}

#[derive(Debug)]
pub enum TypeDecl<'a> {
    Domain(&'a ast::Domain),
    Adt(&'a ast::Adt),
}

impl<'a> AstWalker<'a> for Declarations<'a> {
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
        self.callable.insert(&ast.signature.name, CallableDecl::Function(ast));
    }
    fn walk_predicate(&mut self, ast: &'a ast::Predicate) {
        self.callable.insert(&ast.signature.name, CallableDecl::Predicate(ast));
    }
    fn walk_method(&mut self, ast: &'a ast::Method) {
        self.callable.insert(&ast.signature.name, CallableDecl::Method(ast));
    }
    fn walk_domain(&mut self, ast: &'a ast::Domain) {
        self.ty.insert(&ast.name, TypeDecl::Domain(ast));
    }
    fn walk_adt(&mut self, ast: &'a ast::Adt) {
        self.ty.insert(&ast.name, TypeDecl::Adt(ast));
    }
}
