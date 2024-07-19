
#[derive(Debug)]
pub enum Error<'a> {
    ReadPermission(&'a silver_oxide::ast::Exp, &'a silver_oxide::ast::Ident),
    WritePermission(&'a silver_oxide::ast::Exp, &'a silver_oxide::ast::Ident),
    UndeclaredVariable(&'a silver_oxide::ast::Ident),
    UndeclaredFunction(&'a silver_oxide::ast::Ident),
    UndeclaredField(&'a silver_oxide::ast::Ident),
    ExhaleMissingPermission(&'a silver_oxide::ast::AccExp),
    ExhaleUnknown(&'a silver_oxide::ast::Exp),
    TypeError,

    MiscError(u32),
}

impl<'a> Error<'a> {
    pub fn no_field(receiver: &'a silver_oxide::ast::Exp, field: &'a silver_oxide::ast::Ident) -> Self {
        Error::ReadPermission(receiver, field)
    }
    pub fn missing_write_permission(receiver: &'a silver_oxide::ast::Exp, field: &'a silver_oxide::ast::Ident) -> Self {
        Error::WritePermission(receiver, field)
    }

    pub fn undeclared_variable(ident: &'a silver_oxide::ast::Ident) -> Self {
        Error::UndeclaredVariable(ident)
    }
    pub fn undeclared_function(ident: &'a silver_oxide::ast::Ident) -> Self {
        Error::UndeclaredFunction(ident)
    }
    pub fn undeclared_field(ident: &'a silver_oxide::ast::Ident) -> Self {
        Error::UndeclaredField(ident)
    }

    pub fn exhale_missing_permission(acc: &'a silver_oxide::ast::AccExp) -> Self {
        Error::ExhaleMissingPermission(acc)
    }
    pub fn exhale_unknown(exp: &'a silver_oxide::ast::Exp) -> Self {
        Error::ExhaleUnknown(exp)
    }
    pub fn type_error() -> Self {
        Error::TypeError
    }

    pub fn misc_error(code: u32) -> Self {
        Error::MiscError(code)
    }
}
