use silver_oxide::ast;

#[derive(Debug)]
pub enum Error<'a> {
    Expression {
        exp: &'a ast::Exp,
        kind: ExpressionError,
    },
    Undeclared {
        ident: &'a ast::Ident,
        kind: UndeclaredError,
    },
    Type,
    Misc(u32),
}

impl<'a> Error<'a> {
    pub fn expression(exp: &'a ast::Exp, kind: ExpressionError) -> Self {
        Self::Expression { exp, kind }
    }

    pub fn undeclared_variable(ident: &'a ast::Ident) -> Self {
        Self::Undeclared { ident, kind: UndeclaredError::Variable }
    }

    pub fn undeclared_function(ident: &'a ast::Ident) -> Self {
        Self::Undeclared { ident, kind: UndeclaredError::Function }
    }

    pub fn undeclared_field(ident: &'a ast::Ident) -> Self {
        Self::Undeclared { ident, kind: UndeclaredError::Field }
    }

    pub fn divide_by_zero(exp: &'a ast::Exp, assertion: egg::Id) -> Self {
        Self::Expression { exp, kind: ExpressionError::Assertion { assertion, kind: AssertionError::DivideByZero } }
    }

    pub fn type_error(exp: &'a ast::Exp) -> Self {
        Self::Expression { exp, kind: ExpressionError::TypeError }
    }

    pub fn exhale(exp: &'a ast::Exp, assertion: egg::Id) -> Self {
        Self::Expression { exp, kind: ExpressionError::Assertion { assertion, kind: AssertionError::Exhale } }
    }

    pub fn function_precondition(exp: &'a ast::Exp, assertion: egg::Id) -> Self {
        Self::Expression { exp, kind: ExpressionError::Assertion { assertion, kind: AssertionError::FunctionPrecondition } }
    }
}

#[derive(Debug)]
pub enum UndeclaredError {
    Field,
    Function,
    Variable,
}

#[derive(Debug)]
pub enum ExpressionError {
    Assertion {
        assertion: egg::Id,
        kind: AssertionError,
    },
    MissingResource {
        resource: egg::Id,
    },
    TypeError,
}

impl ExpressionError {
    pub fn negative_permission(assertion: egg::Id) -> Self {
        Self::Assertion { assertion, kind: AssertionError::NegativePermission }
    }
    pub fn read_missing(assertion: egg::Id) -> Self {
        Self::Assertion { assertion, kind: AssertionError::ReadMissing }
    }
    pub fn write_missing(assertion: egg::Id) -> Self {
        Self::Assertion { assertion, kind: AssertionError::WriteMissing }
    }
    pub fn resource_not_found(resource: egg::Id) -> Self {
        Self::MissingResource { resource }
    }
}

#[derive(Debug)]
pub enum AssertionError {
    NegativePermission,
    ReadMissing,
    WriteMissing,
    DivideByZero,
    Exhale,
    FunctionPrecondition,

    // ReadPermission(&'a silver_oxide::ast::Exp, &'a silver_oxide::ast::Ident),
    // WritePermission(&'a silver_oxide::ast::Exp, &'a silver_oxide::ast::Ident),
    // UndeclaredVariable(&'a silver_oxide::ast::Ident),
    // UndeclaredFunction(&'a silver_oxide::ast::Ident),
    // UndeclaredField(&'a silver_oxide::ast::Ident),
    // ExhaleMissingPermission(&'a silver_oxide::ast::Exp),
    // ExhaleUnknown(egg::Id),
    // DivideByZero(&'a silver_oxide::ast::Exp),
    // TypeError,

    // MiscError(u32),
}

// impl<'a> Error<'a> {
//     pub fn no_field(failing_check: egg::Id, receiver: &'a silver_oxide::ast::Exp, field: &'a silver_oxide::ast::Ident) -> Self {
//         Error::ReadPermission(receiver, field)
//     }
//     pub fn missing_write_permission(failing_check: egg::Id, receiver: &'a silver_oxide::ast::Exp, field: &'a silver_oxide::ast::Ident) -> Self {
//         Error::WritePermission(receiver, field)
//     }

//     pub fn undeclared_variable(ident: &'a silver_oxide::ast::Ident) -> Self {
//         Error::UndeclaredVariable(ident)
//     }
//     pub fn undeclared_function(ident: &'a silver_oxide::ast::Ident) -> Self {
//         Error::UndeclaredFunction(ident)
//     }
//     pub fn undeclared_field(ident: &'a silver_oxide::ast::Ident) -> Self {
//         Error::UndeclaredField(ident)
//     }

//     pub fn exhale_missing_permission(acc: &'a silver_oxide::ast::Exp) -> Self {
//         Error::ExhaleMissingPermission(acc)
//     }
//     pub fn exhale_unknown(exp: egg::Id) -> Self {
//         Error::ExhaleUnknown(exp)
//     }
//     pub fn divide_by_zero(exp: &'a silver_oxide::ast::Exp, check: egg::Id) -> Self {
//         Error::DivideByZero(exp, check)
//     }
//     pub fn type_error() -> Self {
//         Error::TypeError
//     }

//     pub fn misc_error(code: u32) -> Self {
//         Error::MiscError(code)
//     }
// }
