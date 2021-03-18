use codespan_reporting::diagnostic::{self, Label};
use lang_c::{span::Node, ast::*};

pub type Diagnostic = diagnostic::Diagnostic<()>;

//TODO: 
pub fn invalid_designation(init: &Node<InitDeclarator>) -> Diagnostic {
    Diagnostic::warning()
        .with_message("invalid designation")
        .with_labels(vec![
            Label::primary((), init.span.start..init.span.end)
        ])
}

//TODO: 
pub fn unsafe_conversion(expr: &Node<Expression>) -> Diagnostic {
    Diagnostic::warning()
        .with_message("discovered unsafe conversion")
        .with_labels(vec![
            Label::primary((), expr.span.start..expr.span.end)
        ])
}

//TODO: 
pub fn required_return() -> Diagnostic {
    todo!()
}

pub fn unreachable_after_return(stmt: &Node<Statement>, range: std::ops::Range<usize>) -> Diagnostic {
    Diagnostic::warning()
        .with_message("unreachable code")
        .with_labels(vec![
            Label::primary((), stmt.span.start..stmt.span.end)
                .with_message("first return of control"),
            Label::primary((), range)
                .with_message("unreachable")
        ])
}

pub fn static_assert(assert: &Node<StaticAssert>) -> Diagnostic {
    let span = assert.span;
    Diagnostic::error()
        .with_message("static assert failure")
        .with_labels(vec![
            Label::primary((), span.start..span.end)
        ])
}

pub fn old_syntax_declaration(declaration: &Node<FunctionDefinition>) -> Diagnostic {
    let span = declaration.span;

    Diagnostic::warning()
    .with_message("using the old syntax is not recommended")
    .with_labels(vec![Label::primary((),span.start..span.end)])
}

pub fn not_listed_parameter_declaration<'a, I>(declaration: &Node<DerivedDeclarator>, iter: I) -> Diagnostic 
where 
    I: IntoIterator<Item = &'a Node<Identifier>>
{
    let mut labels = Vec::new();

    let declarator_span = declaration.span;
    let decl_label = Label::primary((), declarator_span.start..declarator_span.end);
    labels.push(decl_label);
    for item in iter {
        let span = item.span;
        let label = Label::primary((), span.start..span.end);
        labels.push(label)
    }

    Diagnostic::error()
    .with_message("parameter without declaration")
    .with_labels(labels)
}

pub fn parameter_without_declaration<'a, I>(iter: I) -> Diagnostic 
where 
    I: IntoIterator<Item = &'a Node<Identifier>>
{
    Diagnostic::error()
        .with_message("parameter without declaration")
        .with_labels(
            iter.into_iter().map(|item| {
                let span = item.span;
                Label::primary((), span.start..span.end)
            }).collect()
        )
}

pub fn arguments_with_same_name<'a, I>(iter: I) -> Diagnostic 
where 
    I: IntoIterator<Item = &'a Node<Identifier>>
{
    Diagnostic::error()
        .with_message("found multiple argument with same name")
        .with_labels(
            iter.into_iter().map(|item| {
                let span = item.span;
                Label::primary((), span.start..span.end)
            }).collect()
        )
}

pub fn wrong_type_declaration<'a, I>(iter: I) -> Diagnostic 
where 
    I: IntoIterator<Item = &'a Node<TypeSpecifier>>
{
    Diagnostic::warning()
        .with_message("invalid type")
        .with_labels(
            iter.into_iter().map(|item| {
                let span = item.span;
                Label::primary((), span.start..span.end)
            }).collect()
        )
}