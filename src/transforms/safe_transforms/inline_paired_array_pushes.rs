use std::collections::HashMap;

use oxc_allocator::{CloneIn, Vec as ArenaVec};
use oxc_ast::ast::*;
use oxc_ast_visit::VisitMut;

use crate::{Transform, TransformCtx};

pub struct InlinePairedArrayPushes;

impl Transform for InlinePairedArrayPushes {
    fn name(&self) -> &'static str {
        "inlinePairedArrayPushes"
    }

    fn run<'a>(&self, ctx: &mut TransformCtx<'a>, program: &mut Program<'a>) -> bool {
        let mut visitor = Visitor {
            allocator: ctx.allocator,
            modified: false,
        };
        visitor.visit_program(program);
        visitor.modified
    }
}

enum ArrayOperation<'a> {
    Push(Vec<Expression<'a>>),
    Unshift(Vec<Expression<'a>>),
}

struct Visitor<'a> {
    allocator: &'a oxc_allocator::Allocator,
    modified: bool,
}

impl<'a> Visitor<'a> {
    fn empty_array_declarations(stmts: &[Statement<'a>]) -> HashMap<String, (usize, usize)> {
        let mut declarations = HashMap::new();

        for (statement_index, statement) in stmts.iter().enumerate() {
            let Statement::VariableDeclaration(declaration) = statement else {
                continue;
            };
            if declaration.kind != VariableDeclarationKind::Var {
                continue;
            }

            for (declarator_index, declarator) in declaration.declarations.iter().enumerate() {
                let BindingPattern::BindingIdentifier(binding) = &declarator.id else {
                    continue;
                };
                let Some(Expression::ArrayExpression(array)) = declarator.init.as_ref() else {
                    continue;
                };
                if array.elements.is_empty() {
                    declarations.insert(
                        binding.name.as_str().to_string(),
                        (statement_index, declarator_index),
                    );
                }
            }
        }

        declarations
    }

    fn push_call(
        statement: &Statement<'a>,
        allocator: &'a oxc_allocator::Allocator,
    ) -> Option<(String, Expression<'a>)> {
        let Statement::ExpressionStatement(expression_statement) = statement else {
            return None;
        };
        let Expression::CallExpression(call) = &expression_statement.expression else {
            return None;
        };
        let Expression::StaticMemberExpression(member) = &call.callee else {
            return None;
        };
        let Expression::Identifier(object) = &member.object else {
            return None;
        };
        if member.property.name.as_str() != "push" || call.optional || call.arguments.len() != 1 {
            return None;
        }
        let argument = call.arguments.first()?.as_expression()?;
        Some((
            object.name.as_str().to_string(),
            argument.clone_in(allocator),
        ))
    }

    fn append_elements(
        stmts: &mut ArenaVec<'a, Statement<'a>>,
        declaration: (usize, usize),
        values: &[Expression<'a>],
        allocator: &'a oxc_allocator::Allocator,
    ) {
        let Statement::VariableDeclaration(variable_declaration) = &mut stmts[declaration.0] else {
            return;
        };
        let Some(Expression::ArrayExpression(array)) = variable_declaration.declarations
            [declaration.1]
            .init
            .as_mut()
        else {
            return;
        };
        for value in values {
            array
                .elements
                .push(ArrayExpressionElement::from(value.clone_in(allocator)));
        }
    }

    fn process_statement_list(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let declarations = Self::empty_array_declarations(stmts);
        let mut removals = Vec::new();
        let mut index = 0;

        while index + 3 < stmts.len() {
            let Some((key_array, key)) = Self::push_call(&stmts[index], self.allocator) else {
                index += 1;
                continue;
            };
            let Some((value_array, value)) = Self::push_call(&stmts[index + 1], self.allocator)
            else {
                index += 1;
                continue;
            };

            if key_array == value_array
                || !matches!(key, Expression::StringLiteral(_))
                || !declarations.contains_key(&key_array)
                || !declarations.contains_key(&value_array)
                || declarations[&key_array].0 >= index
                || declarations[&value_array].0 >= index
            {
                index += 1;
                continue;
            }

            let mut keys = vec![key];
            let mut values = vec![value];
            let mut end = index + 2;
            while end + 1 < stmts.len() {
                let Some((next_key_array, next_key)) = Self::push_call(&stmts[end], self.allocator)
                else {
                    break;
                };
                let Some((next_value_array, next_value)) =
                    Self::push_call(&stmts[end + 1], self.allocator)
                else {
                    break;
                };
                if next_key_array != key_array
                    || next_value_array != value_array
                    || !matches!(next_key, Expression::StringLiteral(_))
                {
                    break;
                }
                keys.push(next_key);
                values.push(next_value);
                end += 2;
            }

            if keys.len() < 2 {
                index += 1;
                continue;
            }

            Self::append_elements(stmts, declarations[&key_array], &keys, self.allocator);
            Self::append_elements(stmts, declarations[&value_array], &values, self.allocator);
            removals.extend(index..end);
            index = end;
            self.modified = true;
        }

        for index in removals.into_iter().rev() {
            stmts.remove(index);
        }

        self.inline_literal_method_calls(stmts);
    }

    fn is_static_literal(expr: &Expression<'a>) -> bool {
        matches!(
            expr,
            Expression::StringLiteral(_)
                | Expression::NumericLiteral(_)
                | Expression::BooleanLiteral(_)
                | Expression::NullLiteral(_)
                | Expression::BigIntLiteral(_)
                | Expression::RegExpLiteral(_)
        )
    }

    fn literal_arguments(
        call: &CallExpression<'a>,
        allocator: &'a oxc_allocator::Allocator,
    ) -> Option<Vec<Expression<'a>>> {
        let mut values = Vec::new();
        for argument in &call.arguments {
            match argument {
                Argument::SpreadElement(spread) => {
                    let Expression::ArrayExpression(array) = &spread.argument else {
                        return None;
                    };
                    for element in &array.elements {
                        let value = element.as_expression()?;
                        if !Self::is_static_literal(value) {
                            return None;
                        }
                        values.push(value.clone_in(allocator));
                    }
                }
                _ => {
                    let value = argument.as_expression()?;
                    if !Self::is_static_literal(value) {
                        return None;
                    }
                    values.push(value.clone_in(allocator));
                }
            }
        }
        Some(values)
    }

    fn literal_array_operation(
        statement: &Statement<'a>,
        allocator: &'a oxc_allocator::Allocator,
    ) -> Option<(String, ArrayOperation<'a>)> {
        let Statement::ExpressionStatement(expression_statement) = statement else {
            return None;
        };
        let Expression::CallExpression(call) = &expression_statement.expression else {
            return None;
        };
        let Expression::StaticMemberExpression(member) = &call.callee else {
            return None;
        };
        let Expression::Identifier(object) = &member.object else {
            return None;
        };
        if call.optional {
            return None;
        }
        let values = Self::literal_arguments(call, allocator)?;
        let operation = match member.property.name.as_str() {
            "push" => ArrayOperation::Push(values),
            "unshift" => ArrayOperation::Unshift(values),
            _ => return None,
        };
        Some((object.name.as_str().to_string(), operation))
    }

    fn inline_literal_method_calls(&mut self, stmts: &mut ArenaVec<'a, Statement<'a>>) {
        let declarations = Self::empty_array_declarations(stmts);
        let mut removals = Vec::new();
        let mut index = 0;

        while index < stmts.len() {
            let Some((array_name, first_operation)) =
                Self::literal_array_operation(&stmts[index], self.allocator)
            else {
                index += 1;
                continue;
            };
            let Some(declaration) = declarations.get(&array_name).copied() else {
                index += 1;
                continue;
            };
            if declaration.0 + 1 != index {
                index += 1;
                continue;
            }

            let mut elements = Vec::new();
            let mut end = index;
            let mut operation = first_operation;
            loop {
                match operation {
                    ArrayOperation::Push(values) => elements.extend(values),
                    ArrayOperation::Unshift(values) => {
                        elements.splice(0..0, values);
                    }
                }
                end += 1;
                let Some((next_name, next_operation)) = stmts
                    .get(end)
                    .and_then(|statement| Self::literal_array_operation(statement, self.allocator))
                else {
                    break;
                };
                if next_name != array_name {
                    break;
                }
                operation = next_operation;
            }

            if elements.is_empty() {
                index += 1;
                continue;
            }
            Self::append_elements(stmts, declaration, &elements, self.allocator);
            removals.extend(index..end);
            index = end;
            self.modified = true;
        }

        for index in removals.into_iter().rev() {
            stmts.remove(index);
        }
    }
}

impl<'a> VisitMut<'a> for Visitor<'a> {
    fn visit_program(&mut self, program: &mut Program<'a>) {
        self.process_statement_list(&mut program.body);
        oxc_ast_visit::walk_mut::walk_program(self, program);
    }

    fn visit_function_body(&mut self, body: &mut FunctionBody<'a>) {
        self.process_statement_list(&mut body.statements);
        oxc_ast_visit::walk_mut::walk_function_body(self, body);
    }

    fn visit_block_statement(&mut self, block: &mut BlockStatement<'a>) {
        self.process_statement_list(&mut block.body);
        oxc_ast_visit::walk_mut::walk_block_statement(self, block);
    }
}
