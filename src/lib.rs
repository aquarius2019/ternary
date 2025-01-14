
extern crate proc_macro;

use quote::quote;
use syn::{self, parse::{Parse, ParseStream}, parse_macro_input, Token};

// ternary: expression '?' expression ':' expression
// expression: ternary | simple

struct Ternary {
    condition: syn::Expr,
    true_branch: Expression,
    false_branch: Expression,
}

impl Parse for Ternary {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let condition = input.parse()?;

        #[cfg(not(feature = "try"))]
        {
            input.parse::<Token![?]>()?;
        }

        #[cfg(feature = "try")]
        {
            input.parse::<Token![=>]>()?;
        }
        
        let true_branch = input.parse()?;
        input.parse::<Token![:]>()?;
        let false_branch = input.parse()?;

        Ok(Self {
            condition,
            true_branch,
            false_branch,
        })
    }
}

impl quote::ToTokens for Ternary {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let condition = &self.condition;

        let true_branch = match &self.true_branch {
            Expression::Ternary(ternary) => quote! { #ternary },
            Expression::Simple(simple) => quote! { #simple },
        };

        let false_branch = match &self.false_branch {
            Expression::Ternary(ternary) => quote! { #ternary },
            Expression::Simple(simple) => quote! { #simple },
        };

        tokens.extend(quote! { if #condition { #true_branch } else { #false_branch } });
    }
}
struct SimpleExpression {
    expr: syn::Expr,
}

impl Parse for SimpleExpression {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            expr: input.parse()?,
        })
    }
}

impl quote::ToTokens for SimpleExpression {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let expr = &self.expr;
        tokens.extend(quote! { #expr });
    }
}

enum Expression {
    Ternary(Box<Ternary>),
    Simple(SimpleExpression),
}

impl Parse for Expression {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr: syn::Expr = input.parse()?;

        #[cfg(not(feature = "try"))]
        {
            if input.peek(Token![?]) {
                input.parse::<Token![?]>()?;
                let true_branch = input.parse()?;
                input.parse::<Token![:]>()?;
                let false_branch = input.parse()?;
    
                return Ok(Self::Ternary(Box::new(Ternary {
                    condition: expr,
                    true_branch,
                    false_branch,
                })))
            } 
        }
        
        #[cfg(feature = "try")]
        {
            if input.peek(Token![=>]) {
                input.parse::<Token![=>]>()?;
                let true_branch = input.parse()?;
                input.parse::<Token![:]>()?;
                let false_branch = input.parse()?;
    
                return Ok(Self::Ternary(Box::new(Ternary {
                    condition: expr,
                    true_branch,
                    false_branch,
                })))
            } 
        }
    
        Ok(Self::Simple(SimpleExpression { expr }))
    }
}

/// A macro that evaluates a ternary expression, similar to the ternary operator in other languages.
///
/// # Syntax
///
/// ```rust
/// tnr!{ condition ? true_expr : false_expr }
/// ```
///
/// - `condition`: An expression that evaluates to a boolean.
/// - `true_expr`: The expression to return if the condition is `true`.
/// - `false_expr`: The expression to return if the condition is `false`.
///
/// # Examples
///
/// ## Basic Usage
///
/// ```rust
/// let age = 20;
/// let category = tnr!{ age < 18 ? "child" : "adult" };
/// assert_eq!(category, "adult");
/// ```
///
/// ## Chaining Ternary Expressions
///
/// Ternary expressions can be chained to handle multiple conditions:
///
/// ```rust
/// let score = 85;
/// let grade = tnr! {
///     score >= 90 ? "A" :
///     score >= 80 ? "B" :
///     score >= 70 ? "C" :
///     score >= 60 ? "D" : "F"
/// };
/// assert_eq!(grade, "B");
/// ```
///
/// This macro evaluates the condition and returns the corresponding expression based on its truth value.
///
/// ## Important!!!
/// The `?` operator must be replaced with the `=>` operator if the `try` feature is enabled.
/// The `try` feature allows the `?` operator to be used in the macro's expressions. For example:
/// 
/// ```rust
/// let age = Ok(5);
/// let category = tnr!{ age? < 18 => "child" : "adult" };
/// ```
/// This syntax change is necessary because the ? operator is reserved for error handling in Rust.
/// Future versions of this crate may switch to use the => operator only.
/// For now, the ? operator is used to maintain syntax compatibility with C/C++.
/// 
/// # Note
///
/// Ensure that both `true_expr` and `false_expr` are of the same type or can be converted to a common type
/// to avoid type mismatches.
#[proc_macro]
pub fn tnr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tern = parse_macro_input!(input as Ternary);
    quote! { #tern }.into()
}
