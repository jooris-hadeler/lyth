use crate::{
    ast::{Expr, Lit, Operation},
    parser::{ExprParser, LiteralParser},
};

const MOD_NAME: &'static str = "<test>";

#[test]
fn test_literal() {
    let parser = LiteralParser::new();

    let ast = parser
        .parse(MOD_NAME, MOD_NAME, r#""Hello, World!""#)
        .unwrap();
    assert_eq!(ast, Lit::String(r"Hello, World!".to_string()));

    let ast = parser.parse(MOD_NAME, MOD_NAME, "true").unwrap();
    assert_eq!(ast, Lit::Boolean(true));

    let ast = parser.parse(MOD_NAME, MOD_NAME, "false").unwrap();
    assert_eq!(ast, Lit::Boolean(false));

    parser
        .parse(
            MOD_NAME,
            MOD_NAME,
            r#""Invalid
    
    ""#,
        )
        .unwrap_err();

    let ast = parser.parse(MOD_NAME, MOD_NAME, "124").unwrap();
    assert_eq!(ast, Lit::Integer(124));

    parser.parse(MOD_NAME, MOD_NAME, "12e3").unwrap_err();
}

#[test]
fn test_unary() {
    let parser = ExprParser::new();

    let ast = parser.parse(MOD_NAME, MOD_NAME, "-2342").unwrap();
    assert_eq!(
        *ast,
        Expr::Unary(Operation::Neg, Box::new(Expr::Literal(Lit::Integer(2342))))
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "!2342").unwrap();
    assert_eq!(
        *ast,
        Expr::Unary(
            Operation::LogicalNot,
            Box::new(Expr::Literal(Lit::Integer(2342)))
        )
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "~2342").unwrap();
    assert_eq!(
        *ast,
        Expr::Unary(Operation::Not, Box::new(Expr::Literal(Lit::Integer(2342))))
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "1 - -2342").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Sub,
            Box::new(Expr::Literal(Lit::Integer(1))),
            Box::new(Expr::Unary(
                Operation::Neg,
                Box::new(Expr::Literal(Lit::Integer(2342)))
            ))
        )
    );
}

#[test]
fn test_binary() {
    let parser = ExprParser::new();

    let ast = parser.parse(MOD_NAME, MOD_NAME, "12*7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Mul,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "12/7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Div,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "12%7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Mod,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "12+7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Add,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "12-7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Sub,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = parser.parse(MOD_NAME, MOD_NAME, "12&7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::And,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );
}
