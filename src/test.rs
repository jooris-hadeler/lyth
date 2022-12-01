use crate::{
    ast::{Expr, Lit, Operation},
    parser::ExprParser,
};

const MOD_NAME: &'static str = "<test>";

#[test]
fn test_literal() {
    let ast = ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, r#""Hello, World!""#)
        .unwrap();
    assert_eq!(
        *ast,
        Expr::Literal(Lit::String(r"Hello, World!".to_string()))
    );

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "true").unwrap();
    assert_eq!(*ast, Expr::Literal(Lit::Boolean(true)));

    let ast = ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, "false")
        .unwrap();
    assert_eq!(*ast, Expr::Literal(Lit::Boolean(false)));

    ExprParser::new()
        .parse(
            MOD_NAME,
            MOD_NAME,
            r#""Invalid
    
    ""#,
        )
        .unwrap_err();

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "124").unwrap();
    assert_eq!(*ast, Expr::Literal(Lit::Integer(124)));

    ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, "12e3")
        .unwrap_err();
}

#[test]
fn test_unary() {
    let ast = ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, "-2342")
        .unwrap();
    assert_eq!(
        *ast,
        Expr::Unary(Operation::Sub, Box::new(Expr::Literal(Lit::Integer(2342))))
    );

    let ast = ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, "!2342")
        .unwrap();
    assert_eq!(
        *ast,
        Expr::Unary(
            Operation::LogicalNot,
            Box::new(Expr::Literal(Lit::Integer(2342)))
        )
    );

    let ast = ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, "~2342")
        .unwrap();
    assert_eq!(
        *ast,
        Expr::Unary(Operation::Not, Box::new(Expr::Literal(Lit::Integer(2342))))
    );

    let ast = ExprParser::new()
        .parse(MOD_NAME, MOD_NAME, "1 - -2342")
        .unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Sub,
            Box::new(Expr::Literal(Lit::Integer(1))),
            Box::new(Expr::Unary(
                Operation::Sub,
                Box::new(Expr::Literal(Lit::Integer(2342)))
            ))
        )
    );
}

#[test]
fn test_binary() {
    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "12*7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Mul,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "12/7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Div,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "12%7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Mod,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "12+7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Add,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "12-7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::Sub,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );

    let ast = ExprParser::new().parse(MOD_NAME, MOD_NAME, "12&7").unwrap();
    assert_eq!(
        *ast,
        Expr::Binary(
            Operation::And,
            Box::new(Expr::Literal(Lit::Integer(12))),
            Box::new(Expr::Literal(Lit::Integer(7)))
        )
    );
}
