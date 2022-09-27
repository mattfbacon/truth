#![deny(
	absolute_paths_not_starting_with_crate,
	future_incompatible,
	keyword_idents,
	macro_use_extern_crate,
	meta_variable_misuse,
	missing_abi,
	missing_copy_implementations,
	non_ascii_idents,
	nonstandard_style,
	noop_method_call,
	pointer_structural_match,
	private_in_public,
	rust_2018_idioms,
	unused_qualifications
)]
#![warn(clippy::pedantic)]
#![allow(clippy::let_underscore_drop)]
#![forbid(unsafe_code)]

use std::collections::{BTreeSet, HashMap};
use std::fmt::{self, Display, Formatter};
use std::ops::Range;

use chumsky::prelude::*;
use indexmap::IndexSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum UnaryOperator {
	Not,
}

impl UnaryOperator {
	fn apply(self, b: bool) -> bool {
		match self {
			Self::Not => !b,
		}
	}
}

impl Display for UnaryOperator {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
		formatter.write_str(match self {
			Self::Not => r"\neg",
		})
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinaryOperator {
	And,
	Or,
	Xor,
	If,
	IfAndOnlyIf,
	Nand,
	Nor,
}

impl BinaryOperator {
	fn apply(self, a: bool, b: bool) -> bool {
		match self {
			Self::And => a & b,
			Self::Or => a | b,
			Self::Xor => a ^ b,
			Self::If => !a | b,
			Self::IfAndOnlyIf => a == b,
			Self::Nand => !(a & b),
			Self::Nor => !(a | b),
		}
	}
}

impl Display for BinaryOperator {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
		formatter.write_str(match self {
			Self::And => r"\wedge",
			Self::Or => r"\vee",
			Self::Xor => r"\oplus",
			Self::If => r"\rightarrow",
			Self::IfAndOnlyIf => r"\leftrightarrow",
			Self::Nand => r"\mid",
			Self::Nor => r"\downarrow",
		})
	}
}

type Span = Range<usize>;
type Error = Simple<char, Span>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ast {
	Proposition(String),
	Unary(UnaryOperator, Box<Self>),
	Binary(BinaryOperator, Box<(Self, Self)>),
}

impl Ast {
	fn split_for_table<'a, I: Iterator<Item = &'a Self> + DoubleEndedIterator>(
		asts: impl IntoIterator<IntoIter = I>,
	) -> (BTreeSet<&'a str>, IndexSet<&'a Self>) {
		let mut fragments = IndexSet::new();
		let mut propositions = BTreeSet::<&str>::new();

		for ast in asts.into_iter().rev() {
			fragments.insert(ast);
			let mut stack: Vec<_> = vec![ast];
			while let Some(node) = stack.pop() {
				match node {
					Self::Proposition(proposition) => {
						propositions.insert(proposition.as_str());
					}
					Self::Unary(_op, child) => {
						fragments.insert(node);
						stack.push(child);
					}
					Self::Binary(_op, children) => {
						fragments.insert(node);
						stack.extend([&children.0, &children.1]);
					}
				}
			}
		}

		(propositions, fragments)
	}

	fn evaluate(&self, context: &HashMap<&str, bool>) -> bool {
		match self {
			Self::Proposition(proposition) => context[proposition.as_str()],
			Self::Unary(op, child) => op.apply(child.evaluate(context)),
			Self::Binary(op, children) => {
				op.apply(children.0.evaluate(context), children.1.evaluate(context))
			}
		}
	}
}

impl Display for Ast {
	fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::Binary(op, children) => {
				let (left, right) = &**children;
				if matches!(left, Self::Binary(..)) {
					write!(formatter, r"\left({left}\right)")?;
				} else {
					write!(formatter, "{left}")?;
				}
				write!(formatter, " {op} ")?;
				if matches!(right, Self::Binary(..)) {
					write!(formatter, r"\left({right}\right)")?;
				} else {
					write!(formatter, "{right}")?;
				}
				Ok(())
			}
			Self::Unary(op, child) => {
				write!(formatter, "{op} ")?;
				let child: &Self = child;
				if matches!(child, Self::Binary(..)) {
					write!(formatter, r"\left({child}\right)")?;
				} else {
					write!(formatter, "{child}")?;
				}
				Ok(())
			}
			Self::Proposition(proposition) => formatter.write_str(proposition),
		}
	}
}

fn binary_op() -> impl Parser<char, BinaryOperator, Error = Error> {
	choice((
		just("&").to(BinaryOperator::And),
		just("|").to(BinaryOperator::Or),
		just("^").to(BinaryOperator::Xor),
		just("->").to(BinaryOperator::If),
		just("<->").to(BinaryOperator::IfAndOnlyIf),
		just("NAND").to(BinaryOperator::Nand),
		just("NOR").to(BinaryOperator::Nor),
	))
	.padded()
}

fn unary_op() -> impl Parser<char, UnaryOperator, Error = Error> {
	just("!").to(UnaryOperator::Not).padded()
}

fn parser() -> impl Parser<char, Ast, Error = Error> {
	recursive(|expr0| {
		let expr2 = choice((
			text::ident().map(Ast::Proposition),
			expr0.clone().delimited_by(just("("), just(")")).padded(),
		))
		.padded();
		let expr1 = || {
			unary_op()
				.then(expr2.clone())
				.map(|(op, child)| Ast::Unary(op, Box::new(child)))
				.or(expr2.clone())
		};
		expr1()
			.then(binary_op().then(expr1()).map(Some).or(empty().to(None)))
			.map(|(left, right)| match right {
				Some((op, right)) => Ast::Binary(op, Box::new((left, right))),
				None => left,
			})
	})
}

fn main() {
	let exprs: Vec<_> = std::env::args()
		.skip(1)
		.map(|expr| parser().parse(expr.as_str()).expect("parsing failed"))
		.collect();

	let (propositions, fragments) = Ast::split_for_table(&exprs);

	let num_propositions = propositions.len();
	let mut counter = (1 << num_propositions) - 1; // `num_propositions` 1s
	let mut context = HashMap::new();

	print_header(
		propositions.iter().copied(),
		fragments.iter().rev().copied(),
	);

	while counter >= 0 {
		context.extend(propositions.iter().copied().enumerate().map(|(idx, key)| {
			let offset = num_propositions - idx - 1;
			let bit = counter & (1 << offset);
			(key, bit > 0)
		}));

		print_row(
			propositions.iter().copied(),
			fragments.iter().rev().copied(),
			&context,
		);

		counter -= 1;
	}

	print_footer();
}

fn print_header<'a>(
	propositions: impl Iterator<Item = &'a str> + ExactSizeIterator,
	fragments: impl Iterator<Item = &'a Ast> + ExactSizeIterator,
) {
	let num_propositions = propositions.len();
	let num_fragments = fragments.len();

	print!(r"\begin{{tabular}}{{");
	for _ in 0..num_propositions {
		print!("c ");
	}
	print!("| ");
	for _ in 0..num_fragments {
		print!("c ");
	}
	println!(r"}}");

	for proposition in propositions {
		print!("${proposition}$ & ");
	}
	for (i, fragment) in fragments.enumerate() {
		let repr = fragment.to_string();
		print!("${}$", repr);
		if i == num_fragments - 1 {
			println!(r" \\");
		} else {
			print!(" & ");
		}
	}
}

fn print_row<'a>(
	propositions: impl Iterator<Item = &'a str>,
	fragments: impl Iterator<Item = &'a Ast> + ExactSizeIterator,
	context: &HashMap<&str, bool>,
) {
	let num_fragments = fragments.len();

	for proposition in propositions {
		print!("{} & ", tf(context[proposition]));
	}
	for (i, fragment) in fragments.enumerate() {
		print!("{}", tf(fragment.evaluate(context)));
		if i == num_fragments - 1 {
			println!(r" \\");
		} else {
			print!(" & ");
		}
	}
}

fn tf(b: bool) -> &'static str {
	if b {
		"T"
	} else {
		"F"
	}
}

fn print_footer() {
	println!(r"\end{{tabular}}");
}
