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
use std::ops::Range;

use chumsky::prelude::*;
use indexmap::{indexset, IndexSet};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinaryOperator {
	And,
	Or,
	Xor,
	If,
	IfAndOnlyIf,
}

impl BinaryOperator {
	fn apply(self, a: bool, b: bool) -> bool {
		match self {
			Self::And => a & b,
			Self::Or => a | b,
			Self::Xor => a ^ b,
			Self::If => !a | b,
			Self::IfAndOnlyIf => a == b,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ast {
	Proposition(Range<usize>, String),
	Unary(Range<usize>, UnaryOperator, Box<Self>),
	Binary(Range<usize>, BinaryOperator, Box<(Self, Self)>),
}

impl Ast {
	fn split_for_table(&self) -> (BTreeSet<&str>, IndexSet<&Self>) {
		let mut propositions = BTreeSet::<&str>::new();
		let mut fragments = indexset![self];
		let mut stack = vec![self];
		while let Some(node) = stack.pop() {
			match node {
				Self::Proposition(_span, proposition) => {
					propositions.insert(proposition);
				}
				Self::Unary(_span, _op, child) => {
					fragments.insert(node);
					stack.push(child);
				}
				Self::Binary(_span, _op, children) => {
					fragments.insert(node);
					stack.extend([&children.0, &children.1]);
				}
			}
		}
		(propositions, fragments)
	}

	fn repr<'a>(&self, input: &'a str) -> &'a str {
		let span = match self {
			Self::Proposition(span, ..) | Self::Unary(span, ..) | Self::Binary(span, ..) => span,
		};
		&input[span.clone()]
	}

	fn evaluate(&self, context: &HashMap<&str, bool>) -> bool {
		match self {
			Self::Proposition(_span, proposition) => context[proposition.as_str()],
			Self::Unary(_span, op, child) => op.apply(child.evaluate(context)),
			Self::Binary(_span, op, children) => {
				op.apply(children.0.evaluate(context), children.1.evaluate(context))
			}
		}
	}
}

fn binary_op() -> impl Parser<char, BinaryOperator, Error = Simple<char>> {
	choice((
		just("&").to(BinaryOperator::And),
		just("|").to(BinaryOperator::Or),
		just("^").to(BinaryOperator::Xor),
		just("->").to(BinaryOperator::If),
		just("<->").to(BinaryOperator::IfAndOnlyIf),
	))
	.padded()
}

fn unary_op() -> impl Parser<char, UnaryOperator, Error = Simple<char>> {
	just("!").to(UnaryOperator::Not).padded()
}

fn parser() -> impl Parser<char, Ast, Error = Simple<char>> {
	recursive(|expr0| {
		let expr2 = choice((
			text::ident()
				.map_with_span(|name, span| Ast::Proposition(span, name))
				.padded(),
			expr0.clone().delimited_by(just("("), just(")")).padded(),
		));
		let expr1 = || {
			unary_op()
				.then(expr2.clone())
				.map_with_span(|(op, child), span| Ast::Unary(span, op, Box::new(child)))
				.or(expr2.clone())
		};
		expr1()
			.then(binary_op())
			.then(expr1())
			.map_with_span(|((left, op), right), span| Ast::Binary(span, op, Box::new((left, right))))
	})
}

fn main() {
	let expr = std::env::args()
		.nth(1)
		.expect("pass expression as the first argument");
	let parsed = parser()
		.parse(expr.as_str())
		.expect("parsing expression failed");

	let (propositions, fragments) = parsed.split_for_table();

	let num_propositions = propositions.len();
	let mut counter = (1 << num_propositions) - 1; // `num_propositions` 1s
	let mut context = HashMap::new();

	print_header(
		propositions.iter().copied(),
		fragments.iter().rev().copied(),
		&expr,
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
	input: &str,
) {
	let num_propositions = propositions.len();
	let num_fragments = fragments.len();

	print!(r"\begin{{tabular}}{{");
	for _ in 0..(num_propositions + num_fragments) {
		print!("c ");
	}
	println!(r"}}");

	for proposition in propositions {
		print!("${proposition}$ & ");
	}
	for (i, fragment) in fragments.enumerate() {
		let repr = fragment
			.repr(input)
			.replace('&', r"\wedge")
			.replace('|', r"\vee")
			.replace('^', r"\oplus")
			.replace('!', r"\neg")
			.replace("<->", r"\leftrightarrow") // must be before `->` replacement
			.replace("->", r"\rightarrow")
			.replace('(', r"\left( ")
			.replace(')', r" \right)");
		print!("${}$", repr);
		if i == num_fragments - 1 {
			println!(r"\\");
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
			println!(r"\\");
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
