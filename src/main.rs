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

type Span = Range<usize>;
type Error = Simple<char, Span>;

#[derive(Debug, Clone, derivative::Derivative)]
#[derivative(PartialEq, Eq, Hash)]
enum Ast<S> {
	Proposition(
		#[derivative(PartialEq = "ignore", Hash = "ignore")] S,
		String,
	),
	Unary(
		#[derivative(PartialEq = "ignore", Hash = "ignore")] S,
		UnaryOperator,
		Box<Self>,
	),
	Binary(
		#[derivative(PartialEq = "ignore", Hash = "ignore")] S,
		BinaryOperator,
		Box<(Self, Self)>,
	),
}

impl<S> Ast<S> {
	fn split_for_table<'a, I: Iterator<Item = &'a Ast<S>> + DoubleEndedIterator>(
		asts: impl IntoIterator<IntoIter = I>,
	) -> (BTreeSet<&'a str>, IndexSet<&'a Self>) {
		let mut fragments = IndexSet::new();
		let mut propositions = BTreeSet::<&str>::new();

		for ast in asts.into_iter().rev() {
			fragments.insert(ast);
			let mut stack: Vec<_> = vec![ast];
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
		}

		(propositions, fragments)
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

	fn map_span<S2>(self, mut f: impl FnMut(S) -> S2) -> Ast<S2> {
		fn go<S, S2>(ast: Ast<S>, f: &mut impl FnMut(S) -> S2) -> Ast<S2> {
			match ast {
				Ast::Proposition(span, proposition) => Ast::Proposition(f(span), proposition),
				Ast::Unary(span, op, child) => Ast::Unary(f(span), op, Box::new(go(*child, f))),
				Ast::Binary(span, op, children) => Ast::Binary(
					f(span),
					op,
					Box::new((go(children.0, f), go(children.1, f))),
				),
			}
		}
		go(self, &mut f)
	}
}

impl Ast<String> {
	fn repr(&self) -> &str {
		match self {
			Self::Proposition(span, ..) | Self::Unary(span, ..) | Self::Binary(span, ..) => span,
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

fn parser() -> impl Parser<char, Ast<Span>, Error = Error> {
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
	let exprs: Vec<_> = std::env::args()
		.skip(1)
		.map(|expr| (parser().parse(expr.as_str()).expect("parsing failed"), expr))
		.map(|(ast, raw)| {
			ast.map_span(|span| {
				raw[span]
					.replace('&', r"\wedge")
					.replace('|', r"\vee")
					.replace('^', r"\oplus")
					.replace('!', r"\neg")
					.replace("<->", r"\leftrightarrow") // must be before `->` replacement
					.replace("->", r"\rightarrow")
					.replace('(', r"\left( ")
					.replace(')', r" \right)")
					.replace("NAND", r"\mid ")
					.replace("NOR", r"\downarrow ")
			})
		})
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
	fragments: impl Iterator<Item = &'a Ast<String>> + ExactSizeIterator,
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
		let repr = fragment.repr();
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
	fragments: impl Iterator<Item = &'a Ast<String>> + ExactSizeIterator,
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
