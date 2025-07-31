use super::client::Function;
use super::{Delims, Response, ToolDelims};
use serde::Deserialize;
use std::str::FromStr;
use winnow::ascii::{multispace0, multispace1};
use winnow::combinator::{
	alt, delimited, opt, preceded, repeat, separated, separated_foldl1, seq, terminated,
};
use winnow::error::{FromExternalError, ParserError};
use winnow::stream::{Accumulate, AsChar};
use winnow::token::{one_of, rest, take_till, take_until, take_while};
use winnow::{ModalResult, Parser, Result};

/// Parse assistant response containing JSONschema-based tool calls into structured components
pub fn parse(
	input: &mut &str,
	delims: &Option<Delims>,
	tool_delims: Option<&ToolDelims>,
) -> ModalResult<Response> {
	let mut reasoning = None;
	if let Some(rdelims) = delims.as_ref().map(|del| &del.reasoning) {
		reasoning = opt(between(&rdelims))
			.map(|s: Option<&str>| s.map(|s| s.into()))
			.parse_next(input)?;
	}

	let content = match delims.as_ref().and_then(|del| del.content.as_ref()) {
		Some(adelims) => between(&adelims).parse_next(input)?,
		None => match tool_delims {
			Some(del) => alt((take_until(0.., del.tool_call.0.as_str()), rest))
				.map(|s: &str| s.into())
				.parse_next(input)?,
			None => rest.parse_next(input)?,
		},
	}
	.into();

	let mut tool_calls = Vec::new();
	if let Some(delims) = tool_delims.as_ref().map(|del| &del.tool_call) {
		tool_calls = repeat(0.., between(&delims))
			.map(parse_json)
			.parse_next(input)?;
	}

	Ok(Response {
		reasoning,
		content,
		tool_calls,
	})
}

fn between<'s, E: ParserError<&'s str>>(
	(start, end): &(String, String),
) -> impl Parser<&'s str, &'s str, E> {
	|input: &mut &'s str| {
		let (mut start, mut end) = (start.as_str(), end.as_str());
		_ = take_until(0.., start).parse_next(input)?;
		_ = start.parse_next(input)?;
		let between = take_until(0.., end).parse_next(input)?;
		_ = end.parse_next(input)?;
		Ok(between)
	}
}

fn parse_json(list: Vec<&str>) -> Vec<Function> {
	list.into_iter()
		.flat_map(|tc| {
			match serde_json::from_str(tc).unwrap_or(ToolCallSegment::One(Function {
				name: "".into(),
				arguments: serde_json::Map::new(),
			})) {
				ToolCallSegment::One(def) => vec![def],
				ToolCallSegment::Many(defs) => defs,
			}
		})
		.collect()
}

#[derive(Deserialize)]
#[serde(untagged)]
enum ToolCallSegment {
	One(Function),
	Many(Vec<Function>),
}

/// Parse assistant response containing pythonic tool calls into structured components
pub fn parse_py(input: &mut &str, delims: &Option<Delims>) -> ModalResult<Response> {
	let mut reasoning = None;
	if let Some(rdelims) = delims.as_ref().map(|del| &del.reasoning) {
		reasoning = opt(between(&rdelims))
			.map(|s: Option<&str>| s.map(|s| s.into()))
			.parse_next(input)?;
	}

	let content = match delims.as_ref().and_then(|del| del.content.as_ref()) {
		Some(adelims) => between(&adelims).parse_next(input)?,
		None => alt((take_until(0.., "```py"), rest))
			.map(|s: &str| s.into())
			.parse_next(input)?,
	}
	.into();

	_ = ("```py", opt("thon")).parse_next(input)?;
	let tool_calls = separated_foldl1(
		repeat(0.., alt((py_func_call.map(Some), py_comment.map(|_| None)))).fold(
			Vec::new,
			|mut acc, s| {
				acc.extend(s.into_iter());
				acc
			},
		),
		("```", take_until(0.., "```py"), "```py", opt("thon")),
		|mut l: Vec<Function>, _, r| {
			l.extend(r);
			l
		},
	)
	.parse_next(input)?;
	Ok(Response {
		reasoning,
		content,
		tool_calls,
	})
}

pub fn py_comment(input: &mut &str) -> ModalResult<()> {
	alt((
		(repeat::<_, _, (), _, _>(0.., " "), "\n").void(),
		("#", take_until(0.., "\n"), "\n").void(),
	))
	.parse_next(input)
}

fn py_func_call(input: &mut &str) -> ModalResult<Function> {
	_ = multispace0(input)?;
	let name = identifier(input)?.into();
	_ = multispace0(input)?;
	let mut i = 0;
	_ = "(".parse_next(input)?;
	let Map(arguments) = separated(
		0..,
		seq!(
			_: multispace0,
			opt(seq!(identifier, _: multispace0, _: "=")).map(|id| {
				id.map_or_else(|| {
					i += 1;
					(i - 1).to_string()
				}, |(id,)| id.to_string())
			}),
			_: multispace0,
			alt((string, float, hexadecimal, octal, binary, decimal, "None".map(|_| serde_json::Value::Null))),
			_: multispace0,
		),
		",",
	)
	.parse_next(input)?;
	_ = multispace0(input)?;
	_ = ")".parse_next(input)?;
	Ok(Function { name, arguments })
}

fn identifier<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
	(
		one_of(|c: char| c.is_alpha() || c == '_'),
		take_while(0.., |c: char| c.is_alphanum() || c == '_'),
	)
		.take()
		.parse_next(input)
}

fn string<'a, E>(input: &mut &'a str) -> Result<serde_json::Value, E>
where
	E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
	alt((
		delimited('"', build_string, '"'),
		delimited("'", build_string, "'"),
	))
	.parse_next(input)
	.map(serde_json::Value::from)
}

fn build_string<'a, E>(input: &mut &'a str) -> Result<String, E>
where
	E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
	repeat(0.., parse_fragment)
		.fold(String::new, |mut string, fragment| {
			match fragment {
				StringFragment::Literal(s) => string.push_str(s),
				StringFragment::EscapedChar(c) => string.push(c),
				StringFragment::EscapedWS => {}
			}
			string
		})
		.parse_next(input)
}

fn hexadecimal(input: &mut &str) -> ModalResult<serde_json::Value> {
	preceded(
		alt(("0x", "0X")),
		repeat(
			1..,
			terminated(
				one_of(('0'..='9', 'a'..='f', 'A'..='F')),
				repeat(0.., '_').map(|()| ()),
			),
		)
		.map(|()| ())
		.take(),
	)
	.try_map(|out: &str| i64::from_str_radix(&str::replace(out, "_", ""), 16))
	.map(serde_json::Value::from)
	.parse_next(input)
}

fn octal(input: &mut &str) -> ModalResult<serde_json::Value> {
	preceded(
		alt(("0o", "0O")),
		repeat(
			1..,
			terminated(one_of('0'..='7'), repeat(0.., '_').map(|()| ())),
		)
		.map(|()| ())
		.take(),
	)
	.try_map(|out: &str| i64::from_str_radix(&str::replace(out, "_", ""), 8))
	.map(serde_json::Value::from)
	.parse_next(input)
}

fn binary(input: &mut &str) -> ModalResult<serde_json::Value> {
	preceded(
		alt(("0b", "0B")),
		repeat(
			1..,
			terminated(one_of('0'..='1'), repeat(0.., '_').map(|()| ())),
		)
		.map(|()| ())
		.take(),
	)
	.try_map(|out: &str| i64::from_str_radix(&str::replace(out, "_", ""), 2))
	.map(serde_json::Value::from)
	.parse_next(input)
}

fn decimal(input: &mut &str) -> ModalResult<serde_json::Value> {
	repeat(
		1..,
		terminated(one_of('0'..='9'), repeat(0.., '_').map(|()| ())),
	)
	.map(|()| ())
	.take()
	.try_map(|out: &str| str::replace(out, "_", "").parse::<i64>())
	.map(serde_json::Value::from)
	.parse_next(input)
}

fn float(input: &mut &str) -> ModalResult<serde_json::Value> {
	alt((
		(
			'.',
			decimal_str,
			opt((one_of(['e', 'E']), opt(one_of(['+', '-'])), decimal_str)),
		)
			.take(),
		(
			decimal_str,
			opt(preceded('.', decimal_str)),
			one_of(['e', 'E']),
			opt(one_of(['+', '-'])),
			decimal_str,
		)
			.take(),
		(decimal_str, '.', opt(decimal_str)).take(),
	))
	.try_map(f64::from_str)
	.map(serde_json::Value::from)
	.parse_next(input)
}

fn decimal_str<'s>(input: &mut &'s str) -> ModalResult<&'s str> {
	repeat(
		1..,
		terminated(one_of('0'..='9'), repeat(0.., '_').map(|()| ())),
	)
	.map(|()| ())
	.take()
	.parse_next(input)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
	Literal(&'a str),
	EscapedChar(char),
	EscapedWS,
}

fn parse_fragment<'a, E>(input: &mut &'a str) -> Result<StringFragment<'a>, E>
where
	E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
	alt((
		parse_literal.map(StringFragment::Literal),
		parse_escaped_char.map(StringFragment::EscapedChar),
		parse_escaped_whitespace.value(StringFragment::EscapedWS),
	))
	.parse_next(input)
}

fn parse_literal<'a, E: ParserError<&'a str>>(input: &mut &'a str) -> Result<&'a str, E> {
	let not_quote_slash = take_till(1.., ['"', '\\']);

	not_quote_slash
		.verify(|s: &str| !s.is_empty())
		.parse_next(input)
}

fn parse_escaped_char<'a, E>(input: &mut &'a str) -> Result<char, E>
where
	E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
	preceded(
		'\\',
		alt((
			parse_unicode,
			'n'.value('\n'),
			'r'.value('\r'),
			't'.value('\t'),
			'b'.value('\u{08}'),
			'f'.value('\u{0C}'),
			'\\'.value('\\'),
			'/'.value('/'),
			'"'.value('"'),
		)),
	)
	.parse_next(input)
}

fn parse_unicode<'a, E>(input: &mut &'a str) -> Result<char, E>
where
	E: ParserError<&'a str> + FromExternalError<&'a str, std::num::ParseIntError>,
{
	let parse_hex = take_while(1..=6, |c: char| c.is_ascii_hexdigit());
	let parse_delimited_hex = preceded('u', delimited('{', parse_hex, '}'));
	let parse_u32 = parse_delimited_hex.try_map(move |hex| u32::from_str_radix(hex, 16));
	parse_u32.verify_map(std::char::from_u32).parse_next(input)
}

fn parse_escaped_whitespace<'a, E: ParserError<&'a str>>(
	input: &mut &'a str,
) -> Result<&'a str, E> {
	preceded('\\', multispace1).parse_next(input)
}

struct Map(serde_json::Map<String, serde_json::Value>);

impl Accumulate<(String, serde_json::Value)> for Map {
	fn initial(_capacity: Option<usize>) -> Self {
		Map(serde_json::Map::new())
	}
	fn accumulate(&mut self, (key, value): (String, serde_json::Value)) {
		self.0.insert(key, value);
	}
}
