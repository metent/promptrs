use super::client::Function;
use super::{Delims, Segment, ToolDelims};
use winnow::combinator::{alt, opt, repeat};
use winnow::error::ParserError;
use winnow::token::{rest, take_until};
use winnow::{ModalResult, Parser};

/// Parse assistant response containing JSONschema-based tool calls into structured components
pub fn parse(
	input: &mut &str,
	delims: Option<&Delims>,
	tool_delims: Option<&ToolDelims>,
) -> ModalResult<Vec<Segment>> {
	let mut segments = Vec::new();
	if let Some(rdelims) = delims.as_ref().map(|del| &del.reasoning) {
		let reasoning = opt(between(rdelims))
			.map(|s: Option<&str>| s.map(|s| Segment::Reasoning(s.into())))
			.parse_next(input)?;
		segments.extend(reasoning.into_iter());
	}

	let adelims = delims.as_ref().and_then(|del| del.content.as_ref());
	let tdelims = tool_delims.as_ref().map(|del| &del.tool_call);

	match (adelims, tdelims) {
		(Some(adel), Some(tdel)) => repeat(
			0..,
			alt((
				between(&adel).map(|ans| Segment::Answer(ans.into())),
				between(&tdel).map(|tc| Segment::ToolCall(parse_json(tc))),
			)),
		)
		.parse_next(input)?,
		(Some(adel), None) => repeat(
			0..,
			alt((
				between(&adel).map(|ans| Segment::Answer(ans.into())),
				take_until(0.., adel.0.as_str()).map(|cmt: &str| Segment::Commentary(cmt.into())),
			)),
		)
		.parse_next(input)?,
		(None, Some(tdel)) => repeat(
			0..,
			alt((
				between(&tdel).map(|tc| Segment::ToolCall(parse_json(tc))),
				between(&("\n```".into(), "\n```".into())).map(parse_block),
				take_until(0.., (tdel.0.as_str(), "\n```"))
					.map(|cmt: &str| Segment::Commentary(cmt.into())),
			)),
		)
		.parse_next(input)?,
		(None, None) => repeat(
			0..,
			alt((
				between(&("\n```".into(), "\n```".into())).map(parse_block),
				take_until(0.., "\n```").map(|cmt: &str| Segment::Commentary(cmt.into())),
			)),
		)
		.parse_next(input)?,
	}

	Ok(segments)
}

fn parse_block(s: &str) -> Segment {
	let (lang, block) = s.split_once("\n").unzip();
	Segment::CodeBlock(
		lang.and_then(|l| l.len().ne(&0).then_some(l.into())),
		block.unwrap_or(s).into(),
	)
}

fn between<'s, E: ParserError<&'s str>>(
	(start, end): &(String, String),
) -> impl Parser<&'s str, &'s str, E> {
	|input: &mut &'s str| {
		let (mut start, mut end) = (start.as_str(), end.as_str());
		_ = take_until(0.., start).parse_next(input)?;
		_ = start.parse_next(input)?;
		let between = alt((take_until(0.., end), rest)).parse_next(input)?;
		_ = end.parse_next(input)?;
		Ok(between)
	}
}

fn parse_json(json: &str) -> Vec<Function> {
	if let Ok(tc) = serde_json::from_str(json) {
		tc
	} else if let Ok(tc) = serde_json::from_str(json) {
		vec![tc]
	} else {
		vec![Function {
			name: "".into(),
			arguments: serde_json::Map::new(),
		}]
	}
}
