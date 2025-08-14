use super::{Delims, Segment};
use winnow::combinator::{alt, opt, repeat};
use winnow::error::ParserError;
use winnow::token::{rest, take_until};
use winnow::{ModalResult, Parser};

/// Parse assistant response containing JSONschema-based tool calls into structured components
pub fn parse(input: &mut &str, delims: Option<&Delims>) -> ModalResult<Vec<Segment>> {
	let mut segments = Vec::new();
	if let Some(rdelims) = delims.as_ref().map(|del| &del.reasoning) {
		let reasoning = opt(between(rdelims))
			.map(|s: Option<&str>| s.map(|s| Segment::Reasoning(s.into())))
			.parse_next(input)?;
		segments.extend(reasoning.into_iter());
	}

	match delims.as_ref().and_then(|del| del.content.as_ref()) {
		Some(adel) => repeat(
			0..,
			alt((
				between(&adel).map(|ans| Segment::Answer(ans.into())),
				take_until(0.., adel.0.as_str()).map(|cmt: &str| Segment::Commentary(cmt.into())),
			)),
		)
		.parse_next(input)?,
		None => repeat(
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
