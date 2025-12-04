use super::{Delims, Segment};
use winnow::combinator::{alt, eof, opt, repeat_till};
use winnow::error::ParserError;
use winnow::token::{rest, take_until};
use winnow::{ModalResult, Parser};

/// Parse assistant response containing JSONschema-based tool calls into structured components
pub fn parse(input: &mut &str, delims: Option<&Delims>) -> ModalResult<Vec<Segment>> {
	let mut segments = Vec::new();
	if let Some(rdelims) = delims.as_ref().map(|del| &del.reasoning) {
		let reasoning = opt((take_until(0.., rdelims.0.as_str()), between(rdelims)))
			.map(|s: Option<(_, &str)>| s.map(|(_, s)| Segment::Reasoning(s.into())))
			.parse_next(input)?;
		segments.extend(reasoning);
	}

	let start = match delims.as_ref().and_then(|del| del.content.as_ref()) {
		Some(adel) => {
			opt(between(adel).map(|ans| Segment::Answer(ans.into()))).parse_next(input)?
		}
		None => opt(between(&("```".into(), "\n```".into())).map(parse_block)).parse_next(input)?,
	};
	segments.extend(start);

	let (mid, _) = match delims.as_ref().and_then(|del| del.content.as_ref()) {
		Some(adel) => repeat_till(
			0..,
			alt((
				between(adel).map(|ans| Segment::Answer(ans.into())),
				take_until(1.., adel.0.as_str()).map(|cmt: &str| Segment::Commentary(cmt.into())),
				rest.map(|cmt: &str| Segment::Commentary(cmt.into())),
			)),
			eof,
		)
		.parse_next(input)?,
		None => repeat_till(
			0..,
			alt((
				between(&("\n```".into(), "\n```".into())).map(parse_block),
				take_until(1.., "\n```").map(|cmt: &str| Segment::Commentary(cmt.into())),
				rest.map(|cmt: &str| Segment::Commentary(cmt.into())),
			)),
			eof,
		)
		.parse_next(input)?,
	};
	segments.extend::<Vec<_>>(mid);

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
		_ = start.parse_next(input)?;
		let between = alt((take_until(0.., end), rest)).parse_next(input)?;
		_ = end.parse_next(input)?;
		Ok(between)
	}
}
