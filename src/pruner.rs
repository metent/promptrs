use crate::client::Function;

use super::Message;

/// Remove oldest messages in order to fit the message history length within
/// a specified byte limit
pub fn prune(messages: Vec<Message>, size: u64) -> Vec<Message> {
	let pruned = messages.iter().skip(1).rev().scan(0, |acc, msg| {
		if *acc > size as usize {
			return None;
		}
		*acc += match msg {
			Message::System(content) => content.len(),
			Message::User(content) => content.len(),
			Message::Assistant(content) => content.len(),
			Message::ToolCall((func, res)) => function_len(func) + res.len(),
			Message::Status((func, res)) => function_len(func) + res.len(),
		};
		Some(msg)
	});

	let mut messages = if let Some(pos) = pruned.clone().position(is_status) {
		pruned
			.clone()
			.take(pos + 1)
			.chain(pruned.skip(pos + 1).filter(|msg| !is_status(msg)))
			.chain(messages.iter().take(1))
			.cloned()
			.collect::<Vec<_>>()
	} else {
		pruned.chain(messages.iter().take(1)).cloned().collect()
	};

	messages.reverse();
	messages
}

fn is_status(msg: &Message) -> bool {
	matches!(msg, Message::Status(_))
}

fn function_len(Function { name, arguments }: &Function) -> usize {
	name.len()
		+ arguments
			.iter()
			.fold(0, |acc, (k, v)| acc + k.len() + v.to_string().len())
}
