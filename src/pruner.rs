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
			Message::ToolCall((req, res)) => req.len() + res.len(),
			Message::Status((req, res)) => req.len() + res.len(),
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
