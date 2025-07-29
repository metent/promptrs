# promptrs

This crate provides a framework for building agentic workflows that integrate language models
with tool calling capabilities. It handles conversation management, API interactions, tool
invocation, and response parsing in a streamlined, composable manner. The core design
philosophy is to enable fluent method chaining while maintaining minimal state management.

## Features

- Streamed API communication with incremental message processing
- Multi-tool support with function call validation and error handling
- Configurable delimiters for custom message formatting
- Automated message pruning for context size management
- JSON schema generation for tool documentation
- Pythonic tool calling support

## Usage Overview

1. **Configure API**: Set up model parameters and credentials
2. **Define Tools**: Create functions with `#[tool]` attribute
3. **Build Workflow**: Chain configuration and tool registration
4. **Process Messages**: Handle user input through fluent API
5. **Handle Responses**: Manage tool calls and state updates

## Example: Basic Workflow

```rust
use promptrs::{Delims, ToolCallParadigm, UserConfig, tool};
use std::collections::HashMap;

#[derive(Default)]
struct AppState {
    notes: HashMap<String, String>,
}

/// Tool implementation for note-taking
/// id: Unique ID for note
/// title: Note title
#[tool]
fn add_note(state: &mut HashMap, id: String, title: String) -> String {
    // Actual implementation would store the note
    format!("Note {id} added with title: {title}")
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Configure language model
    let config = UserConfig::builder()
        .api_key(Some("sk-examplekey".to_string()))
        .temperature(Some(0.3))
        .paradigm(ToolCallParadigm::Pythonic {
            reasoning_delims: None,
        })
        .char_limit(8192)
        .build("https://api.openai.com".to_string(), "gpt-4".to_string());

    // Initialize conversation state
    let mut state = AppState::default();

    // Setup conversation with system prompt and tools
    let init = config
        .system(Some(
            "You are an assistant that helps organize notes. Use add_note for new notes.",
        ))
        .tool(add_note);

    // Process user interaction
    let response = init
        .user("Create a note about meeting agenda".to_string())
        .process(&mut state)?;

    println!("Assistant response: {}", response.text);

    // Continue conversation
    let next = response
        .user("Add a note for 'Creating promptrs'".to_string())
        .process(&mut state)?;

    println!("Follow-up response: {}", next.text);

    Ok(())
}
```
