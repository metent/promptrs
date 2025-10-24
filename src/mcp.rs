use crate::https::{TlsStream, build_http_request, parse_sse_body, read_http_response};
use serde::Deserialize;
use serde::de::DeserializeOwned;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::io;

/// A client for interacting with the MCP (Model Context Protocol) server.
///
/// This client allows initializing the connection, listing available tools, and
/// calling specific tools with arguments.
#[derive(Debug)]
pub struct McpClient {
	base_url: String,
	bearer_token: Option<String>,
	session_id: Option<String>,
}

impl McpClient {
	/// Creates a new MCP client with the given base URL.
	///
	/// # Arguments
	/// * `base_url` - The base URL of the MCP server.
	pub fn new(base_url: String) -> Self {
		Self {
			base_url: base_url,
			bearer_token: None,
			session_id: None,
		}
	}

	/// Sets the bearer token for authentication.
	///
	/// # Arguments
	/// * `bearer_token` - Optional bearer token for authentication.
	pub fn with_bearer_token(self, bearer_token: Option<String>) -> Self {
		Self {
			bearer_token,
			..self
		}
	}

	/// Sets the session ID for the client.
	///
	/// # Arguments
	/// * `session_id` - Optional session ID to use for this connection.
	pub fn with_session_id(self, session_id: Option<String>) -> Self {
		Self { session_id, ..self }
	}

	/// Sends a JSON-RPC request to the server.
	///
	/// This is an internal method used by other public methods.
	///
	/// # Generic
	/// * `T` - The type of the expected response, which must implement `DeserializeOwned`.
	///
	/// # Arguments
	/// * `payload` - The JSON payload to send.
	///
	/// # Returns
	/// * `Result<T, io::Error>` - The deserialized response or an error.
	async fn send_request<T: DeserializeOwned>(&mut self, payload: &Value) -> Result<T, io::Error> {
		let url = url::Url::parse(&self.base_url)
			.map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e.to_string()))?;
		let host = url
			.host_str()
			.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "missing host"))?;
		let port = url
			.port_or_known_default()
			.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "unknown port"))?;
		let path = url.path();

		let mut tls = TlsStream::connect(host, port).await?;
		let body = serde_json::to_vec(payload)?;
		let mut headers = vec![
			("Content-Type", "application/json"),
			("Accept", "application/json, text/event-stream"),
			("Connection", "close"),
		];
		if let Some(id) = &self.session_id {
			headers.push(("Mcp-Session-Id", id.as_str()));
		}
		let req = build_http_request(
			"POST",
			host,
			path,
			&body,
			&headers,
			self.bearer_token.as_ref().map(|tok| tok.as_str()),
		);
		tls.write_all(&req).await?;

		let (raw_body, headers) = read_http_response(&mut tls).await?;

		if let Some(v) = headers.get("mcp-session-id") {
			self.session_id = Some(v.clone());
		}

		parse_sse_body(raw_body)
	}

	/// Initializes the connection with the MCP server.
	///
	/// # Returns
	/// * `Result<Value, io::Error>` - The server's response, which is a JSON value.
	pub async fn initialize(&mut self) -> Result<Value, io::Error> {
		let payload = json!({
			"jsonrpc": "2.0",
			"method": "initialize",
			"id": 1,
			"params": {
				"protocolVersion": "2025-03-26",
				"capabilities": {},
				"clientInfo": { "name": "test-client", "version": "1.0.0" }
			}
		});
		self.send_request(&payload).await
	}

	/// Lists all available tools on the server.
	///
	/// # Returns
	/// * `Result<ListToolsResult, io::Error>` - The list of tools or an error.
	pub async fn list_tools(&mut self) -> Result<ListToolsResult, io::Error> {
		let payload = json!({
			"jsonrpc": "2.0",
			"method": "tools/list",
			"id": 2,
			"params": {}
		});
		let resp = self
			.send_request::<JsonRpcResult<ListToolsResult>>(&payload)
			.await?;
		Ok(resp.result)
	}

	/// Calls a specific tool with the given arguments.
	///
	/// # Arguments
	/// * `name` - The name of the tool to call.
	/// * `args` - The arguments to pass to the tool as a JSON value.
	///
	/// # Returns
	/// * `Result<CallToolResult, io::Error>` - The tool's response or an error.
	pub async fn call_tool(
		&mut self,
		name: &str,
		args: &Value,
	) -> Result<CallToolResult, io::Error> {
		let payload = json!({
			"jsonrpc": "2.0",
			"method": "tools/call",
			"id": 3,
			"params": { "name": name, "arguments": args }
		});
		let resp = self
			.send_request::<JsonRpcResult<CallToolResult>>(&payload)
			.await?;
		Ok(resp.result)
	}
}

/// Represents the JSON-RPC response from the server.
///
/// # Generic
/// * `T` - The type of the result data.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JsonRpcResult<T> {
	/// The JSON-RPC protocol version (should be "2.0").
	pub jsonrpc: String,
	/// The request ID that matches the original request.
	pub id: u32,
	/// The result data returned by the server.
	pub result: T,
}

/// The result from the `tools/list` method, containing information about available tools.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ListToolsResult {
	/// The array of available tools.
	pub tools: Vec<ToolInfo>,
	/// Next-page cursor when paging is supported (may be `None` if not supported).
	pub next_cursor: Option<String>,
	/// Additional fields that may be present in the server's response.
	#[serde(flatten)]
	pub others: HashMap<String, serde_json::Value>,
	/// Meta information about the response.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, serde_json::Value>>,
}

/// Detailed information about a single tool available on the server.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolInfo {
	/// The unique name of the tool.
	pub name: String,
	/// Optional display title for the tool.
	pub title: Option<String>,
	/// Optional description of what the tool does.
	pub description: Option<String>,
	/// The input schema that defines what parameters the tool accepts.
	pub input_schema: Schema,
	/// Optional output schema that defines what the tool returns.
	pub output_schema: Option<Schema>,
	/// Optional annotations that may provide UI hints or other metadata.
	pub annotations: Option<ToolAnnotations>,
	/// Optional tool-specific metadata.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, serde_json::Value>>,
	/// Additional fields that may be present in the server's response.
	#[serde(flatten)]
	pub others: HashMap<String, serde_json::Value>,
}

/// Defines a JSON schema for tool input or output.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Schema {
	/// The schema type, which should always be "object" according to the MCP spec.
	pub r#type: String,
	/// Optional property definitions for the schema.
	pub properties: Option<HashMap<String, serde_json::Value>>,
	/// Optional list of required property names.
	pub required: Option<Vec<String>>,
	/// Additional fields that may be present in the server's response.
	#[serde(flatten)]
	pub others: HashMap<String, serde_json::Value>,
}

/// Optional UI hints and annotations for a tool.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolAnnotations {
	/// Optional display title for the tool.
	pub title: Option<String>,
	/// Optional hint that the tool is read-only.
	pub read_only_hint: Option<bool>,
	/// Optional hint that the tool is destructive.
	pub destructive_hint: Option<bool>,
	/// Optional hint that the tool is idempotent.
	pub idempotent_hint: Option<bool>,
	/// Optional hint that the tool is open-world (accepts unknown parameters).
	pub open_world_hint: Option<bool>,
	/// Additional fields that may be present in the server's response.
	#[serde(flatten)]
	pub others: HashMap<String, serde_json::Value>,
}

/// The result from calling a tool, containing the tool's output.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CallToolResult {
	/// One or more content blocks that make up the tool's output.
	pub content: Vec<ContentBlock>,
	/// Optional structured representation of the output (type is not strictly defined).
	#[serde(default)]
	pub structured_content: Option<Value>,
	/// Indicates whether the tool call resulted in an error (when present and true).
	#[serde(default)]
	pub is_error: Option<bool>,
	/// Optional metadata about the response.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
	/// Additional fields that may be present in the server's response.
	#[serde(flatten)]
	pub others: HashMap<String, Value>,
}

/// Represents different types of content blocks that can be returned in a tool's output.
///
/// The variant is determined by the "type" field in the JSON.
#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ContentBlock {
	/// A plain text content block.
	Text(TextContent),
	/// An image content block (base64-encoded data).
	Image(ImageContent),
	/// An audio content block (base64-encoded data).
	Audio(AudioContent),
	/// A link to an external resource.
	ResourceLink(ResourceLink),
	/// An embedded resource (file or document).
	Embedded(EmbeddedResource),
}

/// Represents a text content block.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextContent {
	/// The actual text content.
	pub text: String,
	/// Optional annotations for the text.
	pub annotations: Option<Annotations>,
	/// Optional metadata about the text.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents an image content block.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImageContent {
	/// The base64-encoded image data.
	pub data: String,
	/// The MIME type of the image.
	pub mime_type: String,
	/// Optional annotations for the image.
	pub annotations: Option<Annotations>,
	/// Optional metadata about the image.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents an audio content block.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AudioContent {
	/// The base64-encoded audio data.
	pub data: String,
	/// The MIME type of the audio.
	pub mime_type: String,
	/// Optional annotations for the audio.
	pub annotations: Option<Annotations>,
	/// Optional metadata about the audio.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents a link to an external resource.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ResourceLink {
	/// The name of the resource.
	pub name: String,
	/// Optional display title for the link.
	pub title: Option<String>,
	/// The URI of the resource.
	pub uri: String,
	/// Optional description of the resource.
	pub description: Option<String>,
	/// Optional MIME type of the resource.
	pub mime_type: Option<String>,
	/// Optional annotations for the link.
	pub annotations: Option<Annotations>,
	/// Optional size of the resource in bytes.
	pub size: Option<u64>,
	/// Optional metadata about the link.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents an embedded resource (file or document) in the response.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EmbeddedResource {
	/// The actual resource content (see `ResourceContent` for details).
	pub resource: ResourceContent,
	/// Optional annotations for the embedded resource.
	pub annotations: Option<Annotations>,
	/// Optional metadata about the embedded resource.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents different types of embedded resources.
///
/// The variant is determined by the "type" field in the JSON.
#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ResourceContent {
	/// Text resource with URI.
	Text(TextResourceContents),
	/// Binary blob resource with URI.
	Blob(BlobResourceContents),
}

/// Represents a text resource (e.g., a text file) with URI.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TextResourceContents {
	/// The actual text content.
	pub text: String,
	/// The URI where the resource is located.
	pub uri: String,
	/// Optional MIME type of the text.
	pub mime_type: Option<String>,
	/// Optional metadata about the text resource.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents a binary blob resource with URI.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BlobResourceContents {
	/// The base64-encoded binary data.
	pub blob: String,
	/// The URI where the resource is located.
	pub uri: String,
	/// Optional MIME type of the blob.
	pub mime_type: Option<String>,
	/// Optional metadata about the blob resource.
	#[serde(rename = "_meta")]
	pub meta: Option<HashMap<String, Value>>,
}

/// Represents optional metadata annotations that can be attached to various content types.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Annotations {
	/// Optional list of audiences for whom this content is intended.
	pub audience: Option<Vec<String>>,
	/// Optional priority level of the content.
	pub priority: Option<u32>,
	/// Optional timestamp of when the content was last modified.
	pub last_modified: Option<String>,
}
