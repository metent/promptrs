use rustls::pki_types::ServerName;
use rustls::{ClientConfig, ClientConnection, RootCertStore};
use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::sync::Arc;
#[cfg(not(target_os = "wasi"))]
use tokio::io::{AsyncReadExt, AsyncWriteExt};
#[cfg(not(target_os = "wasi"))]
use tokio::net::TcpStream;
#[cfg(target_os = "wasi")]
use wstd::io::{AsyncRead, AsyncWrite};
#[cfg(target_os = "wasi")]
use wstd::net::TcpStream;

/// A TLS overlay that supports `write_all`,
/// `finish_handshake()` and `read_line()`.
pub struct TlsStream {
	conn: ClientConnection,
	tcp: TcpStream,
	plain_buf: Vec<u8>,
}

impl TlsStream {
	/// Create and immediately perform the TLS handshake.
	pub async fn connect(host: &str, port: u16) -> io::Result<Self> {
		let mut root_store = RootCertStore::empty();
		for cert in rustls_native_certs::load_native_certs().certs {
			root_store.add(cert).map_err(io::Error::other)?;
		}

		let mut cfg = ClientConfig::builder()
			.with_root_certificates(root_store)
			.with_no_client_auth();
		cfg.alpn_protocols = vec![b"http/1.1".to_vec()];

		let tcp = TcpStream::connect(&format!("{host}:{port}")).await?;
		let server_name = ServerName::try_from(host.to_string())
			.map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "invalid hostname"))?;

		let mut conn = ClientConnection::new(Arc::new(cfg), server_name)
			.map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;
		conn.set_buffer_limit(Some(1 << 20));

		let mut s = Self {
			conn,
			tcp,
			plain_buf: Vec::new(),
		};
		s.finish_handshake().await?;
		Ok(s)
	}

	async fn finish_handshake(&mut self) -> io::Result<()> {
		loop {
			let mut out = Vec::new();
			self.conn.write_tls(&mut out)?;
			if !out.is_empty() {
				self.tcp.write_all(&out).await?;
			}

			if !self.conn.is_handshaking() {
				break;
			}

			let mut buf = [0u8; 4096];
			let n = self.tcp.read(&mut buf).await?;
			if n == 0 {
				return Err(io::Error::new(
					io::ErrorKind::UnexpectedEof,
					"hand‑shake aborted – server closed connection",
				));
			}
			let mut cur = std::io::Cursor::new(&buf[..n]);
			self.conn.read_tls(&mut cur)?;
			self.conn
				.process_new_packets()
				.map_err(|e| io::Error::other(e))?;
		}
		Ok(())
	}

	pub async fn write_all(&mut self, data: &[u8]) -> io::Result<()> {
		self.conn.writer().write_all(data)?;

		let mut out = Vec::new();
		self.conn.write_tls(&mut out)?;
		if !out.is_empty() {
			self.tcp.write_all(&out).await?;
		}
		Ok(())
	}

	/// Read one line from the TLS connection (read until a `\n`).
	pub async fn read_line(&mut self) -> io::Result<Option<String>> {
		loop {
			if let Some(pos) = self.plain_buf.iter().position(|&b| b == b'\n') {
				let line_bytes = self.plain_buf.drain(..=pos).collect::<Vec<_>>();
				return Ok(Some(String::from_utf8_lossy(&line_bytes).to_string()));
			}

			let mut net_buf = [0u8; 1024];
			let n = self.tcp.read(&mut net_buf).await?;
			if n == 0 {
				if self.plain_buf.is_empty() {
					return Ok(None);
				} else {
					let line_bytes = std::mem::take(&mut self.plain_buf);
					return Ok(Some(String::from_utf8_lossy(&line_bytes).to_string()));
				}
			}

			let mut cur = std::io::Cursor::new(&net_buf[..n]);
			self.conn.read_tls(&mut cur)?;
			self.conn
				.process_new_packets()
				.map_err(|e| io::Error::other(e))?;

			let mut tmp = [0u8; 256];
			let mut plain = Vec::new();
			loop {
				match self.conn.reader().read(&mut tmp) {
					Ok(0) => break,
					Ok(k) => plain.extend_from_slice(&tmp[..k]),
					Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
					Err(e) => return Err(e),
				}
			}
			self.plain_buf.extend_from_slice(&plain);
		}
	}
}

/// Build an HTTP/1.1 request for a JSON body.
/// The caller supplies the method, host, path, body and an optional
/// “Bearer $TOKEN” header – plus any extra custom headers.
pub fn build_http_request(
	method: &str,
	host: &str,
	path: &str,
	body: &[u8],
	extra_headers: &[(&str, &str)],
	bearer: Option<&str>,
) -> Vec<u8> {
	let mut req = format!("{method} {path} HTTP/1.1\r\nHost: {host}\r\n");
	for (k, v) in extra_headers {
		req.push_str(&format!("{k}: {v}\r\n"));
	}
	if let Some(b) = bearer {
		req.push_str(&format!("Authorization: {b}\r\n"));
	}
	req.push_str(&format!("Content-Length: {}\r\n\r\n", body.len()));
	req.into_bytes()
		.into_iter()
		.chain(body.iter().copied())
		.collect()
}

/// Read all lines from a `TlsStream` and parse the HTTP response,
/// returning the decoded body and the header map.
pub async fn read_http_response(
	tls: &mut TlsStream,
) -> io::Result<(Vec<u8>, HashMap<String, String>)> {
	let mut raw = String::new();
	while let Some(line) = tls.read_line().await? {
		raw.push_str(&line);
	}

	let (hdr, body) = raw.split_once("\r\n\r\n").ok_or_else(|| {
		io::Error::new(
			io::ErrorKind::InvalidData,
			"HTTP/1.1 response without header/body separator",
		)
	})?;

	let headers = hdr
		.lines()
		.skip(1)
		.filter_map(|l| {
			l.find(':').map(|idx| {
				(
					l[..idx].trim().to_ascii_lowercase(),
					l[idx + 1..].trim().to_string(),
				)
			})
		})
		.collect::<HashMap<_, _>>();

	let body_bytes = body.as_bytes().to_vec();
	let decoded_body = if headers
		.get("transfer-encoding")
		.map(|v| v.to_ascii_lowercase().contains("chunked"))
		.unwrap_or(false)
	{
		decode_chunked(&body_bytes)?
	} else {
		body_bytes
	};

	Ok((decoded_body, headers))
}

/// Decode an HTTP “chunked” body into a single `Vec<u8>`.
pub fn decode_chunked(body: &[u8]) -> io::Result<Vec<u8>> {
	let mut out = Vec::new();
	let mut pos = 0;
	loop {
		let crlf = body[pos..]
			.windows(2)
			.position(|w| w == b"\r\n")
			.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "malformed chunk"))?;
		let size_line = &body[pos..pos + crlf];
		pos += crlf + 2;

		let size = std::str::from_utf8(size_line)
			.map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "size not UTF‑8"))?
			.trim();
		let chunk = usize::from_str_radix(size, 16)
			.map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "invalid chunk size"))?;
		if chunk == 0 {
			break;
		}
		out.extend_from_slice(&body[pos..pos + chunk]);
		pos += chunk + 2;
	}
	Ok(out)
}

/// Pull the final JSON object that appears after a trailing `data:` line
/// in an SSE stream.
pub fn parse_sse_body<T: DeserializeOwned>(raw_body: Vec<u8>) -> io::Result<T> {
	let text = String::from_utf8(raw_body).map_err(io::Error::other)?;

	let json_str_opt = text
		.lines()
		.filter(|l| l.starts_with("data:"))
		.filter_map(|l| {
			let payload = l["data:".len()..].trim();
			if payload.eq_ignore_ascii_case("[DONE]") {
				None
			} else if payload.starts_with('{') || payload.starts_with('[') {
				Some(payload.to_owned())
			} else {
				None
			}
		})
		.next();

	let json_str = json_str_opt
		.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "no JSON in SSE body"))?;

	serde_json::from_str(&json_str).map_err(io::Error::other)
}
