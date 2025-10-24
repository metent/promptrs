use rustls::pki_types::ServerName;
use rustls::{ClientConfig, ClientConnection, RootCertStore};
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

pub struct TlsStream {
	conn: ClientConnection,
	tcp: TcpStream,
	plain_buf: Vec<u8>,
}

impl TlsStream {
	pub async fn connect(host: &str, port: u16) -> io::Result<Self> {
		let root_store = RootCertStore {
			roots: webpki_roots::TLS_SERVER_ROOTS.to_vec(),
		};

		let mut config = ClientConfig::builder()
			.with_root_certificates(root_store)
			.with_no_client_auth();
		config.alpn_protocols = vec![b"http/1.1".to_vec()];

		let server_name = ServerName::try_from(host.to_string())
			.map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "invalid hostname"))?;

		let addr = format!("{}:{}", host, port);
		let tcp = TcpStream::connect(&addr).await?;

		let mut conn = ClientConnection::new(Arc::new(config), server_name)
			.map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{:?}", e)))?;
		conn.set_buffer_limit(Some(1048576));

		let mut tls = Self {
			conn,
			tcp,
			plain_buf: Vec::new(),
		};

		tls.finish_handshake().await?;
		Ok(tls)
	}

	async fn finish_handshake(&mut self) -> io::Result<()> {
		self.handshake_loop(async |conn, tcp| {
			let mut pending = Vec::new();
			conn.write_tls(&mut pending)?;
			tcp.write_all(&pending).await
		})
		.await
	}

	async fn handshake_loop<F>(&mut self, mut f: F) -> io::Result<()>
	where
		F: AsyncFnMut(&mut ClientConnection, &mut TcpStream) -> io::Result<()>,
	{
		loop {
			self.conn
				.process_new_packets()
				.map_err(|err| io::Error::other(err))?;

			let mut pending = Vec::new();
			self.conn.write_tls(&mut pending)?;
			if !pending.is_empty() {
				f(&mut self.conn, &mut self.tcp).await?;
			}

			if !self.conn.is_handshaking() {
				return Ok(());
			}

			let mut buf = [0u8; 4096];
			let n = self.tcp.read(&mut buf).await?;
			if n == 0 {
				return Err(io::Error::new(
					io::ErrorKind::UnexpectedEof,
					"handshake aborted",
				));
			}

			self.conn.read_tls(&mut std::io::Cursor::new(&buf[..n]))?;
		}
	}

	pub async fn write_all(&mut self, data: &[u8]) -> io::Result<()> {
		self.conn.writer().write_all(data)?;
		self.flush().await?;
		Ok(())
	}

	async fn flush(&mut self) -> io::Result<()> {
		let mut pending = Vec::new();
		self.conn.write_tls(&mut pending)?;
		if !pending.is_empty() {
			self.tcp.write_all(&pending).await?;
		}
		Ok(())
	}

	pub async fn read_line(&mut self) -> io::Result<Option<String>> {
		loop {
			if let Some(pos) = self.plain_buf.iter().position(|&b| b == b'\n') {
				return Ok(Some(
					String::from_utf8_lossy(&self.plain_buf.drain(..=pos).collect::<Vec<_>>())
						.to_string(),
				));
			}

			let mut buf = [0u8; 1024];
			let n = self.tcp.read(&mut buf).await?;
			if n == 0 {
				return if self.plain_buf.is_empty() {
					Ok(None)
				} else {
					Ok(Some(String::from_utf8_lossy(&self.plain_buf).to_string()))
				};
			}

			self.read_tls(&buf[..n]).await?;
		}
	}

	async fn read_tls(&mut self, data: &[u8]) -> io::Result<()> {
		self.conn.read_tls(&mut std::io::Cursor::new(data))?;
		self.conn
			.process_new_packets()
			.map_err(|err| io::Error::other(err))?;

		let mut tmp = [0u8; 256];
		let mut plain = Vec::new();
		while let Ok(k) = self.conn.reader().read(&mut tmp) {
			if k == 0 {
				break;
			}
			plain.extend_from_slice(&tmp[..k]);
		}
		self.plain_buf.extend_from_slice(&plain);
		Ok(())
	}
}
