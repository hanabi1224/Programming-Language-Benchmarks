use hyper::client::HttpConnector;
use hyper_rustls::{HttpsConnector, HttpsConnectorBuilder};
use rand::prelude::*;
use rustls::client::ServerCertVerifier;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::mpsc::{self, Sender};
use warp::Filter;

static CERT: &[u8] = include_bytes!("../self_signed_certs/cert.pem");
static KEY: &[u8] = include_bytes!("../self_signed_certs/key.pem");
lazy_static::lazy_static! {
    static ref H2_CLIENT: hyper::Client<HttpsConnector<HttpConnector>> = h2_client();
}

fn main() -> anyhow::Result<()> {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let mut rng = rand::rng();
    let port = rng.random_range(30000..40000);
    tokio_main(n, port)?;
    std::process::exit(0);
}

#[tokio::main]
async fn tokio_main(n: usize, port: u16) -> anyhow::Result<()> {
    tokio::spawn(run_server(port));
    let (sender, mut receiver) = mpsc::channel::<usize>(n);
    let mut sum = 0;
    let api = format!("https://localhost:{port}/api");
    for i in 1..=n {
        tokio::spawn(send_with_retry(api.clone(), i, sender.clone()));
    }
    for _i in 0..n {
        if let Some(v) = receiver.recv().await {
            sum += v;
        }
    }
    println!("{}", sum);
    Ok(())
}

async fn run_server(port: u16) {
    let api = warp::post()
        .and(warp::path("api"))
        .and(warp::body::json())
        .map(|payload: Payload| warp::reply::json(&payload.value));
    warp::serve(api)
        .tls()
        .cert(CERT)
        .key(KEY)
        .run(([127, 0, 0, 1], port))
        .await;
}

async fn send_with_retry(api: String, value: usize, sender: Sender<usize>) -> anyhow::Result<()> {
    loop {
        match send_once(&api, value).await {
            Ok(r) => {
                sender.send(r).await?;
                break;
            }
            Err(_e) => {
                // eprintln!("{_e}");
            }
        }
    }
    Ok(())
}

async fn send_once(api: &str, value: usize) -> anyhow::Result<usize> {
    let payload = Payload { value };
    let resp = H2_CLIENT
        .request(hyper::Request::post(api).body(serde_json::to_string(&payload)?.into())?)
        .await?;
    if resp.status().is_success() {
        let bytes = hyper::body::to_bytes(resp.into_body()).await?;
        let text = String::from_utf8_lossy(&bytes[..]);
        Ok(text.parse::<usize>()?)
    } else {
        anyhow::bail!("{}", resp.status())
    }
}

struct CustomTlsVerifier;

impl ServerCertVerifier for CustomTlsVerifier {
    fn verify_server_cert(
        &self,
        _end_entity: &rustls::Certificate,
        _intermediates: &[rustls::Certificate],
        _server_name: &rustls::ServerName,
        _scts: &mut dyn Iterator<Item = &[u8]>,
        _ocsp_response: &[u8],
        _now: std::time::SystemTime,
    ) -> Result<rustls::client::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::ServerCertVerified::assertion())
    }
}

fn h2_client() -> hyper::Client<HttpsConnector<HttpConnector>> {
    hyper::Client::builder().build(
        HttpsConnectorBuilder::new()
            .with_tls_config(
                rustls::ClientConfig::builder()
                    .with_safe_defaults()
                    .with_custom_certificate_verifier(Arc::new(CustomTlsVerifier))
                    .with_no_client_auth(),
            )
            .https_only()
            .enable_http2()
            .build(),
    )
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payload {
    pub value: usize,
}
