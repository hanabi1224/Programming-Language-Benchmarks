use std::net::{IpAddr, Ipv4Addr, SocketAddr};

use axum::routing::post;
use axum_server::tls_rustls::RustlsConfig;
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::{self, Sender};

static CERT: &[u8] = include_bytes!("../self_signed_certs/cert.pem");
static KEY: &[u8] = include_bytes!("../self_signed_certs/key.pem");
lazy_static::lazy_static! {
    static ref HTTP_CLIENT: reqwest::Client = reqwest::Client::builder()
        .danger_accept_invalid_certs(true)
        .use_rustls_tls()
        .build()
        .unwrap();
}

fn main() -> anyhow::Result<()> {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let mut rng = thread_rng();
    let port = rng.gen_range(30000..40000);
    tokio_main(n, port)?;
    std::process::exit(0);
}

#[tokio::main]
async fn tokio_main(n: usize, port: usize) -> anyhow::Result<()> {
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

async fn run_server(port: usize) -> anyhow::Result<()> {
    let app = axum::Router::new().route("/api", post(handler));
    let addr = SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), port as u16);
    let rustls_config = RustlsConfig::from_pem(CERT.into(), KEY.into()).await?;
    axum_server::bind_rustls(addr, rustls_config)
        .serve(app.into_make_service())
        .await?;
    Ok(())
}

async fn send_with_retry(api: String, value: usize, sender: Sender<usize>) -> anyhow::Result<()> {
    loop {
        if let Ok(r) = send_once(&api, value).await {
            sender.send(r).await?;
            break;
        }
    }
    Ok(())
}

async fn send_once(api: &str, value: usize) -> anyhow::Result<usize> {
    let payload = Payload { value };
    let resp = HTTP_CLIENT
        .post(api)
        .json(&payload)
        .version(reqwest::Version::HTTP_2)
        .send()
        .await?;
    let resp_text = resp.text().await?;
    Ok(resp_text.parse::<usize>()?)
}

async fn handler(payload: axum::Json<Payload>) -> String {
    format!("{}", payload.value)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payload {
    pub value: usize,
}
