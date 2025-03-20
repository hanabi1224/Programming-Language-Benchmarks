use axum::routing::post;
use hyper::{client::HttpConnector, StatusCode};
use rand::prelude::*;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::{self, Sender};

static HOST: &str = "127.0.0.1";
lazy_static::lazy_static! {
    static ref H1_CLIENT: hyper::Client<HttpConnector> = h1_client();
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
async fn tokio_main(n: usize, port: usize) -> anyhow::Result<()> {
    tokio::spawn(run_server(port));
    let (sender, mut receiver) = mpsc::channel::<usize>(n);
    let mut sum = 0;
    let api = format!("http://{}:{}/api", HOST, port);
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
    axum::Server::bind(&format!("{}:{}", HOST, port).parse()?)
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
    let resp = H1_CLIENT
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

async fn handler(body: axum::extract::RawBody) -> (StatusCode, String) {
    match handler_inner(body).await {
        Ok(b) => (StatusCode::OK, b),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()),
    }
}

async fn handler_inner(body: axum::extract::RawBody) -> anyhow::Result<String> {
    let bytes = hyper::body::to_bytes(body.0).await?;
    let json: Payload = serde_json::from_slice(&bytes[..])?;
    Ok(format!("{}", json.value))
}

fn h1_client() -> hyper::Client<HttpConnector> {
    hyper::Client::builder().build_http()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payload {
    pub value: usize,
}
