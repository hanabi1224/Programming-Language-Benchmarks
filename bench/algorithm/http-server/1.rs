use axum::prelude::*;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::{self, Sender};

static HOST: &str = "127.0.0.1";
lazy_static::lazy_static! {
    static ref HTTP_CLIENT: reqwest::Client = reqwest::Client::new();
}

fn main() -> anyhow::Result<(), anyhow::Error> {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let port = 8080;
    tokio_main(n, port)?;
    std::process::exit(0);
}

#[tokio::main]
async fn tokio_main(n: usize, port: usize) -> anyhow::Result<(), anyhow::Error> {
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

async fn run_server(port: usize) -> anyhow::Result<(), anyhow::Error> {
    let app = route("/api", post(handler));
    axum::Server::bind(&format!("{}:{}", HOST, port).parse()?)
        .serve(app.into_make_service())
        .await?;
    Ok(())
}

async fn send_with_retry(
    api: String,
    value: usize,
    sender: Sender<usize>,
) -> anyhow::Result<(), anyhow::Error> {
    loop {
        if let Ok(r) = send_once(&api, value).await {
            sender.send(r).await?;
            break;
        }
    }
    Ok(())
}

async fn send_once(api: &str, value: usize) -> anyhow::Result<usize, anyhow::Error> {
    let payload = Payload { value };
    let resp = HTTP_CLIENT.post(api).json(&payload).send().await?;
    let resp_text = resp.text().await?;
    Ok(resp_text.parse::<usize>()?)
}

async fn handler(payload: extract::Json<Payload>) -> String {
    format!("{}", payload.value)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payload {
    pub value: usize,
}
