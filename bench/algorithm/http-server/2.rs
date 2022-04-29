use rand::prelude::*;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::{self, Sender};
use warp::Filter;

lazy_static::lazy_static! {
    static ref HTTP_CLIENT: reqwest::Client = reqwest::Client::builder()
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
async fn tokio_main(n: usize, port: u16) -> anyhow::Result<()> {
    tokio::spawn(run_server(port));
    let (sender, mut receiver) = mpsc::channel::<usize>(n);
    let mut sum = 0;
    let api = format!("http://localhost:{port}/api");
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
    warp::serve(api).run(([127, 0, 0, 1], port)).await;
}

async fn send_with_retry(api: String, value: usize, sender: Sender<usize>) -> anyhow::Result<()> {
    loop {
        match send_once(&api, value).await {
            Ok(r) => {
                sender.send(r).await?;
                break;
            }
            Err(err) => eprintln!("{err}"),
        }
    }
    Ok(())
}

async fn send_once(api: &str, value: usize) -> anyhow::Result<usize> {
    let payload = Payload { value };
    let resp = HTTP_CLIENT.post(api).json(&payload).send().await?;
    let resp_text = resp.text().await?;
    Ok(resp_text.parse::<usize>()?)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payload {
    pub value: usize,
}
