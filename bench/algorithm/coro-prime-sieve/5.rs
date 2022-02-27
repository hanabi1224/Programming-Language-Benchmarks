// Tokio version from https://users.rust-lang.org/t/how-to-properly-use-channel-for-coroutine-communication/58761/2?u=hanabi1224
// Use flume mpsp channel instead

use flume::{Receiver, Sender};
use std::io::{self, prelude::*, BufWriter};

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(100);

    async_main(n).unwrap();
    std::process::exit(0)
}

#[tokio::main]
async fn async_main(n: usize) -> anyhow::Result<()> {
    let mut stdout = BufWriter::new(io::stdout());
    let (sender, mut receiver) = flume::bounded::<usize>(1);
    let mut handles = Vec::with_capacity(n + 1);
    handles.push(tokio::spawn(generate(sender)));
    for _i in 0..n {
        let prime = receiver.recv_async().await.unwrap();
        stdout.write_fmt(format_args!("{}\n", prime))?;
        let (sender_next, receiver_next) = flume::bounded::<usize>(1);
        handles.push(tokio::spawn(filter(receiver, sender_next, prime)));
        receiver = receiver_next;
    }
    Ok(())
}

async fn generate(sender: Sender<usize>) -> anyhow::Result<()> {
    let mut i = 2;
    while sender.send_async(i).await.is_ok() {
        i += 1;
    }
    Ok(())
}

async fn filter(
    receiver: Receiver<usize>,
    sender: Sender<usize>,
    prime: usize,
) -> anyhow::Result<()> {
    loop {
        let i = receiver.recv_async().await?;
        if i % prime != 0 {
            if sender.send_async(i).await.is_err() {
                return Ok(());
            }
        }
    }
}
