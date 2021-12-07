use async_channel::{Receiver, Sender};
use async_executor::Executor;
use futures_lite::future;
use std::io::{self, prelude::*, BufWriter};

static EX: Executor = Executor::new();

fn main() -> anyhow::Result<(), anyhow::Error> {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(100);

    future::block_on(EX.run(EX.spawn(async_main(n))))?;
    Ok(())
}

async fn async_main(n: usize) -> anyhow::Result<(), anyhow::Error> {
    let mut stdout = BufWriter::new(io::stdout());
    let (sender, mut receiver) = async_channel::bounded::<usize>(1);
    let mut tasks = Vec::with_capacity(n + 1);
    let t0 = EX.spawn(generate(sender));
    tasks.push(t0);
    for _i in 0..n {
        let prime = receiver.recv().await?;
        stdout.write_fmt(format_args!("{}\n", prime))?;
        let (sender_next, receiver_next) = async_channel::bounded::<usize>(1);
        let t = EX.spawn(filter(receiver, sender_next, prime));
        tasks.push(t);
        receiver = receiver_next;
    }
    Ok(())
}

async fn generate(sender: Sender<usize>) -> anyhow::Result<(), anyhow::Error> {
    let mut i = 2;
    loop {
        sender.send(i).await?;
        i += 1;
    }
}

async fn filter(
    receiver: Receiver<usize>,
    sender: Sender<usize>,
    prime: usize,
) -> anyhow::Result<(), anyhow::Error> {
    loop {
        let i = receiver.recv().await?;
        if i % prime != 0 {
            sender.send(i).await?;
        }
    }
}
