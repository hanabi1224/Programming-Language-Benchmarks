use async_std::{
    channel,
    channel::{Receiver, Sender},
    task,
    task::Task,
};

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(100);

    task::block_on(async_main(n));
}

async fn async_main(n: usize) -> anyhow::Result<(), anyhow::Error> {
    let (sender, mut receiver) = channel::bounded::<usize>(2);
    task::spawn(generate(sender));
    for _i in 0..n {
        let prime = receiver.recv().await?;
        println!("{}", prime);
        let (sender_next, receiver_next) = channel::bounded::<usize>(2);
        task::spawn(filter(receiver, sender_next, prime));
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
