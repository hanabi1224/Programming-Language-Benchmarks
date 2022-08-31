fn main() {
    println!(
        "Hello world {}!",
        std::env::args().nth(1).unwrap_or_default()
    );
}
