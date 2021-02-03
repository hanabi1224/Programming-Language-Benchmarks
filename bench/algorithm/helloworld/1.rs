fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .unwrap_or(String::from(""));
    println!("Hello world {}!", n)
}
