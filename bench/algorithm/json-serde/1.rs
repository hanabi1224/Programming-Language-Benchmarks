use std::fs;

fn main() -> anyhow::Result<()> {
    let file_name = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .unwrap_or("sample".to_string());
    let n = std::env::args_os()
        .nth(2)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let json_str = fs::read_to_string(format!("{}.json", file_name))?;
    let json: serde_json::Value = serde_json::from_str(&json_str)?;
    print_hash(serde_json::to_vec(&json)?);
    let mut array = Vec::with_capacity(n);
    for _i in 0..n {
        let json: serde_json::Value = serde_json::from_str(&json_str)?;
        array.push(json);
    }
    let array = serde_json::json!(array);
    print_hash(serde_json::to_vec(&array)?);
    Ok(())
}

fn print_hash(bytes: impl AsRef<[u8]>) {
    let digest = md5::compute(&bytes);
    println!("{:x}", digest);
}
