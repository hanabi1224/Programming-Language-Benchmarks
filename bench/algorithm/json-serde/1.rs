use std::fs;

fn main() {
    let file_name = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .unwrap_or("sample".to_string());
    let n = std::env::args_os()
        .nth(2)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let json_str = fs::read_to_string(format!("{}.json", file_name))
        .expect("Something went wrong reading the file");
    let json: serde_json::Value = serde_json::from_str(&json_str).unwrap();
    print_hash(&json);
    let mut array = Vec::with_capacity(n);
    for _i in 0..n {
        let json: serde_json::Value = serde_json::from_str(&json_str).unwrap();
        array.push(json);
    }
    let array = serde_json::json!(array);
    print_hash(&array);
}

fn print_hash(data: &serde_json::Value) {
    let bytes = serde_json::to_vec(data).unwrap();
    let digest = md5::compute(&bytes);
    println!("{:x}", digest);
}

// #[derive(Deserialize, Serialize, Debug, Default)]
// struct GeoData {
//     r#type: String,
//     features: Vec<Feature>,
// }
//
// #[derive(Deserialize, Serialize, Debug, Default)]
// struct Feature {
//     r#type: String,
//     properties: Properties,
//     geometry: Geometry,
// }
//
// #[derive(Deserialize, Serialize, Debug, Default)]
// struct Properties {
//     name: String,
// }
//
// #[derive(Deserialize, Serialize, Debug, Default)]
// struct Geometry {
//     r#type: String,
//     coordinates: Vec<Vec<[f64; 2]>>,
// }
