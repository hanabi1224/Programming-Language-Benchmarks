use serde::Serialize;
// use serde::Deserialize;
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
        .unwrap_or(3);
    let json_str = fs::read_to_string(format!("{}.json", file_name))
        .expect("Something went wrong reading the file");
    for i in 0..n {
        let json: serde_json::Value = serde_json::from_str(&json_str).unwrap();
        let indent = vec![b' '; i + 1];
        let formatter = serde_json::ser::PrettyFormatter::with_indent(&indent);
        let buf = Vec::new();
        let mut ser = serde_json::Serializer::with_formatter(buf, formatter);
        json.serialize(&mut ser).unwrap();
        let bytes = ser.into_inner();
        let digest = md5::compute(&bytes);
        // println!("{}", String::from_utf8(bytes).unwrap());
        println!("{:x}", digest);
    }
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
