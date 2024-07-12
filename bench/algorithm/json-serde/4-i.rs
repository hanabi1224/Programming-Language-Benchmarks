use serde::{Deserialize, Serialize, Serializer};
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
    let mut json_str = fs::read(format!("{}.json", file_name))?;
    let json: GeoData = simd_json::from_slice(&mut json_str)?;

    print_hash(simd_json::to_vec(&json)?);
    let mut array = Vec::with_capacity(n);
    for _i in 0..n {
        let json: GeoData = simd_json::from_slice(&mut json_str)?;
        array.push(json);
    }
    print_hash(simd_json::to_vec(&array)?);
    Ok(())
}

fn print_hash(bytes: impl AsRef<[u8]>) {
    let digest = md5::compute(&bytes);
    println!("{:x}", digest);
}

#[derive(Deserialize, Debug, Default)]
struct MyF64(f64);

impl Serialize for MyF64 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self.0.fract() == 0.0 {
            serializer.serialize_i64(self.0 as i64)
        } else {
            serializer.serialize_f64(self.0)
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct GeoData {
    r#type: String,
    features: Vec<Feature>,
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct Feature {
    r#type: String,
    properties: Properties,
    geometry: Geometry,
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct Properties {
    name: String,
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct Geometry {
    r#type: String,
    coordinates: Vec<Vec<[MyF64; 2]>>,
}
