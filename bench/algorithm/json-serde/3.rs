use lasso::{Rodeo, Spur};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fs;
use std::sync::RwLock;

lazy_static::lazy_static! {
    static ref POOL:RwLock<Rodeo> = RwLock::new(Rodeo::default());
}

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
    let json: GeoData = serde_json::from_str(&json_str)?;
    print_hash(serde_json::to_vec(&json)?);
    let mut array = Vec::with_capacity(n);
    for _i in 0..n {
        let json: GeoData = serde_json::from_str(&json_str)?;
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

// String pooling
// Not using enum because it's not generic enough
// although string values can be exhaustive in this case
#[derive(Debug, Default)]
struct MyString(Spur);

impl Serialize for MyString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let pool = POOL.read().unwrap();
        serializer.serialize_str(pool.resolve(&self.0))
    }
}

impl<'de> Deserialize<'de> for MyString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let mut pool = POOL.write().unwrap();
        let spur = pool.get_or_intern(s);
        Ok(MyString(spur))
    }
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct GeoData {
    r#type: MyString,
    features: Vec<Feature>,
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct Feature {
    r#type: MyString,
    properties: Properties,
    geometry: Geometry,
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct Properties {
    name: MyString,
}

#[derive(Deserialize, Serialize, Debug, Default)]
struct Geometry {
    r#type: MyString,
    coordinates: Vec<Vec<[MyF64; 2]>>,
}
