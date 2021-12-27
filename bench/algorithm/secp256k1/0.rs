use elliptic_curve::{group::prime::PrimeCurveAffine, sec1::ToEncodedPoint};
use std::ops::Mul;

// private key is randomly generated from https://biteaddress.org/
// base58 string: KxkzkbgzshmmctDxFjbH6ar7x16PwH7CrGXggCxn33vmgC1Uezp7
// hex: 0x2DEE927079283C3C4FCA3EF970FF4D38B64592E3FE0AB0DAD9132D70B5BC7693
// bigint: 20775598474904240222758871485654738649026525153462921990999819694398496339603
// public key uncompressed: 04BAC4DB182BD8E59DA66EC3B0E1759A102FF7308A916CCCB51C68253D4BF32C16A6BF6C1C8C9261F65983CFB987E0E8B4B8A7CC34376454B27F5ADF7E36DC15D0
// public key affine point(n=1 output): BAC4DB182BD8E59DA66EC3B0E1759A102FF7308A916CCCB51C68253D4BF32C16, A6BF6C1C8C9261F65983CFB987E0E8B4B8A7CC34376454B27F5ADF7E36DC15D0
const PRIVATE_KEY: [u8; 32] = [
    45, 238, 146, 112, 121, 40, 60, 60, 79, 202, 62, 249, 112, 255, 77, 56, 182, 69, 146, 227, 254,
    10, 176, 218, 217, 19, 45, 112, 181, 188, 118, 147,
];

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|s| s.parse().ok())
        .unwrap_or(10);
    let privkey: k256::Scalar = elliptic_curve::ScalarCore::from_be_slice(&PRIVATE_KEY)
        .unwrap()
        .into();
    let mut point = k256::AffinePoint::generator();
    for _i in 0..n {
        point = point.mul(privkey).to_affine();
    }
    let encoded = point.to_encoded_point(false);
    println!("{:x},{:x}", encoded.x().unwrap(), encoded.y().unwrap());
}
