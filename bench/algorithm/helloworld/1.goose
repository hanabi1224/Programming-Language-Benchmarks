import os;

fn main() {
    let a = args();
    print("Hello world ", if a.len > 1 { a[1] } else { "" }, "!");
}
