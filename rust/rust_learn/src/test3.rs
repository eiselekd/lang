#[derive(Debug)]
struct Per {
    n : i32,
    v : i32
}

struct P(i32,i32);

#[allow(unused_variables)]
fn main() {
    let v1 = P(1,2);
    let v0 = Per{n:1,v:2};

    let Per{n,v} = v0;
    println!("{} {}", n, v);
    let P(n1,v1) = v1;
    println!("{} {}", n1, v1);
}
