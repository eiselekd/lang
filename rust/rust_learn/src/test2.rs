
use std::mem;


fn rev(p: (i32, bool)) -> (bool, i32) {
    (p.1, p.0)
}




#[allow(unused_variables)]
fn main() {
    let  mut v : [ i32 ; 4 ]  = [ 1,2,3,4 ];
    v[0] = 2;
    println!("{}: {:?}", v.len(), mem::size_of_val(&v));
    println!("{:?}", &v[1..]);

    for (v,h) in v.iter_mut().enumerate() {
        *h = *h+1;
        println!("{} {}", v, h);
    }
    
    for i in 0..v.len() +1 {
        match v.get(i)
        {
            Some(v) => println!("{}", v),
            None => println!("None"),
        }
    }

    let v0 = (1, 2, 4);
    let (x0, x1, x2) = v0;
    println!("{} {} {}", v0.0, v0.1, v0.2);

    println!("{:?}", rev((1,true)));
}

