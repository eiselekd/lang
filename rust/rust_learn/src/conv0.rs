#[allow(dead_code)]

// use std::convert::Into;

// #[derive(Debug)]
// struct Number {
//     value: i32,
// }

// impl Into<Number> for i32 {
//     fn into(self) -> Number {
//         Number { value: self }
//     }
// }

// fn main() {
//     let int = 5;
//     // Try removing the type annotation
//     let num: Number = int.into();
//     println!("My number is {:?}", num);
// }




//use std::convert::From;
use std::convert::Into;

#[derive(Debug)]
struct N {
    v : i32,
}

/*
impl From<i32> for N
{
    fn from(i:i32) -> Self {
        N {v : i }
    }
}*/


impl Into<N> for i32
{
    fn into(self) -> N {
        N { v : self }
    }
}


#[allow(unused_variables)]
fn main()
{
    let val : i32 = 2;
    let n : N = val.into();
    //println!("{:?}",N::from(1));
    println!("{:?}", n);
}
