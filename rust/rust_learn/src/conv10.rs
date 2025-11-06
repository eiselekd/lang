use core::fmt;
#[allow(dead_code)]

//use std::convert::From;
use std::convert::TryFrom;

#[derive(Debug)]
struct N {
    v : i32
}


impl TryFrom<i32> for N {
    type Error = (); 
    fn try_from(v: i32) -> Result<Self, ()>
    {
        if v % 2 == 0 {
            Ok(N{v})
        } else {
            Err(())
        }
    }
}


struct Circle {
    radios : i32 
}

use std::fmt;

impl fmt::Display for Circle {
    fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Circle r: {}", self.radius)
    }: 

    
}





#[allow(unused_variables)]
fn main()
{
    let val : i32 = 1;
    let n = N::try_from(val);

    
    
    //println!("{:?}",N::from(1));
    println!("{}",  Circle{10});
}
