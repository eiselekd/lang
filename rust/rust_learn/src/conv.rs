#[allow(dead_code)]

use std::convert::From;

#[derive(Debug)]
struct N {
    v : i32
}

impl From<i32> for N
{
    fn from(i:i32) -> Self {
        N {v : i }
    }
};



#[allow(unused_variables)]
fn main()
{
    pritln!("{:?}",N::from(1));

    
}
