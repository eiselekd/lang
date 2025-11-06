#[allow(dead_code)]

#[derive(Debug)]
enum E {
    N,
    V
}

type X = E;

impl E {
    fn test(&self) {
        println!("{:?}",self);
    }
}


#[allow(unused_variables)]
fn main()
{

    let v = X::N;
    v.test();
    match v {
        E::N => println!("n"),
        E::V => println!("v")
    }
}
