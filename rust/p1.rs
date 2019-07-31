trait Callable {
    fn call (&self) -> bool;
}

impl<F> Callable for F
where
    F: Fn(i32) -> bool,
{
    fn call (&self) -> bool {
        self(1)
    }
}

fn a1 () -> impl Callable {
    move |x:i32| { println!("Hello {}", x); return 0==0; }
}

fn main() {

    let a = a1();
    a.call();
}
