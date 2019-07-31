trait Callable {
    fn call (&self) -> bool;
}

impl<F> Callable for F
where
    F: Fn() -> bool,
{
    fn call (&self) -> bool {
        self()
    }
}

fn a1 () -> impl Callable {
    move || { println!("Hello"); return 0==0; }
}

fn main() {

    let a = a1();
    a.call();
}
