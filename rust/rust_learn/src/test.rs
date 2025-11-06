
#[allow(unused_variables, dead_code)]
fn longest<'a, 'b>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

#[allow(unused_variables)]
fn main() {
    let c : &str;
    {
        let b = String::from("StringB");
        {
            let a = String::from("StringA_long");
            {
                c = longest(&a, &b);
            }
        }
    }
    println!("{}", c);
}
