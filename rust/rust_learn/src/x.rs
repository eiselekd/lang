
pub(crate) mod commands;

use clap::Parser;
use std::num::ParseIntError;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long, default_value_t = String::from("test"))]
    name: String,

    /// Number of times to greet
    #[arg(short, long, default_value_t = 1)]
    count: u8,
}

use regex::Regex;

fn main() -> Result<(),ParseIntError> {
    let args = Args::parse();




    
    let re = Regex::new(r"(?m)^([^:]+):([0-9]+):(.+)$").unwrap();
    let hay = "\
    path/to/foo:54:Blue Harvest
path/to/bar:90:Something, Something, Something, Dark Side
path/to/baz:3:It's a Trap!
";

    let mut results = vec![];
    for (_, [path, lineno, line]) in re.captures_iter(hay)
        .map(|c| c.extract()) {
            results.push((path, lineno.parse::<u64>()?, line));
        }


    println!("{:?}", results);
    
    
    for _ in 0..args.count {
        println!("Hello {}!", args.name);
    }

    commands::a();

    
    Ok(())

}
