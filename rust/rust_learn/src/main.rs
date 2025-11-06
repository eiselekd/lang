
#[allow(unused, unused_imports, unused_variables)]
use anyhow::Context;
use anyhow::Result;

use std::error;
use std::mem;

use regex::Regex;

use std::path::Path;
use std::io::{Read};
#[allow(unused_imports)]
use std::{fs::File};

fn slurp(filename : &str) -> Result<String>
{
    let mut file = File::open(filename)
        .context("Open file {p}")?;
    let mut b : String = String::new();
    match file.read_to_string(&mut b) {
        Err(e) => panic!("{}", e),
        Ok(_)  => println!("File opened successfully: {:?} {}", file, b),
    }
    Ok(b)
}

fn split_to_vec(content : &String) -> Result<Vec<&str>>
{
    let _v = Vec::<&str>::new();
    let r = Regex::new(r"\n").unwrap();
    let mut a : Vec<&str> = r.split(&content).collect();
    a.retain(|x| x.len() > 0);
    Ok(a)
}

#[derive(Debug)]
enum MyErr {
    Custom(String),
    IoError(std::io::Error)
}

impl std::fmt::Display for MyErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MyErr::Custom(msg) => write!(f, "Custom error: {}", msg),
            MyErr::IoError(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl From<std::io::Error> for MyErr {
    fn from(err: std::io::Error) -> MyErr {
        MyErr::IoError(err)
    }
}

impl error::Error for MyErr {
    fn cause(&self) -> Option<&dyn error::Error> {
        match self {
            MyErr::Custom(_) => None,
            MyErr::IoError(e) => Some(e),
        }
    }
    
    fn description(&self) -> &str {
        match self {
            MyErr::Custom(msg) => msg,
            MyErr::IoError(e) => e.description(),
        }
    }
    
    
}

fn p() {
    //panic!("Exit");
}

fn f() {
    struct G;
    impl Drop for G {
        fn drop(&mut self) {
            println!("panick Dropped\n");
        }
    }
    let _g = G;

    fn o() {
        p();
    }

    let f_v = || -> () { p() };
    
    let rv = f_v();
    

    mem::forget(_g);
    
    rv
}


fn main () -> Result<()>
{


    let vec1 : Vec<i32> = Vec::new();
    //println!("{:?}", vec1);
    
    let vec2 = vec![1,2,3];
    //println!("{:?}", vec2);

    let mut vec3 = Vec::<i32>::new();
    
    vec3.push(1);
    vec3.push(2);
    vec3.push(3);
    vec3.push(4);
    let e = vec3.get(5);
    //println!("{:?}", e);

    for i in &mut vec3 {
        *i += 1;
        //println!("{}" , i);
    }
    
    #[derive(Debug)]
    enum Spread {
        Int(i32),
        Float(f32)
    }

    impl Drop for Spread {
        fn drop(&mut self) {
            //println!("{:?} ", self);
        }
    }
    
    let r = vec![Spread::Int(1), Spread::Float(0.1)];
    //println!("{:?}", r);

    #[derive(Debug)]
    enum R<M0, M1> {
        V0(M0),
        V1(M1)
    }

    impl<M0, M1> R<M0, M1> {
        fn new_V0(v : M0) -> Self {
            R::V0(v)
        }
        fn new_V1(v : M1) -> Self {
            R::V1(v)
        }
    }

    fn f0<M: std::fmt::Debug>(r : M) {
        println!("r:{:?}", r);
    }
    

    let v0 = R::<i32,i32>::new_V0(0);
    let v1 = R::<i32,i32>::new_V1(1);
    println!("v0:{:?}", v0);
    println!("v1:{:?}", v1);
    
    f0(v0);
    
    
    

    
    
    
    
    
    













    let p : &Path = Path::new("data/file0.txt");
    let pf : &Path = Path::new("data/filters0.txt");

    f();
    
    let n = pf.to_str().unwrap();
    let c = slurp(n)?;
    let a = split_to_vec(&c)?;

    println!("{:?}", a);

    
    //let e = "1".parse().map_err(|x| "a");
    
    
    let pn : i32 = 1;
    println!("{:?}",pn);

    let size = "100";
    let v = size.parse::<u64>().context(format!("parse {}", size))?;
    
    println!("{}", v);
    
    //let file_path = "data/file02.txt";

    // Open the file in read-only mode
    let mut file = File::open(p)
        .context("Open file {p}")?;

    let mut b : String = String::new();

    match file.read_to_string(&mut b) {
        Err(e) => panic!("{}", e),
        Ok(_)  => println!("File opened successfully: {:?} {}", file, b),
    }

    let r = Regex::new(r"\n").unwrap();
    let g: Vec<&str> = r.split(&b).collect();
    //g.retain(|x| x.len() > 0);
    let mut idx  = 0;
    for i in g.
        iter().
        filter(|x| x.len() > 0)
    {
        for j in &a {
            let regfilter = Regex::new(j).unwrap();
            match regfilter.find(i) {
                Some(m) => {
                    println!("{:<4}: {} {:?}: {:?}", idx, i, regfilter, m);
                    idx +=1;
                    break
                },
                None => ()
            }
        }
    }

    Ok(())  
}



