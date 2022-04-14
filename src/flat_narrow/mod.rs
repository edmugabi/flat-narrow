
use std::iter::once;

#[macro_use]
mod term;
mod pattern;
mod goal;
mod clause;
mod env;
mod cpoint;

mod parser;
mod json;

//TODO
/*
Pretty Print Clause
*/


/*
        "A && ! A  = false",


        "( false || A ) = A",
        "( true || A ) = true",
        "X || Y || Z = ( X || Y ) || Z",
        "X || ( Y || Z ) = ( X || Y ) || Z",
        "( X || Y ) = ( Y || X )",

        "A => B =  ! A  || B",
        "A && B => C = ( A && B ) => C",
        "A || ! A  = true"

*/


pub trait PrettyPrint {
    fn pretty_print(&self) -> String;
}

use std::fmt;
use std::collections::BTreeSet;




use std::fs;

use json::ProgramItem;

//RUST_LOG=debug cargo run --bin fnarrow path/to/json/file
fn main() {
    let args: Vec<_> = std::env::args().collect();
    env_logger::init();

    let filename = args.get(1)
        .expect("No file name provided");
    
    let json_str = fs::read_to_string(filename)
        .expect("Could not read the file");

    match json::ProgramItem::test(&json_str) {
        Ok(()) => println!("The queries agree with the solutions provided"),
        Err(err) => println!("{}", err)
    }
        
}


#[derive(Clone, PartialEq, Debug)]
enum Pos {
    Left,
    Right
}

impl Pos {
    fn index(&self) -> usize {
        match self {
            Pos::Left => 0,
            Pos::Right => 1
        }
    }
    fn alt_index(&self) -> usize {
        (self.index() + 1) % 2
    }
}


