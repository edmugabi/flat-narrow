use std::fmt;
use super::term::Term;

use super::PrettyPrint;
use super::term::Symbol;

#[derive(Debug, PartialEq, Clone)]
pub struct Goal(pub [Term;2]);



impl fmt::Display for Goal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.0.iter()
            .map(|term| term.to_string())
            .collect::<Vec<_>>()
            .join(" = ");
        f.write_str(&s)
    }
}

impl PrettyPrint for Goal {
    fn pretty_print(&self) -> String {
        self.0.iter()
            .map(|term| term.pretty_print())
            .collect::<Vec<_>>()
            .join(" = ")
    }
}
impl PrettyPrint for [Goal] {
    fn pretty_print(&self) -> String {
        self.iter()
            .map(|goal| goal.pretty_print())
            .collect::<Vec<_>>()
            .join(" && ")
    }
}