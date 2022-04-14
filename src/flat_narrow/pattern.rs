use std::fmt;
use std::collections::{BTreeSet, BTreeMap};

use super::term::Term;
use super::env::Env;
use super::PrettyPrint;

#[derive(Clone, PartialEq, Debug)]
pub enum Patt {
    List {
        left : Term,
        var  : String,
        right: Term
    },
    Term(Term)
}

impl Patt {

    pub fn all_vars(&self) -> BTreeSet<&str> 
    {
        match self {
            Patt::Term(t) => t.all_vars().into_iter().collect(),
            Patt::List { left, var, right } => {
                let mut vars = left.all_vars();
                vars.insert(var);
                vars.union(&right.all_vars())
                    .cloned()
                    .collect::<BTreeSet<_>>()
            }
        }
    }

    pub fn rename(&mut self, map: &BTreeMap<String, String>)
    {
        match self {
            Patt::Term(t) => t.rename(map),
            Patt::List { left, var, right } => {
                left.rename(map);
                right.rename(map);
                
                match map.get(var.as_str()) {
                    Some(var1) => *var = var1.to_owned(),
                    None => ()
                };
            }
        }
    }

    /*
    fn unify<'a>(&self, other: &Patt) -> Result<Env, &'a str>
    {
        
       match (self, other) {
           (_, Patt::Term(s))  => self.unify_patt_term(s),
           (_, Patt::Term(s))  => self.unify_patt_term(s),

           (
               Patt::List { left, var, right},
               Patt::List { left: left1, var: var1, right: right1 }
           ) => {
                let mut env = Env::empty();
                Term::unify_symbols(&left.symbols, &left1.symbols, &mut env)?;
                // TODO we are doing var -> var1, check soundness
                env.map.insert(var.into(), Term::from(Symbol::Var(var1.into())));
                Term::unify_symbols(&right.symbols, &right1.symbols, &mut env)?;

                Ok(env)
           }
       }
    }
    */

    pub fn unify_patt_term<'a>(&self, other: &Term) -> Result<Env, &'a str>
    {
        match self {
            Patt::Term(t) => {
                t.unify(other)
            },
            Patt::List { left, var: spread_var, right } => {
                let m = left.symbols.len();
                let n = right.symbols.len();
                let p = other.symbols.len();

                if p >= m+n {
                    let other_left   = &other.symbols[0..m];
                    let other_var    = &other.symbols[m..p-n];
                    let other_right  = &other.symbols[p-n..p];

                    let mut map = BTreeMap::new();

                    Term::unify_symbols(&left.symbols, other_left, &mut map)?;
                    Term::unify_symbols(&right.symbols, other_right, &mut map)?;

                    let mut env: Env = map.into();
                    env.map.insert(
                        spread_var.into(),
                        Term { symbols: other_var.to_vec() }
                    );
                    Ok(env)
                }
                else { Err("Could not unify") }
            }
        }
    }
    
}


impl fmt::Display for Patt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        match self {
            Patt::Term(t) => f.write_str(&t.to_string()),
            Patt::List { left, var, right } => {
                f.write_str(&format!("{} ", left))?;
                f.write_str(&var)?;
                f.write_str(&format!(" {}", right))
            }
        }
    }
}


impl PrettyPrint for Patt {
    fn pretty_print(&self) -> String {
        self.to_string()
    }
}

#[test]
fn patt_term_unify_test() {
    let patt = Patt::List {
        left: Term::from([ var!("f"), cnst!("(")]),
        var : "list".into(),
        right : Term::from([cnst!(")")])
    };

    let term = Term::from([
        var!("g"), cnst!("("), var!("x"), cnst!(","), var!("y"), cnst!(")") 
    ]);

    let expected = Env::from([
        ("f", tvar!("g")),
        ("list", Term::from([ var!("x"), cnst!(","), var!("y")]))
    ]);

    assert_eq!(patt.unify_patt_term(&term), Ok(expected));

    let patt = Patt::List {
        left : Term::from(var!("x")),
        var  : "xs".into(),
        right: Term::from([])
    };

    let term = Term::from(cnst!("a"));

    let expected = Env::from([
        ("x", tcnst!("a")),
        ("xs", Term::from([]))
    ]);
    assert_eq!(patt.unify_patt_term(&term), Ok(expected));

    let term = Term::from([]);
    assert!(patt.unify_patt_term(&term).is_err());
}

#[test]
fn index_tests() {
    let m = 2;
    let n = 2;

    let list = [10,20,30,40,50];
    let p = list.len();
    println!("left  : {:?}", &list[0..m]);
    println!("right : {:?}", &list[p-n..p]);
    println!("middle: {:?}", &list[m..p-n]);
}