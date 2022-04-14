
use std::fmt;
use std::collections::{BTreeMap, HashSet};

use super::env::Env;
use super::PrettyPrint;

type Sub = BTreeMap<String, Symbol>;

// TODO, will recurse indefinitely if sub contains a cycle
fn get_transitive<'a>(sub: &'a Sub, key: &str) -> Option<&'a Symbol> {
    match sub.get(key) {
        sym@Some(Symbol::Const(_)) => return sym,
        sym@Some(Symbol::Var(x)) => {
            match get_transitive(sub, x) {
                sym1@Some(_) => return sym1,
                None => return sym
            }
        },
        None => None,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Symbol {
    Const(String),
    Var(String)
}

macro_rules! cnst {
    ($c:literal) => {
        $crate::flat_narrow::term::Symbol::Const($c.to_owned())
    };
}

#[macro_export]
macro_rules! tcnst {
    ($c:literal) => {
        $crate::flat_narrow::term::Term::from(
            $crate::flat_narrow::term::Symbol::Const($c.to_owned())
        )
    };
}
#[macro_export]
macro_rules! var {
    ($x:literal) => {
        $crate::flat_narrow::term::Symbol::Var($x.to_string())
    };
}

macro_rules! tvar {
    ($x:literal) => {
        $crate::flat_narrow::term::Term::from(
            $crate::flat_narrow::term::Symbol::Var($x.to_owned())
        )
    };
}

impl<'a> Symbol {
    fn subst<'b> (&'b self, sub: &'b Sub) -> Symbol {
        match self {
            c@Symbol::Const(_) => c.clone(),
            s@Symbol::Var(x) => 
                match /*sub.get(x) */ get_transitive(sub, x){
                Some(t) => t.clone(),
                None => s.clone()
            }
        }
    }
}

impl Symbol {
    // an option is a list of at most one element
    fn unify<'c>(&self, other: &Symbol)
    -> Result<Option<(String, Symbol)>, &'c str> {
        use Symbol::*;
        match (self, other) {
            (Const(a), Const(b)) => {
                if a == b {
                    Ok(None)
                } else { Err("Cannot unify") }
            }
            (c@Const(_), var@Var(_)) => var.unify(c),
            (Var(x), sym) => Ok(Some((
                x.clone(),
                sym.clone()
            ))),
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Const(c) => f.write_str(c),
            Symbol::Var(x)   => f.write_str(x)
        }
    }
}

impl PrettyPrint for Symbol {
    fn pretty_print(&self) -> String {
        match self {
            Symbol::Const(c) => c.into(),
            Symbol::Var(v)   => format!("[{}]", v)

        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Term {
    pub symbols: Vec<Symbol>
}

impl Term {

    fn subst_symbols(symbols: &mut [Symbol], env: &Env) {
        todo!()
    }
        
    pub fn subst_mut<'a>(&mut self, env: &Env) {
        for i in 0..self.symbols.len() {
            match &mut self.symbols[i] {
                Symbol::Const(_) => continue,
                Symbol::Var(x) => match env.map.get(x) {
                    Some(sym1) => {
                        let t =  sym1.symbols.iter().cloned();

                        let tail = self.symbols.split_off(i);
                        self.symbols.extend_from_slice(&sym1.symbols);
                        //ensure the element is eliminated
                        self.symbols.extend_from_slice(&tail[1..]);

                        //BUGGY Line replat
                        //self.symbols.splice(i..i, t);
                    }
                    None => continue
                }
            }
        }
    }

    pub fn rename(&mut self, map: &BTreeMap<String, String>) {
        for i in 0..self.symbols.len() {
            match &mut self.symbols[i] {
                Symbol::Const(_) => continue,
                Symbol::Var(x) => match map.get(x.as_str()) {
                    Some(y) => {
                        *x = y.clone();
                    }
                    None => continue
                }
            }
        }
    }

    pub fn subst(&self, env: &Env) -> Term {
        let mut t = self.clone();
        t.subst_mut(env);
        t
    }

    pub fn all_vars(&self) -> HashSet<&str> {
        self.symbols.iter().filter_map(|sym| {
            if let Symbol::Var(x) = sym {
                Some(x.as_ref())
            }
            else { None }
        })
        .collect()
    }

    pub fn unify_symbols<'b>(ts0: &[Symbol], ts1: &[Symbol], sub: &mut Sub)
    -> Result<(), &'b str>
    {
        //let mut env = Env::empty();
        use Symbol::*;
        if ts0.len() == ts1.len() {
            for i in 0..ts0.len() {
                let fst = ts0[i].subst(&sub);
                let snd = ts1[i].subst(&sub);
                //println!("{:?}", sub);
                match fst.unify(&snd) {
                    Ok(Some((x, sym))) => {
                        if Symbol::Var(x.clone()) != sym {
                            sub.insert(x.into(), sym);
                        }
                    },
                    Ok(None) => { continue }
                    Err(err)  => { return Err(err) },
                }
            }
            Ok(())
        }
        else { Err("Could not unify")}
    }

    pub fn unify<'b>(&self, other: &Term) -> Result<Env, &'b str>
    {
        let mut sub = BTreeMap::new();
        Self::unify_symbols(&self.symbols, &other.symbols, &mut sub)?;
        Ok(sub.into())
    }

}

impl From<Symbol> for Term {
    fn from(symbol : Symbol) -> Term {
        Term { symbols : Vec::from([symbol]) }
    }
}

impl<Ls> From<Ls> for Term
where Ls: IntoIterator<Item=Symbol>
{
    fn from(ls: Ls) -> Term {
        let symbols = ls.into_iter().collect();
        Term { symbols }
    }
} 

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = self.symbols.iter()
            .map(|sym| sym.to_string())
            .collect::<Vec<_>>()
            .join("");
        f.write_str(&s)
    }
}

impl PrettyPrint for Term {
    fn pretty_print(&self) -> String {
        let s = self.symbols.iter()
        .map(|sym| sym.pretty_print())
        .collect::<Vec<_>>()
        .join("");
        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbol_tests() {
        let map: BTreeMap<String, _> = BTreeMap::from([
            ("x".into(), cnst!("a")),
            ("y".into(), cnst!("b"))
        ]);
        

        assert_eq!(cnst!("a").subst(&map), cnst!("a"));

        assert_eq!(var!("x").subst(&map), cnst!("a"));
        assert_eq!(var!("z").subst(&map), var!("z"));
    }

    #[test]
    fn term_test() {
        let t = Term::from([
            cnst!("a"), var!("x"), var!("y")
        ]);
        let expected = HashSet::from(["x", "y"]);
        assert_eq!(t.all_vars(), expected);


        let map: BTreeMap<String, _> = BTreeMap::from([
            ("x".into(), tcnst!("a")),
            ("y".into(), tcnst!("b"))
        ]);
        let env = Env { map };
        let expected = Term::from([
            cnst!("a"), cnst!("a"), cnst!("b")
        ]);

        assert_eq!(t.subst(&env), expected);
    }

    #[test]
    fn unify_lists_test() {
        assert!(tcnst!("a").unify(&tcnst!("b")).is_err());

        let t = Term::from([
            cnst!("a"), var!("x"), var!("y"), var!("z"), var!("y")
        ]);
        let s = Term::from([
            cnst!("a"), var!("y"), var!("z"), var!("x"), var!("x")
        ]);

        let env = Env::from([
            ("x", tvar!("y")),
            ("y", tvar!("z"))
        ]);

        let result = t.unify(&s);
        assert_eq!(result, Ok(env));
    }

    #[test]
    fn rename_test() {
        let mut t = Term::from([
            cnst!("a"), var!("y"), var!("w"), var!("t")
        ]);
        let env = BTreeMap::from([
            ("t".into(), "y".into()),
            ("y".into(), "y1".into()),
            ("w".into(), "w1".into())
        ]);
        let expected = Term::from([
            cnst!("a"), var!("y1"), var!("w1"), var!("y")
        ]);
        t.rename(&env);
        assert_eq!(t, expected);
    }

    


}