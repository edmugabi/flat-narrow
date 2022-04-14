#[derive(Clone, Debug, PartialEq)]
enum Symbol {
    Const(String),
    Var(String)
}

impl<'a> Symbol {
    fn subst<'b> (&'b self, env: &'b Env) -> &'b Symbol {
        match self {
            c@Symbol::Const(_) => c,
            Symbol::Var(x) => match env.map.get(x) {
                Some(t) => t,
                None => self
            }
        }
    }
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
            (Var(x), sym) => Ok(Some((x.clone(), sym.clone()))),
        }
    }
}

fn unify_symbols(ts0: &[Symbol], ts1: &[Symbol])
-> Result<(),()>
{
    use Symbol::*;
    let mut env = Env::empty();
    match (ts0, ts1) {
        ([], []) => Ok(()),
        ([], [_,..]) => Err(()),
        ([_,..], []) => Err(()),
        ([Var(x)], ts)
        | (ts, [Var(x)]) => { Ok(())},
        ([_,..], [_, ..]) => { Ok(()) }
    }
    // match (ts0.split_first(), ts0.split_first()) {
    //     (Some((t, ts)), Some((u, us))) => {
    //         match t.unify(u) {
    //             Ok(env) => 
    //         }
    //     }
    // }
}

use std::collections::BTreeMap;
#[derive(Clone, PartialEq, Debug)]
pub struct Env {
    map: BTreeMap<String, Symbol>
}

#[derive(Debug, PartialEq, Clone)]
struct Found(String);

impl Env {

    fn empty() -> Self {
        Self { map: BTreeMap::new() }
    }

    fn get_transitive(&self, key: &str) -> Option<&Symbol> {
        match self.map.get(key) {
            sym@Some(Symbol::Const(_)) => return sym,
            Some(Symbol::Var(x)) => self.get_transitive(x),
            None => None,
        }
    }

    pub fn compose(self, other: &Env) -> Env {
        let mut ret_env: BTreeMap<_,_> = other.map.iter()
            //Substitute in rhs
            .map(|(x,t)| (x.clone(), t.subst(&self).clone()))
            // remove identity substitutions created
            .filter(|(x, t)| &Symbol::Var(x.to_string()) != t)
            .collect();

        // remove substitions whose lhs clashes with ret_env
        for (k, v) in self.map.into_iter() {
            ret_env.entry(k).or_insert(v);
        }

        Env { map: ret_env }
    }
    // returns None if it insert the value or the value that could not unify
    // with the the current value
    fn insert(&mut self, var: &str, sym: &Symbol) -> Result<(), Found> {
        use Symbol::*;
        match (self.get_transitive(var).cloned(), sym) {
            (Some(Const(a)), Const(b)) => {
                if &a == b {
                    // for optimisation
                    self.map.insert(var.to_owned(), sym.clone());
                    return Ok(())
                }
                else {
                    return Err(Found(a))
                }
            },
            (Some(Var(x)), sym@_) => {
                // var = x already exists
                // now insert x = sym
                // we want var = sym

                // which will make var = sym transitively

                // if it is success full we can optimise and mutate 
                self.insert(&x, sym)
                    // .map(|_| {
                    //     assert_eq!(self.map.insert(x, cnst.clone()), Some(Var(x)));
                    // })
                
                
            }
            (Some(cnst@Const(_)), Var(x)) => {
                // we have var = cnst
                // we want to insert var = x
                // we should instead insert x = cnst to ensure the new
                // assignment doesn't cause cycles
                return self.insert(x, &cnst);
            }
            (None, _) => {
                // this insert should not fail, otherwise panic
                self.map.insert(var.to_owned(), sym.clone());
                Ok(())
            }
        }

    }
}