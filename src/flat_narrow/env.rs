#[macro_use]
use std::collections::{BTreeMap, HashSet};

use super::PrettyPrint;
use super::term::{Term, Symbol};

// TODO: How to avoid cycles in Env, especially when the transitive
// equality in sub is implicit
#[derive(Clone, PartialEq, Debug)]
pub struct Env {
    pub map: BTreeMap<String, Term>
}

#[derive(Debug, PartialEq, Clone)]
struct Found(String);

impl Env {

    pub fn empty() -> Self {
        Self { map: BTreeMap::new() }
    }
    
    // fn get_transitive(&self, key: &str) -> Option<&Symbol> {
    //     match self.map.get(key) {
    //         sym@Some(Symbol::Const(_)) => return sym,
    //         Some(Symbol::Var(x)) => self.get_transitive(x),
    //         None => None,
    //     }
    // }
    

    pub fn compose(self, other: &Env) -> Env {
        let mut ret_env: BTreeMap<_,_> = other.map.iter()
            //Substitute in rhs
            .map(|(x,t)| (x.clone(), t.subst(&self).clone()))
            // remove identity substitutions created
            .filter(|(x, t)| &Term::from(Symbol::Var(x.to_string())) != t)
            .collect();

        // remove substitions whose lhs clashes with ret_env
        for (k, v) in self.map.into_iter() {
            ret_env.entry(k).or_insert(v);
        }

        Env { map: ret_env }
    }
}

impl<K,V,Ps> From<Ps> for Env
where K: Into<String>,
      V: Into<Term>,
     Ps: IntoIterator<Item=(K,V)>
{
    fn from(ps: Ps) -> Env {
        let map = ps.into_iter()
            .map(|(k,v)| (k.into(), v.into()))
            .collect();
        Env { map }
    }
}

impl PrettyPrint for Env {
    fn pretty_print(&self) -> String {
        let inner = self.map.iter()
            .map(|(k,v)| format!("{} -> {}", k, v))
            .collect::<Vec<_>>()
            .join(", ");
        inner

    }
}




#[cfg(test)]
mod tests {
    use super::*;
    //use crate::flat_narrow::term::{tvar, tcnst};
    #[test]
    fn compose_test() {

        let env = Env { map: BTreeMap::from([
            ("x".into(), tvar!("y")),
            ("z".into(), tcnst!("a")),
            ("w".into(), tcnst!("b"))
        ])};

        let env1 = Env { map: BTreeMap::from([
            ("y".into(), tvar!("x")),
            ("x".into(), tvar!("z")),
            ("w".into(), tcnst!("c"))
        ])};

        let expected = Env { map: BTreeMap::from([
            ("z".into(), tcnst!("a")),
            ("w".into(), tcnst!("b")),
            ("y".into(), tvar!("x")),
            ("x".into(), tvar!("z"))
        ])};

        assert_eq!(env1.compose(&env), expected);
    }

    #[test]
    fn env_test() {
        use Symbol::*;
        // let mut env = Env {
        //     map: BTreeMap::from([
        //         ("x", Var("y")),
                
        //         ("y", Const("a")),
        //         ("u", Var("v")),

        //     ])
        // };
        // assert_eq!(env.insert("y", &Const("a")), Ok(()));
        // assert_eq!(env.insert("y", &Const("b")), Err(Found("a")));
        // assert_eq!(env.insert("x", &Const("a")), Ok(()));
        // assert_eq!(env.get_transitive("x"), Some(&Const("a")));
        // assert_eq!(env.insert("x", &Var("z")), Ok(()));
        // assert_eq!(env.map.get("z"), Some(&Const("a")));
        // assert_eq!(env.insert("v", &Var("u")), Ok(()));
        // assert_eq!(env.insert("x", &Var("x")), Ok(()));

        
    }
}


// Trial on using disjoint union find
use disjoint_sets::UnionFindNode;
type Variable = disjoint_sets::UnionFindNode<Option<String>>;
struct Sub {
    map: BTreeMap<String, Variable>
}

impl Sub {
    fn insert_var_var(&mut self, v1: String, v2: String) {
        let mut w1 = self.map.entry(v1).or_insert_with(|| {
            UnionFindNode::new(None)
        }).clone();
        let mut w2 = self.map.entry(v2).or_insert_with(|| {
            UnionFindNode::new(None)
        }).clone();
        w1.union_with(&mut w2, |val1, val2| val1);
    }

    fn insert_var_const(&mut self, var: String, cnst: String) {
       // self.map.entry(var)
    }
}