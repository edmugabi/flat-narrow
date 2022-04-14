use nom::error::VerboseError;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::{is_alphanumeric};
use nom::character::complete::{alpha1, multispace0, satisfy};
use nom::character::complete::{alphanumeric0, alphanumeric1, space0};
use nom::combinator::{all_consuming, map, recognize, value};
use nom::multi::{many0, many_m_n, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair};

type IResult<I,O> = nom::IResult<I,O, VerboseError<I>>;


const SPECIAL: &str = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";

#[derive(Clone, Debug, PartialEq)]
enum Symbol<'a> {
    Const(&'a str),
    Var(&'a str)
}

impl<'a> Symbol<'_> {
    fn subst<'b> (&'b self, env: &'b Env<'_>) -> &'b Symbol<'_> {
        match self {
            c@Symbol::Const(_) => c,
            Symbol::Var(x) => match env.map.get(*x) {
                Some(t) => t,
                None => self
            }
        }
    }
}

impl<'a> Symbol<'a> {
    // an option is a list of at most one element
    fn unify<'c>(&self, other: &Symbol<'a>)
    -> Result<Option<(&'a str, Symbol<'a>)>, &'c str> {
        use Symbol::*;
        match (self, other) {
            (Const(a), Const(b)) => {
                if a == b {
                    Ok(None)
                } else { Err("Cannot unify") }
            }
            (c@Const(_), var@Var(_)) => var.unify(c),
            (Var(x), sym) => Ok(Some((x, sym.clone()))),
        }
    }
}
struct Patt<'a> {
    list: Vec<Symbol<'a>>
}

fn pconst(input: &str) -> IResult<&str, &str> {
    let predicate = |ch: char| ch.is_ascii_alphanumeric() || SPECIAL.contains(ch);
    recognize(satisfy(predicate))(input)
}

fn pvariable(input: &str) -> IResult<&str, &str> {
    recognize(pair(satisfy(char::is_uppercase), alphanumeric0))(input)
}

fn psymbol(input: &str) -> IResult<&str, Symbol> {
    alt((
        map(pvariable, Symbol::Var),
        map(pconst, Symbol::Const)
    ))(input)
}

fn ppattern(input: &str) -> IResult<&str, Patt<'_>> {
    let plist = separated_list1(space0, psymbol);
    map(plist, |list| Patt { list})(input)
}

fn pequation(input: &str) -> IResult<&str, (Patt<'_>, Patt<'_>)>
{
  separated_pair(
      ppattern, 
      delimited(space0, tag("="), space0), 
      ppattern
  )(input)
}

// struct CPoint<'a> {
//     eqns: &'a [(Patt<'a>, Patt<'a>)],
//     goal: (Patt<'a>, Patt<'a>),
//     env: Env<Has, Patt<'a>> 
// }

use std::collections::{BTreeMap, HashSet};
//type Env<'a> = HashMap<&'a str, Symbol<'a>>;
struct Env<'a> {
    map: BTreeMap<String, Symbol<'a>>
}

#[derive(Debug, PartialEq, Clone)]
struct Found<'a>(&'a str);

impl<'a> Env<'a> {

    fn empty() -> Self {
        Self { map: BTreeMap::new() }
    }

    fn get_transitive(&self, key: &str) -> Option<&Symbol<'a>> {
        match self.map.get(key) {
            sym@Some(Symbol::Const(_)) => return sym,
            Some(Symbol::Var(x)) => self.get_transitive(x),
            None => None,
        }
    }
    // returns None if it insert the value or the value that could not unify
    // with the the current value
    fn insert(&mut self, var: &'a str, sym: &Symbol<'a>) -> Result<(), Found> {
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
                self.insert(x, sym)
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

// fn narrow_one_step(goal: (Patt, Patt), eqns: &[(Patt, Patt)])
// {
//     let (fst, snd) = goal;
//     let mut goal = fst.list.to_vec();
//     goal.push(Symbol::Const("="));
//     goal.extend(snd.list.iter().cloned());

//     for i in 0..eqns.len() {
//         let (lhs, rhs) = &eqns[i];
//         let m = goal.len();
//         let n = lhs.list.len();
//         if m > n {

//             let (head_goal, rest_goal) = (&goal[0..n], &goal[n+1..m]);
//             match unify(head_goal, &lhs.list) {
//                 None => continue,
//                 Some(env) => {
//                     // create two choice points
//                 }

//             }
            
//         }
//         else { continue }
//     }

// }

fn subst<'a>(patt: &mut [Symbol<'a>], env: &Env<'a>) {
    for sym in patt {
        match sym {
            Symbol::Const(_) => continue,
            Symbol::Var(x) => match env.map.get(*x) {
                Some(sym1) => *sym = sym1.clone(),
                None => continue
            }
        }
    }
}

fn unify<'c, 'a: 'c,'b, 'd: 'a>(patt0: &'d [Symbol<'a>], patt1: &'d [Symbol<'a>],
    env: &'c mut Env<'c>
) -> Result<(), &'b str>
{
    //let mut env = Env::empty();
    use Symbol::*;
    if patt0.len() == patt1.len() {
        for i in 0..patt0.len() {
            let fst = patt0[i].subst(&env).clone();
            let snd = patt1[i].subst(&env).clone();

            match fst.unify(&snd) {
                Ok(Some((x,t))) => { env.map.insert(x.to_string(), t); },//env.map.extend(sub.into_iter()),
                Err(err)  => { return Err(err) },
            }
        }
        Ok(())
    }
    else { Err("Could not unify")}

}

#[cfg(test)]
mod tests {
    use super::*;

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
