use std::collections::{BTreeMap};

use crate::sld::{CPoint, Env};
use crate::clause::Clause;
use crate::term::Term;

enum Soln<'a> {
    CP(CPoint<'a>, CPoint<'a>),
    Res(Env),
    None
}

impl Term {

    fn extract_eqn(self) -> (Term, Term) {
        unimplemented!()
    }

    fn narrow_term(
        &mut self,
        rules: &[Clause],
        prev_env: &Env,
        depth: usize,
        k: usize
    ) -> Option<Term>
    {
        match self {
            Term::Var(_)
            | Term::Const(_) => None,
            Term::List(redex) => {
                for i in 0..redex.len() {
                    //let s = &ts[i];
                    match &mut redex[i] {
                        Term::Const(_)
                        | Term::Var(_) => continue,
                        ts@Term::List(_) => {
                            // it is a Term::List(_)
                            // so only the third match arm in narrow_term
                            // function ever applies
                            return ts.narrow_term(rules, prev_env, depth, k);
                        }
                    }
                }
                match narrow_innermost(redex, rules, prev_env, k) {
                    Some((term, conds, env,k1)) => {
                        *self = term;
                        unimplemented!()
                    },
                    None => {
                        let (term, conds, env, k1) = reflect_innermost(redex, prev_env, k);
                        *self = term;
                        unimplemented!()
                        // try to narrow again the term containing the redex
                    }
                }
            }
        }
    }
    fn narrow_redex<'a>(
        redex: &mut [Term],
        index: Option<usize>,
        rules: &'a [Clause],
        prev_env: &Env,
        depth: usize,
        k: usize
    )
    -> (Term, Vec<Term>, Env, usize)
    {
        for i in 0..redex.len() {
            //let s = &ts[i];
            match &mut redex[i] {
                Term::Const(_)
                | Term::Var(_) => continue,
                Term::List(inner) => {
                    return Term::narrow_redex(inner, Some(i), rules, prev_env, depth, k);
                }
            }
        }
        match narrow_innermost(redex, rules, prev_env, k) {
            Some((term, conds, env,k1)) => {
                (term, conds, env,k1)
            },
            None => {
                reflect_innermost(redex, prev_env, k)
                // try to narrow again the term containing the redex
            }
        }
    }
}

fn narrow_innermost<'a>(
    redex: &[Term],
    rules: &'a [Clause],
    prev_env: &Env,
    k: usize
)
// rhs and new conditions, ret_env and k
-> Option<(Term, Vec<Term>, Env, usize)>
{
    for i in 0..rules.len() {
        let (rule, k1)  = rules[i].rename_rule(k);
        let Clause { conds, conseqt } = rule;
        let (lhs, rhs) = conseqt.extract_eqn();

        let t = Term::subst_redex(redex, prev_env);

        let mut new_env = Env::empty();
        match t.unify_baader(&lhs, &mut new_env) {
            Ok(()) => {

                let ret_env = new_env.compose(prev_env);                
                return Some((rhs, conds, ret_env, k1))
            },
            Err(_) => continue
        }

    }

    return None
}

// fn subst<'a>(redex: &'a [Term], env: &Env) -> Term {
//     unimplemented!()
// }

fn reflect_innermost(term: &[Term], prev_env: &Env, k: usize)
// new_term, new_env, new_k
-> (Term, Vec<Term>, Env, usize)
{
    // innermost reflect the term here
    let new_var = format!("ReflVar{}", k);
    let new_env = Env(BTreeMap::from([
        (new_var.clone(), Term::subst_redex(term, prev_env))
    ]));

    let ret_env = new_env.compose(prev_env);
    (Term::Var(new_var), vec![], ret_env, k+1)
}