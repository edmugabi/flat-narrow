

use std::collections::{BTreeMap};

use crate::term::Term;
use crate::sld::Env;
use crate::clause::Clause;
use crate::term::EqnOp;
use crate::sld::{CPoint, Soln};

pub enum NarrowSoln<'a> {
    CPoints(CPoint<'a>, CPoint<'a>),
    CPoint(CPoint<'a>),
    Res(Env),
    NoClause,
    NoSubTerm
}

pub enum ReflSoln<'a> {
    CP(CPoint<'a>),
    NotEqn,
    Res(Env),
    NotUnif
}

impl<'a> CPoint<'a> {
    pub fn narrow(mut self) -> NarrowSoln<'a> {
        //let Self { goals, clauses, env, depth, k} = self;

        match self.goals.split_first_mut() {
            None => NarrowSoln::Res(self.env),
            Some((goal, rest_goals)) => {
                match goal.find_subterm() {
                    None => NarrowSoln::NoSubTerm,
                    Some(subterm) => {
                        for i in 0..self.clauses.len() {
                            let mut new_env = Env(BTreeMap::new());

                            let rule = &self.clauses[i];
                            let (rule, k1) = rule.rename_rule(self.k);
                            let Clause {conds, conseqt } = rule;
                            
                            match conseqt.equation() {
                                Some((lhs, EqnOp::Eq, rhs)) => {
                                    match subterm.unify_baader(lhs, &mut new_env) {
                                        Ok(()) => {
                                            // return two choice points, which will be rewritten
                                            let ret_env = new_env.compose(&self.env);

                                            *subterm = rhs.clone();
                                            let mut ret_goals = vec![goal.clone()];
                                            ret_goals.extend(rest_goals.iter().cloned());
                                            let cp0 = CPoint {
                                                clauses: self.clauses,
                                                goals: ret_goals,
                                                env: ret_env,
                                                depth: self.depth+1,
                                                k: k1

                                            };

                                            let cp1 = CPoint {
                                                clauses: &self.clauses[i+1..self.clauses.len()],
                                                goals: self.goals,
                                                env: self.env,
                                                depth: self.depth,
                                                k: self.k
                                            };
                                            return NarrowSoln::CPoints(cp0, cp1)
                                        },
                                        Err(_) => { continue }
                                    }
                                }
                                _ => { continue }
                            }
                        }

                        // reflection
                        let new_var = format!("ReflVar{}", self.k+1);
                        let new_env = Env(BTreeMap::from([
                            (new_var.clone(), subterm.subst(&self.env))
                        ]));
                        
                        let ret_env = new_env.compose(&self.env);

                        *subterm = Term::Var(new_var);
                        let mut ret_goals = vec![goal.clone()];
                        ret_goals.extend(rest_goals.iter().cloned());
                        let cp = CPoint {
                            goals: ret_goals,
                            clauses: self.clauses,
                            env: ret_env,
                            depth: self.depth + 1,
                            k: self.k + 2
                        };
                        return NarrowSoln::CPoint(cp)
                    }
                }
            }
        }
    }

    pub fn reflect(&self) -> ReflSoln<'a> {
        match self.goals.split_first() {
            None => ReflSoln::Res(self.env.clone()),
            Some((goal, rest_goals)) => {
                match goal.equation() {
                    None | Some((_, EqnOp::Arrow, _)) => ReflSoln::NotEqn,
                    Some((lhs, EqnOp::Eq, rhs)) => {
                        let mut new_env = Env(BTreeMap::new());
                        match lhs.unify_baader(rhs, &mut new_env) {
                            Err(_) => ReflSoln::NotUnif,
                            Ok(()) => {
                                let ret_env = new_env.compose(&self.env);
                                let cp = CPoint {
                                    clauses: self.clauses,
                                    goals: rest_goals.to_vec(),
                                    env: ret_env,
                                    depth: self.depth,
                                    k: self.k
                                };
                                return ReflSoln::CP(cp)
                            },
                            
                        }
                    }
                }
            }
        }
    }


}

//list is assumed to be leftmost innermost subterm,
// whose arguments are variables or constructors
fn narrow_term<'a, 'b: 'a>(
    list: &[Term],
    rules: &'b [Clause],
    prev_env: &Env,
    depth: usize,
    k: usize
) -> Option<(Term,(Vec<Term>, Env, &'b [Clause], usize,usize))> {
    let term = Term::List(list.to_vec());
    let term = term.subst(prev_env);

    for i in 0..rules.len() {
        let mut new_env = Env(BTreeMap::new());
        let (rule, k1) = rules[i].rename_rule(k);
        let Clause {conds, conseqt } = rule;
        
        match conseqt.equation() {
            Some((lhs, EqnOp::Eq, rhs)) => {
                match term.unify_baader(lhs, &mut new_env) {
                    Ok(()) => {
                        let rest_clauses = &rules[i+1..rules.len()];
                        let ret_env = new_env.compose(prev_env);
                        return Some((
                            rhs.clone(),
                            (conds,ret_env, rest_clauses,depth+1,k1))
                        )
                    },
                    Err(_) => { continue }
                }
            }
            _ => { continue }
        }
    }
    return None
}