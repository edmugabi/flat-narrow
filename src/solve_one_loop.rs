
use std::collections::BTreeMap;

use crate::sld::{CPoint, Env};
use crate::clause::Clause;
use crate::term::{Term,EqnOp};


pub enum Soln<'b> {
    CPoints(CPoint<'b>, CPoint<'b>),
    CPoint(CPoint<'b>),
    ReflCP(CPoint<'b>),
    Res(Env),
    NoClauses,
    NoSubTerm
}

/*
goal  rule
t0=t1  c => l=r
t0     c => l=r
t0=t1  c => l
t0     c => l
*/

impl<'a> CPoint<'a> {
        
    fn solve_goal_all<'c>(self) -> Soln<'c> where Self: 'c {

        match self.goals.split_first() {
            None => Soln::Res(self.env.clone()),
            Some((goal, rest_goals)) => {
                
                for i in 0..self.clauses.len() {

                    let (Clause {conds, conseqt }, k1)
                            = self.clauses[i].rename_rule(self.k);
                    let goal = goal.subst(&self.env);


                    match conseqt.equation() {
                        Some((_, EqnOp::Arrow, _)) => continue,
                        Some((lhs, EqnOp::Eq, rhs)) => {
                            // narrow and innermost reflection
                            match goal.find_subterm() {
                                None => Soln::NoSubTerm,
                                Some(subterm) => {
                                    match conseqt.equation() {
                                        Some((lhs, EqnOp::Eq, rhs)) => {
                                            let new_env = Env::empty();
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
                                                    return Soln::CPoints(cp0, cp1)
                                                },
                                                Err(_) => {
                                                    // REFLECT
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
                                                    return Soln::CPoint(cp)
                                                }
                                            }
                                        }
                                        _ => { continue }
                                    }
                                    
                                }
                            }
                        }
                        None => match goal.equation() {
                            Some((lhs, EqnOp::Eq, rhs)) => {
                                // reflection
                                let mut new_env = Env::empty();
                                match lhs.unify_baader(rhs, &mut new_env) {
                                    Ok(()) => {
                                        let ret_env = new_env.compose(&self.env);
                                        let cp = CPoint {
                                            clauses: self.clauses,
                                            goals: rest_goals.to_vec(),
                                            env: ret_env,
                                            depth: self.depth,
                                            k: self.k
                                        };
                                        return Soln::ReflCP(cp)
                                    }
                                    Err(_) => continue
                                }
                            }
                            None => {
                                // resolution
                                let mut new_env = Env::empty();

                                match conseqt.unify_baader(&goal, &mut new_env) {
                                    Ok(()) => {
                                        let mut ret_goals = conds;
                                        ret_goals.extend(rest_goals.into_iter().cloned());
                        
                                        let rest_rules = 
                                            &self.clauses[i+1..self.clauses.len()];
            
                                        let ret_env = new_env.compose(&self.env.clone());
            
                                        let cp0 = CPoint {
                                            goals: ret_goals,
                                            clauses: self.clauses,
                                            env: ret_env,
                                            depth: self.depth+1,
                                            k: k1
                                        };
                                        let cp1 = CPoint {
                                            goals: self.goals.clone(),
                                            clauses: rest_rules,
                                            env: self.env,
                                            depth: self.depth,
                                            k : self.k
                                        };
            
                                        //println!("cp0: {:?}", cp0);
                                        //println!("\n\n");
                                        //println!("cp1: {:?}", cp1);
            
            
                                        return Soln::CPoints(cp0, cp1)
                                        
                                    },
                                    Err(_) => continue
                                }
                            }
                        }
                    }
                    //println!("{}", goal);
                    //println!("{}", renamed.0);
                    //println!("{}", self.env.pretty_print());
                    //println!("\n\n");

                }
                Soln::NoClauses
            },
        }
    }
}
