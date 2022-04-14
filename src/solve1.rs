use std::collections::{BTreeMap};

use crate::sld::{CPoint, Env};
use crate::clause::Clause;
use crate::term::Term;

enum Soln<'a> {
    CP(CPoint<'a>, CPoint<'a>),
    Res(Env),
    None
}

impl<'a> CPoint<'a> {
    fn narrow2<'b>(mut self) -> Soln<'a>
    {
        match self.goals.split_first_mut() {
            None => return Soln::Res(self.env),
            Some((goal, rest_goals)) => {
                goal.narrow2(self.clauses, rest_goals, &self.env, self.depth, self.k)
            }
        }
    }
}


// todo, how to include unification of goal lhs and rhs
impl Term {
    fn narrow2<'a>(
        &mut self,
        rules: &'a [Clause],
        rest_goals: &[Term],
        prev_env: &Env,
        depth: usize,
        k: usize
    )
    -> Soln<'a>
    {
        match self {
            t@Term::Const(_)
            | t@Term::Var(_)  => Soln::None,
            Term::List(ts) => {

                for s in ts {
                    //let s = &ts[i];
                    match s {
                        Term::Const(_)
                        | Term::Var(_) => continue,
                        Term::List(_) => {
                            return s.narrow2(rules, rest_goals, prev_env, depth, k);
                        }
                    }
                }

                for i in 0..rules.len() {
                    let (rule, k1)  = rules[i].rename_rule(k);
                    let Clause { conds, conseqt } = rule;
                    let (lhs, rhs) = conseqt.eqn();

                    let mut new_env = Env::empty();
                    let t = self.subst(prev_env);
                    match t.unify_baader(&lhs, &mut new_env) {
                        Ok(()) => {

                            let ret_env = new_env.compose(prev_env);
                            let mut ret_goals = conds.to_vec();
                            *self = rhs;
                            ret_goals.push(self.clone());
                            ret_goals.extend(rest_goals.into_iter().cloned());

                            let cp0 = CPoint {
                                clauses: rules,
                                goals: ret_goals,
                                env: ret_env,
                                depth: depth + 1,
                                k: k1
                            };

                            let cp1 = CPoint {
                                clauses: &rules[i+1..rules.len()],
                                goals: std::iter::once(self.clone())
                                    .chain(rest_goals.into_iter().cloned()).collect(),
                                env: prev_env.clone(),
                                depth: depth,
                                k: k
                            };

                            // let cp1 = CPoint {
                            //     clauses: &rules[i+1..rules.len()],
                            //     goals: // self.goals,
                            // }
                            
                            return Soln::CP(cp0, cp1)
                        },
                        Err(_) => continue
                    }

                }
                // innermost reflect the term here
                let new_var = format!("ReflVar{}", k);
                let new_env = Env(BTreeMap::from([
                    (new_var.clone(), self.subst(prev_env))
                ]));

                let ret_env = new_env.compose(prev_env);
                *self = Term::Var(new_var);
                //TODO subst
                return self.narrow2(rules, rest_goals, &ret_env, depth, k+1)
            }
        }
    }

    fn eqn(self) -> (Term, Term) {
        unimplemented!()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn narrow_test() {
        use Term::*;
        let eq = |x: &str, y: &str| Term::List(vec![
            Term::var(x), Term::cnst("="), Term::var(y)
        ]);


        let clauses = &[
            Clause::new(vec![], eq("x", "x")),
            Clause::new(vec![ eq("x", "y")], eq("y", "x")),
            Clause::new(vec![eq("x","y"), eq("y", "z")], eq("x", "z"))
            
        ];

    }
}
