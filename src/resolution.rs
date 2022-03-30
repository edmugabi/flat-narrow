use std::collections::BTreeMap;

use crate::clause::Clause;
use crate::term::Term;
use crate::PrettyPrint;
use crate::sld::{CPoint, Env};


pub enum ReslnSoln<'b> {
    CP(CPoint<'b>, CPoint<'b>),
    Res(Env),
    NoClauses
}

impl<'b> CPoint<'b> {

    pub fn resolve<'c>(self) -> ReslnSoln<'c> where Self: 'c {
        match self.goals.split_first() {

            None => ReslnSoln::Res(self.env.clone()),
            Some((goal, rest_goals)) => {
                let clauses = self.clauses;
                for i in 0..clauses.len() {
                    let rule = &clauses[i];
                    let renamed = rule.rename_rule(self.k);
                    let mut new_env = Env(BTreeMap::new());

                    let goal = goal.subst(&self.env);

                    //println!("{}", goal);
                    //println!("{}", renamed.0);
                    //println!("{}", self.env.pretty_print());
                    //println!("\n\n");

                    let (Clause {conds, conseqt }, k1) = renamed;

                    match conseqt.unify_baader(&goal, &mut new_env) {
                        Ok(()) => {
                            let mut ret_goals = conds;
                            ret_goals.extend(rest_goals.into_iter().cloned());
            
                            let rest_rules = &clauses[i+1..clauses.len()];

                            let ret_env = new_env.compose(&self.env.clone());

                            let cp0 = CPoint {
                                goals: ret_goals,
                                clauses: clauses,
                                env: ret_env,
                                depth: self.depth+1,
                                k: k1
                            };
                            let cp1 = CPoint {
                                goals: self.goals.clone(),
                                clauses: rest_rules,
                                env: self.env.clone(),
                                depth: self.depth,
                                k : self.k
                            };

                            //println!("cp0: {:?}", cp0);
                            //println!("\n\n");
                            //println!("cp1: {:?}", cp1);


                            return ReslnSoln::CP(cp0, cp1)
                            
                        },
                        Err(_) => continue
                    }
                }
                ReslnSoln::NoClauses
            },
        }
    }
}
