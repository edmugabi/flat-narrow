use std::fmt;
use std::collections::HashSet;

use serde::{Serialize, Deserialize};
use log::{debug, info};

use super::PrettyPrint;
use super::term::Term;
use super::clause::Clause;
use super::env::Env;
use super::goal::Goal;

#[derive(Clone, Debug, PartialEq)]
pub struct CPoint<'a> {
    clauses: &'a [Clause],
    goals: Vec<Goal>,
    env: Env,
    depth: usize,
    k: usize
}

impl<'a> CPoint<'a> {

    fn with_new_clauses(&mut self, clauses: &'a [Clause])
    //where Self: 'b
    {
        self.clauses = clauses;
    }

    pub fn new(clauses: &[Clause], goals: Vec<Goal>) -> CPoint<'_> {
        CPoint {
            clauses,
            goals,
            env: Env::empty(),
            depth: 0,
            k: 0
        }
    }

    pub fn solve(self, strategy: Strategy)
    -> impl Iterator<Item=Env> + 'a where Self: 'a
    {
        let free: HashSet<String> = self.goals.iter()
            .flat_map(|Goal([l,r])| {
                [l.all_vars(), r.all_vars()]
            })
            .flatten()
            .map(|s| s.to_owned())
            .collect();

        let root = CPointIter {
            cpoints: vec![self],
            strategy
        };

        root.map(move |env| {
            // only return variables that appear in the original goal
                let map = env.map.into_iter()
                    .filter(|(key,_)| free.contains(key))
                    .collect();
                Env { map }
        })
        
    }

    fn narrow(mut self) -> Soln<'a>
    {

        //debug!("\nTrying cp:\n{}\n\n", self);
        match self.goals.split_first() {
            None => { return Soln::Res(self.env) },
            Some((goal, rest_goals)) => {

                // goal.len is 2 always, could we expand it by making a long equation
                // the variability of j allows us to have longer equations e.g x = y = z
                for j in 0..goal.0.len() {
                    //let sub_goal = &goal.selected_goal().subst(&self.env);
                    let sub_goal = &goal.0[j].subst(&self.env);

                    for i in 0..self.clauses.len() {
                        let mut clause = self.clauses[i].clone();
                        let k1 = clause.rename(self.k);
                        let clause_clone = clause.clone(); // for debug printing
                        let Clause { conds, conseqt: (patt, rhs) } = clause;
    
                        
                        match patt.unify_patt_term(&sub_goal) {
                            Ok(new_env) => {
                            // debug!("{} unify {} = {}",
                            //     patt, sub_goal, &new_env.pretty_print()
                            // );
    
                            let ret_env = new_env.compose(&self.env);
                            //let new_goal = goal.subst_selected(rhs);
                            let mut new_goal = goal.clone();
                            new_goal.0[j] = rhs;
                            
                            let mut ret_goals = conds;
                            ret_goals.push(new_goal);
                            ret_goals.extend_from_slice(rest_goals);
                                /*
                            debug!("\n\nClause: \n{}\
                                \nGoal  : {}\
                                \nredex : {}\
                                \nrem_g : {}\
                                \nEnv   : {}\
                                \nk     : {}\
                                \ndepth : {}",
                                &clause_clone,
                                sub_goal,
                                j,
                                rest_goals.pretty_print(),
                                ret_env.pretty_print(),
                                k1, self.depth + 1
                            );
                            */

                            // debug!("\nsubgoal: {}\n, clause: {}\n, ret_goals: {}\n\
                            // ret_env: {}
                            // ",
                            //     sub_goal, clause_clone, ret_goals.pretty_print(),
                            //     ret_env.pretty_print()
                            // );
    
                            let cp0 = CPoint {
                                clauses: self.clauses,
                                goals: ret_goals,
                                env: ret_env,
                                depth: self.depth +1,
                                k: k1,
                            };

                            let rest_clauses = &self.clauses[i+1..self.clauses.len()];
                            let () = self.with_new_clauses(rest_clauses);
                            let cp1 = self;

                            return Soln::CP([cp0,cp1])
                            },
                            Err(_)      => continue
                        }
                          
                    }
                }

                debug!("Failed to narrow {}", goal);

                match goal.0[0].unify(&goal.0[1]) {
                    Ok(new_env) => {
                        let ret_env = new_env.compose(&self.env);
                        self.env = ret_env;
                        self.goals = rest_goals.to_vec();
                        Soln::CP1(self)
                    },
                    Err(_)  => {
                        println!("CP fail, try another cp");
                        return Soln::Fail
                    }
                }

            }
        }
    }
}

impl<'a> fmt::Display for CPoint<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        f.write_str(&format!("\nclauses:\n{}", self.clauses.pretty_print()))?;
        //f.write_str(&format!("prev_env: {}", self.env.pretty_print()))?;
        f.write_str(&format!("\ngoal: {}\n", self.goals.pretty_print()))?;
        f.write_str(&format!("env: {}\n", self.env.pretty_print()))?;
        f.write_str(&format!("depth: {}, k: {}\n", self.depth, self.k))
    }
}

#[derive(Debug, PartialEq)]
pub enum Soln<'b> {
    CP([CPoint<'b>;2]),
    CP1(CPoint<'b>),
    Res(Env),
    Fail
}

impl<'a> fmt::Display for Soln<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Soln::CP([cp0, cp1]) => {
                f.write_str("CP: ")?;
                f.write_str(&cp0.to_string())?;
                f.write_str(&cp1.to_string())
            }
            Soln::CP1(cp) => {
                f.write_str("CP1: ")?;
                f.write_str(&cp.to_string())
            }
            Soln::Res(env) => {
                f.write_str("Res: ")?;

                f.write_str(&env.pretty_print())
            }
            Soln::Fail => {
                f.write_str("No Solution")
            }
        }
   }
} 


#[derive(Clone)]
#[derive(Debug, Serialize, Deserialize)]
pub enum Strategy {
    BFS,
    DFS,
    DLS(usize),
}

#[derive(Clone)]
pub struct CPointIter<'b> {
    cpoints: Vec<CPoint<'b>>,
    strategy: Strategy
}

impl<'b> Iterator for CPointIter<'b> {
    type Item=Env;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.cpoints.pop()
            //.map(|cp| { debug!("\ncp:\n{}\n\n", cp); cp} )
            .map( |mut cp: CPoint<'_>| cp.narrow())
            {
                Some(Soln::Res(env)) => {
                    return Some(env)
                },
                Some(Soln::CP([cp0, cp1])) => match self.strategy {
                    Strategy::DFS => self.cpoints.extend([cp1,cp0]),
                    Strategy::BFS => self.cpoints.extend([cp0,cp1]),
                    Strategy::DLS(n) => {
                        if cp0.depth <= n {
                            self.cpoints.extend([cp1,cp0])
                        } else {
                            self.cpoints.extend([cp1])
                        }
                    },
                },
                Some(Soln::CP1(cp)) => self.cpoints.push(cp),
                Some(Soln::Fail) => {
                    debug!("Failed choice point, trying another"); 
                    continue
                },
                None => {
                    debug!("No more choice points");
                    return None
                }
            }
        }


    }
}

const FACTORIAL: &str = "
    X : int
    Y : int
    X1 : int
    X - 1 = X1
    fact ( X1 ) = Y
=> fact ( X ) = X * Y

";

#[test]
fn pcond_prog_test() {
    use nom::Finish;
    use nom::error::convert_error;
    use super::parser::{pgoal, pcond_program};
    let res = pcond_program(FACTORIAL)
        .finish()
        .map_err(|e| convert_error(FACTORIAL, e));

    let (_, clauses) = res.unwrap();
    //assert_eq!(rest, "");
    println!("{}", clauses.as_slice().pretty_print());

    let GOAL = "fact ( 2 ) = Z";
    let (_, goal) = pgoal(GOAL)
        .finish().map_err(|e| convert_error(GOAL,e))
        .unwrap();

    println!("{:?}", goal);

    let mut cp = CPoint::new(clauses.as_slice(), vec![Goal(goal)]);
    println!("{}", cp);

    // match cp.narrow() {
    //     Soln::CP([mut cp0, cp1]) => {
    //         let soln = cp0.narrow();
    //         println!("{}", soln);
    //     },
    //     _ => {}
    // }
    println!("{}", cp.narrow());
}

