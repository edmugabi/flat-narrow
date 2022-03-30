use crate::sld::{CPoint, Env};
use crate::narrow::{NarrowSoln, ReflSoln};
use crate::resolution::ReslnSoln;

use crate::PrettyPrint;

struct CPoints<'a> {
    cpoints: Vec<CPoint<'a>>
}

impl<'a> CPoint<'a> {
    fn solve(self) -> CPoints<'a> where Self: 'a {
        CPoints {
            cpoints: vec![self]
        }
    }
}


impl<'a> Iterator for CPoints<'a> {
    type Item= Env;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.cpoints.pop() {
                None => { return None }
                Some(cp) => {
                    match cp.clone().narrow() {
                        NarrowSoln::CPoints(cp0,cp1) => {
                            self.cpoints.extend([cp1,cp0]); //DFS
                        }
                        NarrowSoln::CPoint(cp0) => {
                            // should narrow again immediately
                            self.cpoints.push(cp0);
                        }
                        NarrowSoln::Res(env) => {
                            return Some(env)
                        }
                        NarrowSoln::NoClause 
                        | NarrowSoln::NoSubTerm => {
                            match cp.clone().reflect() {
                                ReflSoln::CP(cp) => self.cpoints.push(cp),
                                ReflSoln::Res(env) => {
                                    return Some(env)
                                }
                                ReflSoln::NotEqn
                                | ReflSoln::NotUnif => {
                                    match cp.resolve() {
                                        ReslnSoln::CP(cp0, cp1) => {
                                            self.cpoints.extend([cp1, cp0])
                                        }
                                        ReslnSoln::Res(env) => {
                                            return Some(env)
                                        }
                                        ReslnSoln::NoClauses => {
                                            continue
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } 
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{pprogram, pgoals};

    // Example 2.2 Hanus, 1994 Integration of functions ...
    const clauses: &str = "
=> f(X) = a

=> g(a) = a";

    #[test]
    fn solve_test1() {
        let (rest, program) = pprogram(clauses).unwrap();
        assert_eq!(rest, "");
        let (rest, goals) = pgoals("f(g(X)) = a").unwrap();
        assert_eq!(rest, "");

        let cp = CPoint::new(&program, goals);
        for env in cp.solve() {
            println!("{}", env.pretty_print())
        }
    }


}