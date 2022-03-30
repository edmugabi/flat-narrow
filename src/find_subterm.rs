
use std::collections::{BTreeMap};

use crate::term::Term;
use crate::sld::Env;
use crate::clause::Clause;
use crate::term::EqnOp;
use crate::sld::{CPoint, Soln};

fn reflect<'a>(
    (s,t): (&Term, &Term),
    rest_goals: &'a [Term],
    prev_env: &Env,
) -> Option<(&'a [Term], Env)>
{
    let s = s.subst(prev_env);
    let t = t.subst(prev_env);

    let mut new_env = Env(BTreeMap::new());
    match s.unify_baader(&t, &mut new_env) {
        Ok(()) => {
            let ret_env = new_env.compose(prev_env);
            Some((rest_goals, ret_env))
        },
        Err(_) => None
    }
}


impl Term {
    // apply to f to leftmost innermost subterm
    // that has a defined function symbol at the top and all arguments
    //consist of constructions or variables
    // substitutes the return type of f at the same position if f returns 
    // Some(term)
    fn apply_to_lmim_subterm<F>(&self, f: F) -> Option<Term>
    where F: Fn(Vec<Term>) -> Option<Term>
    {
        unimplemented!()
        // match self {
        //     Term::Const(_)
        //     | Term::Var(_) => Some(self.clone()),
        //     Term::List(ts) => {
        //         let list = ts.into_iter()
        //             .map(|t| t.apply_to_lmim_subterm(f))
        //             .collect();
        //         f(list)
        //     }
        // }
    }
}
// innermost basic narrowing
//fn narrow()

// impl Term {
//     fn lmin(&self, (lhs,rhs, conds): (&Term, &Term, &Term)) -> Term {
//         match self {
//             t@Term::Const(_)
//             | t@Term::Var(_) => t.clone(),
//             Term::List(list) => {
//                 let new_list = list.into_iter()
//                     .map(|t| t.lmin()).collect();

//                 let t = Term::List(new_list);
//                 if !handled {
//                     let env = Env(BTreeMap::new());
//                     match lhs.unify_baader(t, env: &mut Env) {
//                         Ok(()) => rhs.subst(&env),
//                         Err(_) => 
//                     }
//                 }
//             }
//         }
//     }
// }


// conds, lhs, rhs
type Rule<'a> = (&'a [Term], &'a Term, &'a Term);

// fn one_step_narrow<'a>(
//     goal: &Term,
//     rest_goals: &[Term],
//     prev_env: &Env,
//     clause: Rule<'a>
// ) -> Option<(Vec<Term>, Env)>
// {
//     match goal {
//         Term::Const(_)
//         | Term::Var(_) => None,
//         Term::List(list) => {
//             if list.all_leaves() {

//             }
//             else {
//                 match list.split_first() {
//                     Some((head, tail)) => {
//                         head.narrow()
//                     }
//                 }
//             }
//         }
//     }
// }

impl<'a> CPoint<'a> {
    
    fn narrow_old(&self) -> Soln<'a> {
        match self.goals.split_first() {
            None => Soln::Res(self.env.clone()),
            Some((goal, rest_goals)) => {
                match goal {
                    Term::Var(_)
                    | Term::Const(_) => Soln::NoClauses, //should be fail try alternative proof
                    Term::List(list) => {
                        let some_res = apply_to_lmim_subterm(list, |list| {
                            narrow_term(
                                list,
                                self.clauses,
                                &self.env,
                                self.depth,
                                self.k
                            )
                        });

                        match some_res {
                            None => Soln::NoClauses,
                            Some((term, (cond_goals, ret_env, ret_clauses, depth, k))) => {
                                let mut new_goals = cond_goals;
                                new_goals.push(term);
                                new_goals.extend(rest_goals.into_iter().cloned());
                                let cp1 = CPoint {
                                    goals: new_goals,
                                    clauses: self.clauses,
                                    env: ret_env,
                                    depth: depth,
                                    k
                                };
                                let cp2 = CPoint {
                                    goals: self.goals.clone(),
                                    clauses: ret_clauses,
                                    env: self.env.clone(),
                                    depth: self.depth,
                                    k: self.k
                                };

                                Soln::CP(cp1, cp2)
                            }
                        }

                    }
                }
            }
        }
    }
}



fn narrow<'b, 'a: 'b>(
    list: &[Term],
    rules: &'a [Clause],
    prev_env: &Env,
) -> Option<(&'b [Term], Term, Env)> {

    let mut new_list = Vec::new();
    for i in 0..list.len() {

        match &list[i] { 
            s@Term::Const(_)
            | s@Term::Var(_) => { new_list.push(s.clone()) },
            Term::List(inner_list)  => {
                let left  =  &list[0..i];
                let right = &list[i+1.. list.len()];


                let some_t     = narrow(&inner_list,rules, prev_env);
                match some_t {
                    Some((new_conds, t, env)) => {
                        let mut ret_list = left.to_vec();
                        ret_list.push(t.clone());
                        ret_list.extend(right.iter().cloned());
                        return Some((new_conds, Term::List(ret_list), env))
                    },
                    None => { return None }
                }
            } 
        }
    }

    let term = Term::List(list.to_vec());
    let term = term.subst(prev_env);

    for rule in rules {
        let mut new_env = Env(BTreeMap::new());
        let Clause {conds, conseqt } = rule;
        
        match conseqt.equation() {
            Some((lhs, EqnOp::Eq, rhs)) => {
                match term.unify_baader(lhs, &mut new_env) {
                    Ok(()) => {
                        let ret_env = new_env.compose(prev_env);
                        return Some((conds, rhs.clone().clone(), ret_env))
                    },
                    Err(_) => return None
                }
            }
            _ => { continue }
        }
    }
    return None
   
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

// if i is the leftmost innermost position, then the term returned by f is substituted
// at i, other results are captured in return parameter r
fn apply_to_lmim_subterm<F, R>(list: &[Term], f: F) -> Option<(Term,R)>
where F: Fn(&[Term]) -> Option<(Term,R)>
{
    let mut new_list = Vec::new();
    for i in 0..list.len() {

        match &list[i] { 
            s@Term::Const(_)
            | s@Term::Var(_) => { new_list.push(s.clone()) },
            Term::List(inner_list)  => {
                let left  =  &list[0..i];
                let right = &list[i+1.. list.len()];

                let some_res     = apply_to_lmim_subterm(&inner_list,f);
                match some_res {
                    Some((t,res)) => {
                        let mut ret_list = left.to_vec();
                        ret_list.push(t);
                        ret_list.extend(right.iter().cloned());

                        return Some((Term::List(ret_list), res))
                    },
                    None => { return None }
                }
            } 
        }
    }
    return f(list)
}

impl Term {

pub fn find_subterm(&mut self) -> Option<&mut Term>
{

    fn extract_list(t: &Term) -> Option<&Vec<Term>> {
        match t {
            Term::Const(_)
            | Term::Var(_) => None,
            Term::List(l) => Some(l)
        }
    }
    fn extract_list_mut(t: &mut Term) -> Option<&mut Vec<Term>> {
        match t {
            Term::Const(_)
            | Term::Var(_) => None,
            Term::List(l) => Some(l)
        }
    }

    fn all_leaves(list: &Vec<Term>) -> bool {
        list.iter().all(|t| {
            match t {
                Term::Const(_) | Term::Var(_) => true,
                Term::List(_) => false
            }
        })
    }

    match self {
        Term::Const(_)
        | Term::Var(_) => { return None }
        u@Term::List(_) => {
            let list = extract_list(u).unwrap();
            if all_leaves(list) {
                return Some(u)
            }
            else {
                let list_mut = extract_list_mut(u).unwrap();
                for t in list_mut {
                    match t.find_subterm() {
                        ret@Some(_) => { return ret }
                        None => { continue }
                    }
                }
                unreachable!()
            }
        }
    }
}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn find_subterm_test() {
        use Term::*;
        let t = &mut Term::List(vec![
            Const("a".into()),
            Var("x".into()),
            Term::List(vec![
                Term::Var("y".into()),
                Const("b".into())
            ])
        ]);

        println!("{:?}", t.find_subterm());
    }
}
