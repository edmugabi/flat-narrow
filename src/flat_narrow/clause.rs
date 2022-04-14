
use std::fmt;
use std::collections::{BTreeMap, BTreeSet};

use super::PrettyPrint;
use super::term::Term;
use super::pattern::Patt;
use super::goal::Goal;

#[derive(Debug, PartialEq, Clone)]
pub struct Clause {
    pub conds: Vec<Goal>,
    pub conseqt: (Patt, Term)
}

impl Clause {

    fn all_vars(&self) -> BTreeSet<&str> {
        self.conds.iter()
            .flat_map(|Goal([lhs, rhs])| [lhs.all_vars(), rhs.all_vars()])
            .flatten()
            .chain(self.conseqt.0.all_vars())
            .chain(self.conseqt.1.all_vars())
            .collect()
    }
    pub fn rename(&mut self, k: usize) -> usize {
                
        let fvs = self.all_vars();
        let n = fvs.len();

        let map: BTreeMap<String, String> = fvs.into_iter()
            .enumerate()
            .map(|(i, fv)| (fv.to_owned(), format!("_{}{}", fv, i + k)))
            .collect();

        
        for eqn in &mut self.conds {
            eqn.0[0].rename(&map);
            eqn.0[1].rename(&map);
        }

        self.conseqt.0.rename(&map);
        self.conseqt.1.rename(&map);
        k + n
    }

}

impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for cond in &self.conds {
            f.write_str(&format!("    {} = {}\n", cond.0[0], cond.0[1]))?;
        }

        let clause_str = format!("=> {} = {}", 
            self.conseqt.0, self.conseqt.1
        );
        f.write_str(&clause_str)
    }
}

impl PrettyPrint for [Clause] {
    fn pretty_print(&self) -> String {
        self.iter()
            .map(|clause| clause.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[test]
fn rename_test() {
    let patt = Patt::List {
        left: Term::from(var!("x")),
        var : "xs".into(),
        right : Term::from([])
    };

    
    let mut clause = Clause {
        conds: vec![
            Goal([tvar!("x"), tcnst!("a")])
        ],
        conseqt: (patt,  Term::from([ cnst!("a"), var!("y"), ]))
    };

    assert_eq!(clause.all_vars(), BTreeSet::from(["x", "xs", "y"]));

    let patt1 = Patt::List {
        left: Term::from(var!("_x1")),
        var : "_xs2".into(),
        right : Term::from([])
    };

    let expected = Clause {
        conds: vec![
            Goal([tvar!("_x1"), tcnst!("a")])
        ],
        conseqt: (patt1,  Term::from([ cnst!("a"), var!("_y3") ]))
    };

    assert_eq!(clause.rename(1), 4);
    assert_eq!(clause, expected);
}


/*
#[derive(Debug, PartialEq)]
pub struct Eqn {
    terms: [Term; 3]
}

impl Eqn {

    pub fn all_vars(&self) -> BTreeSet<&str> {
        self.terms
            .iter()
            .flat_map(|term| term.all_vars())
            .collect::<BTreeSet<_>>()
    }
    
    pub fn rename(&self, k: usize) -> (Self, usize) {
        
        let fvs = self.all_vars();
        let n = fvs.len();

        let map: BTreeMap<String, Term> = fvs.into_iter()
            .enumerate()
            .map(|(i, fv)| (fv, format!("_{}", i + k)))
            .map(|(k, v)| (k.to_owned(), Term::from(Symbol::Var(v)))  )
            .collect();
        let sub = Env { map };

        let mut terms = self.terms.clone();
        for term in &mut terms {
            term.subst_mut(&sub)
        }

        (Eqn { terms }, k + n)
    }
    
}
*/

/*
#[derive(Clone, PartialEq, Debug)]
struct Goal {
    pair: [Term;2],
    pos  : Pos
}

impl Goal {
    fn selected_goal(&self) -> &Term {
        match self.pos {
            Pos::Left => &self.pair[0],
            Pos::Right => &self.pair[0]
        }
    }
    fn subst_selected_mut(&mut self, t: Term) {
        match self.pos {
            Pos::Left =>  self.pair[0] = t,
            Pos::Right => self.pair[1] = t
        }
    }
    fn subst_selected(&self, t: Term) -> Goal {
        match self.pos {
            Pos::Left =>  {
                Goal {
                    pair: [t, self.pair[1].clone() ],
                    pos : self.pos.clone()
                }
            },
            Pos::Right => {
                Goal {
                    pair: [self.pair[0].clone(), t],
                    pos : self.pos.clone()
                }
            }
        }
    }  
}
*/

