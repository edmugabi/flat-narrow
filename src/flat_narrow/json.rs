
use std::collections::BTreeMap;
use serde::{Serialize, Deserialize};
use nom::Finish;
use nom::error::convert_error;

use super::clause::Clause;
use super::cpoint::{Strategy, CPoint};

use super::parser::{pclause_head, psymbol, pgoal};
use super::term::Term;
use super::env::Env;

fn handle_error<O, F>(mut f: F) -> impl FnMut(&str) -> Result<O, String>
where F: FnMut(&str) -> super::parser::IResult<&str,O>,

{
    //let mut f = all_consuming(f);
    move |input| {
        match f(input).finish() {
            Ok((rest, o)) => {
                if rest == "" {
                    Ok(o)
                } else { Err("Did not consume all the input".into()) }
            }
            Err(err) => {
                Err(convert_error(input, err))
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct ClauseItem {
    conds: Vec<String>,
    conseqt: String
}

impl ClauseItem {
    fn parse(&self) -> Result<Clause, String> {
        let mut conds = Vec::new();
        for goal_str in &self.conds {
            let goal = handle_error(pgoal)(goal_str)?;
            use super::goal::Goal;
            conds.push(Goal(goal));
        }

        let conseqt = handle_error(pclause_head)(&self.conseqt)?;
        let clause = Clause { conds, conseqt };
        Ok(clause)
    }
    fn pclauses(clause_items: &[ClauseItem]) -> Result<Vec<Clause>, String> {
        let mut vec = Vec::new();
        for clause_item in clause_items {
            let clause = clause_item.parse()?;
            vec.push(clause);
        }
        Ok(vec)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProgramItem {
    clauses: Vec<ClauseItem>,
    queries: Vec<QueryItem>
}

#[derive(Debug, Serialize, Deserialize)]
struct QueryItem {
    query: Vec<String>,
    strategy: Strategy,
    results: Vec<BTreeMap<String, String>>
}

impl QueryItem {
    fn check(&self, clauses: &[Clause]) -> Result<(), String> {

        //let expected = QueryItem::pexpected(&self.results);
        let mut goals = Vec::new();
        for goal_str in &self.query {
            let goal = handle_error(pgoal)(goal_str)?;
            use super::goal::Goal;
            goals.push(Goal(goal));
        }

        let mut cp_iter = CPoint::new(clauses, goals)
            .solve(self.strategy.clone());

        for expected in &self.results {
            let expected = QueryItem::pexpected(expected);
            let result = cp_iter.next();

            println!("{:?}", result);
            assert_eq!(result, Some(expected));
        }
        assert_eq!(cp_iter.next(), None);

        Ok(())
    }

    fn pexpected(map: &BTreeMap<String, String>) -> Env
    {
        let expected: BTreeMap<String,_> = map
        .iter()
        .map(|(k, v)| (k.into(), Term::from(psymbol(&v).unwrap().1) ))
        .collect();
        let expected = Env { map: expected };
        // let expected = 
        //     if expected.map.is_empty() { None }
        //     else { Some(expected) };

        expected
    }
}

impl ProgramItem {
    pub fn test(json_input: &str) -> Result<(), String> {

        let ProgramItem { clauses, queries } = serde_json::from_str(json_input)
            .map_err(|err| err.to_string())?;

        let clauses = ClauseItem::pclauses(&clauses)?;

        for q_item in queries {
            q_item.check(&clauses)?;
        }

        Ok(())
    }
}


#[cfg(test)]
mod test_json {
    use super::*;

    #[test]
    fn json_test() {
        let input = r#"
        {
            "query": ["5 + 6 = 4"],
            "strategy": { "DLS": 4},
            "results": [
                {
                    "x": "4",
                    "y": "7"
                }
            ]
        }
        "#;

        let qitem: QueryItem = serde_json::from_str(input).unwrap();
        println!("{:?}", qitem);
    }
}
