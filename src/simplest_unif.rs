use crate::term::Term;
use crate::sld::Env;



impl Term {
    fn simplify<'a>(self, other: Term) -> Result<Env, &'a str> {
        use Term::*;
        let mut env = Env::empty();
        simplify_list(&[], &mut vec![(self, other)], &mut env)?;
        Ok(env)
    }
}

fn simplify_list<'a>(
    rules: &[(Term, Term)],
    pairs: &mut Vec<(Term, Term)>,
    env: &mut Env
)
 -> Result<(), &'a str>
 {
    use Term::*;
    match pairs.pop() {
        //equality of consts
        Some((Const(a), Const(b))) => {
            if a == b {
                return simplify_list(rules, pairs, env)
            }
            else {
                return Err("Constants not equal")
            }
        }
        //equality of vars
        Some((Var(x), y@Var(_))) => {
            env.0.insert(x.into(), y.into());
            return simplify_list(rules, pairs, env)
        }
        //assign consts to vars
        Some((Var(x), b@Const(_))) => {
            env.0.insert(x.into(), b.into());
            return simplify_list(rules, pairs, env)
        }
        //functional equality
        Some((List(l0), List(l1))) => {
            if l0.len() == l1.len() {
                let new_pairs = l0.into_iter().zip(l1.into_iter());
                pairs.extend(new_pairs);
                return simplify_list(rules, pairs, env)
            }
            else {
                Err("Different Lengths")
            }
        }
        _ => {
            Err("Not expected, add rules at language level")
        }
        None => { return Ok(()) }
    }

}