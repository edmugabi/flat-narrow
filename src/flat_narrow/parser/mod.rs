use nom::error::VerboseError;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::{is_alphanumeric};
use nom::character::complete::{alpha1, multispace0, satisfy};
use nom::character::complete::{alphanumeric0, alphanumeric1, space0, space1};
use nom::combinator::{all_consuming, map, recognize, verify, value, opt};
use nom::multi::{many0, many1, many_m_n, separated_list0, separated_list1};
use nom::sequence::{delimited, pair};
use nom::sequence::{tuple, preceded, terminated, separated_pair};

use super::term::{Symbol, Term};
use super::pattern::Patt;
use super::goal::Goal;
use super::clause::Clause;

pub type IResult<I,O> = nom::IResult<I,O, VerboseError<I>>;

fn pconst(input: &str) -> IResult<&str, &str> {

    let start_predicate = |ch: char| ch.is_ascii_lowercase() || ch.is_numeric()
        || SPECIAL.contains(ch);

    let tail_predicate = |ch: char| ch.is_ascii_alphanumeric() || SPECIAL.contains(ch)
        || ch == '_';

    let p = pair(
        satisfy(start_predicate),
        many0(satisfy(tail_predicate))
    );

    // not ...
    verify(recognize(p), |s: &str| !s.starts_with("..."))(input)
}


const RESERVED1: &str = " :=_";
const RESERVED_ESC: [&str; 3] = ["\t", "\r", "\n"];

const ALPHANUMERIC: &str =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

/*
The ASCII char set without:
 - alphanumeric
 - whitespace, =, _ 
 - \t, \r, \n, \", \\  
 - all the non printing characters and escape sequences.
*/
const SPECIAL: &str = "!#$%&'*+,-./:;()<>?@[]^`{|}~";

fn QUOTED_CHAR_NO_ESCAPE() -> String {
    ALPHANUMERIC.to_owned() + SPECIAL + RESERVED1
}

const ESC_TAB: &str = "\\t";
const ESC_CR: &str = "\\r";
const ESC_LF: &str = "\\n";
const ESC_QUOTE: &str = "\\\"";
const ESC_BSLASH: &str = "\\\\";

pub fn pstring(input: &str) -> IResult<&str, String> {

    fn pescape(input: &str) -> IResult<&str, char> {
        alt((
            value('\t', tag(self::ESC_TAB)),
            value('\r', tag(self::ESC_CR)),
            value('\n', tag(self::ESC_LF)),
            value('\"', tag(self::ESC_QUOTE)),
            value('\\', tag(self::ESC_BSLASH))
        ))(input)
    }

    let is_quoted_char_no_escape =
         |ch: char| self::QUOTED_CHAR_NO_ESCAPE().contains(ch);

    let pquoted_char = alt((
        pescape,
        satisfy(is_quoted_char_no_escape)   
    ));

    let pinner = map(
        many0(pquoted_char),
        |chs: Vec<char>| {
            chs.into_iter().collect::<String>()
        }
    );
    delimited(
        tag("\""),
        pinner,
        tag("\"")
    )(input)
}

#[cfg(test)]
mod string_tests {
    use super::*;

    const input_strings: [&str; 7] = [
        "\"\"", // empty string
        "\" \"", // space
        "\"\\t\"", //tab
        "\"\\r\"", //CR
        "\"\\n\"", // LF
        "\"\\\\\"",
        "\"Hello, Word\""
    ];

    const expected_strings: [&str; 7] = [
        "", // empty string
        " ", // space
        "\t", //tab
        "\r", //CR
        "\n", // LF
        "\\",
        "Hello, Word"
    ];

    #[test]
    fn test_pstring(){
        let input = "\"abc\"";
        let result = pstring(input);
        let expected = "abc".into();
        assert_eq!(result, Ok(("", expected)));

        for (input, expected) in input_strings
                .into_iter()
                .zip(expected_strings.into_iter())
        {
            let result = pstring(input.clone());
            let expected: String = expected.to_string();
            assert_eq!(result, Ok(("", expected)));
        }
    }
}

fn pvariable(input: &str) -> IResult<&str, &str> {
    let start_predicate = |ch: char| ch.is_ascii_uppercase() || ch == '_';
    let tail_predicate = |ch: char| ch.is_ascii_alphanumeric() || ch == '_';

    let p = pair(
        satisfy(start_predicate),
        many0(satisfy(tail_predicate))
    );
    recognize(p)(input)
}
/*
fn pbr_variable(input: &str) -> IResult<&str, &str> {
    let predicate = |ch: char| ch.is_ascii_alphanumeric() || SPECIAL.contains(ch)
        || ch == '_' || ch == '=';

    let p = recognize(many1(satisfy(predicate)));
    delimited(tag("["), p, tag("]"))(input)
}

fn pbr_const(input: &str) -> IResult<&str, &str> {
    let predicate = |ch: char| ch.is_ascii_alphanumeric() || SPECIAL.contains(ch)
        || ch == '_' || ch == '=';

    let p = recognize(many1(satisfy(predicate)));
    delimited(tag("("), p, tag(")"))(input)
}
*/

pub fn psymbol(input: &str) -> IResult<&str, Symbol> {
    alt((
        map(pvariable, |x| Symbol::Var(x.to_string())),
        map(pconst, |c|Symbol::Const(c.to_owned())),
        map(pstring, |s| Symbol::Const(s))
        //map(pbr_variable, |c| Symbol::Const(c.to_owned())),
        //map(pbr_const, |c| Symbol::Const(c.to_owned()))
    ))(input)
}


fn plist_pattern(input: &str) -> IResult<&str, (Term, &str, Term)> {
    let p = tuple((
        //term is one or more, so we can make it optional, or use
        // separated_list0 in pterm
        opt(pterm),
        delimited(space0, preceded(tag("..."), pvariable),space0),
        opt(pterm)
    ));

    map(p, |(l, s, r)| {
        let left  = l.unwrap_or( Term { symbols: vec![] });
        let right = r.unwrap_or( Term { symbols: vec![] });
        (left, s, right)
    })(input)
}

fn pterm(input: &str) -> IResult<&str, Term> {
    let plist = separated_list1(space1, psymbol);
    map(plist, |symbols| Term { symbols })(input)
}

pub fn pgoal(input: &str) -> IResult<&str, [Term;2]> {
    let peqn = tuple((
        pterm, 
        delimited(space1, tag("="), space1), 
        pterm
    ));
    let peqn = map(peqn, |(lhs, eq, rhs)| {
        let eq = Term { symbols: vec![Symbol::Const(eq.into())] };
        [lhs, rhs]
    });

    let ppredicate = map(pterm, |t| {
        let TRUE = Term::from(Symbol::Const("true".into()));
        [t, TRUE]
    });

    alt((
        peqn,
        ppredicate
    ))(input)
}

pub fn pclause_head(input: &str) -> IResult<&str, (Patt, Term)> {
    let plist_pattern = map(
        plist_pattern,
        |(left, var, right)| Patt::List { left, var: var.into(), right}
    );
    let ppatt = alt((
        plist_pattern,
        map(pterm, Patt::Term)
    ));

    let peqn = tuple((
        ppatt, 
        opt(
            preceded(delimited(space1, tag("="), space1), pterm)
        )
    ));


    map(peqn, |(left, right)| {
        let TRUE = Term::from(Symbol::Const("true".into()));

        match right {
            Some(t) => (left, t),
            None => (left, TRUE)
        }
    })(input)
}

fn pequation(input: &str) -> IResult<&str, [Term;3]>
{
  let p = tuple((
      pterm, 
      delimited(space1, tag("="), space1), 
      pterm
  ));
  map(p, |(lhs, eq, rhs)| {
      let eq = Term { symbols: vec![Symbol::Const(eq.into())] };
      [lhs, eq, rhs]
  })(input)
}

fn pquery(input: &str) -> IResult<&str, Term> {

    let peqn = tuple((
        pterm, 
        delimited(space1, tag("="), space1), 
        pterm
    ));
    let peqn = map(
        peqn,
        |(t0, s, t1)| {
            let mut symbols = t0.symbols;
            symbols.push(Symbol::Const(s.into()));
            symbols.extend(t1.symbols.into_iter());
            Term { symbols }

        }
    );
    all_consuming(
        alt((peqn, pterm))
    )(input)
}

fn pprogram(input: &str) -> IResult<&str, Vec<[Term;3]>> {
    let line_sep = pair(
        many1(pair(space0, tag("\n"))),
        space0
    );
    let p = delimited(
        multispace0, 
        separated_list1(line_sep, pequation), 
        multispace0
    );
    all_consuming(p)(input)
}

pub fn pcond_program(input: &str) -> IResult<&str, Vec<Clause>> {
    let line_sep = |i| pair(space0, tag("\n"))(i);

    let pgoal = map(
        preceded(tag("    "), pgoal),
        Goal
    );
    let pgoals = separated_list0(many1(line_sep), pgoal);

    let p = pair(
        opt(terminated(pgoals, many1(line_sep))),
        preceded(pair(tag("=>"), space0), pclause_head)
    );
    let pclause = map(p, |(goals, (lhs, rhs))| {
        let goals = goals.unwrap_or(vec![]);
        Clause { conds: goals, conseqt: (lhs, rhs)}
    });

    let pclauses = separated_list0(many1(line_sep), pclause);

    let pclauses = delimited(many0(line_sep), pclauses, multispace0);
    
    all_consuming(pclauses)(input)

}

#[cfg(test)]
mod test_parser {
    use super::*;
    #[test]
    fn consts_test() {
        assert_eq!(pconst("x1 = x"), Ok((" = x", "x1")));
        assert!(pconst("=").is_err());
    }

    #[test]
    fn var_test() {
        assert_eq!(pvariable("X"), Ok(("", "X")));
    }

    #[test]
    fn eqn_test() {
        use Symbol::*;
        let input = "X + a = a + X";
        let t0 = Term {
            symbols: vec![Var("X".into()), Const("+".into()), Const("a".into())]
        };
        let t1 = Term {
            symbols: vec![Const("a".into()), Const("+".into()), Var("X".into())]
        };
        let eq = Term {
            symbols: vec![Const("=".into())]
        };
        assert_eq!(pequation(input), Ok(("", [t0, eq, t1])));

        let res = pequation("X \"=\" X = true");
        assert!(res.is_ok());
        println!("{:?}", res);
    }

    #[test]
    fn pprogram_test() {
        let input = "
        C && D or E = ( C && D ) or E   
           
        not ( not D ) = D
        ";
        let res = pprogram(input);
        assert!(res.is_ok());
        println!("{:?}", res);
    }

    #[test]
    fn p1cond_test() {
   
        let FACT = "    X : int\n    X1 = X - 1\n=> true\
\n    S : char\n=> ty ...Kl yh = true\n=> true";
        use nom::Finish;
        use nom::error::convert_error;
        let result: IResult<_,_> = pcond_program(FACT);
        //.finish()
        //.map_err(|err| println!("{}, ", convert_error(FACT, err)));;

        println!("{:?}", result);
    }
    #[test]
    fn p2cond_test() {
        let input = "=> true";
        use nom::Finish;
        use nom::error::convert_error;
        let result = pcond_program(input)
            .finish()
            .map_err(|err| println!("{}, ", convert_error(input, err)));;

        println!("{:?}", result);
    }

    #[test]
    fn plist_pattern_test() {
        let input = "ty ...Kl yh";
        let result = plist_pattern(input);
        //assert!(result.is_ok());
        println!("{:?}", result);

        let result = plist_pattern("...L");
        println!("{:?}", result);

        let result = plist_pattern("...L X");
        println!("{:?}", result);
        
    }
}
