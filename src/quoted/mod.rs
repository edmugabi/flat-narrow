use nom::branch::alt;
use nom::character::complete::{satisfy};
use nom::bytes::complete::{tag};
use nom::sequence::{delimited};
use nom::multi::{many0};
use nom::combinator::{map, value};


const RESERVED1: &str = " ():";
const RESERVED_ESC: [&str; 3] = ["\t", "\r", "\n"];

const ALPHANUMERIC: &str =
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

/*
The ASCII char set without:
 - alphanumeric
 - whitespace, (, ), : 
 - \t, \r, \n, \", \\  
 - all the non printing characters and escape sequences.
*/
const SPECIAL: &str = "!#$%&'*+,-./;<=>?@[]^_`{|}~";
const BSLASH:  char = '\\';

fn QUOTED_CHAR_NO_ESCAPE() -> String {
    ALPHANUMERIC.to_owned() + SPECIAL + RESERVED1
}

const ESC_TAB: &str = "\\t";
const ESC_CR: &str = "\\r";
const ESC_LF: &str = "\\n";
const ESC_QUOTE: &str = "\\\"";
const ESC_BSLASH: &str = "\\\\";


use nom::error::VerboseError;
type IResult<I,O> = nom::IResult<I,O, VerboseError<I>>;

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
mod tests {
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