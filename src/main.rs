fn main() {
    let a = "aiueo";
    println!("{:?}", the_letter_a(&a));
}

// XMLの要素を表す型
#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

fn the_letter_a(input: &str) -> Result<(&str, ()), &str> {
    match input.chars().next() {
        // 「'a'を取り除いた残り」を['a'.len_utf8()..]で表現している
        Some('a') => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

fn match_literal<'a>(expected: &'static str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.strip_prefix(expected) {
        Some(next) => Ok((next, ())),
        _ => Err(input),
    }
}

#[test]
fn literal_parser() {
    let hello_joe = match_literal("Hello joe");
    assert_eq!(hello_joe.parse("hogehoge joe"), Err("hogehoge joe"));
    assert_eq!(Ok(("", ())), hello_joe.parse("Hello joe"));
    assert_eq!(
        Ok((" Hello Robert!", ())),
        hello_joe.parse("Hello joe Hello Robert!")
    );
}

// TODO: これをparserを返すように変更できないかな？
fn identifier<'a>(input: &'a str) -> ParseResult<'a, String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(chr) if chr.is_alphabetic() => matched.push(chr),
        _ => return Err(input),
    }

    while let Some(chr) = chars.next() {
        if chr.is_alphanumeric() || chr == '-' {
            matched.push(chr);
        } else {
            // NOTE: エラーにしてしまうのではなく，「ここでidentが終わった」と
            // 判定する
            break;
        }
    }

    Ok((&input[matched.len()..], matched))
}
#[test]
fn parse_ident() {
    assert_eq!(identifier("abc-d"), Ok(("", String::from("abc-d"))));
    assert_eq!(identifier("abc*d"), Ok(("*d", String::from("abc"))));
    assert_eq!(identifier("!not ident"), Err("!not ident"));
}

fn alphabet(input: &str) -> Result<(&str, String), &str> {
    match input.chars().next() {
        Some(chr) if chr.is_alphabetic() => Ok((&input[chr.len_utf8()..], String::from(chr))),
        _ => Err(input),
    }
}
#[test]
fn parse_alphabet() {
    assert_eq!(alphabet("abcd"), Ok(("bcd", String::from('a'))));
}

// trait Parser
type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;

    fn map<NewOutput, F>(self, map_fn: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        F: Fn(Output) -> NewOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }

    fn pred<F>(self, pred_fn: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, pred_fn))
    }

    fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        NextParser: Parser<'a, NewOutput> + 'a,
        F: Fn(Output) -> NextParser + 'a,
    {
        BoxedParser::new(and_then(self, f))
    }
}

// あるFという型がParserであるために，以下のようにparse関数を実装する
impl<'a, Output, F> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

// 2つのパーザーを組み合わせる関数
// Errの中身は常にエラーが起きた部分以降の全てを返すものとする
// ParseResult型に対するmapやand_thenを使うとエレガントに書ける
fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(rest, result2)| (rest, (result1, result2)))
        })
    }
}
#[test]
fn pair_parser() {
    let tag_oppener = pair(match_literal("<"), identifier);

    assert_eq!(
        tag_oppener.parse("<hoge-element/>"),
        Ok(("/>", ((), "hoge-element".to_string())))
    );
    assert_eq!(tag_oppener.parse("hogehoge!"), Err("hogehoge!"));
    assert_eq!(tag_oppener.parse("<!fugafuga!"), Err("!fugafuga!"));
}

// parserのResultの型を変換する関数
// Haskell流に書くのであれば，`map :: a -> b -> Parser a -> Parser b` を
// しているに過ぎない．
fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result))) // これはresultに対するmap (Resultの型を変換)
    }
}

fn and_then<'a, P, F, A, B, NextP>(parser: P, f: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    NextP: Parser<'a, B>,
    F: Fn(A) -> NextP,
{
    move |input| match parser.parse(input) {
        Ok((next_input, output)) => f(output).parse(next_input),
        Err(e) => Err(e),
    }
}

fn left<'a, R1, R2>(
    parser1: impl Parser<'a, R1>,
    parser2: impl Parser<'a, R2>,
) -> impl Parser<'a, R1> {
    map(pair(parser1, parser2), |(left, _right)| left) // これはParserに対するmap (Parserの型を変換)
}

fn right<'a, R1, R2>(
    parser1: impl Parser<'a, R1>,
    parser2: impl Parser<'a, R2>,
) -> impl Parser<'a, R2> {
    map(pair(parser1, parser2), |(_left, right)| right) // これはParserに対するmap (Parserの型を変換)
}

#[test]
fn right_combinator() {
    let tag_opener = right(match_literal("<"), identifier);

    assert_eq!(
        Ok(("/>", "my-tag".to_string())),
        tag_opener.parse("<my-tag/>")
    );

    assert_eq!(
        Err("!this-is-not-tag/>"),
        tag_opener.parse("<!this-is-not-tag/>")
    );

    assert_eq!(
        Err("this is just a sentence."),
        tag_opener.parse("this is just a sentence.")
    );
}

fn one_or_more<'a, A, P>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    n_or_more(parser, 1)
}
#[test]
fn one_or_more_test() {
    let white_spaces = one_or_more(match_literal(" "));

    assert_eq!(
        Ok(("", vec![(), (), (), (), ()])),
        white_spaces.parse("     ")
    );
}

fn zero_or_more<'a, A, P>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    n_or_more(parser, 0)
}
#[test]
fn zero_or_more_test() {
    let under_scores = zero_or_more(match_literal("_"));

    assert_eq!(
        Ok(("under_score!", vec![(), (), ()])),
        under_scores.parse("___under_score!")
    );
}

fn n_or_more<'a, A, P>(parser: P, n: u8) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        for _ in 0..n {
            if let Ok((next, item)) = parser.parse(input) {
                input = next;
                result.push(item);
            } else {
                return Err(input);
            }
        }

        while let Ok((next, item)) = parser.parse(input) {
            input = next;
            result.push(item);
        }

        Ok((input, result))
    }
}

fn any_char(input: &str) -> ParseResult<char> {
    match input.chars().next() {
        Some(c) => Ok((&input[c.len_utf8()..], c)),
        None => Err(input),
    }
}

fn pred<'a, A, P, F>(parser: P, predicate: F) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next, item)) = parser.parse(input) {
            if predicate(&item) {
                return Ok((next, item));
            }
        }
        return Err(input);
    }
}

fn whitespace_char<'a>() -> impl Parser<'a, char> {
    pred(any_char, |c| c.is_whitespace())
}

fn space0<'a>() -> impl Parser<'a, Vec<char>> {
    zero_or_more(whitespace_char())
}

fn space1<'a>() -> impl Parser<'a, Vec<char>> {
    one_or_more(whitespace_char())
}

#[test]
fn whitespace_test() {
    assert_eq!(Ok(("hoge", ' ')), whitespace_char().parse(" hoge"));
    assert_eq!(Ok(("  hoge", ' ')), whitespace_char().parse("   hoge"));
}

fn quoted_string<'a>() -> impl Parser<'a, String> {
    right(
        match_literal("\""),
        left(
            zero_or_more(any_char.pred(|c| *c != '"')),
            match_literal("\""),
        ),
    )
    .map(|chars| chars.into_iter().collect())
}
#[test]
fn quoted_string_test() {
    assert_eq!(
        Ok((", he said.", "I love you.".to_string())),
        quoted_string().parse("\"I love you.\", he said.")
    );
}

fn attribute_pair<'a>() -> impl Parser<'a, (String, String)> {
    pair(identifier, right(match_literal("="), quoted_string()))
}
#[test]
fn attribute_pair_test() {
    assert_eq!(
        Ok((
            "... and so on.",
            ("attr1".to_string(), "super attribute".to_string())
        )),
        attribute_pair().parse("attr1=\"super attribute\"... and so on.")
    );
}

fn attributes<'a>() -> impl Parser<'a, Vec<(String, String)>> {
    zero_or_more(right(space1(), attribute_pair()))
}
#[test]
fn attributes_test() {
    assert_eq!(
        Ok((
            "  then, three is ?",
            vec![
                ("one".to_string(), "1".to_string()),
                ("two".to_string(), "2".to_string())
            ]
        )),
        attributes().parse(" one=\"1\"  two=\"2\"  then, three is ?")
    );
}

fn element_start<'a>() -> impl Parser<'a, (String, Vec<(String, String)>)> {
    right(match_literal("<"), pair(identifier, attributes()))
}
#[test]
fn element_start_test() {
    assert_eq!(
        Ok((
            "/>",
            (
                "button".to_string(),
                vec![
                    ("type".to_string(), "button".to_string()),
                    ("label".to_string(), "hoge".to_string())
                ]
            )
        )),
        element_start().parse("<button type=\"button\" label=\"hoge\"/>")
    )
}

fn single_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal("/>")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}
#[test]
fn single_element_test() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "button".to_string(),
                attributes: vec![
                    ("type".to_string(), "button".to_string()),
                    ("label".to_string(), "hoge".to_string())
                ],
                children: vec![]
            }
        )),
        single_element().parse("<button type=\"button\" label=\"hoge\"/>")
    )
}

struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    pub fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, Output> + 'a,
    {
        BoxedParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

fn open_element<'a>() -> impl Parser<'a, Element> {
    left(element_start(), match_literal(">")).map(|(name, attributes)| Element {
        name,
        attributes,
        children: vec![],
    })
}
#[test]
fn open_element_test() {
    assert_eq!(
        Ok((
            "",
            Element {
                name: "button".to_string(),
                attributes: vec![
                    ("type".to_string(), "button".to_string()),
                    ("label".to_string(), "hoge".to_string())
                ],
                children: vec![]
            }
        )),
        open_element().parse("<button type=\"button\" label=\"hoge\">")
    );
}

// use p1 or p2
fn either<'a, P1, P2, A>(parser1: P1, parser2: P2) -> impl Parser<'a, A>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, A>,
{
    move |input| match parser1.parse(input) {
        ok @ Ok(_) => return ok,
        Err(_) => parser2.parse(input),
    }
}

// parse one element
// whitespace_wrapで包むことにより，要素の間にどれだけ空白があっても大丈夫になった
fn element<'a>() -> impl Parser<'a, Element> {
    whitespace_wrap(either(single_element(), parent_element()))
}

// </elem>
fn close_element<'a>(expected: String) -> impl Parser<'a, String> {
    right(match_literal("</"), left(identifier, match_literal(">")))
        .pred(move |name| name == &expected)
}

// 親エレメントのパーザーを作る関数
// 親エレメントとは「まずタグを1つ開き，閉じタグに至るまでの間に，0回以上の
// エレメントの繰り返しを保持する」ようなタグのことである．
fn parent_element<'a>() -> impl Parser<'a, Element> {
    open_element().and_then(|el| {
        left(zero_or_more(element()), close_element(el.name.clone())).map(move |children| {
            let mut el = el.clone();
            el.children = children;
            el
        })
    })
}

// whitespace_wrap: あるパーザーを適用する際，その左右に空白があってもよいようにする
// というパーザーを作るための関数
fn whitespace_wrap<'a, P, A>(parser: P) -> impl Parser<'a, A>
where
    P: Parser<'a, A>,
{
    right(space0(), left(parser, space0()))
}

#[test]
fn xml_parser() {
    let doc = r#"
    <top label="Top">
        <semi-bottom label="Bottom"/>
        <middle>
            <bottom label="Another Bottom"/>
        </middle>
    </top>"#;

    let parsed_doc = Element {
        name: "top".to_string(),
        attributes: vec![("label".to_string(), "Top".to_string())],
        children: vec![
            Element {
                name: "semi-bottom".to_string(),
                attributes: vec![("label".to_string(), "Bottom".to_string())],
                children: vec![],
            },
            Element {
                name: "middle".to_string(),
                attributes: vec![],
                children: vec![Element {
                    name: "bottom".to_string(),
                    attributes: vec![("label".to_string(), "Another Bottom".to_string())],
                    children: vec![],
                }],
            },
        ],
    };

    assert_eq!(Ok(("", parsed_doc)), element().parse(doc));
}
