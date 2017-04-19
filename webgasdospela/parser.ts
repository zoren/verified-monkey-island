import * as P from "parsimmon";
import * as lang from "./lang";

var whitespace = P.regexp(/\s*/m);

function token<T>(p: P.Parser<T>): P.Parser<T> {
  return p.skip(whitespace);
}

let ts = (s: string) => token(P.string(s));

var lbrace = ts('{');
var rbrace = token(P.string('}'));
var lpar = token(P.string('('));
var rpar = token(P.string(')'));
var comma = token(P.string(','));
var colon = token(P.string(':'));
var semiColon = token(P.string(';'));
var setEq = token(P.string(':='));
var isEq = token(P.string('='));
var notEq = token(P.string('!='));
var then = token(P.string('>>'));
var ampersand = token(P.string('&'));
var question = token(P.string('?'));

function commaSep<T>(parser: P.Parser<T>) {
  return P.sepBy(parser, comma);
}
function semiColonSep<T>(parser: P.Parser<T>) {
  return P.sepBy(parser, semiColon);
}

var stringLiteral =
  token(P.regexp(/"((?:\\.|.)*?)"/, 1))
    .desc('string');

const id = token(P.regexp(/[a-z][a-zA-Z0-9.]*/)).desc("id");
const ctor = token(P.regexp(/[A-Z][a-zA-Z0-9]*/)).desc("ctor").map((s) => new lang.Constant(s));

const exp: P.Parser<lang.Expression> = P.lazy(() =>
  whitespace.then(P.alt(
        id.map((s) => new lang.VariableDeref(s)), ctor
    )));

const comparisonOp =
    P.alt(isEq.result(lang.ComparisonOperator.EQ), notEq.result(lang.ComparisonOperator.NEQ));
const comparison = P.seqMap(exp, comparisonOp, exp, (e1, compOp, e2) => new lang.Condition(e1, compOp, e2))

const update = P.seq(id.skip(setEq), exp).map(([id, e]) => new lang.Update(id, e));

const action = P.seq(id.skip(lpar), commaSep(id).skip(rpar)).map(([id, names]) => new lang.Action(id, names));

const arule = P.seq(action.skip(then).skip(lbrace),
                    semiColonSep(update).skip(rbrace).skip(ampersand).skip(lbrace),
                    stringLiteral.skip(rbrace)
                    ).map(([action, updates, sideEffect]) => new lang.ARule(action, updates,
                        new lang.PrintSideEffect(sideEffect)));

const prule =
    P.seq(P.index, lbrace.then(commaSep(comparison)).skip(rbrace).skip(question))
        .chain(([index, conds]) =>
            rule.atLeast(1).map((rules) => new lang.PRule(conds, rules)));

const rule: P.Parser<lang.Rule> = P.lazy(() => P.alt(prule, arule));

const rules = P.sepBy(rule, semiColon);

const initialState = ts("Initial").skip(lbrace).then(commaSep(update)).skip(rbrace);

export const story = P.seqMap(initialState, rules, (state, rules) => new lang.Story(state, rules));
