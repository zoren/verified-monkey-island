import * as P from "parsimmon";
import * as lang from "./lang";

var whitespace = P.regexp(/\s*/m);

function token<T>(p: P.Parser<T>): P.Parser<T> {
  return p.skip(whitespace);
}

let ts = (s: string) => token(P.string(s));

var lbrace = ts('{');
var rbrace = ts('}');
var lpar = ts('(');
var rpar = ts(')');
var comma = ts(',');
var colon = ts(':');
var semiColon = ts(';');
var setEq = ts(':=');
var isEq = ts('=');
var notEq = ts('!=');
var then = ts('>>');
var ampersand = ts('&');
var question = ts('?');

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

const update = P.seq(id.skip(setEq), ctor).map(([id, c]) => new lang.Update(id, c));

const action = P.seq(id.skip(lpar), commaSep(id).skip(rpar)).map(([id, names]) => new lang.Action(id, names));

const arule = P.seq(action.skip(then).skip(lbrace),
                    semiColonSep(update).skip(rbrace),
                    P.alt(ampersand.skip(lbrace).then(stringLiteral).skip(rbrace).map((s) => new lang.PrintSideEffect(s)), P.succeed(undefined))
                    ).map(([action, updates, sideEffect]) => new lang.ARule(action, updates, sideEffect));

const TabSize = 4;

const prule =
    P.seq(P.index, lbrace.then(commaSep(comparison)).skip(rbrace).skip(question))
        .chain(([index, conds]) =>
            P.index.chain((i) => i.column === index.column + TabSize ? rule : P.fail("no more")).atLeast(1).map((rules) => new lang.PRule(conds, rules)));

const rule: P.Parser<lang.Rule> = P.lazy(() => P.alt(prule, arule));

const rules = rule.many();

const initialState = ts("Initial").skip(lbrace).then(commaSep(update)).skip(rbrace);

export const story = P.seqMap(initialState, rules, (state, rules) => new lang.Story(state, rules));
