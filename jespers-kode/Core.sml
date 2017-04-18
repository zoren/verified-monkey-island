(*
 * A script is simply a set/list of game rules. A game rule is a triple of
 * precondition, action, and update.
 *)

(* uncomment this when compiling *)
(*load "Int";*)
structure Core =
struct
(* val mapsto = S.mapsto *)
(* val lookup = S.lookup *)
(* val valToInt = S.valToInt *)

(* val undefined = S.undefined *)
val constName = fn x=> x
val constConst = fn x=> x 
(* val constInt = S.constInt *)
(* val makeState = S.makeState *)
(* val isConst = S.isConst *)
(* val valToConst = S.valToConst *)
		 
type name = State.name

type value = State.value
  (* for some reason concatWith is not defined? *)
  (* we define our own version which is not tail recursive; watch out on big
  * strings *)

  type actionname = string
  datatype vop   = EQ | NEQ  (* | LT | GT | LEQ | GEQ *)

  datatype action = PA of actionname * name list
  datatype eop = EADD | ESUB
  datatype exp = EV of value | EID of name (* | EOP of eop * (exp list) *)
  datatype precondition = Pre of exp * vop * exp
  type update = name * exp
  datatype sideeffect = NOP | PRINT of string


  datatype rule = ARule of action * update list * sideeffect
                | PRule of precondition list * rule list

  fun Rule (ps, action, updates, sideeffect) = PRule(ps, [ARule(action, updates, sideeffect)])
  type story = rule list			  

  (* pretty printing preconditions *)
  val ppop = 
      fn oper => (case oper of EQ => "=" | NEQ => "!=")

  (* pretty printing expressions *)
  local 
      open State
      fun concatWith s [] = ""
	| concatWith s [e] = e
	| concatWith s (e :: l) = e ^ s ^ concatWith s l
  in
  fun ppexp (EV v) = ppval v
    | ppexp (EID x) = ppname x
    (*| ppexp (EOP (EADD, [e1, e2])) = ppexp e1 ^ "+" ^ ppexp e2*)
    (*| ppexp (EOP (ESUB, [e1, e2])) = ppexp e1 ^ "-" ^ ppexp e2*)
    | ppexp _ = raise Fail "ppexp mismatch"

  fun pppre (Pre (x, vo, y)) = ppexp x ^ ppop vo ^ ppexp y

  fun ppprelist pl = "{ " ^ concatWith ", " (map pppre pl) ^ " }"

  (* pretty printing actions *)
  fun ppactname an = an
  fun ppact (PA (an, args)) = ppactname an ^ "(" ^ concatWith ", " (map ppname args) ^ ")"

  (* pretty printing updates *)
  fun ppupdate (x, e) = ppname x ^ ":=" ^ ppexp e
  fun ppupdatelist ul = "{" ^ concatWith "; " (map ppupdate ul) ^ "}"

  fun spaces 0 = ""
    | spaces i = "    " ^ (spaces (i - 1))

  (* pretty printing a game rule *)
  fun pprule' i (ARule (a, ds, NOP)) = (spaces i) ^ ppact a ^ " >> " ^ ppupdatelist ds ^ ""
    | pprule' i (ARule (a, ds, PRINT s)) = (spaces i) ^ ppact a ^
    " >> " ^ ppupdatelist ds ^ " & { " ^ s ^ " }"
    | pprule' i (PRule(ps, rules)) =
      (spaces i) ^ ppprelist ps ^ " ? \n" ^ (concatWith "\n" (map (pprule' (i + 1)) rules))
  end

  fun pprule rule = pprule' 0 rule

  (* evaluation of an expression exp to a value in state state *)
  fun evalexp state exp = 
  let
      open State
      val evalexp' = evalexp state
  in
      case exp of
	  (EV v) => v
	| (EID name) => lookup state name
	(*| (EOP (EADD, [e1, e2])) => constInt(valToInt(evalexp' e1) + valToInt(evalexp' e2))*)
	(*| (EOP (ESUB, [e1, e2])) => constInt(valToInt(evalexp' e1) - valToInt(evalexp' e2))*)
	|  _ => raise Fail "evalexp: Not supported yet"
  end

  (* update (u, s): given state s and update u, the function returns the new
  * state updated with d *)
  fun update ((x, exp), state) = State.mapsto state (x, evalexp state exp)
  val updatelist = fn state => fn ul  => foldl update state ul


  (* vtest takes a vtest a turns it into a function that given a "value"
  * will test whether the vtest is true or false and return a ML bool *)

  fun vtest oper (x,y) = 
      (case oper of
	   EQ  => x = y
	 | NEQ => x <> y
	 | _ => 	      
	   (let
		val (x', y') = (State.valToInt x,State.valToInt y)
	    in
	       (case oper of
		    LT  => x' < y'
		  | GT  => x' > y'
		  | LEQ => x' <= y'
		  | GEQ => x' >= y'
		  | _ => raise Fail "vtest: this shoukd not happen")
	    end))


  (* check that pre is satisfied in state *)
  fun sat state (Pre(e1, vo, e2)) = 
    (vtest vo)(evalexp state e1, evalexp state e2)
  fun satlist state [] = true
    | satlist state (p :: ps) = sat state p andalso satlist state ps

  (* function that given a state, rule, and action returns SOME(upd) if the
  * preconditions for the action are meet and the rule-action corresponds to
  * the given action *)
  fun isEnabled state (act:action) (PRule (ps, rules)) = 
      if satlist state ps
      then List.concat (map (isEnabled state act) rules)
      else []
    | isEnabled state act (ARule (a, updates, _)) =
      if act = a
      then [updates]
      else []

  (* function that given a list of rules, a state and action returns the list
  * of all possible state updates based on the given state and action; if the
  * returned list is empty it means that the action was not enabled in the
  * current state and set of rules. There can be a number of reasons for this:
  * 1) the action given never matched any specified in the rules 
  * 2) the preconditions were not met.
  *)
  fun findEnabled (state, rules: rule list) act =
  let
      val choices =
	  foldl (fn (r, enabled) => (isEnabled state act r) @ enabled) [] rules
  in
    rev choices
  end

  (* given a state and rules, this function returns the list of all actions that
  * have true preconditions 
  *)
  fun findEnabledActions (state, rules) =
  let val choices =
    foldl (fn (rule, actions) => 
      case rule of
      ARule(a,_,_) =>
        if not(List.exists (fn x=>a=x) actions)
        then a :: actions
        else actions
      | PRule(pres, rules) =>
        if satlist state pres
        then findEnabledActions (state, rules) @ actions
        else actions      
    ) [] rules
  in
    rev choices
  end

  (* given a state and rules, this function returns the list of all pairs of
   * actions and sideeffects that have true preconditions 
  *)
  fun findEnabledActionsEffects (state, rules) =
  let val choices =
    foldl (fn (rule, actions) => 
      case rule of
      ARule(a,_,se) =>
        if not(List.exists (fn (x,_)=>a=x) actions)
        then (a, se) :: actions
        else actions
      | PRule(pres, rules) =>
        if satlist state pres
        then findEnabledActionsEffects (state, rules) @ actions
        else actions
    ) [] rules
  in
    rev choices
  end

  (* given rules rules, state and action act, step (rs, state, act) produce a new
  * state according to the operational semantics;
  * First check to see if the state satisfies the "story completed" condition
  * and if that is the case, step acts as the identity function.
   *)
  fun step phi (state,rules: rule list) act = 
    (*if sat state phi*)
    (*then state*)
    (*else*)
      let val possible_upd = findEnabled (state, rules) act
      in
        case possible_upd of
             [] => state
           | _ => updatelist state (hd possible_upd)
      end

  fun stepGenAllStates (state,rules) =
      let
	  fun stepNC (state,rules) act = 
	      let val possible_upd = findEnabled (state, rules) act
	      in
		  case possible_upd of
		      [] => state
		    | _ => updatelist state (hd possible_upd)
	      end

	  val actions = findEnabledActions (state, rules)
      in
	  foldl
	      (fn (act, nstates) => 
		  ( stepNC (state, rules) act) :: nstates)
              [] actions
      end

end

