structure AbstractCore =
struct

(* val _ = List.app load ["E2C", "Extended","Mosml"]; *)

val script_name = List.nth(Mosml.argv (), 1)
(* val script_name = "MI2part1.fsm" *)
(* val script_name = "MI2part1pickupknife.fsm" *)
fun load_script fname = (E2C.transE2C (Extended.parse_file fname))
val trim4last = fn s => Substring.string(Substring.trimr 4(Substring.all s))
val story_name = trim4last script_name
val (initial_state, rules) = load_script script_name

(* local open Core *)
(* in *)
(* val phi1 = (Pre(EID "largohasdirtyshirt",EQ,EV(State.constConst "True"))) *)
(* val phi2 = (Pre(EV(State.constConst "True"),EQ,EV(State.constConst "True"))) *)
(* val test = [phi1,phi2] *)
(* end *)

datatype abstrvalue = 
	 AEQ of State.const |
	 ANEQ of State.const list |
	 AUNDEF

datatype abstractstate = 
	 BOTTOM | 
	 BINDINGS of (Core.name * abstrvalue) list

val TOP = BINDINGS []

fun ruleSatState state (Core.Rule(pre,_,_,_))  = Core.satlist state pre


fun lookupList _ [] = AUNDEF
  | lookupList n ((x,v)::l) = 
    if x = n 
    then v 
    else lookupList n l

fun lookupAbstr n BOTTOM = raise Fail "lookupAbstr: bottom state"
  | lookupAbstr n (BINDINGS bindings) = lookupList n bindings
fun dom (BINDINGS l)  = map (fn (x,_) => x) l
  | dom (BOTTOM) = raise Fail "EMPTY domain"

exception Different

fun isIn x l = List.exists (fn e=>x=e) l

fun removeDups l =
    let
        val sorted = Listsort.sort (String.compare) l
        fun rem (x::y::l) =
            if x = y
            then rem (y::l)
            else x::(rem (y::l))
          | rem x = x
    in
        rem sorted
    end

local
fun mapsto' _ [] (kn,kabsval) = [(kn,kabsval)]
  | mapsto' overwrite ((t as (tn,tabsval)) :: state)  (k as (kn,kabsval)) = 
    (case String.compare(kn,tn) of
	 LESS => (kn,kabsval)::t::state
       | GREATER => t::(mapsto' overwrite state k)
       | EQUAL => 
	 case kabsval of
	     AUNDEF => state
	   | _ => 
	     (tn, if overwrite 
		  then  
		      kabsval
		  else
		      case kabsval of 
			  AEQ v => 
			  (case tabsval of
			       (AEQ _) => kabsval
			     | (ANEQ vl) => if isIn v vl then raise Different 
					    else kabsval)
			| ANEQ kvl => 
			  (case tabsval of
			       (AEQ v) => if isIn v kvl then raise Different 
					  else tabsval
			     | (ANEQ tvl) => ANEQ(removeDups(tvl@kvl))
					     ))::state
	 )
fun mapsto overwrite BOTTOM _ = raise Fail "mapsto bottom state"
  | mapsto overwrite (BINDINGS bl) abind = 
    (BINDINGS(mapsto' overwrite bl abind) handle Different => BOTTOM)
in
val absMapsto = mapsto false
val absMapstoExpand = mapsto true
end

(* local *)
(* val empty = BINDINGS [] *)
(* val teq5 = absMapsto empty ("n", AEQ "5") *)
(* val tneq5 = absMapsto empty ("n", ANEQ ["5"]) *)
(* in *)
(* val t1 = absMapsto teq5 ("n", AEQ "3") = BINDINGS [("n", AEQ "3")] *)
(* val t2 = absMapsto tneq5 ("n", AEQ "3") = BINDINGS [("n", AEQ "3")] *)
(* val t3 = absMapsto tneq5 ("n", ANEQ ["3"]) =  *)
(* 	 BINDINGS [("n", ANEQ[ "3", "5"])] *)
(* val t4 = absMapsto teq5 ("n", ANEQ ["3"]) = BINDINGS [("n", AEQ "5")] *)
(* val t5 = absMapsto teq5 ("n", ANEQ ["5"]) = BOTTOM *)
(* val t6 = absMapsto tneq5 ("n", AEQ "5") = BOTTOM *)

(* val t = empty *)
(* val t = absMapsto t ("b", AEQ "1") *)
(* val t = absMapsto t ("c", AEQ "1") *)
(* val t = absMapsto t ("a", AEQ "1") *)
(* end *)

local
fun removeNameNameComp abstrState (Core.Pre(e1,vop,e2)) =
    let
	fun subst (Core.EID n) =
	    (case lookupAbstr n abstrState of
		 (AEQ c) => Core.EV(State.constConst c)
	       | _ => Core.EID n)
	  | subst (Core.EV c) = Core.EV c
	  | subst _ = raise Fail "subst"
    in
	Core.Pre(subst e1,vop,subst e2)
    end
in
fun norm abstrState (pre as Core.Pre(e1,vop,e2)) =
    (case pre of
	 (Core.Pre(Core.EV c1,Core.EQ,Core.EV c2)) =>
	 if c1 <> c2
	 then BOTTOM
	 else abstrState
       | (Core.Pre(Core.EV c1,Core.NEQ,Core.EV c2)) =>
	 if c1 = c2
	 then BOTTOM
	 else abstrState
       | (Core.Pre(Core.EV _,vop,Core.EID _)) =>
	 norm abstrState (Core.Pre(e2,vop,e1))
       | (Core.Pre(Core.EID n,vop,Core.EV v)) =>
	 let
	     val pre' = removeNameNameComp abstrState pre
	 in
	     if pre = pre' 
	     then
		 if State.isUndefined v
		 then
		     abstrState
		 else
		     (absMapsto abstrState
				(n,(if not(State.isConst v )
				   then raise Fail ("not constant "^(State.ppval v))
				   else 
				       (case vop of
					    Core.EQ => AEQ(State.valToConst v)
					  | Core.NEQ => ANEQ [State.valToConst v]
					  | _ => raise Fail "unmatched")
				       )))
	     else
		 norm abstrState pre'
	 end
       | (Core.Pre(Core.EID n1,vop,Core.EID n2)) =>
	 let
	     val pre' = removeNameNameComp abstrState pre
	 in
	     if pre = pre' 
	     then
		 abstrState
	     else
		 norm abstrState pre'
	 end
       | pre => raise Fail "Not implemented yet")

fun norms abstrState [] = abstrState
  | norms abstrState (pre::pres) = norms (norm abstrState pre) pres
end
    
val precond2AbstrState = norms TOP
fun preconds2AbstrState abstrState [] = abstrState
  | preconds2AbstrState abstrState (pres::preslist) = 
    preconds2AbstrState (norms abstrState pres) preslist

(* fun stateToAbstrState rules state = *)
(*     let *)
(* 	val satRules = rulesSatState rules state *)
(* 	val satPres =  *)
(* 	    map (fn(Core.Rule(pre,_,_,_))=> pre) satRules *)
(*     in *)
(* 	( satPres, preconds2AbstrState TOP satPres) *)
(*     end *)

(* val abstrInitialState = stateToAbstrState rules initial_state *)

open Core
fun evalExp abstrState (EV v) = 
    if State.isConst v 
    then AEQ (State.valToConst v)
    else AUNDEF
  | evalExp abstrState (EID n) = 
    lookupAbstr n abstrState
  | evalExp _ _ = raise Fail "Not implemented"

fun evalPre abstrState (Pre(e1,vop,e2)) = 
    case (evalExp abstrState e1,evalExp abstrState e2) of
	(AEQ v1,AEQ v2) => SOME(case vop of 
				    EQ => (v1=v2)
				  | NEQ => (v1<>v2)
				  | _ => raise Fail "Unmatched"
					       )
      | (AUNDEF,_) => if vop = NEQ then SOME true else NONE
      | (_,AUNDEF) => if vop = NEQ then SOME true else NONE
      | _ => NONE

fun evalPres abstrState [] = SOME true
  | evalPres abstrState (pres::preslist) =
    case (evalPre abstrState pres) of
	(SOME true) => evalPres abstrState preslist
      | _ => NONE

(* fun filterRules abstrState rules =  *)
(*     List.filter  *)
(*     (fn (Rule(pres,_,_,_)) => (evalPres abstrState pres) = SOME true) rules *)

fun evalUpdatelist abstrState [] = abstrState
  | evalUpdatelist abstrState ((n,exp)::l) = 
    (case evalExp abstrState exp of 
(* 	 AUNDEF => evalUpdatelist abstrState l *)
(*        |  *)s =>
	absMapstoExpand (evalUpdatelist abstrState l) 
			(n, s))


fun rule2AbstrRule (Rule(pre,action,updatelist,_)) = 
    let
	val abstrCond = precond2AbstrState pre
    in
	(abstrCond,action,evalUpdatelist abstrCond updatelist)
    end

fun rules2AbstrRule rules = map rule2AbstrRule rules

fun concatWith s [] = []
  | concatWith s [e] = [e]
  | concatWith s (e :: l) = e :: s :: concatWith s l
fun ppAbstrBinding (n,AEQ v) = n :: " = " :: [v]
  | ppAbstrBinding (n,ANEQ vl) = n :: " not in { " :: (concatWith ", " vl) @ ["} "]
  | ppAbstrBinding (n,AUNDEF) = n :: [" undefined "]
fun ppAbstrState (BOTTOM) = ["BOTTOM"]
  | ppAbstrState (BINDINGS []) = ["TOP"]
  | ppAbstrState (BINDINGS bl) = 
    concatWith ", " (((map (String.concat o ppAbstrBinding) bl)))
fun ppAbstrRule (abstrState, action, abstrState') = 
    String.concat("{ " :: ppAbstrState abstrState @
    "} ? " :: Core.ppact action ::
    ">> { " :: ppAbstrState abstrState' @ ["}"])
fun ppAbstrRule' (rule as(abstrState, action, abstrState')) = 
    (if dom abstrState = dom abstrState'
     then "I "
     else "  "
    ) ^ (ppAbstrRule rule)








fun isSubset [] _ = true
  | isSubset (x::xs) S = isIn x S andalso isSubset xs S

fun abstrIsSubset (BINDINGS bl1) (BINDINGS bl2) = isSubset bl1 bl2
  | abstrIsSubset _ _ = raise Fail "abstrIsSubset not implemented"

fun findPreceedingRules cond rules = 
    let
	fun isPreceeding (abstrState,action,abstrState') = 
	    abstrIsSubset (cond) abstrState'
    in
	List.filter isPreceeding rules
    end

val acceptingAbstrState = (story_name, AEQ "True")
fun endsInAcceptingState (_,_,BINDINGS bindings) = 
    isIn acceptingAbstrState bindings
  | endsInAcceptingState (_,_,_) = false
(* val acc = (BINDINGS [("Inn/rope.loc", AEQ "Inn"), ("knife.loc", AEQ "Inv"), *)
(*                 ("l", AEQ "Off"), ("player.loc", AEQ "Inn")], *)
(*       PA("use", ["knife", "Inn/rope"]), *)
(*       BINDINGS [("MI2part1pickupknife", AEQ "True"), ("knife.loc", AEQ "Inv"), *)
(*                 ("l", AEQ "Off"), ("player.loc", AEQ "Inn")]) *)
(* val astate = #1 acc *)
(* val _ = print(String.concat(ppAbstrState astate) ) *)
val _ = print "\n------------------\n\n"
fun ppAbstrRules rules = String.concat(concatWith "\n" (map ppAbstrRule' rules))
val abstrRules = rules2AbstrRule rules

val acc= case List.filter endsInAcceptingState abstrRules of
	  [] => raise Fail "No accepting state"
	| [(_,_,acc)] => acc
	| _ =>  raise Fail "Cannot handle more than one accepting state"


val _ = print(ppAbstrRules abstrRules);
val _ = print "\n\n\n"
(* val astate = acc *)
(* val _ = print (String.concat(ppAbstrState astate)) *)
val _ = print "\n\n1\n"
val finalRules = findPreceedingRules (BINDINGS [acceptingAbstrState]) abstrRules
val _ = print(ppAbstrRules finalRules)
val _ = print "\n\n2\n"

val t = case finalRules of 
	    [] => raise Fail "No rule leads to the final accepting state"
	  | [(astate,_,_)] => astate
	  | _ => raise Fail "Several rules!"
val newRules = findPreceedingRules t abstrRules
val _ = print(ppAbstrRules ( newRules));
val _ = print "\n\n3\n"



val t = case [hd (tl newRules)] of 
	    [] => raise Fail "No rule leads to the final accepting state"
	  | [(astate,_,_)] => astate
	  | _ => raise Fail "Several rules!"

val _ = print(ppAbstrRules ( (findPreceedingRules t abstrRules)));
val _ = print "\n"


































(* fun evalRules abstrState rules =  *)
(*     let *)
(* 	val fRules = filterRules abstrState rules  *)
(* 	val updateLists = map (fn(Rule(_,_,ul,_))=>ul) fRules *)
(*     in *)
(* 	removeDupsEq(map (evalUpdatelist abstrState) updateLists) *)
(*     end *)

(* fun rule2AbstrState (Rule(pre,_,_,_)) = precond2AbstrState pre *)
(* val abstrStates = removeDupsEq(map rule2AbstrState rules) *)


(* fun evalRulesSet [] _ = [] *)
(*   | evalRulesSet (abstrState::abstrStateSet) rules =  *)
(*     ((evalRules abstrState rules)@(evalRulesSet abstrStateSet rules)) *)
(* fun evalRulesSet2 ast rules = removeDupsEq(evalRulesSet ast rules) *)

(* fun compareLength(BOTTOM,BOTTOM) = EQUAL *)
(*   | compareLength(BOTTOM,_) = LESS *)
(*   | compareLength(_,BOTTOM) = GREATER *)
(*   | compareLength(BINDINGS bl1,BINDINGS bl2) =  *)
(*     Int.compare(length bl1,length bl2) *)

(* val abstrStates = removeDupsEq(map rule2AbstrState rules) *)
(* val abstrStates = Listsort.sort compareLength abstrStates *)
(* val abstrStates = evalRulesSet2 abstrStates rules *)

(* datatype abstrStateSetTree =  *)
(* 	 T | *)
(* 	 ABSTR of abstrStateSetTree * (Core.name * abstrvalue) *)

(* fun genTree abstrStates =  *)
(*     let *)
(* 	fun genTree' t = t *)
(*     in *)
(* 	genTree' T *)
(*     end *)

(* val init = [abstrInitialState] *)

(* val init = evalRulesSet init rules *)

(* val testupdatelist  = [("shovel.loc", EV(State.constConst "Inv"))] *)
(* val testupdatelist  = [("riches.loc", EV(State.constConst "Undefined")), *)
(*            ("hasbeenrobbed", EV(State.constConst "True"))] *)
(* val astate =  *)
(*     BINDINGS [("hasbeenrobbed", AEQ "True"), ("player.loc", AEQ "Kitchen"), *)
(*      ("riches.loc", AEQ "Undefined"), *)
(*      ("shovel.loc", AEQ "WoodtickRight")] *)

(* val fRules = filterRules abstrInitialState rules *)
(* val t = evalUpdatelist abstrInitialState testupdatelist; *)

(* val init = abstrStates rules *)
(* val init = evalRulesSet init rules; *)

(* val t = map (evalPre abstrInitialState) (hd pres1) *)
(* val t = evalPres abstrInitialState (hd pres1) *)
(* val t = map (evalPres abstrInitialState) pres *)




(* val t = evalExp abstrInitialState (EV(State.constConst "WoodtickRight")) *)
(* val t = evalExp abstrInitialState (EID "player.loc") *)
(* val t = evalExp abstrInitialState (EID "shovel.loc") *)

(* val t2 = evalPres abstrInitialState  *)



(* fun presToAbstr _ (FALSE::pres) = BOTTOM *)
(*   | presToAbstr abstrState (TRUE::pres) = presToAbstr abstrState pres *)
(*   | presToAbstr abstrState ((BIND bind)::pres) =  *)
(*     presToAbstr (mapsto abstrState bind) pres *)
(*   | presToAbstr abstrState ((BINDNN _)::pres) = presToAbstr abstrState pres *)

(* fun removeNameNameComp abstrState (Core.Pre(e1,vop,e2)) =  *)
(*     let *)
(* 	fun subst (Core.EID n) =  *)
(* 	    (case lookupAbstr n abstrState of  *)
(* 		 (SOME(AEQ c)) => Core.EV(State.constConst c) *)
(* 	       | _ => Core.EID n) *)
(* 	  | subst (Core.EV c) = Core.EV c *)
(* 	  | subst _ = raise Fail "subst" *)
(*     in *)
(* 	Core.Pre(subst e1,vop,subst e2) *)
(*     end *)

(* in *)

(* fun presToAbstrBinding pres =  *)
(*     let *)
(* 	val updates = (map norm pres) *)
(* 	val abstrState = presToAbstr TOP updates *)
(* 	fun fixnn (BINDNN pre) = BINDNN (removeNameNameComp abstrState pre) *)
(* 	  | fixnn x = x *)
(* 	val nupdates = map fixnn updates *)
(*     in *)
(* 	presToAbstr TOP nupdates *)
(*     end  *)
(* fun stateToAbstrState' [] = TOP *)
(*   | stateToAbstrState' (pre::pres) = preToAbstrBinding (stateToAbstrState' pres) pre *)

(* end *)

(* val rulesSatState = fn rules => fn state => List.filter (ruleSatState state) rules *)
(* val preToAbstrBinding = preToAbstrBinding  *)



(* val (precondslist,abstrState) = ((stateToAbstrState rules initial_state) *)
(* 				 handle (NORM pre) => ([[pre]],BOTTOM)) *)

(* val reducedPrecondsList =  *)
(*     map (fn preconds =>(map (removeNameNameComp abstrState))) precondslist *)

(* fun isEnabled (Core.Rule(preslist,_,_,_)) abstrState =  *)
(*     let *)

(* 	fun norm (Pre(Core.C c1,vop,Core.C c2)) = c1 = c2 *)
(* 	  | norm (Pre(Core.N n,Core.EQ,Core.C c)) =  *)
(* 	    (lookupAbstr n abstrState) = (ABIND()) *)
(* 	  | norm (Pre(c as C _,vop,n as N _)) = norm (Pre(n,vop,c)) *)
(* 	  | norm (Pre(n1 as N _,vop,n2 as N _)) = raise Fail "norm" *)
(* 	fun evalExp _ (EV v) = State.valToConst v *)
(* 	  | evalExp abstrState (EID name) = lookupAbstr abstrState name *)
(* 	  | evalExp  _ _ = raise  Fail "evalExp: Not supported yet" *)
(* 	val evalExp' = evalExp abstrState *)
(* 	fun evalPre (Pre(e1,EQ,e2)) = evalExp' e1 = evalExp' e2 *)
(* 	  | evalPre (Pre(e1,NEQ,e2)) = evalExp' e1 <> evalExp' e2 *)
(* 	fun evalPreList [] = true *)
(* 	  | evalPreList (p :: ps) = evalPre p andalso evalPreList ps *)
(*     in *)
(* 	evalPreList preslist *)
(*     end *)



(* fun abstrupdate ((x, exp), state) = mapsto state (x, evalexp state exp) *)
(* val abstrupdatelist = fn state => fn ul  => foldl update state ul *)

(* fun findRulesWithVar var rules =  *)
(* fun findRulesWithVars var rules =  *)


end