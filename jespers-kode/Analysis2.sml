structure Analysis2 =
struct

val script_name = List.nth(Mosml.argv (), 1)
fun load_script fname = (E2C.transE2C (Extended.parse_file fname))
val (initial_state, rules) = load_script script_name
(* fun load_script_compact fname = E2C.transE2CCompactRules *)
(* 				    (Extended.parse_file fname) *)

(* val (initial_state, rules,c,v) = load_script_compact script_name *)
(* val cvmapList = (c,v) *)

(* val (constMap,varMap) = (E2C.lookupException c,E2C.lookupException v) *)

(* structure Core = E2C.Core *)
(* local open Core *)
(* in *)
(* val phi = (Pre(EID "Small",EQ,EV(Core.state.constConst "True"))) *)
(* end *)

(* structure Core = E2C.CompactCore *)
local open Core
in


(* val phi = (Pre(EID (E2C.lookupException v "largohasdirtyshirt"),EQ,EV(Core.state.constConst(E2C.lookupException c "True")))) *)
(* val phi = Pre(EID (E2C.lookupException v "bucket.cont"),EQ, EV(Core.state.constConst(E2C.lookupException c "Mud"))) *)

(* val phi = Pre(EID (E2C.lookupException v "player.loc"),EQ, EV(Core.state.constConst(E2C.lookupException c "A"))) *)
val phi = Pre(EID "light.switch",EQ, EV(State.constConst("On")))
end


fun fastGetAllStates rules state =
    let
	open Core
	fun isGuarded (Rule(preconds,_,_,_)) = Core.satlist state preconds
	val unguardedRules = List.filter isGuarded rules
	fun filterSameAction ((r1 as Rule(_,a,_,_))::(r2 as Rule(_,a',_,_))::l) = 
	    if a = a' 
	    then
		r1::filterSameAction(l)
	    else
		r1::(filterSameAction(r2::l))
	  | filterSameAction l = l
	val unguardedFiltertedRules = filterSameAction unguardedRules
	fun makeStateSet [] acc = acc
	  | makeStateSet ((Core.Rule(_,_,updates,_))::ruleList) acc =
	    makeStateSet ruleList (Splayset.add(acc,Core.updatelist state updates))
    in
	case unguardedRules of
	    [] => raise Fail "No rules match"
	  | _ => makeStateSet unguardedRules (Splayset.empty State.compare)
    end

fun fastGetAllStatesList rules state =
    let
	open Core
	fun isGuarded (Rule(preconds,_,_,_)) = Core.satlist state preconds
	val unguardedRules = List.filter isGuarded rules
	fun filterSameAction ((r1 as Rule(_,a,_,_))::(r2 as Rule(_,a',_,_))::l) = 
	    if a = a' 
	    then
		r1::filterSameAction(l)
	    else
		r1::(filterSameAction(r2::l))
	  | filterSameAction l = l
	val unguardedFiltertedRules = filterSameAction unguardedRules
(* 	fun makeStateSet [] acc = acc *)
(* 	  | makeStateSet ((Core.Rule(_,_,updates,_))::ruleList) acc = *)
(* 	    makeStateSet ruleList ((Core.updatelist(state,updates))::acc) *)
	val makeStateSet = map (fn (Core.Rule(_,_,updates,_)) => Core.updatelist state updates)
    in
	case unguardedRules of
	    [] => raise Fail "No rules match"
	  | _ => makeStateSet unguardedRules
    end


fun fastGetAllStatesFromStates rules states =
    Splayset.foldr(fn(item,acc)=>
	    Splayset.union(fastGetAllStates rules item,acc)) (Splayset.empty State.compare) states

datatype 'a out = TRUE of 'a | FALSE

(* (\* Breadth-first search *\) *)
(* fun EFbf rules phi (s_init:State.state) = *)
(*     let *)
(*       fun R states = *)
(* 	  fastGetAllStatesFromStates rules states *)
(*       fun satStates states phi = *)
(* 	  Splayset.find (fn state => Core.sat state phi) states *)
(*       fun fix (oldS,S) = *)
(* 	  if (Splayset.equal(oldS, S)) *)
(* 	  then *)
(* 	      FALSE *)
(* 	  else *)
(* 	      (let *)
(* 		   val _ = print "S start\n" *)
(* 		   val states = satStates S phi *)
(* 		   val _ = print "S stop\n" *)
(* 	       in *)
(* 		   (case states of *)
(* 			SOME s => TRUE s *)
(* 		      | NONE => *)
(* 			(let *)
(* 			     val _ = print "D start\n" *)
(* 			     val d = Splayset.difference(S,oldS) *)
(* 			     val _ = print "D stop\n" *)
(* 			     val _ = print "R start\n" *)
(* 			     val r = R d *)
(* 			     val _ = print "R stop\n" *)
(* 			 in *)
(* 			     (print ((Int.toString (Splayset.numItems d))^" "^(Int.toString (Splayset.numItems S))^"\n"); *)
(* 			      fix(S,Splayset.union (S, r))) *)
(* 			 end) *)
(* 			) *)
(* 	       end) *)
(*   in *)
(* 	fix (Splayset.empty State.compare,Splayset.singleton State.compare s_init) *)
(*   end *)


fun EFbf2 rules phi (s_init:State.state) =
    let
      fun R states =
	  fastGetAllStatesFromStates rules states
      fun satStates states phi =
	  Splayset.find (fn state => Core.sat state phi) states
      fun fix (S,newStates,added) =
	  if added
	  then
	      (let
		   val _ = print "S start\n"
		   val states = satStates S phi
		   val _ = print "S stop\n"
	       in
		   (case states of
			SOME s => TRUE s
		      | NONE =>
			(let
			     val _ = print "D start\n"
			     val d = newStates
			     val _ = print "D stop\n"
			     val _ = print "R start\n"
			     val r = R d
			     val _ = print "R stop\n"
			 in
			     (print ((Int.toString (Splayset.numItems d))^" "^(Int.toString (Splayset.numItems S))^"\n");
			      fix(Splayset.union (S, r),r,Splayset.numItems r > 0))
			 end)
			)
	       end)
	  else
	      FALSE
  in
	fix (Splayset.empty State.compare,Splayset.singleton State.compare s_init,true)
  end

(*Depth-first search*)
(* structure MutableSet = MutableHashSet *)
(* fun EFdf rules phi s_init = *)
(*     let *)
(* (\*       fun R states = *\) *)
(* (\* 	  fastGetAllStatesFromStates rules states *\) *)
(*       fun findFirst p [] = NONE *)
(* 	| findFirst p (x::xs) = if p x then SOME x else findFirst p xs *)
(*       fun satStates states phi = *)
(* 	  findFirst (fn state => Core.sat state phi) states *)

(* (\*       fun fix (newStates) = *\) *)
(* (\* 	  case satStates newStates phi of *\) *)
(* (\* 	      SOME s => TRUE s *\) *)
(* (\* 	    | NONE => *\) *)
(* (\* 	      let *\) *)
(* (\* 		  val t = List.concat( *\) *)
(* (\* 				      List.foldr(fn(state,acc)=> *\) *)
(* (\* 						       (fastGetAllStatesList rules state) ::acc) [] newStates) *\) *)
(* (\* 		  val _ = List.app (fn(state)=>HashSet.add(s,state)) t *\) *)
(* (\* 	      in *\) *)
(* (\* 		  fix t *\) *)
(* (\* 	      end *\) *)
(*       val S = MutableSet.empty Core.state.hash Core.state.compare *)
(*       val _ = MutableSet.add S s_init *)
(*       fun fix [] = FALSE *)
(* 	| fix newStates =  *)
(* 	  case satStates newStates phi of *)
(* 	      SOME s => TRUE s *)
(* 	    | NONE => *)
(* 	      let *)
(* 		  open List *)
(* 		  val _ = print "1\n" *)
(* 		  val newStates' = concat( *)
(* 				 foldr(fn(state,acc)=> *)
(* 					 (fastGetAllStatesList rules state) ::acc) [] newStates) *)
(* 		  val _ = print "2\n" *)
(* 		  val _ = print ("ns: "^(Int.toString(length newStates'))^"\n") *)
(* 		  val _ = print ("S:  "^(Int.toString(MutableSet.numItems S))^"\n") *)
(* 		  val _ = List.app (fn(state)=>MutableSet.add S state) newStates' *)
(* 		  val _ = print ("3 "^(Int.toString (Core.state.getCount()))^"\n") *)
(* 	      in *)
(* 		  fix newStates' *)
(* 	      end *)
(*   in *)
(* 	fix [s_init] *)
(*   end *)


(* fun EFdf rules phi S =(\*using one stateset*\) *)
(*     let *)
(*       fun R states = *)
(* 	  fastGetAllStatesFromStates rules states *)
(*       fun satStates states phi = *)
(* 	  Splayset.find (fn state => Core.sat state phi) states *)
(*       fun fix S newStates = *)
(* 	  if newStates *)
(* 	  then *)
(* 	      (let *)
(* 		   val _ = print "S start\n" *)
(* 		   val states = satStates S phi *)
(* 		   val _ = print "S stop\n" *)
(* 	       in *)
(* 		   (case states of *)
(* 			SOME s => TRUE s *)
(* 		      | NONE => *)
(* 			(let *)
(* 			     val _ = print "D start\n" *)
(* 			     val d = Splayset.difference(S,oldS) *)
(* 			     val _ = print "D stop\n" *)
(* 			     val _ = print "R start\n" *)
(* 			     val r = R d *)
(* 			     val _ = print "R stop\n" *)
(* 			 in *)
(* 			     (print ((Int.toString (Splayset.numItems d))^" "^(Int.toString (Splayset.numItems S))^"\n"); *)
(* 			      fix(Splayset.union (S, r))) *)
(* 			 end) *)
(* 			) *)
(* 	       end) *)
(* 	  else *)
(*       FALSE *)
(*   in *)

(*   end *)

  fun concatWith s [] = ""
    | concatWith s [e] = e
    | concatWith s (e :: l) = e ^ s ^ concatWith s l
local

open Core
val (initial_state, rules) = load_script script_name
in
val _ = print (concatWith "\n" (map Core.pprule rules))
end
fun toAr [] ar = ar
  | toAr ((i,v)::l) ar = (print ((Int.toString i)^"\n");(Array.update(ar,i,v) ;toAr l ar))

val _ = 
    print
	("\n\n"^(
		 case (EFbf rules phi initial_state  (* (toAr initial_state (Core.state.empty))  *)) of
	(TRUE s) => "TRUE \n"^(* (ppstate s)^ *)"\n--FUTURE STATES------------\n"
      | (FALSE) => "FALSE \n"
    ))

(* val _ = print("\n\n"^( *)
(* 		      case (EFbf rules phi *)
(* 			       (initial_state)) of *)
(* 	(TRUE s) => "TRUE \n"^(\* (ppstate s)^ *\)"\n--FUTURE STATES------------\n" *)
(*       | (FALSE) => "FALSE \n" *)
(*     )) *)





(* fun fastGetAllStatesFromStates rules states = *)
(* (\*     (StateSet.unionList Core.compareValue) *\) *)
(* (\* 		       (map (fn state => fastGetAllStates rules state) (Splayset.listItems states)) *\) *)
(* (\* 	('item * 'b -> 'b) -> 'b -> 'item set -> 'b	        *\) *)
(*     StateSet.foldr (fn(state,acc)=> StateSet.union(acc,fastGetAllStates rules state)) (StateSet.emptyStateSet Core.compareValue) states  *)



(*   val ((initial_state, rules),(constMap,varMap)) = load_script_compact script_name; *)

(* open CC *)

(* local *)
(* open Core *)
(* in *)
(* (\* fun ppstates [] = "" *\) *)
(* (\*   | ppstates (s::S) = (ppstate s)^ "\n-------------------\n"^(ppstates S) *\) *)

(* (\* val phi = Pre(EID "hasbeenrobbed",EQ,EV(C "True")) *\) *)
(* (\* val phi = Pre(EID "player.loc",EQ,EV(C "Inn")) *\) *)
(* (\* val phi = Pre(EID "knife.loc",EQ,EV(C "Inv")) *\) *)
(* (\* val phi = Pre(EID "spit.loc",EQ,EV(C "BloddyLips")) *\) *)

(* (\* val phi = Pre(EID "rat.loc",EQ,EV(C "Inv"))(\\*error*\\) *\) *)
(* (\* val phi = Pre(EID "largoembargo",EQ,EV(C "Alleviated"))(\\*error*\\) *\) *)
(* (\* val phi = Pre(EID "cheesesq.loc",EQ,EV(C "Inv"))(\\*error*\\) *\) *)
(* (\* val phi = Pre(EID "paper.hasspit",EQ,EV(C "True"))(\\*error*\\) *\) *)
(* (\* val phi = Pre(EID "spit.loc",EQ,EV(C "BloddyLips")) *\) *)


(* (\* val phi = Pre(EID "paper.hasspit",EQ,EV(C "True"))(\\*error*\\) *\) *)
(* (\* val phi = Pre(EID "paper.loc",EQ,EV(C "Inv")) *\) *)
(* (\* val phi = Pre(EID "spit.loc",EQ,EV(C "BloddyLips")) *\) *)

(* val phi = Pre(EID "paper.hasspit",EQ,EV(C "True")) *)
(* val phi = Pre(EID "bones.loc",EQ,EV(C "Inv")) *)
(* val phi = Pre(EID "toupee.loc",EQ,EV(C "Inv"))(\*1,4 sec*\) *)
(* val phi = Pre(EID "bra.loc",EQ,EV(C "Inv"))(\*long time*\) *)


(* val phi = Pre(EID "bucket.loc",EQ,EV(C "Inv")) *)
(* val phi = Pre(EID "bucket.cont",EQ,EV(C "Mud")) *)
(* val phi = Pre(EID "ldoor",EQ,EV(C "Closed")) *)
(* val phi = Pre(EID (E2C.lookupException varMap "bucket.cont"),EQ, *)
(* 	      EV(CC.state.constInt(E2C.lookupException constMap  "Empty"))) *)

(*val phi = Pre(EID ("bucket.cont"),EQ, EV(Core.state.constConst "Empty"))*)
(* val phi = Pre(EID "largohasdirtyshirt",EQ,EV(Core.state.constConst "True")) *)

(* (\* val phi = Pre(EID "doll.loc",EQ,EV(C "Inv"))(\\*error*\\) *\) *)
(* open Core *)

(* val phi = Pre(EID "MI2part1",EQ,EV(Core.state.constConst "True")) *)

(* (\*       fun R (states : (string * value) list list) = *\) *)
(* (\* 	  getAllStatesFromStates rules states *\) *)

(* fun SSinterp rules state action = *)
(*     let *)
(* 	open Core *)
(* 	fun isGuarded (Rule(preconds,_,_)) = satlist state preconds *)
(* 	fun hasAction (Rule(_,action',_)) = action = action' *)
(* 	val unguardedRules = List.filter isGuarded rules *)
(* 	val actionMatchedRules = List.filter hasAction unguardedRules *)
(*     in *)
(* 	case actionMatchedRules of *)
(* 			[] => raise Fail "No rules match" *)
(* 		| ((Rule(_,_,updates))::_) => updatelist(state,updates) *)
(*     end *)

(*       fun isIn (e,[]) = false *)
(* 	| isIn (e,x::xs) = e = x orelse isIn(e,xs) *)

(*       fun union [] states' = states' *)
(* 	| union (state::states) states' = *)
(* 	  if isIn(state, states') *)
(* 	  then *)
(* 	      union states states' *)
(* 	  else *)
(* 	      state::(union states states') *)

(*       fun unionList ([],S) = S *)
(* 	| unionList (x::xs,S) = union x (unionList(xs,S)) *)

(*       fun subsetEQ [] S' = true *)
(* 	| subsetEQ (x::xs) S' = isIn(x,S') andalso subsetEQ xs S' *)
(*       fun setEq S S' = (subsetEQ S S') andalso (subsetEQ S' S) *)
	  
(* fun getAllStates rules state = *)
(*     let *)
(* 	open Core *)
(* 	fun isGuarded (Rule(preconds,_,_)) = satlist state preconds *)
(* 	val unguardedRules = List.filter isGuarded rules *)
(* 	fun makeAllStates [] =  *)
(* 	  | makeAllStates ((Rule(_,_,updates))::ruleList) = *)
(* 	    (updatelist(state,updates))::(makeAllStates ruleList) *)
(*     in *)
(* 	case unguardedRules of *)
(* 	    [] => raise Fail "No rules match" *)
(* 	  | ((Rule _)::_) => makeAllStates unguardedRules *)
(*     end *)

(* fun getAllStates rules state :  (string * Core.value) list Splayset.set = *)
(*     let *)
(* 	open Core *)
(* 	open StateSet *)
(* 	fun isGuarded (Rule(preconds,_,_)) = satlist state preconds *)
(* 	val unguardedRules = List.filter isGuarded rules *)
(* 	fun makeStateSet [] = (emptyStateSet Core.compareValue) *)
(* 	  | makeStateSet ((Rule(_,_,updates))::ruleList) = *)
(* 	    add((makeStateSet ruleList):value StateSet.stateset,updatelist(state,updates)) *)
(*     in *)
(* 	case unguardedRules of *)
(* 	    [] => raise Fail "No rules match" *)
(* 	  | ((Rule _)::_) => makeStateSet unguardedRules *)
(*     end *)

(* (\* fun getAllStatesFromStates rules states = *\) *)
(* (\*     List.concat(map (fn state =>getAllStates rules state) states) *\) *)

(* fun getAllStatesFromStates rules (states:  (string * Core.value) list Splayset.set) :  (string * Core.value) list Splayset.set= *)
(*     StateSet.unionList Core.compareValue (map (fn state => getAllStates rules state) (Splayset.listItems states)) *)




(* (\* (\\* val t = R ([initial_state]) *\\) *\) *)
(* (\* (\\* val _ = print (ppstates t) *\\) *\) *)
(* (\* (\\* val _ = map (fn  t=> print  *\\) *\) *)
(* (\* (\\* ((Bool.toString(sat t (Pre(EID "hasbeenrobbed",EQ,EV(C "True"))))) *\\) *\) *)
(* (\* (\\* ^"\n") *\\) *\) *)
(* (\* (\\* ) t *\\) *\) *)
	     
(* end *)




end