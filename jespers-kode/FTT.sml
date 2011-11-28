structure FTT =
struct
(* val _ = List.app load ["Tools","E2C", "Extended","Mosml"]; *)


exception RES of State.state


(* datatype node = NODE of State.state *)
(* datatype edge = EDGE of State.state * node ref * node ref *)


(* fun canCompleteStoryGraph phi (initial_state, rules) = *)
(*     let *)
(* 	fun isIn x l = List.exists (fn e=> x = e ) l *)
(* 	fun fix _ [] = raise Fail "" *)
(* 	  | fix S ((NODE state)::pending) = *)
(* 	    (if Core.sat state phi *)
(* 	     then raise (RES state) *)
(* 	     else (let *)
(* 		       fun isSeen state = not(isIn state S) *)
(* 		       val pot_actstates = (Core.stepGenAllStates (state,rules)) *)
(* 		       val newPend = List.filter isSeen pot_actstates *)
(* (\* 		       val _ = print ((Int.toString (length newPend'))^"\n") *\) *)
(* 		   in *)
(* 		       fix (state::S) (newPend@pending) *)
(* 		   end) *)
(* 		  ) *)
(*     in *)
(* 	(fix [] [NODE initial_state] handle RES s => s) *)
(*     end *)


(* fun genAllStates (initial_state, rules) = *)
(*     let *)
(* 	fun fix S [] = raise Fail "" *)
(* 	  | fix S (state::pending) =  *)
(* 	    ((let *)
(* 		       val S = state :: S *)
(* 		       fun isIn x l = List.exists (fn e=> x  = e) l *)
(* 		       fun isSeen state = not(isIn state S) *)
(* 		       val pot_actstates = (Core.stepGenAllStates (state,rules)) *)
(* 		       val newPend = List.filter isSeen pot_actstates *)
(* 		       val _ = case pot_actstates of [] => raise RES state | _ => NONE *)
(* 		       val newPend' = newPend@pending *)
(* 		       val _ = print ((Int.toString (length S))^" "^(Int.toString (length newPend'))^"\n") *)
(* 		   in *)
(* 		       fix S newPend' *)
(* 		   end) *)
(* 		  ) *)
(*     in *)
(* 	(fix [] [initial_state]) *)
(*     end *)



fun canCompleteStoryList phi (initial_state, rules) =
    let
	fun fix _ [] = raise Fail ""
	  | fix S (state::pending) = 
	    (if Core.sat state phi
	     then raise (RES state)
	     else (let
		       fun isIn x l = List.exists (fn e=> x = e ) l
		       fun isSeen state = not(isIn state S)
		       val pot_actstates = (Core.stepGenAllStates (state,rules))
		       val newPend = List.filter isSeen pot_actstates
		   in
		       fix (state::S) (newPend@pending)
		   end)
		  )
    in
	(fix [] [initial_state] handle RES s => s)
    end

fun canCompleteStorySplaySet phi (initial_state, rules) =
    let
	fun fix _ [] = raise Fail ""
	  | fix S (state::pending) =
	    (if Core.sat state phi
	     then raise (RES state)
	     else (let
		       val S' = Splayset.add(S, state)
		       fun notSeen state = Splayset.peek(S,state) = NONE
		       val pot_actstates = (Core.stepGenAllStates (state,rules))
		       val newPend = List.filter notSeen pot_actstates
		   in
		       fix S' (newPend@pending)
		   end)
		  )
    in
	(fix (Splayset.empty State.compare) [initial_state] handle RES s => s)
    end


fun canCompleteStorySplayMap phi (initial_state, rules) =
    let
	fun fix _ [] = raise Fail ""
	  | fix S (state::pending) =
	    (if Core.sat state phi
	     then raise (RES state)
	     else (let
		       val S' = Splaymap.insert(S, state,())
		       fun isIn x l = List.exists (fn e=> x  = e) l
		       fun notSeen state = Splaymap.peek(S,state) = NONE
		       val pot_actstates = (Core.stepGenAllStates (state,rules))
		       val newPend = List.filter notSeen pot_actstates
		   in
		       fix S' (newPend@pending)
		   end)
		  )
    in
	(fix (Splaymap.mkDict State.compare) [initial_state] handle RES s => s)
    end

fun canCompleteStoryPolyHash phi (initial_state, rules) =
    let
	exception Find
	fun fix _ [] = raise Fail ""
	  | fix S (state::pending) =
	    (if Core.sat state phi
	     then raise (RES state)
	     else (let
		       val _ = Polyhash.insert S (state,())
		       fun isIn x l = List.exists (fn e=> x  = e) l
		       fun notSeen state = (Polyhash.peek S state) = NONE
		       val pot_actstates = (Core.stepGenAllStates (state,rules))
		       val newPend = List.filter notSeen pot_actstates
		   in
		       fix S (newPend@pending)
		   end)
		  )
    in
	(fix (Polyhash.mkTable (Polyhash.hash,State.equal) (40000,Find) ) [initial_state] handle RES s => s)
    end

fun canCompleteStoryMyhashset phi (initial_state, rules) =
    let
	fun fix _ [] = raise Fail ""
	  | fix S (state::pending) =
	    (if Core.sat state phi
	     then raise (RES state)
	     else (let
		       val _ = Myhashset.insert S state
		       fun isIn x l = List.exists (fn e=> x  = e) l
		       fun notSeen state = not(Myhashset.peek S state)
		       val pot_actstates = Core.stepGenAllStates (state,rules)
		       val newPend = List.filter notSeen pot_actstates
		   in
		       fix S (newPend@pending)
		   end)
		  )
    in
	(fix (Myhashset.mkTable (Polyhash.hash,State.equal) 40000 ) [initial_state] handle RES s => s)
    end


fun canCompleteStoryMyhashmap phi (initial_state, rules) =
    let
	fun fix _ [] = raise Fail ""
	  | fix S (state::pending) =
	    (if Core.sat state phi
	     then raise (RES state)
	     else (let
		       val _ = Myhashmap.insert(S,state,())
		       fun isIn x l = List.exists (fn e=> x  = e) l
		       fun notSeen state = (Myhashmap.peek(S, state)) = NONE
		       val pot_actstates = Core.stepGenAllStates (state,rules)
		       val newPend = List.filter notSeen pot_actstates
		   in
		       fix S (newPend@pending)
		   end)
		  )
    in
	(fix (Myhashmap.mkTable (Polyhash.hash,State.equal) 40000 ) [initial_state] handle RES s => s)
    end

datatype node = NODE of node ref list * bool

exception RES of State.state

val startnode = ref (NODE([],false))

fun canCompleteStoryMyhashmap phi (initial_state, rules) =
    let
	fun addEdge (fromNode, toNode) = 
	    let
		val NODE(nodereflist,b) = !fromNode
	    in
		fromNode := NODE(toNode::nodereflist,b)
	    end
	fun isIn x l = List.exists (fn e=> x  = e) l
	fun fix S [] = S
	  | fix S ((state,node)::pending) =
	    (let
(* 		 val (node as NODE(state,nodereflist)) = !noderef *)
	     in
(* 		 if Core.sat state phi *)
(* 		 then raise (RES state) *)
(* 		 else  *)(let
			   val _ = Myhashmap.insert(S,state,node)
			   val pot_actstates = Core.stepGenAllStates (state,rules)
			   fun f [] = []
			     | f (s::states) =
			       (case Myhashmap.peek(S, s) of
				    NONE => 
				    (addEdge(node,ref(NODE ([],false))); (s,node)::f states)
				  | SOME graphNode => 
				    (addEdge(node,graphNode); f states)
				
				)
			   val newPend = f pot_actstates
		       in
			   fix S (newPend@pending)
		       end)
	     end
		 )
    in
	(fix (Myhashmap.mkTable (Polyhash.hash,State.equal) 4000000 ) 
	     [(initial_state,startnode)] )
    end

fun countNode (refnode) =
    let
	val NODE(nodereflist,seen) = !refnode
    in
	if seen then
	    0
	else
	    (refnode := NODE(nodereflist,true);
	foldr (fn(node,a)=>countNode node + a) 1 nodereflist)
    end

local open Core
val trim4last = fn s => Substring.string(Substring.trimr 4(Substring.all s))
in
val phi = 
    case Mosml.argv () of
	[_,_,id,c] => 
	SOME(Pre(EID id,EQ,EV(State.constConst c)))
      | [_,script_name] => 
	SOME(Pre(EID (trim4last script_name),EQ,EV(State.constConst "True")))
      | _ => (print ("Usage:\nFTT <storyfile> [<id> <const>]\n");NONE)
end

val script_name = List.nth(Mosml.argv (), 1)

fun load_script fname = 
    let 
	val (ms, sname, init, locs) = Extended.parse_file fname
    in
	(sname, E2C.transE2C (ms, sname, init, locs))
    end
val (name, s) = load_script script_name;
val initial_state = #1 s
val rules = #2 s

(* val s = ((canCompleteStoryMyhashmap (valOf phi) (initial_state, rules))) handle RES s => s *)
val _ = ((canCompleteStoryMyhashmap (valOf phi) (initial_state, rules)))
(* val s = (genAllStates (initial_state, rules) ; [] )handle RES s => s *)
val _ = print "FTT-analyser\n"
(* val _ = print ("TRUE \n"^(State.ppstate s)^"\n--FUTURE STATES------------\n") *)
val _ = print ("countNode siger: "^(Int.toString (countNode startnode))^"\n")
end