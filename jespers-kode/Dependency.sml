(* fun isIn value list = List.exists (fn x=>value=x) list  *)

(* structure Set =  *)
(* struct  *)
(* datatype 'a set = SET of 'a list *)
(* fun fromList l = SET l *)
(* fun singleton e = SET [e] *)
(* fun isSubset' ([],s') = true *)
(*   | isSubset' (e::s,s') = isIn e s' andalso isSubset' (s,s') *)
(* fun isSubset(SET s,SET s') = isSubset'(s,s') *)
(* fun insert(SET s,e) =  *)

(* fun toSetOfSets (SET s) = SET(map SET s) *)
(* fun map (SET s) = SET(map SET s) *)
(* end *)

structure Set = Splayset

local
fun fromList list = Set.addList(Set.empty String.compare,list)
val (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,R,S,T,U,V,X,Y,Z) = 
    ("A","B","C","D","E","F","G","H","I","J","K","L",
     "M","N","O","P","R","S","T","U","V","X","Y","Z")
val conc = Set.singleton String.compare A
val init = fromList [C,D]
fun convRule (prelist,d,conclist) =
    (fromList prelist,d, fromList conclist)
fun number' _ [] = []
  | number' n ((x,y)::l) = (x,n,y)::(number' (n+1) l)
fun number rules = number' 1 rules
fun conv(conc,init,rules) = (conc,init, map convRule (number rules))
in
val test1 = conv(conc,init,
	     [([C,D],[A,B])])
val test2 = conv(conc,init,
	     [([C,D],[A,B]),
	      ([C],[A,E])])
val test3 = conv(conc,init,
	     [([F,G],[A,B]),
	      ([C],[F]),
	      ([D],[G])])
val test3' = conv(conc,init,
	     [([F],[A,B]),
	      ([C],[F]),
	      ([D],[G])])
val test4 = conv(conc,init,
	     [([F,G],[A,B]),
	      ([C,D],[A,C]),
	      ([C],[F]),
	      ([D],[G])])
val test5 = conv(conc,init,
	     [([B],[A]),
	      ([C],[B]),
	      ([A],[C])])
val test6 = conv(conc,fromList [],
	     [([B],[A]),
	      ([C],[B]),
	      ([A],[C])])
val test7 = conv(Set.singleton String.compare E,fromList [A],
	     [([A],[C,B]),
	      ([C],[D]),
	      ([D,B],[E])])
end


fun toSetOfSets set = Set.foldr (fn (e,acc) => Set)) (Set.empty )

fun findConc(conc,rules) =
    let
	val pred = fn (_,_,ruleConc) =>  Set.isSubset(conc, ruleConc)
    in
	List.filter pred rules
    end

fun findConc2(conc,rules) =
    let
	val concs = toSetOfSets conc
	val pred = fn (_,_,ruleConc) =>  Set.isSubset(conc, ruleConc)
    in
	List.filter pred rules
    end

exception NONER of string * string Set.set

fun findAll(conc,initSet,rules) = 
    let
	val rulesThatEndWitchConc = findConc(conc,rules)
    in
	case rulesThatEndWitchConc of 
	    [] => raise NONER("None reaches conclusion",conc)
	  | _ => 
    end



fun dept(conc,initSet,rules) = 
    let
	val rulesThatEndWitchConc = findConc(conc,rules)
    in
	case rulesThatEndWitchConc of 
	    [] => raise NONER("None reaches conclusion",conc)
	  | [(pre,d,conc)] => 
	    if Set.isSubset(pre, initSet) then
		d
	    else
		dept(pre,initSet,rules)
	  | _ => raise Fail "More than 2 match"
    end