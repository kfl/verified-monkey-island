structure State =
struct
  type name      = string
  type const     = string
  datatype value = I of int | C of const | U
  type state = (name * value) list

  val compareName = String.compare
  val compareConst = String.compare
  fun compareValue (I i1,I i2) = Int.compare(i1,i2)
    | compareValue (C c1,C c2) = compareConst(c1,c2)
    | compareValue (U,U) = EQUAL
    | compareValue (U,_) = LESS
    | compareValue (_,U) = GREATER
    | compareValue _ = raise Fail "compareValue: argh"



  fun concatWith s [] = ""
    | concatWith s [e] = e
    | concatWith s (e :: l) = e ^ s ^ concatWith s l

  val ppname = fn s => s
  fun ppnum i = Int.toString i
  fun ppconst c = c
  fun ppval (I i) = ppnum i
    | ppval (C c) = "C" ^ ppconst c
    | ppval U = "Undefined"
  fun ppbinding (name, v) = (ppname name) ^ " --> " ^ ppval v
  fun ppstate s = "T = { " ^ concatWith ", " (map ppbinding s) ^ " }\n"


    
  val undefined = U
  fun isUndefined U = true
    | isUndefined _ = false
  fun constInt i = I i
  fun constConst str = C str
  fun isConst (C _) = true
    | isConst _ = false
		
  (* conversion of gscript int-value to ml-int *)
  fun valToInt (I n) = n
    | valToInt _ = raise Fail "valtoInt: this should not happen"

  exception Value of value
  fun valToConst (C s) = s
    | valToConst v = 
      if isConst v 
      then 
	  raise Fail "isConst: this should not happen1"
      else
	  raise Fail ("isConst: this should not happen2 "^(ppval v))


  val empty = []

  fun lookup [] x = undefined
    | lookup ((y, v) :: st) x = if compareName(x, y) = EQUAL then v else lookup st x

  fun mapsto [] (x, v) = 
      (if isUndefined v then [] else [(x,v)])
    | mapsto ((y, v') :: state) (x, v) = 
      (case compareName(x,y) of
	   LESS => (x,v)::(y,v')::state
         | EQUAL => (if isUndefined v then state else (x,v)::state)
         | GREATER => (y,v')::(mapsto state (x,v)))

  fun makeState [] = empty
    | makeState ((x,v)::l) = mapsto (makeState l) (x,v)

  fun equal (x,y) = x = y

  fun compare ((s,v)::l,(s',v')::l') = 
    (case compareName(s,s') of
          LESS => GREATER
        | GREATER => LESS
        | EQUAL => (case compareValue(v,v') of
                         EQUAL => compare (l,l')
                       | a => a ))
    | compare ([],[]) = EQUAL
    | compare (_,[]) = GREATER
    | compare ([],_) = LESS

end









(* functor fState (N : Name) = *)
(* struct *)
(*   type name      = N.name  *)
(*   type const     = string *)
(*   datatype value = I of int | C of const | U *)
(*   type state = (name * value) list *)

(*   val compareName = N.compare *)
(*   val compareConst = String.compare *)
(*   fun compareValue (I i1,I i2) = Int.compare(i1,i2) *)
(*     | compareValue (C c1,C c2) = compareConst(c1,c2) *)
(*     | compareValue (U,_) = LESS *)
(*     | compareValue (_,U) = GREATER *)
(*     | compareValue (U,U) = EQUAL *)
(*     | compareValue _ = raise Fail "compareValue: argh" *)

(*   fun ppconst s = s *)
(*   fun ppname s = N.pp s *)
(*   fun ppnum i = Int.toString i *)
(*   fun ppval (I i) = ppnum i *)
(*     | ppval (C c) = ppconst c *)
(*     | ppval U = "Undefined" *)
(*   fun concatWith s [] = "" *)
(*     | concatWith s [e] = e *)
(*     | concatWith s (e :: l) = e ^ s ^ concatWith s l *)
(*   fun ppbinding (name, v) = (ppname name) ^ " --> " ^ ppval v *)
(*   fun ppstate s = "T = { " ^ concatWith ", " (map ppbinding s) ^ " }\n" *)

(*   val undefinedValue = U *)

(*   fun constInt i = I i *)
(*   fun constConst str = C str *)
(*   fun isConst (C s) = true *)
(*     | isConst _ = false *)

(*   (\* conversion of gscript int-value to ml-int *\) *)
(*   fun valToInt (I n) = n *)
(*     | valToInt _ = raise Fail "valtoInt: this should not happen" *)
(*   fun valToConst (C s) = s *)
(*     | valToConst _ = raise Fail "valtoConst(str): this should not happen" *)

(*   (\*state*\) *)
(*   val empty = [] *)

(*   fun lookup [] x = U *)
(*     | lookup ((y, v) :: st) x = if compareName(x, y) = EQUAL then v else lookup st x *)

(*   fun mapsto [] (x, v) =  *)
(*       (case v of U => [] | _ => [(x,v)]) *)
(*     | mapsto ((y, v') :: state) (x, v) =  *)
(*       (case compareName(x,y) of *)
(* 	   LESS => (x,v)::(y,v')::state *)
(*          | EQUAL => (case v of U => state | _ => (x,v)::state) *)
(*          | GREATER => (y,v')::(mapsto state (x,v))) *)

(*   fun makeState [] = empty *)
(*     | makeState ((x,v)::l) = mapsto (makeState l) (x,v) *)

(*   fun equal (x,y) = x = y *)

(*   fun compare ((s,v)::l,(s',v')::l') =  *)
(*     (case compareName(s,s') of *)
(*           LESS => GREATER *)
(*         | GREATER => LESS *)
(*         | EQUAL => (case compareValue(v,v') of *)
(*                          EQUAL => compare (l,l') *)
(*                        | a => a )) *)
(*     | compare ([],[]) = EQUAL *)
(*     | compare (_,[]) = GREATER *)
(*     | compare ([],_) = LESS *)

(* end *)



(* structure ValueInt :> Value =  *)
(* struct *)
(* type const = int *)
(* datatype value = I of int | C of const | U *)
(* val compareConst = Int.compare *)
(* fun compare (I i1,I i2) = Int.compare(i1,i2) *)
(*   | compare (C c1,C c2) = compareConst(c1,c2) *)
(*   | compare (U,_) = LESS *)
(*   | compare (_,U) = GREATER *)
(*   | compare (U,U) = EQUAL *)
(*   | compare _ = raise Fail "compare: argh" *)

(* fun ppnum i = Int.toString i *)
(* fun ppconst c =  Int.toString c *)
(* fun pp (I i) = ppnum i *)
(*   | pp (C c) = ppconst c *)
(*   | pp U = "Undefined" *)


(* val undefined = U *)
(* fun isUndefined U = true *)
(*   | isUndefined _ = false *)
(* fun constInt i = I i *)
(* fun constConst str = raise Fail "not implemented" *)
(* fun isConst (C s) = true *)
(*   | isConst _ = false *)
		
(* (\* conversion of gscript int-value to ml-int *\) *)
(* fun valToInt (I n) = n *)
(*   | valToInt _ = raise Fail "valtoInt: this should not happen" *)
(* fun valToConst (C s) = s *)
(*   | valToConst _ = raise Fail "valtoConst(str): this should not happen" *)
(* end *)


(* structure NameInt :> Name=  *)
(* struct *)
(* type name = int *)
(* val compare = Int.compare *)
(* val pp = Int.toString *)
(* val constName = fn s => raise Fail "Not implemented" *)
(* end *)
