structure SharedNodeStateSet =
struct
  (* Basic assumptions:
  * - all state-lists come in a fixed order
  *)

  datatype sharedstate = Empty
                       | Leaf of int
                       | Node of 
                            int option * 
                            ((string * Core.value) * sharedstate) list

  fun inList [] _ = NONE
    | inList ((b, nextnode) :: ss) b' =
        if b = b'
        then SOME nextnode
        else inList ss b'

  fun getNode [] _ = NONE
    | getNode ((b, nextnode) :: ss) b' =
        if b = b'
        then SOME (ss, nextnode)
        else case getNode ss b' of
                  NONE => NONE
                | SOME (ss', node) => SOME ((b, nextnode) :: ss', node)

  (* empty ; the empty set of nodes *)
  val empty = Empty

  (* add (ss, s); add the node (i,s) to the sharedstate ss. *)
fun add (Empty, (i, s)) = foldr (fn (b, t) => Node (NONE, [(b, t)])) (Leaf i) s
  | add (Leaf i', (i, b :: state)) = Node (SOME i', [(b, add (Empty, (i,state)))])
  | add (Leaf i', (i, [])) = 
    (*raise Fail ("Adding existing state "^Int.toString i^" to leaf " ^Int.toString i')*)
    Leaf i'
  | add (Node (SOME i', _), (i, [])) = raise Fail
    ("Adding existing state "^Int.toString i^" to node " ^Int.toString i')
  | add (Node (NONE, es), (i, [])) = Node (SOME i, es)
  | add (Node (oi, es), (i, b :: state)) = 
        (case getNode es b of
             NONE => Node (oi, (b, add (Empty, (i, state))) :: es)
           | SOME (ss, n) => Node (oi, (b, add (n, (i, state))) :: ss))

    
  (* peek; look for state s and ignore the node index i' *)
  fun peek ((Leaf i), (_, [])) = SOME i
    | peek ((Leaf i), (_, _))  = NONE
    | peek ((Node (SOME i, [])), (_, [])) = SOME i
    | peek ((Node (NONE, [])), _) = raise Fail "Violated sharedstate!"
    | peek ((Node (_, e :: edges)), (i', (s, v) :: state)) = 
        (case inList (e :: edges) (s,v) of 
              NONE => NONE
            | SOME node => peek (node, (i', state)))
    | peek (_, _) = (print "error peek"; NONE)

  (* listItems ss *)
  fun listItems t =
    let
      fun depthfirst Empty _ = []
        | depthfirst (Leaf i) s = [(i, rev s)]
        | depthfirst (Node (NONE, (b, t') :: edges)) s = 
            (depthfirst t' (b :: s)) @ 
            (depthfirst (Node (NONE, edges)) s)
        | depthfirst (Node (SOME i, (b, t') :: edges)) s = 
            (i, s) ::
            (depthfirst t' (b :: s)) @ 
            (depthfirst (Node (NONE, edges)) s)
        | depthfirst _ _ = []
    in
      depthfirst t State.empty
    end


  val st0 = State.mapsto State.empty ("x", State.constConst "jesper")
  val st1 = State.mapsto st0 ("y", State.constConst "hulla")
  val st2 = State.mapsto st0 ("z", State.constConst "bulla")
  val st3 = State.mapsto st0 ("y", State.constConst "lonnie")
  val t0  = add (Empty, (0,st0))
  val t1  = add (t0, (1, st1))
  val t2  = add (t1, (2, st2))
  val t3  = add (t2, (3, st3))
end
