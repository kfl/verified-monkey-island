structure DeadAnalysis =
struct
  (*structure Core = E2C.CompactCore*)
  val notvisited = "N/V"
  val bfs = ref false;

  open TextIO
  val output = fn x => (output (stdOut, x); flushOut stdOut )
  fun concatWith s [] = ""
    | concatWith s [e] = e
    | concatWith s (e :: l) = e ^ s ^ concatWith s l
  val ccW = concatWith
  
  fun findNode [] i = (i, [("player.loc", State.constConst notvisited)])
    | findNode ((i', s) :: nodes) i = if i=i' then (i', s) else findNode nodes i
  
  fun dotNode (i, state) = 
    output (
      "\"(" ^ 
      Int.toString i ^ 
      ", " ^ 
      State.ppval (State.lookup state "player.loc") ^
      ")\""
    )
  
  fun dotEdge nodes (i, a, i') = (
    dotNode (findNode nodes i);  
    output "->";
    dotNode (findNode nodes i');
    output ("[label=\""^ Core.ppact a ^"\"];")
  )
  
  fun dotG (nodes, edges) = (
    output "digraph StoryGraph {\n";
    (foldl (fn (edge, ()) => (dotEdge nodes edge; output "\n")) () edges);
    output "}\n"
  )

  fun compareNodes ((i1,s1), (i2,s2)) = State.compare (s1, s2)
  
  exception Found 
    of
       State.state *
       (SharedNodeStateSet.sharedstate * (int * Core.action * int) list)

  (* index to use for the first node in the constructed graph *)
  val nindex = ref 0  
  fun get_index () = let val v = !nindex in (nindex := v + 1; if v mod 1000 = 0
    then (output ("["^Int.toString v^"] "); v) else v) end
  (*fun get_index () = let val v = !nindex in (output ("."^Int.toString v^".");nindex := v + 1; v) end*)

  (* This function builds a state-transition graph given an initial state and
  * the rules of the script
  *)
  fun buildG compl_pred (initial_state, rules) =
  let
    (* edge: i * action * i *)
    (* node: i * state   but I would like to include loc as well *)
    (*
    * newnodes is the list of all nodes already seen in the graph
    * fromindex is the index of the state we are supposed to "come from"
    * the final list is a list of pairs of action and state; we can transition
    * from fromindex to state on action
    *)
    fun get_back_edges newnodes fromindex  [] = ([], [])
      | get_back_edges newnodes fromindex ((act, state) :: pot_actstates) =
          let
            val common = SharedNodeStateSet.peek (newnodes, (0,state))
            val (rec_back, rec_pend) = get_back_edges newnodes fromindex pot_actstates
          in
            case common of
              (* we have a NEW state w.r.t. the ones already seen so we must
              * create a new node index and add an edge from the fromIndex node
              * to the new one
              *)
            NONE =>
              let 
                val newIndex = get_index ()
                val newNode = (newIndex, state)
                val newEdge = (fromindex, act, newIndex)
              in
                (newEdge :: rec_back, newNode :: rec_pend)
              end
            (*| SOME (i, _) =>  (* this was used on splaysets *) *)
            | SOME i =>
              (* we have already seen the state in previous nodes and must
              * therefore add edges from the fromIndex to all the ones which
              * were equal to the "potential new" (corresponding to state)
              * Thus, no new node is created
              *)
              ( (fromindex, act, i) :: rec_back, rec_pend)
          end
    (* (cur_idx, cur_state) is assumed disjoint from already seen nodes *)
    (* this function takes the already contructed graph, (nodes, edges) and a
    * list of nodes that still need to be considered for inclusion in the graph 
    *)
    fun loop (nodes, edges) [] = (nodes, edges)
      | loop (nodes, edges) ((new_idx, cur_state) :: pendingStates) =
      (*
      if Core.sat cur_state compl_pred
      (* the pending state satisfied the "story complete" property, so we need
      * not add any back-edges and skip the examination of the state entirely
      *)
      then 
        (
        output "Node: ";
        dotNode (new_idx, cur_state);
        output " is accepting\n";
        loop (SharedNodeStateSet.add (nodes, (new_idx, cur_state)), edges) pendingStates
        (*raise Found*)
          (*(cur_state,*)
          (*(SharedNodeStateSet.add (nodes, (new_idx, cur_state)),edges))*)
          )
      else
        *)
      let
        val lp= length(pendingStates) + 1
        val _ = if (lp mod 50 = 0) then print (Int.toString lp ^ "\n") else ()
        (*val _ = output (Int.toString (length pendingStates + 1) ^" ")*)
        val newnode = (new_idx, cur_state)
        val newnodes = SharedNodeStateSet.add (nodes, newnode)
        (* from cur_state we can perform the following actions *)
        val actions    = Core.findEnabledActions (cur_state, rules)
        (* pot_actstates: (act * state) list *)
        (* find all the reachable states using the actions; 
        *)
        val pot_actstates = foldl
          (fn (act, nstates) => 
            (act, Core.step compl_pred (cur_state, rules) act) :: nstates)
          [] actions
        (* for each (act, state) in pot_states we must see if state is disjoint
        * from already seen states in nodes and add edges to those it is not
        * disjoint from and only if it is disjoint from ALL nodes we add an edge to
        * this new node (next_index, state) and add it to the pendinglist; this
        * also ensures that anything put in the pendinglist is actually disjoint
        * from anythin in the graph already seen *)
        val (back_edges, newpending) = get_back_edges newnodes new_idx pot_actstates
        val newedges = back_edges @ edges
      in
        (case actions of
             [] => 
             (output "Node: ";
             dotNode (new_idx, cur_state);
             output  " is stuck\n";
              raise Found (cur_state, (newnodes, newedges)))
           | _  => 
               if !bfs
               then loop (newnodes, newedges) (pendingStates @ newpending) 
               else loop (newnodes, newedges) (newpending @ pendingStates))
      end
  in
    (* initialize pendinglist to contain the initial state and first index *)
    loop (SharedNodeStateSet.empty, []) [(get_index (), initial_state)]
  end

  fun makeCompletedPre storyname = 
      Core.Pre (Core.EID storyname, Core.EQ, Core.EV (State.constConst "True"))

  val script_name = List.nth(Mosml.argv (), 1)
  val toprint     = List.nth(Mosml.argv (), 2) = "print"
  val use_bfs     = List.nth(Mosml.argv (), 3) = "bfs"
  fun load_script fname = 
    let val (ms, sname, init, locs) = Extended.parse_file fname
    in
      (sname, E2C.transE2C (ms, sname, init, locs))
    end
  val (name, s) = load_script script_name;
  val _ = output "Loaded script, now making graph..."
  val _ = output (if use_bfs 
        then "using breadth-first search" 
        else "using depth-first search")
  val _ = bfs := use_bfs
  val (nodes, edges) = buildG (makeCompletedPre name) s 
    handle Found (s, gg) => gg
  val _ = 
    if toprint then (
      output "done\nNow making printable version...\n";
      dotG (SharedNodeStateSet.listItems nodes, edges)
      )
    else
      output "done."


end
