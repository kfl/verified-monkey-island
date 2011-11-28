structure Analysis =
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

  
  (*local *)
    open Gcl
    open Core
    open State
    fun par x = "(" ^ x ^ ")"
    fun pp_ope AND = "&"
      | pp_ope OR  = "v"
      | pp_ope IMP = "=>"
      | pp_ope BIIMP = "<=>"
    fun pp_bexp (BVar v) = v
      | pp_bexp (BBin (e1,ope,e2)) = par (pp_bexp e1 ^ pp_ope ope ^ pp_bexp e2)
      | pp_bexp (NOT e) = "NOT" ^ par (pp_bexp e)
      | pp_bexp TRUE = "T"
      | pp_bexp FALSE = "F"
    fun get_vars_bexp bset (BVar v) = Binaryset.add(bset, v)
      | get_vars_bexp bset (BBin(b1, _, b2)) = 
          get_vars_bexp (get_vars_bexp bset b1) b2
      | get_vars_bexp bset _ = bset
    fun get_vars_cmd bset (CMD(bexp, ups)) =
      foldl (fn ((var,beexp),abset) => 
        get_vars_bexp (Binaryset.add (abset, var)) bexp
      ) (get_vars_bexp bset bexp) ups
    fun get_vars (init, cmds) =
      let 
        val bset = Binaryset.empty String.compare 
      in
        foldl 
        (fn ((var,bexp), acc_bset) => 
          get_vars_bexp (Binaryset.add (acc_bset, var)) bexp
        ) 
        (foldl (fn (cmd, acc_bset) => 
          get_vars_cmd acc_bset cmd
        ) bset cmds) init
      end
    fun pp_update (v, bexp) = v ^ ":=" ^ pp_bexp bexp
    fun pp_cmd (CMD(bexp, updates)) = 
      pp_bexp bexp ^ " ? " ^ String.concatWith ", " (map pp_update updates) 
    fun pp_prg (PRG(init, cmds)) = ""
    fun handle_assign name oo v =
      let
        val bvar = BVar name
        val ope  = if oo = EQ 
                   then fn (x,y) => BBin(x, BIIMP, y) 
                   else fn (x,y) => NOT(BBin(x, BIIMP, y))
        val bval = case v of 
                        U => BVar (name ^"_C" ^ "Undefined")
                      | C "True" => TRUE
                      | C "False" => FALSE
                      | C c => BVar (name ^"_C"^c)
                      | I i => raise Fail "Integer values not supported"
      in
        ope (bvar, bval)
      end
    fun exp2bexp (EID(name)) = BVar(name)
      | exp2bexp e = raise Fail ("Non supported exp type:" ^ ppexp e)
    fun pre2bexp (Pre(EID name, oo, EV v)) = handle_assign name oo v
      | pre2bexp (Pre(EV v, oo, EID name)) = handle_assign name oo v
      | pre2bexp (Pre(e1,EQ,e2)) = BBin(exp2bexp e1, BIIMP, exp2bexp e2)
      | pre2bexp (Pre(e1,NEQ,e2)) = NOT(BBin(exp2bexp e1, BIIMP, exp2bexp e2))
    fun up2up (var, uexp) = 
        let 
          val bvar = var
          val bval = case uexp of
                          EID n    => BVar (n ^ var)
                        | EV U     => BVar ("Undef" ^ var)
                        | EV (C "True") => TRUE
                        | EV (C "False") => FALSE
                        | EV (C c) => BVar (c ^ var)
                        | EV (I _) => raise Fail "Integer values not assignable"
        in (bvar, bval)
        end
    fun val2up (var, U)  = (var, BVar ("Undef" ^ var))
      | val2up (var, (C "True")) = (var, TRUE)
      | val2up (var, (C "False")) = (var, TRUE)
      | val2up (var, (C c)) = (var ^ "_" ^ State.ppval (C c), TRUE)
      | val2up (var, (I _)) = raise Fail "Integers not assignable in init"
    fun rule2cmd (Rule(pres, act, ups, sidee)) = 
      CMD(
        foldl (fn (p, abexp) => BBin(abexp, AND, pre2bexp p)) TRUE pres, 
        foldl (fn (u, assigns) => up2up u :: assigns) [] ups
        )
        (*
  in
    (*
    fun core2gcl (init, story) =
      (*PRG([], map rule2cmd story)*)
      let 
        val (inits, cmds) = (map val2up init, map rule2cmd story)
        val vars = get_vars (inits, cmds)
        val vmap = Polyhash.mkPolyTable (Binaryset.numItems vars * 2, Fail "Polyvar not fond")
        val ((), last) =
          Binaryset.foldl 
            (fn (var, ((), n)) =>
              (print ("Mapping var: " ^ var ^ "\n")
              ;(Polyhash.insert vmap (var,  {var = n, primed = n + 1}),
               n + 2)
              )
            )
            ((), 0)
            vars
        val () = print ("init with " ^ Int.toString last ^ " vars\n")
        val () = bdd.init (last + 100) 100000
        val () = bdd.setVarnum last
        val () = print ("number of vars mapped: " ^ Int.toString (Polyhash.numItems vmap) ^ "\n")
        val () = print ("number of nodes alloc'ed : " ^ 
                        Int.toString (bdd.getVarnum()) ^ "\n")
        val () = app (fn (n,d) => print (n ^ " -> " ^ 
                    Int.toString (#var d) ^ "," ^
                    Int.toString (#primed d) ^ "\n"
                    )) (Polyhash.listItems vmap)
        val () = output "Making bdd rep\n"
        val (initbdd, cmdbdds) = program vmap (PRG(inits, cmds))
      in
        (print "Initial state\n"
        ;print (String.concatWith "\n" (map pp_update (map val2up init)))
        ;print "\nStory\n"
        ;print (String.concatWith "\n" (map pp_cmd (map rule2cmd story)))
        ;print "\nNumber of variables in translation: "
        ;print (Int.toString (Binaryset.numItems (get_vars (inits, cmds))))
        ;print "\n"
        ;print "Init state bdd\n"
        ;bdd.printdot initbdd
        ;print "BDDs for rules"
        ;app bdd.printdot cmdbdds
        ;print "BDD for transition relation\n"
        ;bdd.printdot (Bddutils.disj (fn x => x) cmdbdds)
        )
      end
      *)
  end
  *)

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
(*<<<<<<< .mine*)
      (*let *)
				(*val _ = output ("[" ^ Int.toString (length pendingStates + 1) ^"]")*)
(*=======*)
      if Core.sat cur_state compl_pred
      (* the pending state satisfied the "story complete" property, so we need
      * not add any back-edges and skip the examination of the state entirely
      *)
      then 
        (
        output "Node: ";
        dotNode (new_idx, cur_state);
        output " is accepting\n";
        (*loop (SharedNodeStateSet.add (nodes, (new_idx, cur_state)), edges) pendingStates*)
        raise Found
          (cur_state,
          (SharedNodeStateSet.add (nodes, (new_idx, cur_state)),edges))
          )
      else
      let
        val lp= length(pendingStates) + 1
        val _ = if (lp mod 50 = 0) then print (Int.toString lp ^ "\n") else ()
        (*val _ = output (Int.toString (length pendingStates + 1) ^" ")*)
(*>>>>>>> .r142*)
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
        if !bfs
        then loop (newnodes, newedges) (pendingStates @ newpending) 
        else loop (newnodes, newedges) (newpending @ pendingStates)
      end
  in
    (* initialize pendinglist to contain the initial state and first index *)
    loop (SharedNodeStateSet.empty, []) [(get_index (), initial_state)]
  end

  fun makeCompletedPre storyname = 
      Core.Pre (Core.EID storyname, Core.EQ, Core.EV (State.constConst "True"))
  
  fun mem x xs = List.exists (fn y => x = y) xs
  fun uniq_cons (x, xs) = if mem x xs then xs else x :: xs

  fun exp_cmp (Core.EV s1, Core.EV s2) = State.compareValue(s1, s2)
    | exp_cmp (Core.EID s1, Core.EID s2) = String.compare(s1,s2)
    | exp_cmp (_, _) = LESS

  fun setToString s =
    Binaryset.foldl (fn (item, acc) => 
      Core.ppexp item ^ ", " ^ acc) "" s

  fun pp_map m = 
    Binarymap.app (fn (k,v) => 
      output (k ^ " -> " ^ setToString v ^ "\n")
    ) m

  fun fix eq f x =
    let 
      val fx = f x
    in 
      if eq x fx then x else fix eq f fx
    end

  fun sub_map eq m1 m2  = 
    Binarymap.foldl (fn (key, value, flag) => 
      eq (Binarymap.find(m2, key), value)
      andalso flag
    ) true m1 handle _ => false
  fun eq_maps m1 m2 =
    sub_map Binaryset.equal m1 m2 andalso
    sub_map Binaryset.equal m2 m1
  fun single_map x = Binaryset.singleton exp_cmp x
  fun add_map x ve m = 
    case Binarymap.peek(m, x) of
         NONE => Binarymap.insert(m, x, single_map ve)
       | SOME vv => if Binaryset.member(vv, ve) then m
                    else Binarymap.insert(m, x, Binaryset.add(vv, ve))
  fun add_map_mul x vv m = 
    Binaryset.foldl (fn (ve, m) => 
      add_map x ve m  
    ) m vv
  fun map_find x m = case Binarymap.peek(m, x) of
                          NONE => Binaryset.empty exp_cmp
                        | SOME vv => vv
  fun val_map (init, rules) =
    (* get a list of all updates in all rules incl. init *)
    let
      val init_list =
        (map (fn (x,v) => (x, Core.EV v)) init)
      val up_list =
            foldl 
              (fn (Core.Rule(pres,_,ups,_), acc_map) => 
                foldl 
                  uniq_cons 
                  (foldl (fn (Pre(e1,vop,e2), acc_map) => 
                    case (e1, e2) of 
                         (EID i, _) => uniq_cons ((i, e2), acc_map)
                       | (_, EID i) => uniq_cons ((i, e2), acc_map)
                       | (_  ,   _) => acc_map) 
                   acc_map pres) 
                  ups
              ) init_list rules
      fun step ((x, ve), acc_map) = 
        case ve of 
             Core.EV v => add_map x ve acc_map
           | Core.EID y => add_map_mul x (map_find y acc_map) acc_map
      fun f m = foldl step m up_list
    in
      fix eq_maps f (Binarymap.mkDict String.compare)
    end

  open Gcl
  fun mkName v con = v ^ "_" ^ Core.ppexp con
  fun zeroOther val_map v exp =
    Binaryset.foldl (fn (value, acc) => 
      if value = exp
      then acc
      else (mkName v value, FALSE) :: acc
    ) [] (Binarymap.find (val_map, v))
  fun up2gups val_map (v, exp as (Core.EID y)) = 
        let 
          val y_vals = Binarymap.find (val_map, y)
        in
          Binaryset.foldl (fn (value, acc) => 
            if(Binaryset.member (y_vals, value))
            then (mkName v value, BVar (mkName y value)) :: acc
            else (mkName v value, FALSE) :: acc
          ) [] (Binarymap.find (val_map, v))
        end
    | up2gups val_map (v, exp) = (mkName v exp, TRUE) :: zeroOther val_map v exp
  fun shouldbeeq x y vals =
        Binaryset.foldl (fn (x_val, acc) => 
          BBin(BBin(BVar (mkName x x_val)
                   ,BIIMP
                   ,BVar (mkName y x_val))
          (*BBin(BBin(BVar (mkName x x_val)*)
                   (*,AND*)
                   (*,BVar (mkName y x_val))*)
              ,AND
              ,acc
              )
        ) TRUE vals
  fun shouldbeF x vals = 
        Binaryset.foldl (fn (v,bexp) => BBin(NOT (BVar (mkName x v)),AND, bexp)) TRUE vals
  fun conj ls = foldl (fn (e,a) => BBin(e, AND, a)) TRUE ls
  fun disj ls = foldl (fn (e,a) => BBin(e, OR, a)) FALSE ls
  fun pre2bexp_new val_map (Pre(EID id,EQ, exp as (EV _))) = BVar (mkName id exp)
    | pre2bexp_new val_map (Pre(EID id,NEQ, exp as (EV _))) = NOT(BVar (mkName id exp))
    | pre2bexp_new val_map (Pre(exp as (EV _),EQ,EID id)) = BVar (mkName id exp)
    | pre2bexp_new val_map (Pre(exp as (EV _),NEQ,EID id)) = NOT(BVar (mkName id exp))
    | pre2bexp_new val_map (Pre(EID x, EQ, EID y)) =
        let val x_vals = Binarymap.find(val_map, x)
            val y_vals = Binarymap.find(val_map, y)
            val only_x = Binaryset.difference(x_vals, y_vals)
            val only_y = Binaryset.difference(y_vals, x_vals)
        in
          if Binaryset.isEmpty only_x 
             andalso Binaryset.isEmpty only_y
          then 
            (* generate bexp that all should be = *)
            shouldbeeq x y x_vals
          else 
            (* generate bexp for all in intersection and all outside (in
             * intersection must be false 
             *)
            conj[shouldbeeq x y (Binaryset.intersection (x_vals, y_vals))
                ,shouldbeF x only_x
                ,shouldbeF y only_y
                ]
        end
    | pre2bexp_new val_map (Pre(EID x, NEQ, EID y)) =
        let val x_vals = Binarymap.find(val_map, x)
            val y_vals = Binarymap.find(val_map, y)
            val only_x = Binaryset.difference(x_vals, y_vals)
            val only_y = Binaryset.difference(y_vals, x_vals)
        in
          if Binaryset.isEmpty only_x 
             andalso Binaryset.isEmpty only_y
          then 
            (* generate bexp that all should be diff *)
            NOT(shouldbeeq x y x_vals)
          else 
            (* generate bexp for all in intersection and all outside (in
             * intersection must be false 
             *)
            disj[shouldbeeq x y (Binaryset.intersection (x_vals, y_vals))
                ,NOT(shouldbeF x only_x)
                ,NOT(shouldbeF y only_y)
                ]
        end
    | pre2bexp_new val_map pre = raise Fail ("Pre unsupported: " ^ pppre pre)
    (*| pre2bexp (Pre(e1,NEQ,e2)) = e*)
  fun handle_rule val_map (Rule (pre_list, action, up_list, sideeff)) =
    CMD(conj (map (pre2bexp_new val_map) pre_list)
       ,foldl (fn (up, acc) => up2gups val_map up @ acc) [] up_list
       )
  fun core2gcl_new (init, rules) val_map = 
    let val init_gcl = 
      foldl (fn ((core_var, value), acc_u) => 
        (up2gups val_map (core_var, (Core.EV value))) 
        @ acc_u
      ) 
      [] init
    in
      (init_gcl, map (handle_rule val_map) rules)
    end
      
  val script_name = List.nth(Mosml.argv (), 1)
  (*val toprint     = List.nth(Mosml.argv (), 2) = "print"*)
  (*val use_bfs     = List.nth(Mosml.argv (), 3) = "bfs"*)
  fun load_script fname = 
    let val (ms, sname, init, locs) = Extended.parse_file fname
    in
      (sname, E2C.transE2C (ms, sname, init, locs))
    end
  val (name, (initS, rules)) = load_script script_name;
  val init = (name, State.C "False") :: initS
  val _ = output ("Loaded script ("^name^"), translating to GCL\n")
  val _ = output ("There are " ^ Int.toString (length rules) ^ " rules\n")
  val _ = app (fn x => output (pprule x ^ "\n")) rules
  val m = val_map (init, rules)
  val _ = output "Value overapproximation\n"
  val _ = pp_map m
  (* For each core variable we need to provide a mapping to the set of gcl
  * variables whose values combined represents the value of the core variable.
  * Additionally, for each set of gcl variables we must generate a constraint
  * that maintain that only one of the gcl variables = 1
  *)
  val (init_gcl, cmds) = core2gcl_new (init, rules) m
  val _ = output "\n\nInitial assignments\n"
  val _ = app (fn (var, bexp) => output (var ^ ":=" ^ pp_bexp bexp ^ "\n")) init_gcl
  val _ = output "\n\nList of Gcl commands\n"
  val _ = app (fn cmd => output ((pp_cmd cmd) ^ "\n")) cmds 
  val vars_gcl = get_vars (init_gcl, cmds)
  val _ = output ("Number of Gcl vars : " 
                 ^ Int.toString (Binaryset.numItems vars_gcl)^ "\n")
  val vmap = Polyhash.mkPolyTable (Binaryset.numItems vars_gcl * 2, Fail "Polyvar not fond")
  val ((), last) =
    Binaryset.foldl 
      (fn (var, ((), n)) =>
        (print ("Mapping var: " ^ var ^ " -> " ^ Int.toString n ^ "\n")
        ;(Polyhash.insert vmap (var,  {var = n, primed = n + 1}),
         n + 2)
        )
      )
      ((), 0)
      vars_gcl
  val () = print ("init with " ^ Int.toString last ^ " vars\n")
  val () = bdd.init (last + 100) 100000
  val () = bdd.setVarnum last
  val () = output "Making bdd rep\n"
  val (initbdd, cmdbdds) = program vmap (PRG(init_gcl, cmds))
  val () = print "Init state bdd\n"
  val () = bdd.printdot initbdd
  val () = print "BDDs for rules\n"
  val () = app bdd.printdot cmdbdds
  val () = flushOut stdOut
  val () = print "BDD for transition relation\n"
  val ft = Bddutils.disj (fn x => x) cmdbdds
  val () = output ("Is ->BDD 0? " ^ Bool.toString (bdd.equal ft bdd.FALSE) ^ "\n")
  val () = output ("Is ->BDD 1? " ^ Bool.toString (bdd.equal ft bdd.TRUE) ^ "\n")
  val () = bdd.printdot ft
  (*val () = flushOut stdOut*)
  (*val rbdd = Bddutils.reachable *)
              (*initbdd ft*)
              (*(bdd.makeset (map (fn (vname, vrec) => #var vrec) (Polyhash.listItems vmap)))*)
              (*(bdd.makeset (map (fn (vname, vrec) => #primed vrec) (Polyhash.listItems vmap)))*)
  (*val () = output "BDD representing *reachable* states\n"*)
  (*val () = output ("Is RBDD 0? " ^ Bool.toString (bdd.equal rbdd bdd.FALSE) ^ "\n")*)
  (*val () = output ("Is RBDD 1? " ^ Bool.toString (bdd.equal rbdd bdd.TRUE) ^ "\n")*)
  (*val () = bdd.printdot rbdd*)
end
