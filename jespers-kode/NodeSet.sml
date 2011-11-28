structure NodeSet =
struct

  val default_size = 1000000
  exception NodeSetFind

  type nodeset = (Core.value State.state, int) Polyhash.hash_table

  val hash = Polyhash.hash
  val comp = fn (s1, s2) => State.compareState Core.compareValue (s1, s2) = EQUAL

  fun mkEmpty () = (print "makeEmpty\n"; 
    Polyhash.mkTable (hash, comp) (default_size, NodeSetFind))

  fun insert nodes (k, d) = Polyhash.insert nodes (k, d)

  fun find nodes k = Polyhash.peek nodes k

  fun listItems nodes = Polyhash.listItems

end
