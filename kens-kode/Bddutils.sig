signature Bddutils =
sig
    val conj            : ('a -> bdd.bdd) -> 'a list -> bdd.bdd
    val disj            : ('a -> bdd.bdd) -> 'a list -> bdd.bdd
    val isConst         : bdd.bdd -> bool
    val foldVarset      : (bdd.varnum * 'a -> 'a) -> 'a -> bdd.varSet -> 'a
    val foldVarsetLower : (bdd.varnum * 'a -> 'a) -> 'a -> bdd.varSet -> 'a
    val reachable : bdd.bdd -> bdd.bdd -> bdd.varSet -> bdd.varSet -> bdd.bdd
end
