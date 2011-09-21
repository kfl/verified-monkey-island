structure Bddutils :> Bddutils =
struct

fun conj f = List.foldl (fn(x, res) => bdd.AND(f x, res)) bdd.TRUE
fun disj f = List.foldl (fn(x, res) => bdd.OR(f x, res)) bdd.FALSE

val eq = bdd.equal

fun isConst b = eq b bdd.TRUE orelse eq b bdd.FALSE

fun foldVarset f init vs =
    let fun loop bdd acc =
            if isConst bdd then acc
            else loop (bdd.high bdd) (f(bdd.var bdd, acc))
    in  loop (bdd.fromSet vs) init end

val varsetToList = foldVarset op:: []

fun foldVarsetLower f init vs = List.foldl f init (varsetToList vs)

fun reachable I T xs xs' =
    let val renamelist = ListPair.zip (varsetToList xs, varsetToList xs')
        val pairset    = bdd.makepairSet renamelist
        open bdd infix OR
        fun loop R =
            let val next = R OR replace (appex T R And xs) pairset
            in  if eq R next then R else loop next end
    in  loop I end
            
end
