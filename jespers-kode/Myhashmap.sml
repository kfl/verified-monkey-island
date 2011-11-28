structure Myhashmap = 
struct
datatype ('k,'d) hashset = HS of ('k -> int) * ('k * 'k -> bool) * int * ('k * 'd) list array
fun mkTable (hash,eq) size =
    let
	val ar = Array.array(size,[])
    in
	HS (hash,eq,size,ar)
    end

fun isIn k [] = NONE
  | isIn k ((k',d')::l) = if k = k' then SOME d' else isIn k l

fun insert (HS(hash,eq,size,ar),newK,newD) = 
    let 
	val i = (hash newK) mod size
	val l = Array.sub(ar,i)
    in
	Array.update(ar,i,(if isIn newK l <> NONE then [] else [(newK,newD)])@l)
    end

fun peek (HS(hash,eq,size,ar), newK) = 
    let 
	val l = Array.sub(ar,(hash newK) mod size)
    in
	isIn newK l 
    end
end