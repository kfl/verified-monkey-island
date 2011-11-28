structure Myhashset = 
struct
datatype 'k hashset = HS of ('k -> int) * ('k * 'k -> bool) * int * 'k list array
fun mkTable (hash,eq) size =
    let
	val ar = Array.array(size,[])
    in
	HS (hash,eq,size,ar)
    end
fun isIn x l = List.exists (fn e=> x  = e) l
fun insert (HS(hash,eq,size,ar)) newK = 
    let 
	val i = (hash newK) mod size
	val l = Array.sub(ar,i)
    in
	Array.update(ar,i,(if isIn newK l then [] else [newK])@l)
    end
fun peek (HS(hash,eq,size,ar)) newK = 
    let 
	val l = Array.sub(ar,(hash newK) mod size)
    in
	isIn newK l
    end
end
