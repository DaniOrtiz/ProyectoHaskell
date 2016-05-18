import FuncionesAP
import DefinicionesAP

verify = let theorem = ( (p !<==> q) <==> r === p !<==> (q <==> r) ) in
         proof theorem
         >>=
         statement 3.14 with (p =: p) using lambda b (b <==> r)
         >>=
         statement 3.1 with (neg p =: p) using lambda a (a)
         >>=
         statement 3.14 with (q <==> r =:q) using lambda a (a)
         >>=
         done theorem
