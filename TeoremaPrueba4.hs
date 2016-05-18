import FuncionesAP
import DefinicionesAP

verify = let theorem = (p <==> q) <==> (q <==> p) === true in
         proof theorem
         >>=
         statement 3.2 with (p =: p) using lambda z ((p <==> q) <==> z)
         >>=
         statement 3.3 with (p <==> q =: p) using lambda z (z)
         >>=
         done theorem