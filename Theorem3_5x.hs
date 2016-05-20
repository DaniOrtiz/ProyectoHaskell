import FuncionesAP
import DefinicionesAP

verify = let theorem = (p <==> q) <==> q === p in
         proof theorem
         >>=
         statement 3.33 with (q =: r) using lambda z (z)
         >>=
         statement 3.3 with (q =: p) using lambda z (p <==> z)
         >>=
         statement 3.4 with (p =: p) using lambda z (z)
         >>=
         done theorem
