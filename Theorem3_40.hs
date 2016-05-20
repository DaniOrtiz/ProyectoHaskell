import FuncionesAP
import DefinicionesAP

verify = let theorem = ( p /\ false === false ) in
         proof theorem
         >>=
         statement 3.35 with (false =: q) using lambda z (z)
         >>=
         statement 3.301 with (p =: p) using lambda z ((p <==> false) <==> z)
         >>=
         statement 3.22 with (p =: p) using lambda z (z <==> p)
         >>=
         statement 3.15 with (p =: p) using lambda z (z)
         >>=
         done theorem
