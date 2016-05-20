import FuncionesAP
import DefinicionesAP

verify = let theorem = ( p /\ true === p ) in
         proof theorem
         >>=
         statement 3.35 with (true =: q) using lambda z (z)
         >>=
         statement 3.29 with (p =: p) using lambda z ((p <==> true) <==> z)
         >>=
         statement 3.4 with (p <==> true =: p) using lambda z (z)
         >>=
         statement 3.4 with (p =: p) using lambda z (z)
         >>=
         done theorem
