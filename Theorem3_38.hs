import FuncionesAP
import DefinicionesAP

verify = let theorem = ( p /\ p === p ) in
         proof theorem
         >>=
         statement 3.35 with (p =: q) using lambda z (z)
         >>=
         statement 3.3 with (p =: p) using lambda z (z <==> p \/ p)
         >>=
         statement 3.26 with (p =: p) using lambda z ( true <==> z)
         >>=
         statement 3.2 with (true, p =: p, q) using lambda z (z)
         >>=
         statement 3.4 with (p =: p) using lambda z (z)
         >>=
         done theorem
