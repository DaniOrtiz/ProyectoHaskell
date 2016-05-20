import FuncionesAP
import DefinicionesAP

verify = let theorem = ( p /\ q === q /\ p ) in
         proof theorem
         >>=
         statement 3.35 with (p =: p) using lambda z (z)
         >>=
         statement 3.2 with (p =: p) using lambda z (z <==> p \/ q)
         >>=
         statement 3.24 with (p =: p) using lambda z ( (q <==> p) <==> z)
         >>=
         statement 3.35 with (q, p =: p, q) using lambda z (z)
         >>=
         done theorem
