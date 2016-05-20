import FuncionesAP
import DefinicionesAP

verify = let theorem = ( p /\ neg p === false ) in
         proof theorem
         >>=
         statement 3.35 with (neg p =: q) using lambda z (z)
         >>=
         statement 3.2 with (neg p =: q) using lambda z (z <==> p \/ neg p)
         >>=
         statement 3.15 with (p =: p) using lambda z (z <==> p \/ neg p)
         >>=
         statement 3.2 with (false, p \/ neg p =: p, q) using lambda z (z)
         >>=
         statement 3.28 with (p =: p) using lambda z (z <==> false)
         >>=
         statement 3.22 with (true =: p) using lambda z (z)
         >>=
         statement 3.8 with (p =: p) using lambda z (z)
         >>=
         done theorem
