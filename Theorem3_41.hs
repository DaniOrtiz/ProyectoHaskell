import FuncionesAP
import DefinicionesAP

verify = let theorem = ( p /\ (q /\ r) === (p /\ q) /\ (p /\ r) ) in
         proof theorem
         >>=
         statement 3.38 with (p =: p) using lambda z (z /\ (q /\ r))
         >>=
         statement 3.37 with (p /\ p =: p) using lambda z (z)
         >>=
         statement 3.37 with (p, q =: q, r) using lambda z (z /\ r)
         >>=
         statement 3.36 with (p =: p) using lambda z ((p /\ z) /\ r)
         >>=
         statement 3.37 with (p =: r) using lambda z (z /\ r)
         >>=
         statement 3.37 with (p /\ q, p =: p, q) using lambda z (z)
         >>=
         done theorem
