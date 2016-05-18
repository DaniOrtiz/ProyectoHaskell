import FuncionesAP
import DefinicionesAP

verify = let theorem = (p <==> p) <==> true === true in
         proof theorem
         >>=
         statement 3.3 with (p =: p) using lambda z (z <==> true)
         >>=
         statement 3.3 with (true =: p) using lambda z (z)
         >>=
         done theorem