import FuncionesAP
import DefinicionesAP

verify = let theorem = ( (p /\ q) /\ r === p /\ (q /\ r) ) in
         proof theorem
         >>=
         statement 3.35 with (p =: p) using lambda z (z /\ r)
         >>=
         statement 3.35 with ((p <==> q) <==> p \/ q, r =: p, q) using lambda z (z)
         >>=
         statement 3.24 with ((p <==> q) <==> p \/ q, r =: p, q) using lambda z ( (((p <==> q) <==> p \/ q) <==> r) <==> z)
         >>=
         statement 3.27 with (r, p <==> q, p \/ q =: p, q, r) using lambda z ((((p <==> q) <==> p \/ q) <==> r) <==> z)
         >>=
         statement 3.27 with (r, p, q =: p, q, r) using lambda z ((((p <==> q) <==> p \/ q) <==> r) <==> (z <==> r \/ (p \/ q)))
         >>=
         statement 3.24 with (r, p \/ q =: p, q) using lambda z ( (((p <==> q) <==> p \/ q) <==> r) <==> ((r \/ p <==> r \/ q) <==> z) )
         >>=
         statement 3.25 with (p =: p) using lambda z ( (((p <==> q) <==> p \/ q) <==> r) <==> ((r \/ p <==> r \/ q) <==> z) )
         >>=
         statement 3.24 with (q, r =: p, q) using lambda z ( (((p <==> q) <==> p \/ q) <==> r) <==> ((r \/ p <==> z) <==> p \/ (q \/ r)) )
         >>=
         statement 3.24 with (r =: q) using lambda z ( (((p <==> q) <==> p \/ q) <==> r) <==> ((z <==> q \/ r) <==> p \/ (q \/ r)) )
         >>=
         statement 3.1 with (p <==> q, p \/ q =: p, q) using lambda z ( z <==> ((p \/ r <==> q \/ r) <==> p \/ (q \/ r)) )
         >>=
         statement 3.2 with (p \/ q, r =: p, q) using lambda z ( ((p <==> q) <==> z) <==> ((p \/ r <==> q \/ r) <==> p \/ (q \/ r)) )
         >>=
         statement 3.2 with (p \/ r, q \/ r =: p, q) using lambda z ( ((p <==> q) <==> (r <==> p \/ q)) <==> (z <==> p \/ (q \/ r)) )
         >>=
         statement 3.1 with (p <==> q, r <==> p \/ q, (q \/ r <==> p \/ r) <==> p \/ (q \/ r) =: p, q, r) using lambda z (z)
         >>=
         statement 3.1 with (r, p \/ q, (q \/ r <==> p \/ r) <==> p \/ (q \/ r) =: p, q, r) using lambda z ((p <==> q) <==> z)
         >>=
         statement 3.1 with (q \/ r, p \/ r, p \/ (q \/ r) =: p, q, r) using lambda z ( (p <==> q) <==> (r <==> (p \/ q <==> z ) ))
         >>=
         statement 3.1 with (p \/ q, q \/ r, p \/ r <==> p \/ (q \/ r) =: p, q, r) using lambda z ( (p <==> q) <==> (r <==> z) )
         >>=          
         statement 3.2 with (p \/ q, q \/ r =: p, q) using lambda z ( (p <==> q) <==> (r <==> (z <==> (p \/ r <==> p \/ (q \/ r)))) )
         >>=
         statement 3.1 with (p <==> q, r, (q \/ r <==> p \/ q) <==> (p \/ r <==> p \/ (q \/ r)) =: p, q, r) using lambda z ( z )
         >>=
         statement 3.1 with (q \/ r, p \/ q, p \/ r <==> p \/ (q \/ r) =: p, q, r) using lambda z ( ((p <==> q) <==> r) <==> z )
         >>=
         statement 3.1 with ((p <==> q) <==> r, q \/ r, p \/ q <==> (p \/ r <==> p \/ (q \/ r)) =: p, q, r) using lambda z ( z )
         >>=
         statement 3.1 with (p \/ q, p \/ r, p \/ (q \/ r) =: p, q, r) using lambda z ( (((p <==> q) <==> r) <==> q \/ r) <==> z )
         >>=
         statement 3.1 with (p =: p) using lambda z ( (z <==> q \/ r) <==> ((p \/ q <==> p \/ r) <==> p \/ (q \/ r)) )
         >>=
         statement 3.1 with (q <==> r, q \/ r =: q, r) using lambda z ( z <==> ((p \/ q <==> p \/ r) <==> p \/ (q \/ r)) )
         >>=
         statement 3.27 with (p =: p) using lambda z ( (p <==> ((q <==> r) <==> q \/ r)) <==> ( z <==> p \/ (q \/ r)) )
         >>=
         statement 3.27 with (q <==> r, q \/ r =: q, r) using lambda z ( (p <==> ((q <==> r) <==> q \/ r)) <==> z )
         >>=
         statement 3.35 with ( (q <==> r) <==> q \/ r =: q) using lambda z ( z )
         >>=
         statement 3.35 with ( q, r =: p, q) using lambda z ( p /\ z )
         >>=	
         done theorem 
