{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

{-	Universidad Simon Bolivar
	Departamento de Computacion y Tecnologia de la Informacion
	Asignatura: CI3661 - Laboratorio de Lenguajes de Programacion I
	Proyecto 1 - Lenguaje Haskell
	Elaborado por:  Daniela Ortiz 10-10517	
					Maria Lourdes Garcia 10-10264
-}

module FuncionesAP where

import Theorems
import DefinicionesAP


{---------------------- DEFINICION SUSTITUCION ----------------------}

class Sustitucion t1 where
	sust :: Term -> t1 -> Term

instance Sustitucion Sust where
	sust (Var p) (t1, (Var a)) = if p == a then t1 else (Var p)

instance Sustitucion SustDoble where
	sust (Var p) (t1, (t2, (Var a)), (Var b)) 
		| (p == a) = t1
		| (p == b) = t2
		| otherwise = (Var p)

instance Sustitucion SustTriple where
	sust (Var p) (t1, t2, (t3, (Var a)), (Var b), (Var c)) 
		| (p == a) = t1
		| (p == b) = t2
		| (p == c) = t3
		| otherwise = (Var p)

sustitucion :: (Sustitucion t1) => Term -> t1 -> Term
sustitucion (T) _ = T
sustitucion (F) _ = F
sustitucion (Var a) st = sust (Var a) st
sustitucion (Neg t1) st = Neg (sustitucion t1 st)
sustitucion (And t1 t2) st = And (sustitucion t1 st) (sustitucion t2 st)
sustitucion (Or t1 t2) st = Or (sustitucion t1 st) (sustitucion t2 st)
sustitucion (Imp t1 t2) st = Imp (sustitucion t1 st) (sustitucion t2 st)
sustitucion (DobleImp t1 t2) st = DobleImp (sustitucion t1 st) (sustitucion t2 st)
sustitucion (Inequiv t1 t2) st = Inequiv (sustitucion t1 st) (sustitucion t2 st)

{------ FUNCIONES INSTANCIACION, LEIBNIZ, INFERENCIA, DEDUCCION 1 PASO ------}

-- Instanciacion

instantiate :: (Sustitucion t1) => Equation -> t1 -> Equation
instantiate (Equiv t1 t2) st = Equiv (sustitucion t1 st) (sustitucion t2 st)

-- Regla de Leibniz

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equiv t1 t2) e (Var z) = Equiv (sustitucion e (t1 =: (Var z)))(sustitucion e (t2 =: (Var z)))

-- Inferencia

infer :: (Sustitucion t1) => Float -> t1 -> Term -> Term -> Equation
infer n sus (Var z) e = leibniz (instantiate (prop n) sus) e (Var z)

-- Deduccion de un paso

step :: (Sustitucion t1) => Term -> Float -> t1 -> Term -> Term -> Term
step t1 n sus (Var z) e 
	| (t1 == derecho) = izquierdo
	| (t1 == izquierdo) = derecho
	| otherwise = error "Ningun lado de la ecuacion resultante de la inferencia es igual al primer termino (t1)"
	where
	  	(Equiv izquierdo derecho) = infer n sus (Var z) e

{---------------------------- SINTAXIS PARA LA DEMOSTRACION ----------------------------}

-- Funciones dummy

with :: () 
with = ()

using :: ()
using = ()

lambda :: ()
lambda = ()

-- Funciones IO

statement:: (Sustitucion t1, Show t1) => Float -> () -> t1 -> () -> () -> Term -> Term -> Term -> IO Term
statement n _ sus _ _ z e t1 = do
							   		 let v1 = (step t1 n sus z e)
							   		 putStrLn("=== <statement " ++ show n ++ " with " ++ show sus ++ 
							   				  " using lambda " ++ show z ++ " (" ++ show e ++ ")"++ ")>" ++
							   				  "\n"++ showTerm(v1) ++ "\n")
							   		 return(v1)

proof :: Equation -> IO Term
proof (Equiv t1 t2) = do
					  putStrLn("prooving " ++ showTerm(t1) ++ " === " ++ showTerm(t2) ++ "\n") 
					  putStrLn(showTerm t1)
					  return t1


done :: Equation -> Term -> IO ()
done (Equiv t1 t2) t3 = if ( t3 == t2) then putStrLn("proof successful") else putStrLn("proof not successful")					  	
					  

{--------- FUNCIONES SHOW PARA MOSTRAR LOS TERMINOS COMO ES ESPERADO --------}

instance Show Term where show = showTerm

showTerm :: Term -> String

-- Mostrar Constantes True y False
showTerm (T) = "true"
showTerm (F) = "false"

-- Mostrar Variables
showTerm (Var i) = i:[]

-- Mostrar Operador neg (Negacion)
showTerm (Neg (Var i)) = "¬" ++ showTerm(Var i)
showTerm (Neg T) = "¬true"
showTerm (Neg F) = "¬false"
showTerm (Neg t1) = "¬(" ++ showTerm(t1) ++ ")"

-- Operaciones con Negacion
-- \/  FALTAN POSIBLES MEZCLAS CON TRUE Y FALSE
showTerm (Or (Neg (Var i)) (Neg (Var j))) = "¬" ++ showTerm(Var i) ++ " \\/ ¬" ++ showTerm(Var j)
showTerm (Or (Neg (Var i)) (Var j)) = "¬" ++ showTerm(Var i) ++ " \\/ " ++ showTerm(Var j)
showTerm (Or (Var i) (Neg (Var j))) = showTerm(Var i) ++ " \\/ ¬" ++ showTerm(Var j)

showTerm (Or (Neg (Var i)) (Neg t1)) = "¬" ++ showTerm(Var i) ++ " \\/ ¬(" ++ showTerm(t1) ++ ")"
showTerm (Or (Var i) (Neg t1)) = showTerm(Var i) ++ " \\/ ¬(" ++ showTerm(t1) ++ ")"
showTerm (Or (Neg (Var i)) t1) = "¬" ++ showTerm(Var i) ++ " \\/ (" ++ showTerm(t1) ++ ")"

showTerm (Or (Neg t1) (Neg (Var i))) = "¬(" ++ showTerm(t1) ++ ") \\/ ¬" ++ showTerm(Var i)
showTerm (Or  t1 (Neg (Var i))) = "(" ++ showTerm(t1) ++ ") \\/ ¬" ++ showTerm(Var i)
showTerm (Or (Neg t1) (Var i)) = "¬(" ++ showTerm(t1) ++ ") \\/ " ++ showTerm(Var i)

showTerm (Or (Neg t1) (Neg t2)) = "¬(" ++ showTerm(t1) ++ ") \\/ ¬(" ++ showTerm(t2) ++ ")"
showTerm (Or t1 (Neg t2)) = "(" ++ showTerm t1 ++ ") \\/ ¬(" ++ showTerm t2 ++ ")"
showTerm (Or (Neg t1) t2 ) = "¬(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

-- /\  FALTAN POSIBLES MEZCLAS CON TRUE Y FALSE
showTerm (And (Neg (Var i)) (Neg (Var j))) = "¬" ++ showTerm(Var i) ++ " /\\ ¬" ++ showTerm(Var j)
showTerm (And (Neg (Var i)) (Var j)) = "¬" ++ showTerm(Var i) ++ " /\\ " ++ showTerm(Var j)
showTerm (And (Var i) (Neg (Var j))) = showTerm(Var i) ++ " /\\ ¬" ++ showTerm(Var j)

showTerm (And (Neg (Var i)) (Neg t1)) = "¬" ++ showTerm(Var i) ++ " /\\ ¬(" ++ showTerm(t1) ++ ")"
showTerm (And (Var i) (Neg t1)) = showTerm(Var i) ++ " /\\ ¬(" ++ showTerm(t1) ++ ")"
showTerm (And (Neg (Var i)) t1) = "¬" ++ showTerm(Var i) ++ " /\\ (" ++ showTerm(t1) ++ ")"

showTerm (And (Neg t1) (Neg (Var i))) = "¬(" ++ showTerm(t1) ++ ") /\\ ¬" ++ showTerm(Var i)
showTerm (And  t1 (Neg (Var i))) = "(" ++ showTerm(t1) ++ ") /\\ ¬" ++ showTerm(Var i)
showTerm (And (Neg t1) (Var i)) = "¬(" ++ showTerm(t1) ++ ") /\\ " ++ showTerm(Var i)

showTerm (And (Neg t1) (Neg t2)) = "¬(" ++ showTerm t1 ++ ") /\\ ¬(" ++ showTerm t2 ++ ")"
showTerm (And t1 (Neg t2)) = "(" ++ showTerm t1 ++ ") /\\ ¬(" ++ showTerm t2 ++ ")"
showTerm (And (Neg t1) t2 ) = "¬(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"

-- ==>  FALTAN POSIBLES MEZCLAS CON TRUE Y FALSE
showTerm (Imp (Neg (Var i)) (Neg (Var j))) = "¬" ++ showTerm(Var i) ++ " ==> ¬" ++ showTerm(Var j)
showTerm (Imp (Neg (Var i)) (Var j)) = "¬" ++ showTerm(Var i) ++ " ==> " ++ showTerm(Var j)
showTerm (Imp (Var i) (Neg (Var j))) = showTerm(Var i) ++ " ==> ¬" ++ showTerm(Var j)

showTerm (Imp (Neg (Var i)) (Neg t1)) = "¬" ++ showTerm(Var i) ++ " ==> ¬(" ++ showTerm(t1) ++ ")"
showTerm (Imp (Var i) (Neg t1)) = showTerm(Var i) ++ " ==> ¬(" ++ showTerm(t1) ++ ")"
showTerm (Imp (Neg (Var i)) t1) = "¬" ++ showTerm(Var i) ++ " ==> (" ++ showTerm(t1) ++ ")"

showTerm (Imp (Neg t1) (Neg (Var i))) = "¬(" ++ showTerm(t1) ++ ") ==> ¬" ++ showTerm(Var i)
showTerm (Imp  t1 (Neg (Var i))) = "(" ++ showTerm(t1) ++ ") ==> ¬" ++ showTerm(Var i)
showTerm (Imp (Neg t1) (Var i)) = "¬(" ++ showTerm(t1) ++ ") ==> " ++ showTerm(Var i)

showTerm (Imp (Neg t1) (Neg t2)) = "¬(" ++ showTerm t1 ++ ") ==> ¬(" ++ showTerm t2 ++ ")"
showTerm (Imp t1 (Neg t2)) = "(" ++ showTerm t1 ++ ") ==> ¬(" ++ showTerm t2 ++ ")"
showTerm (Imp (Neg t1) t2 ) = "¬(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"

-- <==>  FALTAN POSIBLES MEZCLAS CON TRUE Y FALSE
showTerm (DobleImp (Neg (Var i)) (Neg (Var j))) = "¬" ++ showTerm(Var i) ++ " <==> ¬" ++ showTerm(Var j)
showTerm (DobleImp (Neg (Var i)) (Var j)) = "¬" ++ showTerm(Var i) ++ " <==> " ++ showTerm(Var j)
showTerm (DobleImp (Var i) (Neg (Var j))) = showTerm(Var i) ++ " <==> ¬" ++ showTerm(Var j)

showTerm (DobleImp (Neg (Var i)) (Neg t1)) = "¬" ++ showTerm(Var i) ++ " <==> ¬(" ++ showTerm(t1) ++ ")"
showTerm (DobleImp (Var i) (Neg t1)) = showTerm(Var i) ++ " <==> ¬(" ++ showTerm(t1) ++ ")"
showTerm (DobleImp (Neg (Var i)) t1) = "¬" ++ showTerm(Var i) ++ " <==> (" ++ showTerm(t1) ++ ")"

showTerm (DobleImp (Neg t1) (Neg (Var i))) = "¬(" ++ showTerm(t1) ++ ") <==> ¬" ++ showTerm(Var i)
showTerm (DobleImp  t1 (Neg (Var i))) = "(" ++ showTerm(t1) ++ ") <==> ¬" ++ showTerm(Var i)
showTerm (DobleImp (Neg t1) (Var i)) = "¬(" ++ showTerm(t1) ++ ") <==> " ++ showTerm(Var i)

showTerm (DobleImp (Neg t1) (Neg t2)) = "¬(" ++ showTerm t1 ++ ") <==> ¬(" ++ showTerm t2 ++ ")"
showTerm (DobleImp t1 (Neg t2)) = "(" ++ showTerm t1 ++ ") <==> ¬(" ++ showTerm t2 ++ ")"
showTerm (DobleImp (Neg t1) t2 ) = "¬(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"

-- !<==>  FALTAN POSIBLES MEZCLAS CON TRUE Y FALSE
showTerm (Inequiv (Neg (Var i)) (Neg (Var j))) = "¬" ++ showTerm(Var i) ++ " !<==> ¬" ++ showTerm(Var j)
showTerm (Inequiv (Neg (Var i)) (Var j)) = "¬" ++ showTerm(Var i) ++ " !<==> " ++ showTerm(Var j)
showTerm (Inequiv (Var i) (Neg (Var j))) = showTerm(Var i) ++ " !<==> ¬" ++ showTerm(Var j)

showTerm (Inequiv (Neg (Var i)) (Neg t1)) = "¬" ++ showTerm(Var i) ++ " !<==> ¬(" ++ showTerm(t1) ++ ")"
showTerm (Inequiv (Var i) (Neg t1)) = showTerm(Var i) ++ " !<==> ¬(" ++ showTerm(t1) ++ ")"
showTerm (Inequiv (Neg (Var i)) t1) = "¬" ++ showTerm(Var i) ++ " !<==> (" ++ showTerm(t1) ++ ")"

showTerm (Inequiv (Neg t1) (Neg (Var i))) = "¬(" ++ showTerm(t1) ++ ") !<==> ¬" ++ showTerm(Var i)
showTerm (Inequiv  t1 (Neg (Var i))) = "(" ++ showTerm(t1) ++ ") !<==> ¬" ++ showTerm(Var i)
showTerm (Inequiv (Neg t1) (Var i)) = "¬(" ++ showTerm(t1) ++ ") !<==> " ++ showTerm(Var i)

showTerm (Inequiv (Neg t1) (Neg t2)) = "¬(" ++ showTerm(t1) ++ ") !<==> ¬(" ++ showTerm(t2) ++ ")"
showTerm (Inequiv t1 (Neg t2)) = "(" ++ showTerm(t1) ++ ") !<==> ¬(" ++ showTerm(t2) ++ ")"
showTerm (Inequiv (Neg t1) t2 ) = "¬(" ++ showTerm(t1) ++ ") !<==> (" ++ showTerm(t2) ++ ")"


-- Mostrar Operador \/ (Disyuncion)
showTerm (Or (Var i) (Var j)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Var j)
showTerm (Or (Var i) t1) = showTerm(Var i)  ++ " \\/ (" ++ showTerm(t1) ++ ")"
showTerm (Or t1 (Var i)) = "(" ++ showTerm(t1) ++ ")" ++ " \\/ " ++ showTerm(Var i)
showTerm (Or t1 t2) = "(" ++ showTerm(t1) ++ ") \\/ (" ++ showTerm(t2) ++ ")"

-- Mostrar Operador /\ (Conjuncion)
showTerm (And (Var i) (Var j)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Var j)
showTerm (And (Var i) t1) = showTerm(Var i)  ++ " /\\ (" ++ showTerm(t1) ++ ")"
showTerm (And t1 (Var i)) = "(" ++ showTerm(t1) ++ ")" ++ " /\\ " ++ showTerm(Var i)
showTerm (And t1 t2) = "(" ++ showTerm(t1) ++ ") /\\ (" ++ showTerm(t2) ++ ")"

-- Mostrar Operador ==> (Implicacion)
showTerm (Imp (Var i) (Var j)) = showTerm(Var i) ++ " ==> " ++ showTerm(Var j)
showTerm (Imp (Var i) t1) = showTerm(Var i)  ++ " ==> (" ++ showTerm(t1) ++ ")"
showTerm (Imp t1 (Var i)) = "(" ++ showTerm(t1) ++ ")" ++ " ==> " ++ showTerm(Var i)
showTerm (Imp t1 t2) = "(" ++ showTerm(t1) ++ ") ==> (" ++ showTerm(t2) ++ ")"

-- Mostrar Operador <==> (Dobleimplicacion)
showTerm (DobleImp (Var i) (Var j)) = showTerm(Var i) ++ " <==> " ++ showTerm(Var j)
showTerm (DobleImp (Var i) t1) = showTerm(Var i)  ++ " <==> (" ++ showTerm(t1) ++ ")"
showTerm (DobleImp t1 (Var i)) = "(" ++ showTerm(t1) ++ ")" ++ " <==> " ++ showTerm(Var i)
showTerm (DobleImp t1 t2) = "(" ++ showTerm(t1) ++ ") <==> (" ++ showTerm(t2) ++ ")"

-- Mostrar Operador !<==> (Inequivalecia)
showTerm (Inequiv (Var i) (Var j)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Var j)
showTerm (Inequiv (Var i) t1) = showTerm(Var i)  ++ " !<==> (" ++ showTerm(t1) ++ ")"
showTerm (Inequiv t1 (Var i)) = "(" ++ showTerm(t1) ++ ")" ++ " !<==> " ++ showTerm(Var i)
showTerm (Inequiv t1 t2) = "(" ++ showTerm(t1) ++ ") !<==> (" ++ showTerm(t2) ++ ")"


-- Mostrar Operador === (Equivalencia)
instance Show Equation where
	show (Equiv t1 t2) = showTerm(t1) ++ " === " ++ showTerm (t2)

{--------- FUNCIONES SHOW PARA MOSTRAR LA SUSTITUCION COMO ES ESPERADO --------}

instance Show Sust where  
	show (t1,t2) = "(" ++ showTerm(t1) ++ " =: " ++ showTerm(t2) ++ ")" 

instance Show SustDoble where
	show (t3, (t1,t2), t4) = "(" ++ showTerm(t3) ++ "," ++ showTerm(t1) ++ "=: " ++ 
								   showTerm(t2) ++ ","++ showTerm(t4)++")" 

instance Show SustTriple where 
	show (t3, t4, (t1,t2), t5, t6) = "(" ++ showTerm(t3) ++ "," ++ showTerm(t4) ++ "," ++
											showTerm(t1) ++ "=: " ++ showTerm(t2) ++ "," ++ 
											showTerm(t5) ++ "," ++ showTerm(t6) ++ ")" 
