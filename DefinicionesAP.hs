{-  Universidad Simon Bolivar
    Departamento de Computacion y Tecnologia de la Informacion
    Asignatura: CI3661 - Laboratorio de Lenguajes de Programacion I
    Proyecto 1 - Lenguaje Haskell
    Elaborado por:  Daniela Ortiz        10-10517   
                    Maria Lourdes Garcia 10-10264
    Descripción: Este archivo contiene las definiciones de los tipos de datos,
    operadores, precedencias, variables y constantes necesarias para las 
    funciones de nuestro asistente de pruebas.
-}

module DefinicionesAP where

{----------------------- DEFINICIONES DE TIPOS DE DATOS ----------------------}

-- Definicion de data Term
data Term = T -- True
          | F -- False
          | Var Char 
          | Neg Term
          | And Term Term
          | Or Term Term
          | Imp Term Term
          | DobleImp Term Term
          | Inequiv Term Term
          deriving Eq

-- Definicion de data Equation

data Equation = Equiv Term Term

-- Definicion del tipo Sust
type Sust = (Term, Term)
type SustDoble = (Term, Sust, Term)
type SustTriple = (Term, Term, Sust, Term, Term)

{------------------------ DEFINICION DE LOS OPERADORES -----------------------}
        
neg :: Term -> Term
neg t1 = Neg t1

(\/) :: Term -> Term -> Term 
(\/) t1 t2 = Or t1 t2

(/\) :: Term -> Term -> Term 
(/\) t1 t2 = And t1 t2

(==>) :: Term -> Term -> Term 
(==>) t1 t2 = Imp t1 t2

(<==>) :: Term -> Term -> Term 
(<==>) t1 t2 = DobleImp t1 t2

(!<==>) :: Term -> Term -> Term 
(!<==>) t1 t2 = Inequiv t1 t2

(===) :: Term -> Term -> Equation
(===) t1 t2 = Equiv t1 t2

(=:) :: Term -> Term -> Sust
(=:) t1 t2 = (t1,t2)

{----------------------- PRECEDENCIA DE LOS OPERADORES -----------------------}

infix 0 ===, =:
infixl 1 <==>, !<==> 
infixr 2 ==>
infixl 3 \/, /\


{------------- DEFINICION DE LAS VARIABLES (LETRAS DEL ALFABETO) -------------}

a :: Term
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'

{---------------- DEFINICION DE LAS CONSTANTES (TRUE Y FALSE) ----------------}

true :: Term
true = T

false :: Term 
false = F