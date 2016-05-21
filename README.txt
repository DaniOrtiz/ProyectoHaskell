###############################################################################
#                                                                             #
#   Universidad Simon Bolivar                                                 #
#   Departamento de Computacion y Tecnologia de la Informacion                #
#   Asignatura: CI3661 - Laboratorio de Lenguajes de Programacion I           #
#   Abril-Julio 2016                                                          #
#   Proyecto 1: Lenguaje Haskell                                              #
#   Elaborado por: María Lourdes Garcia Florez 10-10264                       #
#                  Daniela Ortiz               10-10517                       #
#                                                                             #
###############################################################################

------------------------------Instrucciones------------------------------------

Intrucciones para poder realizar los casos de prueba a nuestro proyecto:

1. En el archivo Theorems.hs, colocar en el encabezado:

module Theorems where
import DefinicionesAP

2. En los archivos que contienen las pruebas de cada teorema, colocar en el 
encabezado:

import FuncionesAP
import DefinicionesAP

Nota: Luego de realizar los pasos anteriores, para correr una prueba, debe 
ubicarse en el terminal (luego en la carpeta donde esten todos los archivos) 
Escribir: ghci nombreTeorema.hs
Ejecutar 
Escribir: verify
Ejecutar

donde 'nombreTeorema' es el nombre del archivo que contiene la prueba del 
teorema que se quiere realizar

-------------------------------------------------------------------------------