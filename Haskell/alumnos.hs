--Alumm

import Text.Show.Functions

data Alumno = Alumno {
	nombre :: String,
	dedicacion :: Int,
	materiasAprobadas :: [Materia],
	conceptos :: [String]
}deriving (Show)

data Materia = Materia{
	nombreMateria :: String,
	efecto :: Efecto,
	requisito :: Requisito
}deriving (Show)

type Efecto = Alumno -> Alumno
type Requisito = Alumno -> Bool


--1
juan = Alumno "Juan" 100 [] ["integrar"]

--2 

paradigmas = Materia "Paradigmas" (incorporarConceptos ["polimorfismo","orden superior"].incrementarEstudio 100) (aprendioParametrizacion )
so reentregas = Materia "SO" (incrementarEstudio (1000*reentregas).agregarPrefijo "Excelentisim@") (cumpleLimiteDeEntregas reentregas)
recursividad = Materia "Recursividad" (incorporarConceptos conceptosRecursividad) (aproboRecursividad)
desastre2 = Materia "Desastre 2" (desaprenderPolimorfismo) noRequiereNada

incorporarConceptos :: [String] -> Efecto
incorporarConceptos lista alumno =
	alumno {conceptos = lista ++ conceptos alumno}

incrementarEstudio :: Int -> Efecto
incrementarEstudio tiempo alumno =
	alumno { dedicacion = dedicacion alumno + tiempo}

agregarPrefijo :: String -> Efecto
agregarPrefijo prefijo alumno =
	alumno { nombre = prefijo ++ nombre alumno}

conceptosRecursividad = 
	map (\n -> "Recursividad " ++ show n) [1..5]  

desaprenderPolimorfismo :: Efecto
desaprenderPolimorfismo alumno =
	alumno { conceptos = filter (/="polimorfismo")(conceptos alumno)}

agregarMateria :: Materia -> Alumno -> Alumno
agregarMateria materia alumno =
	alumno { materiasAprobadas = materia : materiasAprobadas alumno }

aprobarMateria :: Materia -> Alumno -> Alumno
aprobarMateria materia alumno =
	(agregarMateria materia.efecto materia) alumno

-- (aprobarMateria paradigmas.aprobarMateria desastre2)juan

type Cursada = [Materia]

cursada1 = [paradigmas]


agregazo :: Cursada -> Alumno -> Alumno
agregazo cursada alumno =
	(foldl1 (.) (map agregarMateria cursada)) alumno

aprobar :: Cursada -> Alumno -> Alumno
aprobar cursada alumno =
	(foldl1 (.) (map aprobarMateria cursada)) alumno



type Criterio = Alumno -> Cursada -> Cursada -> Bool

dejaMasConceptos :: Criterio
dejaMasConceptos alumno c1 c2 =
	conceptosTotal c1 alumno > conceptosTotal c2 alumno


conceptosTotal :: Cursada -> Alumno -> Int
conceptosTotal cursada alumno =
	(length.conceptos.aprobar cursada) alumno

tieneMenosHorasDeDedicacion :: Criterio
tieneMenosHorasDeDedicacion alumno c1 c2 =
	(dedicacion.aprobar c1) alumno < (dedicacion.aprobar c2) alumno


cantidadLetrasConceptoReciente :: Alumno -> Int
cantidadLetrasConceptoReciente alumno =
	(length.last.conceptos) alumno

tieneMasLetras :: Criterio
tieneMasLetras alumno c1 c2 =
	(cantidadLetrasConceptoReciente.aprobar c1) alumno >
	(cantidadLetrasConceptoReciente.aprobar c2) alumno

--6
aprendioParametrizacion :: Requisito
aprendioParametrizacion alumno = any ("parametrizacion"==) (conceptos alumno)

aproboRecursividad :: Requisito
aproboRecursividad alumno = elem (nombreMateria recursividad) (map nombreMateria (materiasAprobadas alumno))

noRequiereNada :: Requisito
noRequiereNada alumno = True

cumpleLimiteDeEntregas :: Int -> Requisito
cumpleLimiteDeEntregas cantidad alumno = cantidad < 5
