-- Pdepmon
import Text.Show.Functions

data Digimon = Digimon{
	nombre :: String,
	nivel :: String,
	tamer :: String,
	energia :: Int,
	habilidades :: [Habilidad]
}deriving (Show)

type Habilidad = Digimon -> Digimon

--1

agumon = Digimon "Agumon" "Rookie" "Tai" 80 [llamaBebe,garraAfilada]

--2

llamaBebe :: Habilidad
llamaBebe digimon = 
	modificarEnergia (-20) digimon

modificarEnergia :: Int -> Digimon -> Digimon
modificarEnergia cantidad digimon = 
	digimon { energia = (energia digimon) + cantidad}

sacar3LetrasNombre :: Digimon -> Digimon
sacar3LetrasNombre digimon = 
	digimon {nombre = drop 3 (nombre digimon)}

garraAfilada :: Habilidad
garraAfilada digimon = 
	(sacar3LetrasNombre.modificarEnergia (-40)) digimon

megaGolpe :: Habilidad
megaGolpe digimon = 
	digimon { tamer = ""}

gigaDestructor :: Habilidad
gigaDestructor digimon = 
	(megaGolpe.sacar3LetrasNombre.llamaBebe) digimon

--3

-- Funcion inventada
nombreOriginal :: Digimon -> String
nombreOriginal digimon = 
	nombre digimon
--

comer :: Habilidad
comer digimon = 
	modificarEnergia 50 digimon

dormir :: Habilidad
dormir digimon = 
	(cambiarNombre (nombreOriginal digimon).modificarEnergia (100 - energia digimon).cambiarNivel "Rookie")digimon

cambiarNivel :: String -> Habilidad
cambiarNivel nuevoNivel digimon = 
	digimon {nivel = nuevoNivel}

cambiarNombre :: String -> Habilidad
cambiarNombre nuevoNombre digimon = 
	digimon {nombre = nuevoNombre}

--4

esNivel :: String -> Digimon -> Bool
esNivel unNivel digimon = 
	nivel digimon == unNivel

cumpleCondicion :: String -> Digimon -> Bool
cumpleCondicion unNivel digimon
	| esNivel "Rookie" digimon = energia digimon > 50
	| esNivel "Champion" digimon = energia digimon > 250
	| esNivel "Ultimate" digimon = energia digimon > 500 && tieneTamer digimon

tieneTamer :: Digimon -> Bool
tieneTamer digimon = 
	tamer digimon /= ""

prefijoNombre :: String -> Digimon -> Digimon
prefijoNombre prefijo digimon = 
	digimon {nombre = prefijo ++ nombre digimon}

cambiarHabilidades :: [Habilidad] -> Digimon -> Digimon
cambiarHabilidades lista digimon =
	digimon { habilidades = lista}

digievolucionar :: Habilidad
digievolucionar digimon
	| cumpleCondicion "Rookie" digimon = evolucionar "were" "Champion" [head (habilidades digimon), megaGolpe] digimon
	| cumpleCondicion "Champion" digimon = evolucionar "metal" "Ultimate" [gigaDestructor,last (habilidades digimon)] digimon
	| cumpleCondicion "Ultimate" digimon = evolucionar "war" "Mega" [megaGolpe, megaGolpe] digimon
	| otherwise = digimon

evolucionar :: String -> String -> [Habilidad] -> Digimon -> Digimon
evolucionar unPrefijo unNivel lista digimon = 
	(cambiarHabilidades lista.modificarEnergia (energia digimon).cambiarNivel unNivel.prefijoNombre unPrefijo) digimon


tieneMasDe870DeEnergia :: Digimon -> Bool
tieneMasDe870DeEnergia digimon = 
	energia digimon > 870

losMasPolenta :: [Digimon] -> [Digimon]
losMasPolenta lista =
	filter tieneMasDe870DeEnergia (map digievolucionar lista)
