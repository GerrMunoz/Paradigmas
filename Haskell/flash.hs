-- flash
import Text.Show.Functions

data Personaje = Personaje{
	vida :: Float,
	nombre :: String,
	tipo :: String,
	poderes :: [Poder]
} deriving (Show)


type Poder = Personaje -> Personaje

sonEnemigos :: Personaje -> Personaje -> Bool
sonEnemigos p1 p2 = 
	(esHeroe p1 && esVillano p2) || (esHeroe p2 && esVillano p1)

esHeroe :: Personaje -> Bool
esHeroe  = 
	("heroe" ==).tipo

esVillano :: Personaje -> Bool
esVillano  = 
	("villano" ==).tipo

esCientifico :: Personaje -> Bool
esCientifico  = 
	("cientifico" ==).tipo

algunoEsCientifico :: Personaje ->Personaje -> Bool
algunoEsCientifico p1 p2 = 
	esCientifico p1 || esCientifico p2

puedenPelear :: Personaje -> Personaje -> Bool
puedenPelear p1 p2 =
	sonEnemigos p1 p2 || algunoEsCientifico p1 p2

estaBienArmado :: [Personaje] -> Personaje -> Bool
estaBienArmado personajes enemigo =
	all (puedenPelear enemigo) personajes 


algunoTieneHR :: [Personaje] -> Bool
algunoTieneHR personajes = 
	any ((=="HR").(take 2)) (map nombre personajes)

puedenGanar :: [Personaje] -> Personaje -> Bool
puedenGanar personajes enemigo =
	estaBienArmado personajes enemigo && algunoTieneHR personajes

viajarEnElTiempo :: Poder
viajarEnElTiempo personaje = 
	personaje { poderes = []}


reducirVida :: Float -> Poder
reducirVida cantidad personaje = 
	personaje { vida = vida personaje - cantidad}

perderAlgunaHabilidad :: Poder
perderAlgunaHabilidad personaje =
	personaje {poderes = tail (poderes personaje)}

tirarRayo:: Poder
tirarRayo personaje = 
	(perderAlgunaHabilidad. reducirVida 50) personaje

dispararArma :: Float -> Poder
dispararArma cantidad personaje =
	reducirVida cantidad personaje

hablarHastaElCansancio :: Poder
hablarHastaElCansancio personaje
	| esVillano personaje = personaje
	| otherwise = olvidarNombre personaje

olvidarNombre :: Poder
olvidarNombre personaje =
	personaje{ nombre = ""} 

hacerCorrer :: Personaje -> Float -> Float
hacerCorrer personaje velocidad=
	vida (reducirVida (10 + 0.05 * velocidad) personaje)

correr :: [Float] -> Poder
correr lista personaje = 
	reducirVida (sum(map (hacerCorrer personaje) lista)) personaje

harrisonWellsTierra2 = Personaje 300 "harrisonWellsTierra2" "cientifico" [correr [1..10],dispararArma 32]
harrisonWellsTierra32 = Personaje 300 "harrisonWellsTierra32" "cientifico" []
deathStrokes = Personaje 800 "deathStrokes" "villano" [dispararArma 32,dispararArma 32]


quedoKO :: Personaje -> Bool
quedoKO personaje =
	((>10).vida) personaje

atacarConTodosLosPoderes :: Personaje -> Personaje -> Personaje
atacarConTodosLosPoderes personaje enemigo =
	(foldl1 (.) (poderes personaje )) enemigo 



--atacarEnGrupo :: [Personaje] -> Personaje -> Personaje


grupoDebilitado :: [Personaje] -> Bool
grupoDebilitado lista =
	((>2).length) (filter quedoKO lista)

noRecuerdaSuNombre :: Personaje -> Bool
noRecuerdaSuNombre personaje = nombre personaje == ""


quedoInservible :: Personaje -> Bool
quedoInservible personaje
	| esCientifico personaje = null (poderes personaje)
	| esVillano personaje = quedoKO personaje
	| esHeroe personaje = noRecuerdaSuNombre personaje
