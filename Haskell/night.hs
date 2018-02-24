nombre (n, _, _, _) = n
levante (_, l, _, _) = l
aguante (_, _, a, _) = a
tragos (_, _, _, t) = t
vasos = snd
bebida = fst
graduacion = snd
find criterio = head . filter criterio

discotequers = [("fer", 500, 10, [("Coca cola", 1), ("Sprite Zero", 1)]),("mati", 1670, 2000, [("Cerveza", 2)]),("german", 40000, 200000, [("Grog XD", 25), ("Cerveza", 1)]),("flor", 5000, 15, [("Grapa", 1)])]

bebidas = [("Coca cola", 0), ("Grog XD", 350), ("Sprite Zero", 0),("Cerveza", 10), ("Grapa", 40)]

{- punto 1a -}

datosDe nom = find ((nom==).nombre) discotequers

{- punto 1b -}

graduacionAlcoholica marca = (graduacion.(find ((marca==).bebida))) bebidas

{- punto 1c -}

alcoholEnSangre nom = (sum.(map (graduacionPorTrago)).tragos.datosDe) nom

graduacionPorTrago (marca,cantidadVasos) = cantidadVasos * (graduacionAlcoholica marca)

{- punto 2a -}

estaBorracho nom = ( (alcoholEnSangre nom >).aguante.datosDe) nom

{- punto 2b -}

nivelLevanteReal nom 	| estaBorracho nom == True = (levante.datosDe) nom - alcoholEnSangre nom
						| otherwise = (levante.datosDe) nom + alcoholEnSangre nom
						
{- punto 3a -}

quienEsMejor funcion nom1 nom2  | (funcion.datosDe) nom1 > (funcion.datosDe) nom2 = nom1
								|  otherwise = nom2
								
{- punto 4 -}

estaOrdenada funcion [] = True
estaOrdenada funcion [x] = True
estaOrdenada funcion (h:t) = (funcion h <= (funcion.head) t) && estaOrdenada funcion t

{- punto 5a -}

estaRoto nom = ((length.bebidasAlcoholicas.tragos.datosDe) nom) >= 2

bebidasAlcoholicas lista = [ trago | trago <- lista, (esAlcoholica.fst) trago ]

esAlcoholica marca = ((0<).graduacion.(find ((marca==).bebida))) bebidas 

{- punto 5b -}

estoEsUnDescontrol personas = all (estaRoto.nombre) personas 

{- punto 6 -}

funcionHeavy w x y z 	| elem w x = filter y x
						| otherwise = map z x
