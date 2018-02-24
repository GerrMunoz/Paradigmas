vigilantes = [("Jack Bauer",["asdasd","asdasdsadsda"],1460),("asd",["asdasd","asdasdsadsda"],142),("asd",["asdasd","asdasdsadsda"],1468),("asd",["asdasd","asdasdsadsda"],1444)]
agentesDelGobierno = [("Jack Bauer","24"), ("El Comediante", "Watchmen"), ("Dr. Manhattan", "Watchmen"), ("Liam Neeson", "Taken")]

destruccion vigilantes = filter (noseanrnim) vigilantes
noseanrnim (nombre, _, _) = nombre /= "Rorschach" && nombre /= "Manhattan"

muerte nombre vigilantes = filter (norepite nombre) vigilantes
norepite nombre (nombrev, _, _) = nombre /= nombrev

guerra vigilantes = map (esDelGobierno) vigilantes
esDelGobierno (nombre, habilidades, fecha) | any (esAgente nombre) agentesDelGobierno = (nombre,("Cinismo":habilidades),fecha)
										   | otherwise = (nombre, habilidades, fecha)
esAgente nombre (nombreg,agrupacion) = nombre == nombreg 

accidente a vigilantes = ("Manhattan", ["manipulacion de la materia a nivel molecular"], a):vigilantes


elemBy criterio elemento lista = elem (criterio elemento) (map (criterio) lista)
actadeKeene vigilantes = filter (noTieneSucesorEn vigilantes) vigilantes
noTieneSucesorEn vigilantes unVigilante = all(noEsSucesorDeUnVigilante unVigilante) vigilantes
noEsSucesorDeUnVigilante (nombre1,_,ano1) (nombre2, _,ano2) = (nombre1 /= nombre2) || (ano1 >= ano2)

esta nombre (nombrev,_,_) = nombre == nombrev

historia x = (destruccion.(muerte "El comediante" ).guerra.(accidente 1959).actadeKeene) x
nombre (n,_,_) = n
cantHab (_,hab,_) = length hab
maximumBy criterio [x] = x
maximumBy criterio (vigilante:vigilantes) | (criterio vigilante) > criterio (head vigilantes) = maximumBy criterio (vigilante:(tail vigilantes))
										  | otherwise = maximumBy criterio vigilantes
nombreDelSalvador = nombre.(maximumBy cantHab).destruccion

nombreLargo (n,_,_) = length n
primerHabilidad (_,hab,_) = head hab
elElegido = primerHabilidad.(maximumBy nombreLargo).guerra 

edad (_,_,e) = e
patriarca x = (edad.(maximumBy edad).actadeKeene) x
