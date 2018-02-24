type Genero = (String,Float,Float)
type Banda = (String,Float,String)
type Festival = (String,[Banda])


-- genero :: Nobre , seguidores, decontrol
generos = [("Rock",2342,43),("Rap",45,3),("Reggae",42,1)]
bandas = [("Smog",23,"Rock"),("Agua",23,"Reggae")]
festivales = [("Lolla",[("Smog",23,"Rock"),("Agua",23,"Reggae")]),("Cosquin",[("Smog",23,"Rock"),("Agua",23,"Reggae")]) ]

f1 = ( "Lolla",[("Smog",23,"Rock"),("Agua",23,"Reggae")] )

nombre (nom,_,_) = nom
seguidores (_,seg,_) = seg
descontrol (_,_,des) = des

genero (_,_,gen) = gen

bandasF (_,ban) = ban


tieneNombre :: String -> Genero -> Bool
tieneNombre unNombre unGenero = 
	((==unNombre).nombre) unGenero


--nombreDe
generoAPartirDeNombre :: String -> Genero
generoAPartirDeNombre unNombre = 
	find (tieneNombre unNombre ) generos
	--(head.filter (tieneNombre unNombre)) generos

nivelDeDescontrolTotal :: String -> Float
nivelDeDescontrolTotal unNombre = 
	((descontrol .generoAPartirDeNombre) unNombre) + ((seguidores .generoAPartirDeNombre) unNombre)

esPobreton :: Genero -> Bool
esPobreton unGenero = 
	elem (nombre unGenero)(map genero bandas)


generoBanda :: Banda -> Genero
generoBanda unaBanda = 
	generoAPartirDeNombre (genero unaBanda)

seguidoresTotales :: Festival -> Float
seguidoresTotales unFestival = 
	sum (map seguidores (bandasF unFestival) )

esBandaColada :: Banda -> String -> Bool
esBandaColada unaBanda unGenero =
	((== unGenero).genero ) unaBanda
-- unGenero == (genero unaBanda)
