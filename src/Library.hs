module Library where
import PdePreludat

--1.
--a.
data Pizza = Pizza{
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
} deriving (Show)

--b.
grandeDeMuzza = Pizza {
    ingredientes=["Salsa", "Mozarella", "Orégano"],
    tamanio= 8,
    calorias=350
}

grandeDePalmitos = Pizza {
    ingredientes=["Salsa", "Mozarella", "Orégano", "Palmitos"],
    tamanio= 8,
    calorias=350
}

grandeDeJamonHuevoYMorron= Pizza {
    ingredientes=["Salsa", "Mozarella", "Orégano", "Jamon", "Morron", "Huevo"],
    tamanio= 8,
    calorias=501
}
-- O TAMBIEN: grandeDeMuzza = Pizza ["Salsa", "Mozarella", "Orégano"] 8 350

--2.
nivelDeSatisfaccion pizza
    | elem "Palmitos" $ ingredientes pizza= 0
    | calorias pizza < 500 = calculoSatisfaccion pizza
    | otherwise = calculoSatisfaccion pizza /2
    where calculoSatisfaccion = (* 80).cantidadIngredientes 

cantidadIngredientes = length.ingredientes

--3.
valorDePizza pizza= (*tamanio pizza).(* 120).cantidadIngredientes $ pizza

--4.
--a.
nuevoIngrediente ingrediente = sumarCalorias ingrediente.agregarIngrediente ingrediente
agregarIngrediente ingrediente pizza = pizza {ingredientes = ingrediente : ingredientes pizza}

sumarCalorias ingrediente pizza= pizza{ calorias = (length ingrediente *2 ) + calorias pizza}

--b.
agrandar pizza = pizza{tamanio =min 10 (tamanio pizza + 2)}

--c.combina dos gustos de pizza => los ingredientes se suman (EVITANDO REPETIR) y sumo calorias tal que calorias2 + calorias1/2 
mezcladita pizza1 pizza2 = Pizza {
    ingredientes = filtrarRepetidos $ ingredientes pizza1 ++ ingredientes pizza2,
    tamanio = tamanio pizza2,
    calorias = calorias pizza2 + calorias pizza1/2
}

filtrarRepetidos [] = []
filtrarRepetidos (elemento : elementos)
    |elem elemento elementos= filtrarRepetidos elementos
    |otherwise = elemento : filtrarRepetidos elementos

--5. 
type Pedido = [Pizza]


nivelDeSatisfaccionPedido = sum . map nivelDeSatisfaccion

--6. 
type Pizzeria = Pedido -> Pedido

--a
pizzeriaLosHijosDePato = map $ agregarIngrediente "Palmito" 
--b Entrega las combinaciones de una pizza con la siguiente: la 1ra con la 2da, la 2da con la tercera y asi. Queda 1 pizza menos

pizzeriaElResumen pedido = zipWith mezcladita pedido (tail pedido)

--c 

pizzeriaEspecial saborPredilecto= map (mezcladita saborPredilecto)

anchoasBasica= Pizza{
    ingredientes=["Salsa", "Anchoas"],
    tamanio= 8,
    calorias=270
}

pizzeriaPescadito = pizzeriaEspecial anchoasBasica

--d
--pizzeriaGourmet :: Number -> Pedido -> Pedido
{-pizzeriaGourmet nivelDeExquisitez [] = []
pizzeriaGourmet nivelDeExquisitez (pizza : pizzas)
    | nivelDeSatisfaccion pizza > nivelDeExquisitez = pizza : pizzeriaGourmet nivelDeExquisitez pizzas
    | otherwise = pizzeriaGourmet nivelDeExquisitez pizzas-}

pizzeriaGourmet nivelDeExquisitez = map agrandar . filter ((>nivelDeExquisitez).nivelDeSatisfaccion)

pizzeriaLaJauja = pizzeriaGourmet 399

--7
--a Implementar la función sonDignasDeCalleCorrientes que, dado un pedido y una lista de pizzerías, devuelve aquellas pizzerías que mejoran la satisfacción del pedido.
sonDignasDeCalleCorrientes :: Pedido -> [Pizzeria] -> [Pizzeria]
sonDignasDeCalleCorrientes pedido = filter (esDignaDeCalleCorrientes pedido)

esDignaDeCalleCorrientes :: Pedido -> Pizzeria -> Bool
esDignaDeCalleCorrientes pedido pizzeria= (nivelDeSatisfaccionPedido.pizzeria) pedido > nivelDeSatisfaccionPedido pedido

--b
mejorPizzeria :: Pedido -> [Pizzeria] -> Pizzeria
mejorPizzeria pedido = foldl1 (masSatisfactoria pedido)

masSatisfactoria :: Pedido -> Pizzeria -> Pizzeria -> Pizzeria
masSatisfactoria pedido pizzeria1 pizzeria2
    |valorSatisfacion pizzeria1 > valorSatisfacion pizzeria2 = pizzeria1
    |otherwise = pizzeria2
    where valorSatisfacion pizzeria = nivelDeSatisfaccionPedido.pizzeria $ pedido

--busco la pizzeria que tenga mejor nivelDeSatisfaccionPedido

--8
yoPidoCualquierPizza x y z = any (odd . x . fst) z && all (y . snd) z
{-se trata de una funcion que recibe una funcion x, otra funcion y (lo inferimos ya que se le estan aplicando una composicion)
por otro lado z se trata de una lista de tuplas, a la que se le aplicaran las composiciones odd.x.fst y y.snd
En la primer parte del && so se encuentra que el primer valor de alguna tupla de z cumple odd.x entonces devolvera true,
mientras que la segunda parte devolvera true si el segundo valor de alguna de las tuplas cumple con la funcion y
-}

--9
laPizzeriaPredilecta :: [Pizzeria] -> Pizzeria
laPizzeriaPredilecta = foldl1 (.)