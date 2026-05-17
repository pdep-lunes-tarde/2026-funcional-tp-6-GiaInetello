module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconTofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente BaconTofu = 10
precioIngrediente PanIntegral =3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


cuartoDeLibra:: Hamburguesa
cuartoDeLibra =Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

precioFinal:: Hamburguesa -> Number
precioFinal hamburguesa = (sum.map precioIngrediente.ingredientes $ hamburguesa) + precioBase hamburguesa

queAgregar::[Ingrediente] ->Ingrediente
queAgregar ingredientes
    |Carne `elem` ingredientes = Carne
    |PatiVegano `elem` ingredientes = PatiVegano
    |Pollo `elem` ingredientes = Pollo

agregarIngrediente::  Hamburguesa -> Ingrediente -> Hamburguesa
agregarIngrediente hamburguesa ing  = hamburguesa {ingredientes = ing : ingredientes hamburguesa}

agrandar:: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente hamburguesa . queAgregar. ingredientes $ hamburguesa

descuento:: Number -> Hamburguesa -> Hamburguesa
descuento nDesc hamburguesa = hamburguesa {precioBase= (precioBase hamburguesa) - (precioBase hamburguesa)* nDesc/100 }

pdpBurguer:: Hamburguesa 
pdpBurguer = agrandar. agrandar.flip agregarIngrediente Cheddar. flip agregarIngrediente Panceta. descuento 20 $ cuartoDeLibra

--Parte 2

dobleCuarto:: Hamburguesa
dobleCuarto = agrandar. flip agregarIngrediente Cheddar $ cuartoDeLibra

bigPdp :: Hamburguesa
bigPdp = flip agregarIngrediente Curry $ dobleCuarto

delDia:: Hamburguesa -> Hamburguesa
delDia hamburguesa = flip agregarIngrediente Papas. descuento 30 $ hamburguesa

--Parte 3
cambio:: Ingrediente -> Ingrediente
cambio Pollo = PatiVegano
cambio Carne = PatiVegano
cambio Panceta = BaconTofu
cambio Cheddar = QuesoDeAlmendras
cambio nocambia = nocambia

cambioPan :: Ingrediente -> Ingrediente
cambioPan Pan = PanIntegral
cambioPan nocambia = nocambia

hacerVeggie:: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa{ingredientes= map cambio (ingredientes hamburguesa)}


cambiarPanDePati::Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa{ingredientes = map cambioPan (ingredientes hamburguesa)} 

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = hacerVeggie.cambiarPanDePati $ dobleCuarto