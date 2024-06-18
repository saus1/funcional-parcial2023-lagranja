module Library where
import PdePreludat
import GHC.Windows (BOOL)

doble :: Number -> Number
doble numero = numero + numero

data Animal= Animal {
 nombre::String,
 tipoDeAnimal::String,
 peso::Number,
 edad::Number,
 estaEnfermo::Bool,
 veterinarios::[Veterinario]
} deriving (Show)

data Veterinario =Veterinario {
    diaRecuperacion::Number,
    costo::Number
} deriving (Show)


vaca =Animal "loly" "mamifero" 690 6 False [coco,pepe]


perro =Animal "firulais" "mamifero" 6 6 True [coco]

coco :: Veterinario
coco=Veterinario 31 500
pepe :: Veterinario
pepe=Veterinario 10 800

-- PUNTO 1
laPasoMal::Animal->Bool
laPasoMal  = any ((>30).diaRecuperacion) . veterinarios

nombreFalopa :: Animal -> Bool
nombreFalopa = (=='i').last.nombre



-- PUNTO 2
-- 2.1
engorde :: Number -> Animal -> Animal
engorde kilos animal =animal{peso= peso animal + div (min 5 kilos) 2}

-- 2.2
revisacion::Animal->Animal
revisacion animal | estaEnfermo animal = engorde 2 (registroDeVisitaMedica 2 400 animal)
                  |otherwise           = animal

registroDeVisitaMedica :: Number -> Number -> Animal -> Animal
registroDeVisitaMedica dias dinero animal=animal{veterinarios= Veterinario{ diaRecuperacion=dias,costo=dinero}:veterinarios animal}

-- 2.3
festejoCumple::Animal->Animal
festejoCumple animal= animal{edad=edad animal+1,peso=peso animal -1}

-- 2.4
chequeDePeso::Number->Animal->Animal
chequeDePeso xpeso animal =animal{estaEnfermo=  ((<= xpeso) . peso) animal }
