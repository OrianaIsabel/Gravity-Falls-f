module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Persona = UnaPersona {
    edad :: Number,
    items :: [String],
    experiencia :: Number
} deriving (Show, Eq)

data Criatura = UnaCriatura {
    peligrosidad :: Number,
    paraDerrotarlo :: Requerimiento
} deriving (Eq, Show)

type Requerimiento = Persona -> Bool

posee :: String -> Requerimiento
posee requerido = (elem requerido).items

siempreDetras :: Criatura
siempreDetras = UnaCriatura 0 (\p -> True)

gnomos :: Number -> Criatura
gnomos cantidad = UnaCriatura (2 ^ cantidad) (posee "soplador de hojas")

fantasma :: Number -> Requerimiento -> Criatura
fantasma categoria requerido = UnaCriatura (20 * categoria) requerido

thom :: Persona
thom = UnaPersona 62 ["microfono", "soplador de hojas"] 15

-- Punto 2

puedeDerrotar :: Criatura -> Persona -> Bool
puedeDerrotar criatura = (paraDerrotarlo criatura)

ganarExperiencia :: Number -> Persona -> Persona
ganarExperiencia n persona = persona {experiencia = (experiencia persona) + n}

enfrentarCriatura :: Criatura -> Persona -> Persona
enfrentarCriatura criatura persona
    | puedeDerrotar criatura persona = ganarExperiencia (peligrosidad criatura) persona
    | otherwise = ganarExperiencia 1 persona

-- Punto 3

enfrentarCriaturas :: [Criatura] -> Persona -> Persona
enfrentarCriaturas criaturas persona = foldr enfrentarCriatura persona criaturas

cuantoPuedeGanar :: [Criatura] -> Persona -> Number
cuantoPuedeGanar criaturas persona = (experiencia (enfrentarCriaturas criaturas persona)) - (experiencia persona)

losMalos :: [Criatura]
losMalos = [siempreDetras, (gnomos 10), (fantasma 3 (\p -> ((edad p) < 13) && (posee "disfraz" p))), (fantasma 1 (\p -> (experiencia p) > 10))]

-- Punto 4

zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf _ _ [] ys = ys
zipWithIf _ _ _ [] = []
zipWithIf f cond (x:xs) (y:ys)
    | cond y = f x y : zipWithIf f cond xs ys
    | otherwise = y : zipWithIf f cond (x:xs) ys

-- Punto 5

letrasInferiores :: Char -> [Char]
letrasInferiores letra = filter (< letra) ['a'..'z']

letrasSuperiores :: Char -> [Char]
letrasSuperiores letra = filter (> letra) ['a'..'z']

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = [letra] ++ (letrasSuperiores letra) ++ (letrasInferiores letra)

retornarPosicion:: Char -> [Char] -> Number
retornarPosicion letra (x:yz) 
    | x == letra = 0
    | otherwise = 1 + retornarPosicion letra yz

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra clave reemplazar = ['a'..'z'] !! (((retornarPosicion reemplazar).abecedarioDesde) clave)

esLetra :: Char -> Bool
esLetra = flip elem ['a'..'z']

cesar :: Char -> String -> String
cesar clave codificado = zipWithIf desencriptarLetra esLetra (repeat clave) codificado

todasLasDesencripciones :: String -> [String]
todasLasDesencripciones codificado = map (flip cesar codificado) ['a'..'z']

-- Punto 6

vigenere :: String -> String -> String
vigenere clave codificado = zipWithIf desencriptarLetra esLetra ((concat.repeat) clave) codificado