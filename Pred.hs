module Pred where

import Dibujo
import Language.Haskell.TH (FamilyResultSig(TyVarSig))

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.


--cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a 
cambiar :: Pred a -> (a -> a) -> Dibujo a -> Dibujo a
cambiar pred f  = mapDib (\x-> if pred x then f x else x) 
-- *Pred> cambiar (even) (+1) (Encimar (Basica 2) (Basica 3))
-- Encimar (Basica 3) (Basica 3)


-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib (\x -> p x) id id id (\_ _ b1 b2 -> b1 || b2) (\_ _ b1 b2 -> b1 || b2) (\b1 b2 -> b1 || b2)     

--anyDib (even) (Encimar (Basica 2) (Basica 3))
--True
--Pred> anyDib (even) (Encimar (Basica 3) (Basica 3))
--False 



-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib (\x -> p x) id id id (\_ _ b1 b2 -> b1 && b2) (\_ _ b1 b2 -> b1 && b2) (\b1 b2 -> b1 && b2)
--Pred> allDib (even) (Encimar (Basica 3) (Basica 2))
--False
--Pred> allDib (even) (Encimar (Basica 8) (Basica 2))
--True

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 dib = 
    case foldDib
        (\_ -> (0, False))  -- Caso base: figura básica
        (\(n, _) -> (n + 1, n + 1 == 4))  -- Rotar: incrementa contador y verifica si llega a 4
        (\(n, _) -> (0, False))  -- Rotar45: reinicia contador
        (\(n, _) -> (0, False))  -- Espejar: reinicia contador
        (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))  -- Apilar: combina resultados
        (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))  -- Juntar: combina resultados
        (\(n1, b1) (n2, b2) -> (0, b1 || b2))  -- Encimar: combina resultados
        dib of
    (_, True) -> True  -- Si se encontró una secuencia de 4 rotaciones
    _ -> False

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 = checkFlip2 False
  where
    checkFlip2 :: Bool -> Dibujo a -> Bool
    checkFlip2 anteriorEspejo dib = case dib of
        Basica _ -> False
        Rotar d -> checkFlip2 False d
        Rotar45 d -> checkFlip2 False d
        Espejar d -> anteriorEspejo || checkFlip2 True d
        Apilar _ _ d1 d2 -> checkFlip2 False d1 || checkFlip2 False d2
        Juntar _ _ d1 d2 -> checkFlip2 False d1 || checkFlip2 False d2
        Encimar d1 d2 -> checkFlip2 False d1 || checkFlip2 False d2


data Superfluo = RotacionSuperflua | FlipSuperfluo deriving (Show)
---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion dib 
    | esRot360 dib = [RotacionSuperflua]
    | otherwise    = []

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip dib
    | esFlip2 dib = [FlipSuperfluo]
    | otherwise   = []

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo dib =
    let rotErrs = errorRotacion dib
        flipErrs = errorFlip dib
        allErrs = rotErrs ++ flipErrs
    in if null allErrs 
       then Right dib 
       else Left allErrs

