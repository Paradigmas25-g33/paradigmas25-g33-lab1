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

-- Hay 4 rotaciones seguidas
esRot360 :: Pred (Dibujo a)
esRot360 dib = 
    let check (n, found) = (n, found || n >= 4)
    in case foldDib
        (\_ -> (0, False))  -- Caso base
        (\(n, found) -> check (n + 1, found))  -- Rotar
        (\_ -> (0, False))  -- Rotar45
        (\_ -> (0, False))  -- Espejar
        (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))  -- Apilar
        (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))  -- Juntar
        (\(n1, b1) (n2, b2) -> (0, b1 || b2))  -- Encimar
        dib of
    (_, True) -> True  
    _ -> False

-- Hay 2 espejados seguidos
esFlip2 :: Pred (Dibujo a)
esFlip2 dib = 
    let check (n, found) = (n, found || n >= 2)
    in case foldDib
        (\_ -> (0, False))  -- Caso base
        (\(n, b) -> (0, b))  -- Rotar
        (\(n, b) -> (0, b))  -- Rotar45
        (\(n, found) -> check (n + 1, found))  -- Espejar: incrementa contador y verifica si llega a 2
        (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))  -- Apilar
        (\_ _ (n1, b1) (n2, b2) -> (0, b1 || b2))  -- Juntar
        (\(n1, b1) (n2, b2) -> (0, b1 || b2))  -- Encimar
        dib of
    (_, True) -> True 
    _ -> False



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

