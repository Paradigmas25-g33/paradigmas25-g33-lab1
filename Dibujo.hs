module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a 
              | Rotar (Dibujo a)
              | Rotar45 (Dibujo a)  
              | Espejar (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving (Show, Eq)


-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 x = x
comp f n x = f (comp f (n-1) x)


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 d = comp Rotar 2 d 

r270 :: Dibujo a -> Dibujo a
r270 d = comp Rotar 3 d

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) a b = Apilar 1 1 a b

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) a b = Juntar 1 1 a b

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) a b = Encimar a b


-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (a.-.b) /// (c.-.d)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = d ^^^ (Rotar d) ^^^ (r180 d) ^^^ (r270 d)


-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (Rotar d) (r180 d) (r270 d)


-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib x = Basica x

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica x) = Basica (f x)
mapDib f (Rotar d) = Rotar (mapDib f d)
mapDib f (Rotar45 d) = Rotar45 (mapDib f d)
mapDib f (Espejar d) = Espejar (mapDib f d)
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)


-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b
foldDib f _ _ _ _ _ _ (Basica x) = f x
foldDib f r r45 e ap ju en (Rotar d) = r (foldDib f r r45 e ap ju en d)
foldDib f r r45 e ap ju en (Rotar45 d) = r45 (foldDib f r r45 e ap ju en d)
foldDib f r r45 e ap ju en (Espejar d) = e (foldDib f r r45 e ap ju en d)
foldDib f r r45 e ap ju en (Apilar x y d1 d2) = ap x y (foldDib f r r45 e ap ju en d1) (foldDib f r r45 e ap ju en d2)
foldDib f r r45 e ap ju en (Juntar x y d1 d2) = ju x y (foldDib f r r45 e ap ju en d1) (foldDib f r r45 e ap ju en d2)
foldDib f r r45 e ap ju en (Encimar d1 d2) = en (foldDib f r r45 e ap ju en d1) (foldDib f r r45 e ap ju en d2)




