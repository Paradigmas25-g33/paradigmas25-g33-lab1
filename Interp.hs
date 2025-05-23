-- Sacar del esqueleto final!
module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Resta de dos vectores
sub :: Vector -> Vector -> Vector
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- Interpretaciones de los constructores de Dibujo

-- Interpreta el operador de rotación
interp_rotar :: ImagenFlotante -> ImagenFlotante --funciona
interp_rotar f v1 v2 v3 =
    let rotacion = (f (add v1 v2) v3 (sub (0,0) v2))
    in rotacion

-- Interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante --funciona
interp_espejar f v1 v2 v3 =
    let rotar45 = f (add v1 v2) (sub (0,0) v2) v3
    in rotar45


-- interpreta el operador de rotación 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante --funciona
interp_rotar45 f v1 v2 v3 =
    let espejar = (f (add v1 (mitad (add v2 v3))) (mitad (add v2 v3)) (mitad (sub v3 v2)))
    in espejar

-- interpreta el operador de apilar
interp_apilar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante --funciona
interp_apilar m n f g v1 v2 v3 =
    let m' = fromIntegral m
        n' = fromIntegral n
        apiladoF = f (add v1 ((n' / (m' + n')) V.* v3)) v2 ((m' / (m' + n')) V.* v3)
        apiladoG = g v1 v2 ((n' / (m' + n')) V.* v3)
    in pictures [apiladoF, apiladoG]

-- interpreta el operador de juntar
interp_juntar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante --funciona
interp_juntar m n f g v1 v2 v3 =
    let m' = fromIntegral m
        n' = fromIntegral n
        juntadoF = f v1 ((m' / (m' + n')) V.* v2) v3
        juntadoG = g (add v1 ((m' / (m' + n')) V.* v2)) ((n' / (m' + n')) V.* v2) v3
    in pictures [juntadoF, juntadoG]

-- interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante --funciona
interp_encimar f g v1 v2 v3 =
    let encimadoF = f v1 v2 v3
        encimadoG = g v1 v2 v3
    in pictures [encimadoF, encimadoG]

-- interpreta cualquier expresión del tipo Dibujo a

interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f = foldDib
    f
    interp_rotar
    interp_rotar45
    interp_espejar
    (\m n d1 d2 -> interp_apilar (round m) (round n) d1 d2)
    (\m n d1 d2 -> interp_juntar (round m) (round n) d1 d2)
    interp_encimar