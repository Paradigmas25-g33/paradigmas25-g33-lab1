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
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar f v1 v2 v3 =
    let rotacion = rotate 90 (f v1 v2 v3)
    in rotacion

-- Interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar f v1 v2 v3 =
    let espejo = f (add v1 v2) (sub (sub v2 v2) v2) v3
    in espejo

-- interpreta el operador de rotación 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 f v1 v2 v3 =
    let rotacion = (f (add v1 (mitad (add v2 v3))) (mitad (add v2 v3)) (mitad (sub v3 v2)))
    in rotacion

-- interpreta el operador de apilar
interp_apilar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m f g v1 v2 v3 =
    let apiladoF = translate 0 (fromIntegral n) (f v1 v2 v3)
        apiladoG = translate 0 (fromIntegral m) (g v1 v2 v3)
    in pictures [apiladoF, apiladoG]

-- interpreta el operador de juntar
interp_juntar :: Int -> Int -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m f g v1 v2 v3 =
    let juntadoF = translate (fromIntegral n) 0 (f v1 v2 v3)
        juntadoG = translate (fromIntegral m) 0 (g v1 v2 v3)
    in pictures [juntadoF, juntadoG]

-- interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar f g v1 v2 v3 =
    let encimadoF = f v1 v2 v3
        encimadoG = g v1 v2 v3
    in pictures [encimadoF, encimadoG]

-- interpreta cualquier expresión del tipo Dibujo a

interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f = foldDib 
    f 
    interp_rotar 
    interp_espejar 
    interp_rotar45 
    (\n m d1 d2 -> interp_apilar (round n) (round m) d1 d2) 
    (\n m d1 d2 -> interp_juntar (round n) (round m) d1 d2) 
    interp_encimar
