module Main where

import Tarefa4_2021li1g025
import Tarefa2_2021li1g025
import MyTests
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import LI12122
--ghc -i="src" -i="tests" src2/Gui

jogoInicial :: Jogo
jogoInicial = (Jogo map1result (Jogador (2,2) Este False))

estadoBase :: Picture -> Estado
estadoBase pics = (jogoInicial, pics)

type Estado = (Jogo, Picture)

window :: Display
window = FullScreen

fr :: Int
fr = 50


-- draw é responsável por passar ao getPictures o mapa a desenhar (para não o desenhar todo se este for demasiado grande)
-- por agora assumimos que não pode carregar uma caixa
-- falta os casos em que o mapa é maior que 18 blocos em comprimento
draw :: Estado -> Picture
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures pics)) -- head pics é o player
    | xmax <= 18 = Pictures ((Translate ((x2*64)+offset) (((-y2*64))+128) (head pics)):(getPictures (tail pics) (offset,offset,128) mapa)) -- offset de altura é manual por agora
    -- | x <= 8 = 
    -- | x >= xmax - 8 = 
    -- | otherwise =
    where xmax = fromIntegral (length (head mapa))
          offset = (-64)*(xmax/2)
          x2 = fromIntegral x
          y2 = fromIntegral y



-- fazer getPictures receber offset igual a metade da distancia para ficar centrado
getPictures :: [Picture] -> (Float,Float,Float) -> Mapa -> [Picture] -- offset xmax, valor com acumulador, offset altura
getPictures _ _ [] = [Blank] -- é preciso o blank??
getPictures pics@[brick, crate] (x,_,y) ([]:mapa) = getPictures pics (x,x,y-64) mapa -- reset x, go down a line
getPictures pics@[brick, crate] (x,x2,y) ((peca:linha):mapa)
    | peca == Bloco = (Translate x2 y brick):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Caixa = (Translate x2 y crate):(getPictures pics (x,x2+64,y) (linha:mapa))
    | otherwise = getPictures pics (x,x2+64,y) (linha:mapa)
     

eventListener :: Event -> Estado -> Estado
eventListener (EventKey (SpecialKey KeyUp) Down _ _) (jogo, pic) = (moveJogador jogo Trepar, pic)
eventListener (EventKey (SpecialKey KeyDown) Down _ _) (jogo, pic) = (moveJogador jogo InterageCaixa, pic)
eventListener (EventKey (SpecialKey KeyLeft) Down _ _) (jogo, pic) = (moveJogador jogo AndarEsquerda, pic)
eventListener (EventKey (SpecialKey KeyRight) Down _ _) (jogo, pic) = (moveJogador jogo AndarDireita, pic)
eventListener _ s = s

afk :: Float -> Estado -> Estado
afk n a = a

main :: IO()
main = do
    Just brick <- loadJuicy "brick.png"
    Just player <- loadJuicy "player.jpg"
    Just crate <- loadJuicy "crate.jpg"
    play window
        (white)
        fr
        (estadoBase (Pictures [(Scale 0.057 0.115 player), (Scale 0.125 0.125 brick), (Scale 0.25 0.25 crate)])) -- 64x64 px
        draw
        eventListener
        afk