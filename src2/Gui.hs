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
jogoInicial = (Jogo hugeMap (Jogador (1,9) Este False))

estadoBase :: Picture -> Estado
estadoBase pics = (jogoInicial, pics, (21,1), 0)

type Estado = (Jogo, Picture, (Int,Int), Int) -- jogo, imagens, coords da porta, numero de movimentos

window :: Display
window = FullScreen

fr :: Int
fr = 50

-- offset é para que o getPictures saiba onde começar a desenhar as imagens (o mais à esquerda possível)
draw :: Estado -> Picture
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door]), coords, mov)
    | (x,y) == coords = Translate (-200) 0 (Scale 0.5 0.5 (Pictures [Text "You Won!!!", Translate 0 (-120) (Text ("Movements: " ++ (show mov)))]))
    | x <= 10 = let map2 = map (\linha -> take 20 linha) mapa in
        Pictures ((Translate ((x2*64)+offset) (((-y2*64))+192) picFinal):(getPictures [brick, crate, door] (offset,offset,192) map2)) -- offset de altura é manual por agora
    
    | x2 >= (xmax - 10) = let wallDistance = (round xmax)-20 -- nomes de variaveis errados?
                              map2 = map (\linha -> drop wallDistance linha) mapa
                              x3 = (x2-(fromIntegral wallDistance)) in -- converte coordenadas do jogador em coordenadas no ecrã
        -- debug Scale 0.1 0.1 (Text (show map2))
        Pictures ((Translate ((x3*64)+offset) (((-y2*64))+192) picFinal):(getPictures [brick, crate, door] (offset,offset,192) map2))
    
    | otherwise = let map2 = (map (\linha -> take 20 linha) (map (\linha -> drop (x-10) linha) mapa)) -- não pode ser sempre só 10 aqui -- usar fold??
                      offset = getOffset map2 in
        Pictures ((Translate 0 (((-y2*64))+192) picFinal):(getPictures [brick, crate, door] (offset,offset,192) map2))

    where xmax = fromIntegral (length (head mapa))
          getOffset :: Mapa -> Float
          getOffset map3 = (-64)*(fromIntegral ((length (head map3)))/2) -- é preciso????
          offset = -640
          x2 = fromIntegral x
          y2 = fromIntegral y
          player = (case dir of Este -> playerRight
                                Oeste -> playerLeft)
          picFinal = (case caixa of False -> player
                                    True -> Pictures [player, Translate 0 64 crate])



-- fazer getPictures receber offset igual a metade da distancia para ficar centrado (offset é o valor inicial onde as imagens começam a ser colocadas)
getPictures :: [Picture] -> (Float,Float,Float) -> Mapa -> [Picture] -- offset xmax, valor com acumulador, offset altura
getPictures _ _ [] = [Blank] -- é preciso o blank??
getPictures pics@[brick, crate, door] (x,_,y) ([]:mapa) = getPictures pics (x,x,y-64) mapa -- reset x, go down a line
getPictures pics@[brick, crate, door] (x,x2,y) ((peca:linha):mapa)
    | peca == Bloco = (Translate x2 y brick):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Caixa = (Translate x2 y crate):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Porta = (Translate x2 y door):(getPictures pics (x,x2+64,y) (linha:mapa))
    | otherwise = getPictures pics (x,x2+64,y) (linha:mapa)
     

eventListener :: Event -> Estado -> Estado
eventListener (EventKey (SpecialKey KeyUp) Down _ _) (jogo, pic, coords, mov) = (moveJogador jogo Trepar, pic, coords, mov+1)
eventListener (EventKey (SpecialKey KeyDown) Down _ _) (jogo, pic, coords, mov) = (moveJogador jogo InterageCaixa, pic, coords, mov+1)
eventListener (EventKey (SpecialKey KeyLeft) Down _ _) (jogo, pic, coords, mov) = (moveJogador jogo AndarEsquerda, pic, coords, mov+1)
eventListener (EventKey (SpecialKey KeyRight) Down _ _) (jogo, pic, coords, mov) = (moveJogador jogo AndarDireita, pic, coords, mov+1)
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, coords, mov) = estadoBase pic
eventListener _ s = s

afk :: Float -> Estado -> Estado
afk n a = a

main :: IO()
main = do
    Just brick <- loadJuicy "brick.png"
    Just playerLeft <- loadJuicy "playerLeft.png"
    Just playerRight <- loadJuicy "playerRight.jpg"
    Just crate <- loadJuicy "crate.jpg"
    Just door <- loadJuicy "door.png"
    play window
        (white)
        fr
        (estadoBase (Pictures [(Scale 0.057 0.115 playerLeft),(Scale 0.057 0.115 playerRight), (Scale 0.125 0.125 brick), (Scale 0.25 0.25 crate), (Scale 0.119 0.06 door)])) -- 64x64 px
        draw
        eventListener
        afk