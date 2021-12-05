module Main where

import Tarefa4_2021li1g025
import Tarefa2_2021li1g025
import Maps
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import LI12122
--ghc -i="src" -i="src2" src2/Gui

jogoInicial :: Int -> (Jogo,(Int,Int))
jogoInicial n = maps !! (n-1) -- começa no 1, não no 0

estadoBase :: Picture -> GameMode -> Estado
estadoBase pics (Play n) = (jogo, pics, coords, 0, Play n, 0)
                         where (jogo,coords) = jogoInicial n
estadoBase pics gm = (Jogo [] (Jogador (0,0) Este False), pics, (0,0), 0, gm, 0)

numeroMapas :: Int
numeroMapas = (length maps)

data MenuStates = New | Load | MapEditS deriving (Eq,Show)

data Mov = U | D | L | R | None deriving (Eq,Show) -- up down left rigth

type MapEditInfo = ((Int,Int), (Int,Int), Peca, (Mov,Mov)) -- peca | jogador?-- posicao absoluta do mapa, posicao absoluta da peca, peca selecionada, informações para ajudar no movimento contínuo

data GameMode = Menu MenuStates | Play Int | MapEdit MapEditInfo | Won deriving (Eq,Show)

type Estado = (Jogo, Picture, (Int,Int), Int, GameMode, Float) -- jogo atual, imagens, coords da porta, numero de movimentos, modo de jogo, tempo em segundos (tempo total)

window :: Display
window = FullScreen

fr :: Int
fr = 50

-- offset é para que o getPictures saiba onde começar a desenhar as imagens (o mais à esquerda possível)
-- offsetY ainda não funciona corretamente (em mapas com mais de 15 blocos de altura)
-- Menu
draw :: Estado -> Picture
draw ((Jogo mapaOriginal (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door]), coords, mov, Menu state, _) =
    Pictures ([Translate 0 200 (Text "New"), Translate 0 100 (Text "Load"), Translate 0 0 (Text "Map Editor")] ++ [(case state of New -> Translate (-50) 200 quadrado
                                                                                                                                  Load -> Translate (-50) 100 quadrado
                                                                                                                                  MapEditS -> Translate (-50) 0 quadrado)])
    where quadrado = Polygon [(0,0),(50,0),(50,50),(0,50)]
-- Jogo
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics, coords, mov, Won, sec) =
    Translate (-200) 0 (Scale 0.5 0.5 (Pictures [Text "You Won!!!", Translate 0 (-120) (Text ("Movements: " ++ (show mov))), Translate 0 (-250) (Text ("In " ++ show (round sec) ++ " seconds"))])) -- jogador chegou à porta final
    
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door]), coords, mov, Play n, _)
    | xmax <= 20 = let offset = getOffset mapa
                       map3 = getLines y mapa
                       offsetY = getOffsetY map3 in -- scrolling desnecessário para mapas pequenos, temos de centrar o mapa consoante xmax
        Pictures ((Translate ((xf*64)+offset) ((-y2*64)+offsetY) picFinal):(getPictures [brick, crate, door] (offset,offset,offsetY) map3))

    | x <= 10 = let map2 = map (\linha -> take 20 linha) mapa
                    map3 = getLines y map2
                    offsetY = getOffsetY map3 in
        Pictures ((Translate ((xf*64)+offset) (((-y2*64))+offsetY) picFinal):(getPictures [brick, crate, door] (offset,offset,offsetY) map3)) -- offset de altura é manual por agora
    
    | xf >= (xmax - 10) = let wallDistance = (round xmax)-20
                              map2 = map (\linha -> drop wallDistance linha) mapa
                              x2 = (xf-(fromIntegral wallDistance)) -- converte coordenadas do jogador em coordenadas no ecrã
                              map3 = getLines y map2
                              offsetY = getOffsetY map3 in 
        -- debug Scale 0.1 0.1 (Text (show map2))
        Pictures ((Translate ((x2*64)+offset) (((-y2*64))+offsetY) picFinal):(getPictures [brick, crate, door] (offset,offset,offsetY) map3))
    
    | otherwise = let map2 = getLines y mapa
                      map3 = (map (\linha -> take 20 linha) (map (\linha -> drop (x-10) linha) map2))
                      offset = getOffset map3
                      offsetY = getOffsetY map3 in
        Pictures ((Translate 0 (((-y2*64))+offsetY) picFinal):(getPictures [brick, crate, door] (offset,offset,offsetY) map3))

    where xmax = fromIntegral (length (head mapa))
          ymax = length mapa 
          getOffsetY :: Mapa -> Float
          getOffsetY map3 = (64)*((ymax2/2))
                          where ymax2 = fromIntegral (length map3)
          getOffset :: Mapa -> Float -- assume se que o mapa já só tem 20 blocos de comprimento
          getOffset map3 = (-64)*((fromIntegral (length (head map3)))/2)
          offset = -640
          xf = fromIntegral x
          yf = fromIntegral y
          y2 = fromIntegral (y-(15*(div y 15)))
          player = (case dir of Este -> playerRight
                                Oeste -> playerLeft)
          picFinal = (case caixa of False -> player
                                    True -> Pictures [player, Translate 0 64 crate])
          getLines :: Int -> Mapa -> Mapa
          getLines n l = drop (newY-15) (take newY l)
                       where newY = ymax - (15*(div (ymax-y) 15))

-- MapEditor
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door]), coords, mov, MapEdit ((x1,y1), (x2,y2), peca, _), _) =
    --Translate (-500) 200 (Scale 0.1 0.1 (Text (show (Jogo mapa (Jogador (x,y) dir caixa)))))
    Pictures ((Translate (64*x2f) (y2f*64) pecaPic):(getPictures [brick, crate, door] (64*x1f,64*x1f,((-64)*y1f)) map2))
    where map2 = (map (\linha -> take 20 linha) (map (\linha -> drop (x1-10) linha) mapa))
          pecaPic = case peca of Bloco -> brick
                                 Caixa -> crate
                                 Porta -> door
          x1f = fromIntegral x1
          x2f = fromIntegral x2
          y1f = fromIntegral y1
          y2f = fromIntegral y2

getPictures :: [Picture] -> (Float,Float,Float) -> Mapa -> [Picture] -- offset xmax, valor com acumulador, offset altura
getPictures _ _ [] = [Blank] -- é preciso o blank??
getPictures pics@[brick, crate, door] (x,_,y) ([]:mapa) = getPictures pics (x,x,y-64) mapa -- reset x, go down a line
getPictures pics@[brick, crate, door] (x,x2,y) ((peca:linha):mapa)
    | peca == Bloco = (Translate x2 y brick):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Caixa = (Translate x2 y crate):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Porta = (Translate x2 y door):(getPictures pics (x,x2+64,y) (linha:mapa))
    | otherwise = getPictures pics (x,x2+64,y) (linha:mapa)

eventListener :: Event -> Estado -> Estado
eventListener (EventKey key Down _ _) (jogo@(Jogo mapa jogador), pic, coords, mov, m@(MapEdit ((x1,y1), (x2,y2), peca, states)), sec) 
    | key == SpecialKey KeyEnter = (Jogo (adicionaMapa) jogador, pic, coords, mov, m, sec)
    | otherwise = (jogo, pic, coords, mov, MapEdit ((x3,y3),(x4,y4),peca2,(state1,state2)), 0)
    where (x3,y3,state1) | key == Char 'w' = (x1,y1+1,U)
                         | key == Char 'a' = (x1-1,y1,L)
                         | key == Char 's' = (x1,y1-1,D)
                         | key == Char 'd' = (x1+1,y1,R)
                         | otherwise = (x1,y1,None)
          (x4,y4,state2) | key == SpecialKey KeyUp = (x2,y2+1,U)
                         | key == SpecialKey KeyDown = (x2,y2-1,D)
                         | key == SpecialKey KeyLeft = (x2-1,y2,L)
                         | key == SpecialKey KeyRight = (x2+1,y2,R)
                         | otherwise = (x2,y2,None)
          peca2 | key == Char '1' = Bloco
                | key == Char '2' = Caixa
                | key == Char '3' = Porta
                | otherwise = peca
    -- | key == 'm' full map view
    -- | key == 'v' -- validate ?
    -- | key == SpecialKey KeySpace -- finish, o que é guardado é o jogo e as coords da porta
    
    -- | key == SpecialKey KeyEnter = (Jogo (adicionaMapa mapa peca (x2,y2)) (Jogador (x,y) dir caixa), pic, coords, mov, MapEdit ((x1,y1), (x2,y2), peca, (None,None)), sec)
   -- | key == SpecialKey KeyDelete = (Jogo (deleteMapa mapa peca (x2,y2)) (Jogador (x,y) dir caixa), pic, coords, mov, MapEdit ((x1,y1), (x2,y2), peca, (None,None)), sec)
    -- não esquecer perguntar onde spawnar o jogador
          adicionaMapa :: Mapa
          adicionaMapa = constroiMapa ((peca,(x2,y2)):(desconstroiMapa mapa))
       --   deleteMapa :: Mapa -> Peca -> (Int,Int) -> Mapa
         -- deleteMapa a b c = a

eventListener (EventKey key Up _ _) (jogo, pic, coords, mov, MapEdit ((x1,y1), (x2,y2), peca, (state1,state2)), sec) =
    (jogo, pic, coords, mov, MapEdit ((x1,y1), (x2,y2), peca, (state3,state4)), 0)
    where state3 | key == Char 'w' = None
                 | key == Char 'a' = None
                 | key == Char 's' = None
                 | key == Char 'd' = None
                 | otherwise = state1
          state4 | key == SpecialKey KeyUp = None
                 | key == SpecialKey KeyDown = None
                 | key == SpecialKey KeyLeft = None
                 | key == SpecialKey KeyRight = None
                 | otherwise = state2
-- chave de reset global
eventListener (EventKey (SpecialKey KeyTab) Down _ _) (_, pic, _, _, _, _) = estadoBase pic (Menu New) 
eventListener (EventKey key Down _ _) (jogo, pic, coords, mov, Menu state, _)
    | key == SpecialKey KeyUp = (jogo, pic, coords, mov, Menu (case state of New -> MapEditS
                                                                             Load -> New
                                                                             MapEditS -> Load), 0)
    | key == SpecialKey KeyDown = (jogo, pic, coords, mov, Menu (case state of New -> Load
                                                                               Load -> MapEditS
                                                                               MapEditS -> New), 0)
    | key == SpecialKey KeyEnter = (jogo, pic, coords, mov, stateToGm, 0) -- no futuro usar um mapa vazio
    where stateToGm | state == New = Play 1
                    | state == Load = Play 1
                    | state == MapEditS = MapEdit ((0,0), (0,0), Bloco, (None,None))
          (jogo,coords) = jogoInicial 1

eventListener (EventKey (SpecialKey KeyUp) Down _ _) (jogo, pic, coords, mov, gm, sec) = (moveJogador jogo Trepar, pic, coords, mov+1, gm, sec)
eventListener (EventKey (SpecialKey KeyDown) Down _ _) (jogo, pic, coords, mov, gm, sec) = (moveJogador jogo InterageCaixa, pic, coords, mov+1, gm, sec)
eventListener (EventKey (SpecialKey KeyLeft) Down _ _) (jogo, pic, coords, mov, gm, sec) = (moveJogador jogo AndarEsquerda, pic, coords, mov+1, gm, sec)
eventListener (EventKey (SpecialKey KeyRight) Down _ _) (jogo, pic, coords, mov, gm, sec) = (moveJogador jogo AndarDireita, pic, coords, mov+1, gm, sec)
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, coords, mov, gm, sec) = estadoBase pic gm
-- debug, toggle do bool da caixa do jogador
eventListener (EventKey (SpecialKey KeyF1) Down _ _) (Jogo mapa (Jogador pos dir caixa), pic, coords, mov, gm, sec) = (Jogo mapa (Jogador pos dir (not caixa)), pic, coords, mov, gm, sec)
eventListener _ s = s

step :: Float -> Estado -> Estado
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, coords, mov, Play n, sec)
    | (x,y) == coords =
        if n == numeroMapas
        then (jogo, pic, coords, mov, Won, sec)
        else let (jogo2,coords2) = jogoInicial (n+1) in (jogo2, pic, coords2, mov, Play (n+1), sec+time)
    | otherwise = (jogo, pic, coords, mov, Play n, sec+time)
step time e@(jogo, pic, coords, mov, m@(MapEdit ((x1,y1), (x2,y2), peca, (state1,state2))), sec) =
    if mod (round (sec*1000)) 500 < 300
    then (jogo, pic, coords, mov, m, sec+time)
    else (jogo, pic, coords, mov, MapEdit ((x3,y3), (x4,y4), peca, (state1,state2)), 0)
    where x3 | state1 == L = x1-1
             | state1 == R = x1+1
             | otherwise = x1
          y3 | state1 == U = y1+1
             | state1 == D = y1-1
             | otherwise = y1
          x4 | state2 == L = x2-2
             | state2 == R = x2+2
             | otherwise = x2
          y4 | state2 == D = y2-2
             | state2 == U = y2+2
             | otherwise = y2
step time e = e

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
        (estadoBase (Pictures [(Scale 0.057 0.115 playerLeft),(Scale 0.057 0.115 playerRight), (Scale 0.125 0.125 brick), (Scale 0.25 0.25 crate), (Scale 0.119 0.06 door)]) (Menu New)) -- 64x64 px
        draw
        eventListener
        step