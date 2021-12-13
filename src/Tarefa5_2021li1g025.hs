module Main where

import Tarefa4_2021li1g025
import Tarefa2_2021li1g025
import Tarefa1_2021li1g025
import Maps
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import LI12122

-- este jogo inicial não é o jogo inicial absoluto, mas sim o de cada mapa. o mesmo acontece com o estadoBase
jogoInicial :: Int -> (Jogo,(Int,Int))
jogoInicial n = maps !! (n-1) -- começa no mapa 1, o menu

estadoBase :: Picture -> GameMode -> Estado
estadoBase pics (Menu n) = (jogo, pics, (0,0), 0, Menu n, 0) -- estado inicial, quando o jogador está no menu, obtido de infoMenu.
                         where jogo = Jogo mainMenuMap (Jogador (11,9) Este False)
estadoBase pics (Play n) = (jogo, pics, coords, 0, Play n, 0) -- !no futuro o mov não será 0 mas poderá ser carregado dos dados guardados -- estado nos níveis
                         where (jogo,coords) = jogoInicial n
estadoBase pics (MapSelector n) = (jogo, pics, (0,0), 0, MapSelector n, 0) -- devo meter o tempo e os movimentos a contar já no seletor ?
                        where jogo = Jogo mapSelectorMap (Jogador (2,1) Este False)
estadoBase pics gm = (Jogo [] (Jogador (0,0) Este False), pics, (0,0), 0, gm, 0)

numeroMapas :: Int
numeroMapas = (length maps)

--data e tipos principais--

type Estado = (Jogo, Picture, (Int,Int), Int, GameMode, Float) -- jogo atual, imagens, coords da porta, numero de movimentos, modo de jogo, tempo em segundos (tempo total)

data GameMode = Menu MenuInfo | MapSelector MapSelectInfo | Play Int | MapEdit MapEditInfo | Won | TabMenu TabMenuInfo
    deriving (Eq,Show)

-- menu-- 

type MenuInfo = ([(Int,Int,MenuStates)],MenuStates)

infoMenu :: ([(Int,Int,MenuStates)],MenuStates)
infoMenu = ([(5,5, New),(11,5,Continue),(17,5,MapEdit1)], Normal)

data MenuStates = New | Continue | MapEdit1 | Normal 
    deriving (Eq,Show)

-- map selector --

type MapSelectInfo = ([(Int,Int,Int)],Int) -- coordenadas de portas e nº dos níveis correspondentes, acumulador para a posição do jogador, TODO: parâmetro- nº do último nível guardado

infoMapSelect :: MapSelectInfo -- não inclui o nível do último nível guardado porque será adicionado mais tarde com IO.
infoMapSelect = ([(4,1,1),(6,1,2),(8,1,3),(10,1,4),(12,1,5)],0)

-- Map editor -- 

data Mov = U | D | L | R | None deriving (Eq,Show) -- up down left rigth

-- no mapEditor, o número de movimentos vai indicar se o jogador está no menu do editor, se quer validar o mapa, ou se quer ver o mapa completo, ou se quer alterar a posição do jogador
-- 0 1 2 3 modo normal, menu do editor (ecrã com controlos), validar, ver mapa
-- mo futuro estas informações vão estar no MapEditInfo?
type MapEditInfo = ((Int,Int), (Int,Int), Peca, (Mov,Mov)) -- peca | jogador?-- posicao absoluta do mapa, posicao absoluta da peca, peca selecionada, informações para ajudar no movimento contínuo

-- TabMenu --

type TabMenuInfo = (Int,Estado) -- posição atual do cursor pela ordem de cima para baixo na lista, modo de jogo em que é aberto o Tab.



window :: Display
window = FullScreen

fr :: Int
fr = 50

-- offset é para que o getPictures saiba onde começar a desenhar as imagens (o mais à esquerda possível)
draw :: Estado -> Picture
-- MapEditor
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door,menuplay,menuselector,snowbg,grassbg,sandbg]), coords, mov, MapEdit ((x1,y1), (x2,y2), peca, _), _) =
    --Translate (-500) 200 (Pictures [Scale 0.1 0.1 (Text (show (Jogo mapa (Jogador (x,y) dir caixa)))), (Translate 0 (-150) (Scale 0.1 0.1 (Text (show (x2,y2)))))])
    Scale scale scale (Pictures (snowbg:(Translate offsetxJogador offsetyJogador picFinal):(linha1):(linha2):(getPictures [brick, crate, door] (64*x1f,64*x1f,((-64)*y1f)) map2) ++ 
        [Translate offsetx offsety (Pictures [pecaPic, outline])] ++ [Scale 0.25 0.25 (Translate (-200) 150 texto)]))
    where (map2,scale) | mov == 3 = (mapa,0.25)
                       | otherwise = (mapa,1) -- ((map (\linha -> take 20 linha) (map (\linha -> drop (x1-10) linha) mapa)),1)
          pecaPic = case peca of Bloco -> brick
                                 Caixa -> crate
                                 Porta -> door
          player = (case dir of Este -> playerRight
                                Oeste -> playerLeft)
          picFinal = (case caixa of False -> player
                                    True -> Pictures [player, Translate 0 64 crate])
          x1f = fromIntegral x1
          x2f = fromIntegral x2
          y1f = fromIntegral y1
          y2f = fromIntegral y2
          xf = fromIntegral x
          yf = fromIntegral y
          offsetx = 64*(x2f+x1f)
          offsety = -64*(y2f+y1f)
          offsetxJogador = 64*(xf+x1f)
          offsetyJogador = -64*(yf+y1f)
          yline = (-y1f*64)+32
          xline = -32+64*x1f
          linha1 = Line [(xline,-2400),(xline,yline)] -- números grandes para evitar que a linha não seja completa ao fazer zoom
          linha2 = Line [(xline,yline),(5400,yline)]
          outline = Color cyan (Line [(-32,-32),(32,-32),(32,32),(-32,32),(-32,-32)])
          texto | mov == 1 = Text "Menu" -- aqui em vez de texto será a imagem do menu
                | mov == 2 = Text ("O mapa" ++ (case validaPotencialMapa (desconstroiMapa mapa) of True -> " "
                                                                                                   False -> " nao ") ++ "e valido")
                | otherwise = Blank

-- Won
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics, coords, mov, Won, sec) =
    Translate (-200) 0 (Scale 0.5 0.5 (Pictures [Text "You Won!!!", Translate 0 (-120) (Text ("Movements: " ++ (show mov))), Translate 0 (-250) (Text ("In " ++ show (round sec) ++ " seconds"))])) -- jogador chegou à porta final


-- TabMenu
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door,menuplay,menuselector,snowbg,grassbg,sandbg]), _, _, (TabMenu (posMenu,e2@(_,_,_,_,gm,_))), _) = case gm of 
        Play n -> Pictures ((draw e2):menuplay:[tabpointer1])
        MapSelector n -> Pictures ((draw e2):menuselector:[tabpointer2])
        MapEdit n -> Pictures ((draw e2):menuselector:[tabpointer2])
    where tabpointer1 = Pictures ((Translate (-250) (305-125*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (305-125*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer2 = Pictures ((Translate (-250) (216-156*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (216-156*((fromIntegral (posMenu))-1)) playerLeft])

-- Play, Menu e Map Selector
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door,menuplay,menuselector,snowbg,grassbg,sandbg]), coords, mov, gamemode, _)
    | xmax <= 20 = let offset = getOffset mapa
                       map3 = getLines y mapa
                       offsetY = getOffsetY map3 in -- scrolling desnecessário para mapas pequenos, temos de centrar o mapa consoante xmax
        Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) ((-y2*64)+offsetY) picFinal])

    | x <= 10 = let map2 = map (\linha -> take 20 linha) mapa
                    map3 = getLines y map2
                    offsetY = getOffsetY map3 in
        Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) (((-y2*64))+offsetY) picFinal]) -- offset de altura é manual por agora
    
    | xf >= (xmax - 10) = let wallDistance = (round xmax)-20
                              map2 = map (\linha -> drop wallDistance linha) mapa
                              x2 = (xf-(fromIntegral wallDistance)) -- converte coordenadas do jogador em coordenadas no ecrã
                              map3 = getLines y map2
                              offsetY = getOffsetY map3 in 
        --debug Scale 0.1 0.1 (Text (show map2))
        Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate ((x2*64)+offset) (((-y2*64))+offsetY) picFinal])
    
    | otherwise = let map2 = getLines y mapa
                      map3 = (map (\linha -> take 20 linha) (map (\linha -> drop (x-10) linha) map2))
                      offset = getOffset map3
                      offsetY = getOffsetY map3 in
        Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate 0 (((-y2*64))+offsetY) picFinal])

    where xmax = fromIntegral (length (head mapa))
          ymax = (length mapa)-1 -- +1 que o valor maximo de y
          getOffset :: Mapa -> Float -- assume se que o mapa já só tem 20 blocos de comprimento
          getOffset map3 = (-64)*((fromIntegral (length (head map3)))/2)
          offset = -640
          xf = fromIntegral x
          yf = fromIntegral y
          player = (case dir of Este -> playerRight
                                Oeste -> playerLeft)
          picFinal 
            | caixa = Pictures [player, Translate 0 64 crate] -- n tem em conta os casos em que o jogador vai para uma porta a segurar uma caixa
            | otherwise = case gamemode of (Menu (_,Normal)) -> player
                                           (Menu (_,zona))  | zona == New -> Pictures [player, scale (0.5) (0.5) (Translate (-320) (96) (Text "New Game"))]
                                                            | zona == Continue -> Pictures [player, scale (0.5) (0.5) (Translate (-384) (96) (Text "Level Selector"))]
                                                            | zona == MapEdit1 -> Pictures [player, scale (0.5) (0.5) (Translate (-288) (96) (Text "Map Editor"))]
                                           (MapSelector (_,0)) -> player
                                           (MapSelector (_,nivel)) -> Pictures [player, scale (0.5) (0.5) (Translate (-208) (96) (Text ("Level " ++ (show nivel))))]
                                           _ -> player
          -- offset vertical
          getOffsetY :: Mapa -> Float
          getOffsetY map3 = ((64)*((ymax2/2))-64)
                          where ymax2 = fromIntegral (length map3)
          y2 = fromIntegral (y-(15*(div y 15)))
          getLines :: Int -> Mapa -> Mapa -- devolve linhas do mapa a renderizar consoante altura do jogador
          getLines y l = drop (15*(section-1)) (take (15*section) l)
                       where section = (div y 15)+1

-- desenha o mapa dado, desenhando da esquerda para a direita e depois de cima para baixo, consoante os offsets dados
getPictures :: [Picture] -> (Float,Float,Float) -> Mapa -> [Picture] -- offset xmax, valor com acumulador, offset altura
getPictures _ _ [] = [Blank] -- é preciso o blank??
getPictures pics@[brick, crate, door] (x,_,y) ([]:mapa) = getPictures pics (x,x,y-64) mapa -- reset x, go down a line
getPictures pics@[brick, crate, door] (x,x2,y) ((peca:linha):mapa)
    | peca == Bloco = (Translate x2 y brick):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Caixa = (Translate x2 y crate):(getPictures pics (x,x2+64,y) (linha:mapa))
    | peca == Porta = (Translate x2 y door):(getPictures pics (x,x2+64,y) (linha:mapa))
    | otherwise = getPictures pics (x,x2+64,y) (linha:mapa)

eventListener :: Event -> Estado -> Estado

-- esta secção são comandos debug ou reset
eventListener (EventKey (SpecialKey KeyBackspace) Down _ _) (_, pic, _, _, _, _) = estadoBase pic (Menu infoMenu)
eventListener (EventKey (SpecialKey KeyF1) Down _ _) (_, pic, _, _, _, _) = estadoBase pic (Play 1)
eventListener (EventKey (SpecialKey KeyF2) Down _ _) (_, pic, _, _, _, _) = (Jogo [] (Jogador (0,0) Este False),pic,(0,0), 0, MapEdit ((-13,-7),(0,0),Bloco,(None,None)),0)
eventListener (EventKey (SpecialKey KeyF3) Down _ _) (_, pic, _, _, _, _) = (Jogo tallMap (Jogador (0,28) Este False), pic, (0,0), 0, Play 0, 0)
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, coords, mov, gm, sec) = estadoBase pic gm
eventListener (EventKey (SpecialKey KeyF5) Down _ _) (Jogo mapa (Jogador pos dir caixa), pic, coords, mov, gm, sec) = (Jogo mapa (Jogador pos dir (not caixa)), pic, coords, mov, gm, sec)

-- TabMenu
eventListener (EventKey (SpecialKey KeyTab) Down _ _) e@(jogo, pics, coords, mov, gm, sec) = case gm of -- para entrar e sair do estado TabMenu
    Won -> e
    Menu n -> e
    TabMenu (n,estAnterior) -> estAnterior
    otherwise -> (jogo, pics, coords, mov, TabMenu (1,e), sec)

eventListener (EventKey (SpecialKey KeyUp) Down _ _) e@(_,_,_,_,TabMenu (pos,e2@(_,_,_,_,gm,_)), sec) = case gm of -- para mover no estado TabMenu
        Play n  -> tabMenuOrganizer e 6
        MapSelector n -> tabMenuOrganizer e 4
        MapEdit n -> tabMenuOrganizer e 4

    --coordena a interação com os menus de pausa
    where   tabMenuOrganizer :: Estado -> Int -> Estado -- estado atual do menu de pausa, número de botões no menu (para um gamemode específico) (permite clicar para cima no topo do menu e ir ter ao último elemento do mesmo)
            tabMenuOrganizer e@(jogo, pic, coords, mov, (TabMenu (pos,e2@(_,_,_,_,gm,_))), sec) x
                | pos == 1 = (jogo, pic, coords, mov, (TabMenu (x,e2)),sec)
                | otherwise = (jogo, pic, coords, mov, (TabMenu ((pos-1),e2)),sec)

eventListener (EventKey (SpecialKey KeyDown) Down _ _) e@(_,_,_,_,TabMenu (pos,e2@(_,_,_,_,gm,_)), sec) = case gm of -- para mover no estado TabMenu
        Play n  -> tabMenuOrganizer2 e 6
        MapSelector n -> tabMenuOrganizer2 e 4
        MapEdit n -> tabMenuOrganizer2 e 4

        --coordena a interação com os menus de pausa
    where   tabMenuOrganizer2 :: Estado -> Int -> Estado -- igual a tabMenuOrganizer, mas para a teclaDown
            tabMenuOrganizer2 e@(jogo, pic, coords, mov, (TabMenu (pos,e2@(_,_,_,_,gm,_))), sec) x
                | pos == x = (jogo, pic, coords, mov, TabMenu (1,e2),sec)
                | otherwise = (jogo, pic, coords, mov, (TabMenu ((pos+1),e2)),sec)

    -- coordena a interação de Enter com os menus de pausa
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(_,pics,_,_,TabMenu (pos,e2@(_,_,_,_,gm,_)), sec) = case gm of -- interaç
    Play n  | pos == 1 -> e2
            | pos == 2 -> estadoBase pics (Play n)
            | pos == 3 -> e -- TODO
            | pos == 4 -> e -- TODO
            | pos == 5 -> e -- TODO
            | pos == 6 -> estadoBase pics (Menu infoMenu) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    MapEdit n   | pos == 1 -> e2
                | pos == 2 -> e -- TODO
                | pos == 3 -> e -- TODO
                | pos == 4 -> estadoBase pics (Menu infoMenu) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    MapSelector n   | pos == 1 -> e2
                    | pos == 2 -> e -- TODO
                    | pos == 3 -> e -- TODO
                    | pos == 4 -> estadoBase pics (Menu infoMenu) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

eventListener _ e@(_,_,_,_,TabMenu n, _) = e

-- MapEditor (tecla pressionada)
eventListener (EventKey key Down _ _) (Jogo mapa (jogador@(Jogador (x,y) dir caixa)), pic, coords, mov, m@(MapEdit ((x1,y1), (x2,y2), peca, states)), sec) 
    | key == SpecialKey KeyEnter = (Jogo adicionaMapa jogador, pic, coords, mov, m, sec)
    | key == SpecialKey KeyDelete = (Jogo deleteMapa jogador, pic, coords, mov, m, sec)
    | otherwise = (Jogo mapa jogador2, pic, coords, mov2, MapEdit ((x3,y3),(x4,y4),peca2,(state1,state2)), 0)
    where (x3,y3,state1) | key == Char 'w' = (x1,y1+1,U)
                         | key == Char 'a' = (x1+1,y1,R)
                         | key == Char 's' = (x1,y1-1,D)
                         | key == Char 'd' = (x1-1,y1,L)
                         | otherwise = (x1,y1,None)
          (x4,y4,state2) | key == SpecialKey KeyUp = (x2,y2-1,U)
                         | key == SpecialKey KeyDown = (x2,y2+1,D)
                         | key == SpecialKey KeyLeft = (x2-1,y2,L)
                         | key == SpecialKey KeyRight = (x2+1,y2,R)
                         | otherwise = (x2,y2,None)
          jogador2 | key == Char '4' = Jogador (x2,y2) dir caixa
                   | key == Char '5' = Jogador (x,y) dir (not caixa)
                   | key == Char '6' = Jogador (x,y) (if dir == Este then Oeste else Este) caixa
                   | otherwise = jogador
          peca2 | key == Char '1' = Bloco
                | key == Char '2' = Caixa
                | key == Char '3' = Porta
                | otherwise = peca 
          mov2 | key == Char 'm' = if mov == 1 then 0 else 1 
               | key == Char 'v'= if mov == 2 then 0 else 2
               | key == SpecialKey KeySpace = if mov == 3 then 0 else 3
               | otherwise = mov
          adicionaMapa :: Mapa -- try catch para evitar erros ou mudar constroiMapa? ex sobreposição de peças
          adicionaMapa | newMap /= [] = constroiMapa newMap
                       | otherwise = mapa 
                       where newMap = ((peca,(x2,y2)):(desconstroiMapa mapa))
          deleteMapa :: Mapa
          deleteMapa | newMap /= [] = constroiMapa newMap
                     | otherwise = mapa
                     where newMap = (filter (\(p,c) -> c /= (x2,y2)) (desconstroiMapa mapa))

-- MapEditor (tecla libertada)
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

-- Menu, map selector e Play
eventListener (EventKey key Down _ _) e@(jogo, pic, coords, mov, gamemode, sec) = case gamemode of 

        Play n -> (jogo2, pic, coords, mov+1, gamemode, sec)
        Menu (lista,atual) -> 
            let novoEstado1 | atual == New = estadoBase pic (Play 1) 
                            | atual == Continue = estadoBase pic (MapSelector infoMapSelect)
                            | atual == MapEdit1 = (Jogo [] (Jogador (0,0) Este False),pic,coords, 0, MapEdit ((0,0),(0,0),Bloco,(None,None)),0)
                            | otherwise = e 
            in  if key == SpecialKey KeyEnter 
                then novoEstado1
                else (jogo2, pic, coords, mov, Menu (lista,atual),0) 
        MapSelector (lista,atual) -> 
            let novoEstado2 | atual == 0 = e
                            | otherwise = estadoBase pic (Play atual)
            in  if key == SpecialKey KeyEnter
                then novoEstado2
                else (jogo2, pic, coords, mov, MapSelector (lista,atual),0) 

    where jogo2     | key == SpecialKey KeyUp = moveJogador jogo Trepar
                    | key == SpecialKey KeyDown = moveJogador jogo InterageCaixa
                    | key == SpecialKey KeyLeft = moveJogador jogo AndarEsquerda
                    | key == SpecialKey KeyRight = moveJogador jogo AndarDireita
                    | otherwise = jogo

eventListener _ s = s

--Jogo
step :: Float -> Estado -> Estado
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, coords, mov, Play n, sec)
    | (x,y) == coords =
        if n == numeroMapas
        then (jogo, pic, coords, mov, Won, sec)
        else let (jogo2,coords2) = jogoInicial (n+1) in (jogo2, pic, coords2, mov, Play (n+1), sec+time)
    | otherwise = (jogo, pic, coords, mov, Play n, sec+time)

--Map edit
step time e@(jogo, pic, coords, mov, m@(MapEdit ((x1,y1), (x2,y2), peca, (state1,state2))), sec) =
    if timeBetweenActions < 250
    then (jogo, pic, coords, mov, m, sec+time)
    else (jogo, pic, coords, mov, MapEdit ((x3,y3), (x4,y4), peca, (state1,state2)), 0)
    where x3 | state1 == L = x1-3
             | state1 == R = x1+3
             | otherwise = x1
          y3 | state1 == U = y1+3
             | state1 == D = y1-3
             | otherwise = y1
          x4 | state2 == L = x2-3
             | state2 == R = x2+3
             | otherwise = x2
          y4 | state2 == D = y2+3
             | state2 == U = y2-3
             | otherwise = y2
          timeBetweenActions = mod (round (sec*1000)) 500

-- Menu
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, coords, mov, Menu (lista,antigo), sec) = 
    (jogo, pic, coords, mov, Menu (lista,atual), 0)
    where atual = convert1 (x,y) lista

-- Map selector 
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, coords, mov, MapSelector (lista,antigo), sec) = 
    (jogo, pic, coords, mov, MapSelector (lista,atual), sec+time)
    where atual = convert2 (x,y) lista

-- Para outros inputs
step _ e = e

convert1 :: (Int,Int) -> [(Int,Int,MenuStates)] -> MenuStates -- percorre lista infoMenu e verifica correspondências da posição do jogador com as portas do jogo
convert1 (x,y) [] = Normal
convert1 (x,y) ((a,b,c):t)  | (a,b) == (x,y) = c
                            | otherwise = convert1 (x,y) t

convert2 :: (Int,Int) -> [(Int,Int,Int)] -> Int -- percorre lista infoMenu e verifica correspondências da posição do jogador com as portas do jogo
convert2 (x,y) [] = 0
convert2 (x,y) ((a,b,c):t)  | (a,b) == (x,y) = c
                            | otherwise = convert2 (x,y) t

main :: IO()
main = do
    Just brick <- loadJuicy "red-brick.png"
    Just playerLeft <- loadJuicy "character-left.png"
    Just playerRight <- loadJuicy "character-right.png"
    Just crate <- loadJuicy "red-box.png"
    Just door <- loadJuicy "armored-door.png"
    Just menuplay <- loadJuicy "menu-play.png"
    Just menuselector <- loadJuicy "menu-selector.png"
    Just snowbg <- loadJuicy "snow.png"
    Just grassbg <- loadJuicy "greenfields.png"
    Just sandbg <- loadJuicy "sand.png"

    play window
        (white)
        fr
        (estadoBase (Pictures [(Scale 2.783 2.783 playerLeft),(Scale 2.783 2.783 playerRight), (Scale 0.186 0.186 brick), (Scale 2.0 2.0 crate), (Scale 0.674 0.451 door),menuplay,menuselector,snowbg,grassbg,sandbg]) (Menu infoMenu)) -- 64x64 px
        draw
        eventListener
        step