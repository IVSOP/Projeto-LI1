module Main where

import Tarefa6_2021li1g025
import Tarefa4_2021li1g025
import Tarefa2_2021li1g025
import Tarefa1_2021li1g025
import Maps
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicy)
import LI12122
import System.Exit
import System.IO

-- este jogo inicial não é o jogo inicial absoluto, mas sim o de cada mapa. o mesmo acontece com o estadoBase
jogoInicial :: Int -> (Jogo,(Int,Int))
jogoInicial n = maps !! (n-1) -- começa no mapa 1, o menu

estadoBase :: Picture -> GameMode -> Estado
estadoBase pics (Menu n) = (jogo, pics, Menu n) -- estado inicial, quando o jogador está no menu, obtido de infoMenu.
                         where jogo = Jogo menuMap (Jogador (11,4) Este False)
estadoBase pics (Play (n,coords,sec,mov)) = (jogo, pics, Play (n,coords2,0,0)) -- !no futuro o mov não será 0 mas poderá ser carregado dos dados guardados -- estado nos níveis
                                             where (jogo,coords2) = jogoInicial n
estadoBase pics (MapSelector n) = (jogo, pics, MapSelector n) -- devo meter o tempo e os movimentos a contar já no seletor ?
                        where jogo = Jogo mapSelectorMap (Jogador (2,4) Este False)
estadoBase pics gm = (Jogo [] (Jogador (0,0) Este False), pics, gm)

numeroMapas :: Int
numeroMapas = (length maps)

--data e tipos principais--

type Estado = (Jogo, Picture, GameMode) -- jogo atual, imagens, modo de jogo

data GameMode = Menu MenuInfo | MapSelector MapSelectInfo | Play PlayInfo | MapEdit MapEditInfo | Won WonInfo | TabMenu TabMenuInfo | Solver SolverInfo
    deriving (Eq,Show)

-- won
type WonInfo = (Int,Float) -- mov, time
-- play
type PlayInfo = (Int,(Int,Int),Float,Int) -- mapa, coords da porta, segundos, movimentos
-- menu --
data MenuStates = New | Continue | MapEdit1 | Normal 
    deriving (Eq,Show)
    
type MenuInfo = ([(Int,Int,MenuStates)],MenuStates)

infoMenu :: ([(Int,Int,MenuStates)],MenuStates)
infoMenu = ([(5,8, Continue),(11,8,New),(17,8,MapEdit1)], Normal)

-- map selector --

type MapSelectInfo = ([(Int,Int,Int)],Int) -- coordenadas de portas e nº dos níveis correspondentes, acumulador para a posição do jogador, TODO: parâmetro- nº do último nível guardado

infoMapSelect :: MapSelectInfo -- não inclui o nível do último nível guardado porque será adicionado mais tarde com IO.
infoMapSelect = ([(4,4,1),(6,4,2),(8,4,3),(10,4,4),(12,4,5),(21,1,6),(23,1,7),(25,1,8),(27,1,9),(29,1,10)],0)

-- Map editor -- 

data Mov = U | D | L | R | None deriving (Eq,Show) -- up down left rigth

type MapEditInfo = ((Int,Int), (Int,Int), Peca, (Mov,Mov),Int,Float) -- posicao absoluta do mapa, posicao absoluta da peca, peca selecionada, informações para ajudar no movimento contínuo, Int que representa o modo, segundos passados

-- TabMenu --

type TabMenuInfo = (Int,Estado) -- posição atual do cursor pela ordem de cima para baixo na lista, modo de jogo em que é aberto o Tab.

-- Solver --

-- estado antes de entrar no TabMenu (antes de entrar no Solver) , lista de movimentos de resolveJogo, lista para onde vão os movimentos já efetuados (caso o jogador queria repetir a solução), número que regista a opção de começar a solução do início ou do lugar atual, tempo decorrido (para espaçar os movimentos da solução) , número que distingue o menu em que se pergunta onde deve começar a solução e a própria solução
type SolverInfo = ((Estado, Maybe [Movimento], Maybe [Movimento], Int, Float),Int) 
window :: Display
window = FullScreen

fr :: Int
fr = 50

saveFile = "SaveGame.txt"

savePlay :: String -> IO ()
savePlay str = do savedata <- readFile saveFile
                  let contents = tail (lines savedata) -- para não apagar os dados do mapeditor
                  putStrLn (concat contents) -- para evitar conflito read e write
                  writeFile saveFile (unlines (str:contents))

-- offset é para que o getPictures saiba onde começar a desenhar as imagens (o mais à esquerda possível)
draw :: Estado -> IO Picture
-- MapEditor transferido para play (se mode == 3)
draw (jogo, pics, MapEdit ((x1,y1), (x2,y2), peca, _, 3, sec)) =
    draw (jogo, pics, Play (-1,(-1,-1),-1,-1))

-- MapEditor
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg]), MapEdit ((x1,y1), (x2,y2), peca, _, mode, sec)) =
    -- TODO: fix background ao mostrar o mapa todo
    --Translate (-500) 200 (Pictures [Scale 0.1 0.1 (Text (show (Jogo mapa (Jogador (x,y) dir caixa)))), (Translate 0 (-150) (Scale 0.1 0.1 (Text (show (x2,y2)))))])
    return (Pictures [snowbg, (Scale scale scale (Pictures ((Translate offsetxJogador offsetyJogador picFinal):(linha1):(linha2):(getPictures [brick, crate, door] (64*x1f,64*x1f,((-64)*y1f)) map2) ++ 
        [Translate offsetx offsety (Pictures [pecaPic, outline])] ++ [Scale 0.25 0.25 (Translate (-200) 150 texto)])))])
    where (map2,scale) | mode == 2 = (mapa,0.25)
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
          texto | mode == 1 = Text ("O mapa" ++ (case validaPotencialMapa (desconstroiMapa mapa) of True -> " "
                                                                                                    False -> " nao ") ++ "e valido")
                | otherwise = Blank

-- Won
draw ((Jogo mapa (Jogador (x,y) dir caixa)), Pictures pics, Won (mov,sec)) =
    return (Pictures [bg, Translate (-200) 0 (Scale 0.5 0.5 (Pictures [Text "You Won!!!", Translate 0 (-120) (Text ("Movements: " ++ (show mov))), Translate 0 (-250) (Text ("In " ++ show (round sec) ++ " seconds"))]))]) -- jogador chegou à porta final
    where bg = pics !! 13

-- TabMenu
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg]), (TabMenu (posMenu,e2@(_,_,gm)))) = 
        do drawjogo <- draw e2
           return $ case gm of 
                        Play n -> Pictures (drawjogo:menuplay:[tabpointer1])
                        MapSelector n -> Pictures (drawjogo:menuselector:[tabpointer2])
                        Solver (_,1) -> Pictures (drawjogo:menusolvertype:[tabpointer3])
                        Solver (_,2) -> Pictures (drawjogo:menusolver:[tabpointer4])
                        Solver (_,3) -> Pictures (drawjogo:menusolverend:[tabpointer5])
                        Solver (_,4) -> Pictures (drawjogo:menusolverimp:[tabpointer6])
                        MapEdit n -> Pictures (drawjogo:menueditor:[tabpointer7])
    where tabpointer1 = Pictures ((Translate (-250) (305-125*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (305-125*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer2 = Pictures ((Translate (-250) (240-156*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (240-156*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer3 = Pictures ((Translate (-250) (170-168*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (170-168*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer4 = Pictures ((Translate (-250) (250-165*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (250-165*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer5 = Pictures ((Translate (-250) (95-185*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (95-185*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer6 = Pictures ((Translate (-250) (-100) playerRight):[Translate 250 (-100) playerLeft])
          tabpointer7 = Pictures ((Translate (-250) (280-140*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (280-140*((fromIntegral (posMenu))-1)) playerLeft])
          
-- Play, Menu, Map Selector e Solver
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg]), gamemode)
    | xmax <= 20 = let offset = getOffset mapa
                       map3 = getLines y mapa
                       offsetY = getOffsetY map3 in -- scrolling desnecessário para mapas pequenos, temos de centrar o mapa consoante xmax
        return (Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) ((-y2*64)+offsetY) picFinal]))

    | x <= 10 = let map2 = map (\linha -> take 20 linha) mapa
                    map3 = getLines y map2
                    offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) (((-y2*64))+offsetY) picFinal]))
    
    | xf >= (xmax - 10) = let wallDistance = (round xmax)-20
                              map2 = map (\linha -> drop wallDistance linha) mapa
                              x2 = (xf-(fromIntegral wallDistance)) -- converte coordenadas do jogador em coordenadas no ecrã
                              map3 = getLines y map2
                              offsetY = getOffsetY map3 in
        --debug Scale 0.1 0.1 (Text (show map2))
        return (Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate ((x2*64)+offset) (((-y2*64))+offsetY) picFinal]))
    
    | otherwise = let map2 = getLines y mapa
                      map3 = (map (\linha -> take 20 linha) (map (\linha -> drop (x-10) linha) map2))
                      offset = getOffset map3
                      offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door] (offset,offset,offsetY) map3)++[Translate (-32) (((-y2*64))+offsetY) picFinal]))

    where xmax = fromIntegral (length (head mapa))
          ymax = (length mapa)-1 -- +1 que o valor maximo de y
          getOffset :: Mapa -> Float -- assume se que o mapa já só tem 20 blocos de comprimento
          getOffset map3 = ((-64)*((fromIntegral (length (head map3)))/2))-32
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
          y2 = fromIntegral (mod y 15) -- (y-(15*(div y 15)))
          getLines :: Int -> Mapa -> Mapa -- devolve linhas do mapa a renderizar consoante altura do jogador
          getLines y l = drop (15*(section-1)) (take ((15*section)+1) l)
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

eventListener :: Event -> Estado -> IO Estado
-- esta secção são comandos debug ou reset
eventListener (EventKey (SpecialKey KeyEsc) Down _ _) _ = do exitSuccess -- comando para exit temporário
eventListener (EventKey (SpecialKey KeyBackspace) Down _ _) (_, pic, _) = return (estadoBase pic (Menu infoMenu))
eventListener (EventKey (SpecialKey KeyF1) Down _ _) (_, pic, _) = return (estadoBase pic (Play (1,(0,0),0,0))) -- primeiro nível
eventListener (EventKey (SpecialKey KeyF2) Down _ _) (_, pic, _) = return ((Jogo [] (Jogador (0,0) Este False), pic, MapEdit ((-13,-7),(0,0),Bloco,(None,None),0,0))) -- editor
eventListener (EventKey (SpecialKey KeyF3) Down _ _) (_, pic, _) = return ((Jogo stairMap (Jogador (0,28) Este False), pic, Play (0,(0,0),0,0))) -- mapa alto para testar offset vertical
eventListener (EventKey (SpecialKey KeyF5) Down _ _) (Jogo mapa (Jogador pos dir caixa), pic, gm) = return (Jogo mapa (Jogador pos dir (not caixa)), pic, gm) -- toggle da caixa do jogador

-- Quick save e load no Play
eventListener (EventKey (Char 's') Down _ _) e@(Jogo mapa jogador, _, Play info) -- escrever estado Play
    = do let str = show ((desconstroiMapa mapa),jogador,info)
         savePlay str
         return e
eventListener (EventKey (Char 'l') Down _ _) (_, pic, _) -- carregar estado play
    = do texto <- readFile saveFile
         let (mapa,jogador,info) = read texto
         return (Jogo (constroiMapa mapa) jogador, pic, Play info)

-- reset do gamemode
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, gm@(Play _)) = do savedata <- readFile saveFile
                                                                           let (_,_,info) = read (head (lines savedata))::([(Peca,(Int,Int))],Jogador,PlayInfo) -- especificar o tipo para o read não fazer conflito
                                                                               (jogo,_,_) = estadoBase pic gm
                                                                           return (jogo,pic,Play info)
--eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, Won _) = return (estadoBase pic (Play (1,(0,0),0,0)))
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, gm) = return (estadoBase pic gm)

-- Won (basta carregar em qualquer tecla)
eventListener (EventKey _ Down _ _) (_, pic, Won _) = return (estadoBase pic (Menu infoMenu))

-- MapEditor (mode == 3, jogar o mapa, usa o Play para permitir jogar)
eventListener keyinfo@(EventKey key Down _ _) (jogo, pic, m@(MapEdit (c1, c2, peca, states, 3, sec))) =
    case key of SpecialKey KeyTab -> undefined -- TODO
                Char 'p' -> return (jogo, pic, MapEdit (c1,c2,peca,states,0,sec))
                _ -> do (jogo2,_,_) <- eventListener keyinfo (jogo, pic, Play (-1,(-1,-1),-1,-1)) -- numeros que não irão dar conflito
                        return (jogo2,pic,m)

-- TabMenu e Solver
eventListener (EventKey (SpecialKey KeyTab) Down _ _) e@(jogo, pics, gm) = return $ case gm of -- para entrar e sair do estado TabMenu
    Won _ -> e
    Menu _ -> e
    TabMenu (_,(_,_,(Solver ((emenu,_,_,_,_),1)))) -> emenu
    TabMenu (_,(_,_,(Solver (_,3)))) -> e -- previne o uso de tab no fim de Solver
    TabMenu (_,(_,_,(Solver (_,4)))) -> e -- previne o uso de tab no fim de Solver
    TabMenu (n,estAnterior) -> estAnterior
    otherwise -> (jogo, pics, TabMenu (1,e))

eventListener (EventKey (SpecialKey KeyUp) Down _ _) e@(_,_,TabMenu (pos,e2@(_,_,gm))) = case gm of -- para mover no estado TabMenu
        Play n  -> return (tabMenuOrganizer e 6)
        MapSelector n -> return (tabMenuOrganizer e 4)
        MapEdit n -> return (tabMenuOrganizer e 5)
        Solver (_,n) | n == 1 -> return (tabMenuOrganizer e 3)
                     | n == 2 -> return (tabMenuOrganizer e 4)
                     | n == 3 -> return (tabMenuOrganizer e 2)
                     | n == 4 -> return e

    --coordena a interação com os menus de pausa
    where   tabMenuOrganizer :: Estado -> Int -> Estado -- estado atual do menu de pausa, número de botões no menu (para um gamemode específico) (permite clicar para cima no topo do menu e ir ter ao último elemento do mesmo)
            tabMenuOrganizer e@(jogo, pic, (TabMenu (pos,e2@(_,_,gm)))) x
                | pos == 1 = (jogo, pic, (TabMenu (x,e2)))
                | otherwise = (jogo, pic, (TabMenu ((pos-1),e2)))

eventListener (EventKey (SpecialKey KeyDown) Down _ _) e@(_,_,TabMenu (pos,e2@(_,_,gm))) = case gm of -- para mover o cursor entre as opções no TabMenu
        Play n  -> return (tabMenuOrganizer2 e 6)
        MapSelector n -> return (tabMenuOrganizer2 e 4)
        MapEdit n -> return (tabMenuOrganizer2 e 5)
        Solver (_,n) | n == 1 -> return (tabMenuOrganizer2 e 3)
                     | n == 2 -> return (tabMenuOrganizer2 e 4)
                     | n == 3 -> return (tabMenuOrganizer2 e 2)
                     | n == 4 -> return e
        --coordena a interação com os menus de pausa
    where   tabMenuOrganizer2 :: Estado -> Int -> Estado -- igual a tabMenuOrganizer, mas para a teclaDown
            tabMenuOrganizer2 e@(jogo, pic, (TabMenu (pos,e2@(_,_,gm)))) x
                | pos == x = (jogo, pic, TabMenu (1,e2))
                | otherwise = (jogo, pic, (TabMenu ((pos+1),e2)))

    -- coordena a interação de Enter com os menus de pausa - clicar num dos botões do menu
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(_,pics, TabMenu (pos,e2@(jogo,_,gm))) = case gm of
    Play info   | pos == 1 -> return e2
                | pos == 2 -> return (estadoBase pics (Play info))
                | pos == 3 -> return e -- TODO: CONTROLS
                | pos == 4 -> return e -- TODO: SAVE
                | pos == 5 -> return (jogo,pics, TabMenu (1,(jogo,pics,Solver ((e,Nothing,Just [],0,0.0),1)))) -- retorna um estado de tabMenu em que o estado guardado é Solver (estadoPlay,1) - permite usar os mesmo controlos no menu do Solver
                | pos == 6 -> return (estadoBase pics (Menu infoMenu)) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    MapEdit n   | pos == 1 -> return e2
                | pos == 2 -> return e -- TODO
                | pos == 3 -> return e -- TODO
                | pos == 4 -> return e -- TODO: SAVE
                | pos == 5 -> return (estadoBase pics (Menu infoMenu)) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    MapSelector n   | pos == 1 -> return e2
                    | pos == 2 -> return e -- TODO
                    | pos == 3 -> return e -- TODO
                    | pos == 4 -> return (estadoBase pics (Menu infoMenu)) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    Solver ((emenu@(_,_,TabMenu (_,(ebefore@(jogo,_,Play (n,_,_,_))))),_,movelist,choice,_),state) -> case state of
            1   | pos == 1 -> return (fst(maps !! (n-1)),pics,Solver ((emenu,getMoves1,getMoves1,1,0.1),2)) -- começa a solução do início do nível atual, o número 500 é arbitrariamente grande, para não haver um limite máximo de moves ao resolver o nível; o tempo é 0.1 (para dar tempo ao jogador para se habituar)
                | pos == 2 -> return (jogo,pics,Solver ((emenu,getMoves2, getMoves2, 2,0.1),2)) -- começa a solução da posição atual
                | pos == 3 -> return emenu

            2   | pos == 1 -> return e2
                | pos == 2 -> return e -- TODO
                | pos == 3 -> return emenu
                | pos == 4 -> return (estadoBase pics (Menu infoMenu)) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

            3   | pos == 1 -> return $ case choice of
                                        1 -> (fst(maps !! (n-1)),pics,Solver ((emenu,movelist, movelist,1,0.1),2))
                                        2 -> (jogo,pics,Solver ((emenu, movelist, movelist, 2,0.1),2))
                | pos == 2 -> return emenu

            4   | pos == 1 -> return emenu

             -- dá a lista de movimentos para a solução. Permite calcular apenas uma vez a solução, mesmo que o jogador queira repetir a solução.
              
        where getMoves1 = resolveJogo 500 (fst (maps !! (n-1)))
              getMoves2 = resolveJogo 500 jogo

eventListener _ e@(_,_,TabMenu _) = return e
eventListener _ e@(_,_,Solver (_,2)) = return e

-- MapEditor (tecla pressionada)
eventListener (EventKey key Down _ _) (Jogo mapa (jogador@(Jogador (x,y) dir caixa)), pic, m@(MapEdit ((x1,y1), (x2,y2), peca, states, mode, sec))) 
    | key == SpecialKey KeyEnter = return (Jogo adicionaMapa jogador, pic, m)
    | key == SpecialKey KeyDelete = return (Jogo deleteMapa jogador, pic, m)
    | otherwise = return (Jogo mapa jogador2, pic, MapEdit ((x3,y3),(x4,y4),peca2,(state1,state2),mode2,0))
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
          mode2 | key == Char 'v'= if mode == 1 then 0 else 1
                | key == SpecialKey KeySpace = if mode == 2 then 0 else 2
                | key == Char 'p' = if mode == 3 then 0 else 3
                | otherwise = mode
          adicionaMapa :: Mapa
          adicionaMapa | newMap /= [] = constroiMapa newMap
                       | otherwise = mapa 
                       where newMap = ((peca,(x2,y2)):(desconstroiMapa mapa))
          deleteMapa :: Mapa
          deleteMapa | newMap /= [] = constroiMapa newMap
                     | otherwise = mapa
                     where newMap = (filter (\(p,c) -> c /= (x2,y2)) (desconstroiMapa mapa))

-- MapEditor (tecla libertada)
eventListener (EventKey key Up _ _) (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, (state1,state2), mode, sec)) =
    return (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, (state3,state4), mode, 0))
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

-- Menu
eventListener (EventKey key Down _ _) e@(jogo, pic, Menu (lista,atual))
    | key /= SpecialKey KeyEnter = return (jogo2, pic, Menu (lista,atual))
    | atual == New = do let (Jogo mapa jogador,coords) = jogoInicial 1
                            str = show ((desconstroiMapa mapa),jogador,(1,coords,0,0))
                        savePlay str
                        return (estadoBase pic (Play (1,(0,0),0,0)))
    | atual == Continue = return (estadoBase pic (MapSelector infoMapSelect))
    | atual == MapEdit1 = return (Jogo [] (Jogador (0,0) Este False),pic, MapEdit ((-13,-7),(0,0),Bloco,(None,None),0,0))
    | otherwise = return e
    


    where jogo2 | key == SpecialKey KeyUp = moveJogador jogo Trepar
                | key == SpecialKey KeyDown = moveJogador jogo InterageCaixa
                | key == SpecialKey KeyLeft = moveJogador jogo AndarEsquerda
                | key == SpecialKey KeyRight = moveJogador jogo AndarDireita
                | otherwise = jogo

-- Map selector e Play
eventListener (EventKey key Down _ _) e@(jogo, pic, gamemode) = case gamemode of 
        Play (mapa,coords,sec,mov) -> return (jogo2, pic, Play (mapa,coords,sec,mov+1))
        MapSelector (lista,atual) -> 
            let novoEstado2 | atual == 0 =  e
                            | otherwise = (estadoBase pic (Play (atual,(0,0),0,0)))
            in  if key == SpecialKey KeyEnter
                then return novoEstado2
                else return (jogo2, pic, MapSelector (lista,atual)) 

    where jogo2     | key == SpecialKey KeyUp = moveJogador jogo Trepar
                    | key == SpecialKey KeyDown = moveJogador jogo InterageCaixa
                    | key == SpecialKey KeyLeft = moveJogador jogo AndarEsquerda
                    | key == SpecialKey KeyRight = moveJogador jogo AndarDireita
                    | otherwise = jogo

eventListener _ s = return s

step :: Float -> Estado -> IO Estado
--Play
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, Play (n,coords,sec,mov))
    | (x,y) == coords =
        if n == numeroMapas
        then return (jogo, pic, Won (mov,sec))
        else do let (jogo2@(Jogo mapa jogador),coords2) = jogoInicial (n+1)
                    str = show ((desconstroiMapa mapa),jogador,(n+1,coords2,sec+time,mov))
                savePlay str
                return (jogo2, pic, Play (n+1,coords2,sec+time,mov))
    | otherwise = return (jogo, pic, Play (n,coords,sec+time,mov))

--Map edit
step time e@(jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, (state1,state2),mode,sec)) =
    if timeBetweenmodes < 250
    then return (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, (state1,state2),mode,sec+time))
    else return (jogo, pic, MapEdit ((x3,y3), (x4,y4), peca, (state1,state2),mode,0.235)) -- este número é para enquanto a pessoa não largou a tecla as peças se moverem mais rápido. se fosse 0 teria um delay chato
    where x3 | state1 == L = x1-1
             | state1 == R = x1+1
             | otherwise = x1
          y3 | state1 == U = y1+1
             | state1 == D = y1-1
             | otherwise = y1
          x4 | state2 == L = x2-1
             | state2 == R = x2+1
             | otherwise = x2
          y4 | state2 == D = y2+1
             | state2 == U = y2-1
             | otherwise = y2
          timeBetweenmodes = mod (round (sec*1000)) 500

-- Menu
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, Menu (lista,antigo)) = 
    return (jogo, pic, Menu (lista,atual))
    where atual = convert1 (x,y) lista

-- Map selector 
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, MapSelector (lista,antigo)) = 
    return (jogo, pic, MapSelector (lista,atual))
    where atual = convert2 (x,y) lista

-- Solver
step time e@(jogo, pic, Solver ((prevE, solution, movelist,choice,sec),2)) = 
    return $ case solution of  -- prevE será sempre tabMenu porque o Solver é aberto no tabMenu
                Nothing -> (jogo,pic,TabMenu (1,(jogo, pic, Solver ((prevE, Nothing, Nothing ,choice,0),4)))) -- os valores dentro de Solver são na maioria arbitrários (_,4)
                Just l  | null l -> (jogo,pic,TabMenu (1,(jogo, pic, Solver ((prevE, Nothing, movelist,choice,0),3)))) -- quando a lista de movimentos terminar, ou seja, o jogador chegar à porta, retorna o jogador para o menu antes de abrir o solver
                        | otherwise ->  if timeBetweenMoves <= 450
                                        then (jogo, pic, Solver ((prevE, Just l, movelist,choice,sec+time),2))
                                        else (jogo2, pic, Solver ((prevE, Just (tail l),movelist,choice,0),2))
                    where jogo2 = moveJogador jogo (head l)
                          timeBetweenMoves = mod (round (sec*1000)) 700 -- os movimentos serão efetuados a cada 650 milissegundos

-- Para outras situações
step _ e = return e

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
    Just menueditor <- loadJuicy "menu-editor.png"
    Just menusolvertype <- loadJuicy "menu-solvertype.png"
    Just menusolver <- loadJuicy "menu-solver.png"
    Just menusolverend <- loadJuicy "menu-solverend.png"
    Just menusolverimp <- loadJuicy "menu-solverimpossible.png"
    Just snowbg <- loadJuicy "snow.png"
    Just grassbg <- loadJuicy "greenfields.png"
    Just sandbg <- loadJuicy "sand.png"
    playIO window
           (white)
           fr
           (estadoBase (Pictures [(Scale 2.783 2.783 playerLeft),(Scale 2.783 2.783 playerRight), (Scale 0.186 0.186 brick), (Scale 2.0 2.0 crate), (Scale 0.674 0.451 door),menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg]) (Menu infoMenu)) -- 64x64 px
           draw
           eventListener
           step