{- |
Module      : Tarefa5_2021li1g025
Description : Interface gráfica do jogo
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

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
                         where jogo = Jogo (makeMap menuMap) (Jogador (9,4) Este False)
estadoBase pics (Play (n,coords,sec,mov)) = (jogo, pics, Play (n,coords2,0,0)) -- !no futuro o mov não será 0 mas poderá ser carregado dos dados guardados -- estado nos níveis
                                             where (jogo,coords2) = jogoInicial n
estadoBase pics (MapSelector n) = (jogo, pics, MapSelector n) -- devo meter o tempo e os movimentos a contar já no seletor ?
                        where jogo = Jogo (makeMap mapSelectorMap) (Jogador (2,4) Este False)
estadoBase pics gm = (Jogo [] (Jogador (0,0) Este False), pics, gm)

-- lista de jogos e pos da porta
maps :: [(Jogo,(Int,Int))]
maps = [(Jogo (makeMap map1) (Jogador (1,2) Este False),(6,2)),(Jogo (makeMap map2) (Jogador (0,2) Este False), (8,2)),(Jogo (makeMap map3) (Jogador (1,3) Este False), (9,5)),(Jogo (makeMap map4) (Jogador (1,1) Este False), (1,3)),
       (Jogo (makeMap map5) (Jogador (1,2) Este False),(4,0)),(Jogo (makeMap map6) (Jogador (1,1) Este False), (5,5)), (Jogo (makeMap map7) (Jogador (1,9) Este False),(21,1)), (Jogo (makeMap map8) (Jogador (6,3) Oeste False),(6,1)),
       (Jogo (makeMap map9) (Jogador (12,13) Este False),(8,13)),(Jogo (makeMap map10) (Jogador (1,1) Este True),(4,0))]
       
makeMap :: [(Peca,[(Int,Int)])] -> Mapa
makeMap m = constroiMapa (decompressMapa m)

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
data MenuStates = New | Continue | LevelSelector | MapEdit1 | Normal 
    deriving (Eq,Show)
    
type MenuInfo = ([(Int,Int,MenuStates)],MenuStates)

infoMenu :: ([(Int,Int,MenuStates)],MenuStates)
infoMenu = ([(3,8, New),(7,8,Continue),(11,8,LevelSelector),(15,8,MapEdit1)], Normal)

-- map selector --

type MapSelectInfo = ([(Int,Int,Int)],Int) -- coordenadas de portas e nº dos níveis correspondentes, acumulador para a posição do jogador, TODO: parâmetro- nº do último nível guardado

infoMapSelect :: MapSelectInfo -- não inclui o nível do último nível guardado porque será adicionado mais tarde com IO.
infoMapSelect = ([(4,4,1),(6,4,2),(8,4,3),(10,4,4),(12,4,5),(21,1,6),(23,1,7),(25,1,8),(27,1,9),(29,1,10)],0)

-- Map editor -- 

type MapEditInfo = ((Int,Int), (Int,Int), Peca, (Int,Int,Int,Int,Int),Int,Float,Int) -- posicao absoluta do mapa, posicao absoluta da peca, peca selecionada, informações para ajudar no movimento contínuo, Int que representa o modo, segundos passados, numero do mapa (a contar do 1)

-- TabMenu --

type TabMenuInfo = (Int,Estado) -- posição atual do cursor pela ordem de cima para baixo na lista, modo de jogo em que é aberto o Tab.

-- Solver --

-- estado antes de entrar no TabMenu (antes de entrar no Solver) , lista de movimentos de resolveJogo, lista para onde vão os movimentos já efetuados (caso o jogador queria repetir a solução), número que regista a opção de começar a solução do início ou do lugar atual, tempo decorrido (para espaçar os movimentos da solução) , número que distingue o menu em que se pergunta onde deve começar a solução e a própria solução
type SolverInfo = ((Estado, Maybe [Movimento], Maybe [Movimento], Int, Float),Int) 
window :: Display
window = FullScreen

fr :: Int
fr = 50

-- | Escreve uma string no final do ficheiro
appendSaveGame :: String -- ^ nome do ficheiro
    -> String -- ^ string a gravar
    -> IO ()
appendSaveGame file s = appendFile file s 

{- | Escreve numa dada linha de um ficheiro (começando na linha 0)

Se o ficheiro não tiver as linhas necessárias irá ser preenchido com linhas vazias
-}
saveGame :: String -- ^ nome do ficheiro
    -> String -- ^ string a gravar
    -> Int -- ^ numero da linha a gravar (a primeira linha é a 0)
    -> IO ()
saveGame file str n = do savedata <- readFile file
                         let l = lines (savedata)
                             len = length l
                         if n > len-1 
                         then appendFile file ((replicate (n-len) '\n')++str)
                         else do let (a,b) = splitAt n l
                                 writeFile file (unlines (a ++ (str:(tail b))))

loadGameEditor :: Int -> IO (Jogo,MapEditInfo)
loadGameEditor n = do txt <- readFile "SaveGameMapEditor.txt"
                      let (mapa,jogador,coords1,coords2,peca) = read ((lines txt) !! n) :: ([(Peca,[(Int,Int)])],Jogador,(Int,Int),(Int,Int),Peca)
                      return (Jogo (makeMap mapa) jogador, (coords1,coords2,peca,(0,0,0,0,0),0,0,n+1))


compressMapa :: [(Peca,(Int,Int))] -> [(Peca,[(Int,Int)])]
compressMapa [] = []
compressMapa mapa = [(Bloco,a),(Caixa,b),(Porta,c),(Picos,d)]
                    where (a,b,c,d) = foldr (\(peca,coords) (r1,r2,r3,r4) -> case peca of Bloco -> (coords:r1,r2,r3,r4)
                                                                                          Caixa -> (r1,coords:r2,r3,r4)
                                                                                          Porta -> (r1,r2,coords:r3,r4)
                                                                                          Picos -> (r1,r2,r3,coords:r4)) ([],[],[],[]) mapa

decompressMapa :: [(Peca,[(Int,Int)])] -> [(Peca,(Int,Int))]
decompressMapa [] = []
decompressMapa [(Bloco,a),(Caixa,b),(Porta,c),(Picos,d)] = blocos ++ caixas ++ portas ++ picos
                    where blocos = map (\coords -> (Bloco,coords)) a
                          caixas = map (\coords -> (Caixa,coords)) b
                          portas = map (\coords -> (Porta,coords)) c
                          picos = map (\coords -> (Picos,coords)) d

adicionaMapa :: (Int,Int) -> Peca -> Mapa -> Mapa
adicionaMapa (x,y) peca mapa | x < 0 || y < 0 = mapa
                             | newMap /= [] = constroiMapa newMap
                             | otherwise = [] 
                             where newMap = ((peca,(x,y)):(desconstroiMapa mapa))
deleteMapa :: (Int,Int) -> Mapa -> Mapa
deleteMapa (x,y) mapa | x < 0 || y < 0 = mapa
                      | newMap /= [] = constroiMapa newMap
                      | otherwise = []
                      where newMap = (filter (\(p,c) -> c /= (x,y)) (desconstroiMapa mapa))

-- offset é para que o getPictures saiba onde começar a desenhar as imagens (o mais à esquerda possível)
draw :: Estado -> IO Picture
-- MapEditor transferido para play (se mode == 3)
draw (jogo, pics, MapEdit ((x1,y1), (x2,y2), peca, _, 3, sec,_)) =
    draw (jogo, pics, Play (-1,(-1,-1),-1,-1))

-- MapEditor
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door, spikes,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,overwrite,loadquestion,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]), MapEdit ((x1,y1), (x2,y2), peca, _, mode, _,_)) =
    --Translate (-500) 200 (Pictures [Scale 0.1 0.1 (Text (show (Jogo mapa (Jogador (x,y) dir caixa)))), (Translate 0 (-150) (Scale 0.1 0.1 (Text (show (x2,y2)))))])
    return (Pictures [snowbg, (Scale scale scale (Pictures ((Translate offsetxJogador offsetyJogador picFinal):(linha1):(linha2):(getPictures [brick, crate, door, spikes] (64*x1f,64*x1f,((-64)*y1f)) map2) ++ 
        [Translate offsetx offsety (Pictures [pecaPic, outline])] ++ [Scale 0.25 0.25 (Translate (-200) 150 texto)])))])
    where (map2,scale) | mode == 2 = (mapa,0.25)
                       | otherwise = (mapa,1) -- ((map (\linha -> take 20 linha) (map (\linha -> drop (x1-10) linha) mapa)),1)
          pecaPic = case peca of Bloco -> brick
                                 Caixa -> crate
                                 Porta -> door
                                 Picos -> spikes
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
    where bg = pics !! 14

-- TabMenu
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door, spikes ,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,overwrite,loadquestion,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]), (TabMenu (posMenu,e2@(_,_,gm)))) = 
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
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door,spikes,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,overwrite,loadquestion,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]), gamemode)
    | xmax <= 21 = let offset = getOffset mapa
                       map3 = getLines y mapa
                       offsetY = getOffsetY map3 in -- scrolling desnecessário para mapas pequenos, temos de centrar o mapa consoante xmax
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) ((-y2*64)+offsetY) picFinal]))

    | x <= 10 = let map2 = map (\linha -> take 21 linha) mapa
                    map3 = getLines y map2
                    offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) (((-y2*64))+offsetY) picFinal]))
    
    | xf >= (xmax - 10) = let wallDistance = (round xmax)-20
                              map2 = map (\linha -> drop wallDistance linha) mapa
                              x2 = (xf-(fromIntegral wallDistance)) -- converte coordenadas do jogador em coordenadas no ecrã
                              map3 = getLines y map2
                              offsetY = getOffsetY map3 in
        --debug Scale 0.1 0.1 (Text (show map2))
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate ((x2*64)+offset) (((-y2*64))+offsetY) picFinal]))
    
    | otherwise = let map2 = getLines y mapa
                      map3 = (map (\linha -> take 21 linha) (map (\linha -> drop (x-10) linha) map2))
                      offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate 0 (((-y2*64))+offsetY) picFinal]))

    where xmax = fromIntegral (length (head mapa))
          ymax = (length mapa)-1 -- +1 que o valor maximo de y

          -- offset horizontal
          getOffset :: Mapa -> Float -- assume se que o mapa já só tem 20 blocos de comprimento
          getOffset map3 = ((-64)*((fromIntegral (length (head map3)))/2))
          offset = -640

          xf = fromIntegral x
          yf = fromIntegral y

          -- imagem do jogador
          player = (case dir of Este -> playerRight
                                Oeste -> playerLeft)
          picFinal 
            | caixa = Pictures [player, Translate 0 64 crate] -- n tem em conta os casos em que o jogador vai para uma porta a segurar uma caixa
            | otherwise = case gamemode of (Menu (_,Normal)) -> player
                                           (Menu (_,zona))  | zona == New -> Pictures [player, scale (0.4) (0.4) (Translate (-320) (96) (Text "New Game"))]
                                                            | zona == Continue -> Pictures [player, scale (0.5) (0.5) (Translate (-256) (96) (Text "Continue"))]
                                                            | zona == LevelSelector -> Pictures [player, scale (0.4) (0.4) (Translate (-384) (96) (Text "Level Selector"))]
                                                            | zona == MapEdit1 -> Pictures [player, scale (0.4) (0.4) (Translate (-288) (96) (Text "Map Editor"))]
                                           (MapSelector (_,0)) -> player
                                           (MapSelector (_,nivel)) -> Pictures [player, scale (0.5) (0.5) (Translate (-208) (96) (Text ("Level " ++ (show nivel))))]
                                           _ -> player
          -- offset vertical
          getOffsetY :: Mapa -> Float
          getOffsetY map3 = ((64)*((ymax2/2))-64)
                          where ymax2 = fromIntegral (length map3)
          y2 = fromIntegral (mod y 15)
          getLines :: Int -> Mapa -> Mapa -- devolve linhas do mapa a renderizar consoante altura do jogador
          getLines y l = drop (15*(section-1)) (take ((15*section)+1) l)
                       where section = (div y 15)+1

{- | Transforma o mapa dado numa lista de imagens

Esta começa a desenhar as peças aplicando-lhes uma translação Translate (offset x) (offset y), de seguida somando ao acumulador 64 (comprimento da peça)

Ao chegar ao fim de uma linha, é subtraído 64 ao offset vertical e o acumulador é reiniciado

Assim, as peças são desenhadas da esquerda para a direita até ao fim da linha, e de seguida de cima para baixo

O valor inicial do acumulador deve ser o mesmo do que o do offset horizontal
-}
getPictures :: [Picture] -- ^ Imagens a ser utilizadas (bloco, caixa, porta)
    -> (Float,Float,Float) -- ^ Offsets (horizontal, acumulador do horizontal, vertical)
    -> Mapa -- ^ O mapa a desenhar 
    -> [Picture] -- ^ O mapa transformado em imagens
getPictures _ _ [] = [Blank] -- é preciso o blank??
getPictures pics@[brick, crate, door, spikes] (x,_,y) ([]:mapa) = getPictures pics (x,x,y-64) mapa -- reset x, go down a line
getPictures pics@[brick, crate, door, spikes] (x,x2,y) ((peca:linha):mapa) =
    (Translate x2 y image):(getPictures pics (x,x2+64,y) (linha:mapa))
    where image | peca == Bloco = brick
                | peca == Caixa = crate
                | peca == Porta = door
                | peca == Picos = spikes
                | otherwise = Blank

eventListener :: Event -> Estado -> IO Estado
-- esta secção são comandos debug ou reset
-- comando para exit temporário
eventListener (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
-- menu
eventListener (EventKey (SpecialKey KeyBackspace) Down _ _) (_, pic, _) = return (estadoBase pic (Menu infoMenu))
-- põe as informações do modo de jogo no terminal
eventListener (EventKey (Char 'i') Down _ _) e@(_,_,info) = do putStrLn (show info)
                                                               return e
 -- primeiro nível
eventListener (EventKey (SpecialKey KeyF1) Down _ _) (_, pic, _) = return (estadoBase pic (Play (1,(0,0),0,0)))
-- editor de mapas vazio
eventListener (EventKey (SpecialKey KeyF2) Down _ _) (_, pic, _) = return ((Jogo [] (Jogador (0,0) Este False), pic, MapEdit ((-13,-7),(0,0),Bloco,(0,0,0,0,0),0,0,1)))
-- mapa alto para testar offset vertical
eventListener (EventKey (SpecialKey KeyF3) Down _ _) (_, pic, _) = return ((Jogo (makeMap stairMap) (Jogador (0,28) Este False), pic, Play (0,(0,0),0,0)))
-- toggle da caixa do jogador
eventListener (EventKey (SpecialKey KeyF5) Down _ _) (Jogo mapa (Jogador pos dir caixa), pic, gm) = return (Jogo mapa (Jogador pos dir (not caixa)), pic, gm)
 -- mapa comprido para testar offset horizontal (é um dos mapa base)
eventListener (EventKey (SpecialKey KeyF6) Down _ _) (_, pic, _) = return (estadoBase pic (Play (5,(0,0),0,0)))
-- load do primeiro mapa no ficheiro do editor (pode crashar se não houver nenhum (?))
eventListener (EventKey (SpecialKey KeyF7) Down _ _) (_, pic, _) = do (jogo,gm) <- loadGameEditor 0
                                                                      return (jogo, pic, MapEdit gm)
-- mapa anterior no play (crasha se já for o primeiro mapa)
eventListener (EventKey (SpecialKey KeyF8) Down _ _) (_, pic, Play (n,_,_,_)) = return (estadoBase pic (Play (n-1,(0,0),0,0)))
-- próximo mapa no play (crasha se já for o último mapa)
eventListener (EventKey (SpecialKey KeyF9) Down _ _) (_, pic, Play (n,_,_,_)) = return (estadoBase pic (Play (n+1,(0,0),0,0)))


-- Quick save e load
eventListener (EventKey (Char 's') Down _ _) e@(Jogo mapa jogador, _, Play info@(n,_,_,_)) -- escrever estado Play
    = do let str = show ((compressMapa (desconstroiMapa mapa)),jogador,info)
         saveGame "SaveGamePlay.txt" str n
         return e
eventListener (EventKey (Char 'l') Down _ _) (_, pic, Play (n,_,_,_)) -- carregar estado Play
    = do texto <- readFile "SaveGamePlay.txt"
         let (mapa,jogador,info) = read ((lines (texto)) !! (n-1))
         return (Jogo (constroiMapa (decompressMapa mapa)) jogador, pic, Play info)
eventListener (EventKey (Char 'l') Down _ _) e@(_, pic, MapEdit (_,_,_,(_,_,_,_,a),_,_,n)) | a == 2 = do (jogo,gm) <- loadGameEditor (n-1)
                                                                                                         return (jogo, pic, MapEdit gm)
                                                                                           | otherwise = return e
eventListener (EventKey (Char 'r') Down _ _) e@(_, pic, MapEdit (_,_,_,(_,_,_,_,a),_,_,n)) | a == 2 = do (jogo,gm) <- loadGameEditor (n-1) -- l e r fazem a mesma coisa, pelo menos por agora
                                                                                                         return (jogo, pic, MapEdit gm)
                                                                                           | otherwise = return e
{-                                                                                           
eventListener (EventKey (Char 's') Down _ _) e@(jogo@(Jogo mapa jogador), pic, MapEdit gm@(c1,c2,peca,(_,_,_,_,a),_,_,n)) | a == 2 = do let str = show (compressMapa (desconstroiMapa mapa),jogador,c1,c2,peca)
                                                                                                                                        saveGame "SaveGameMapEditor.txt" str (n-1)
                                                                                                                                        return (jogo, pic, MapEdit gm)
                                                                                                                         | otherwise = return e
faz com que depois ao carregar no s não mova o mapa -}
-- reset do Play
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, gm@(Play (a,_,sec,mov))) = do savedata <- readFile "SaveGamePlay.txt"
                                                                                       let (_,_,(_,b,_,_)) = read (head (lines savedata))::([(Peca,[(Int,Int)])],Jogador,PlayInfo) -- especificar o tipo para o read não fazer conflito
                                                                                           (jogo,_,_) = estadoBase pic gm
                                                                                       return (jogo,pic,Play (a,b,sec,mov))
--eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, Won _) = return (estadoBase pic (Play (1,(0,0),0,0)))
--eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, gm) = return (estadoBase pic gm)

-- Won (basta carregar em qualquer tecla)
eventListener (EventKey _ Down _ _) (_, pic, Won _) = return (estadoBase pic (Menu infoMenu))

-- MapEditor (mode == 3, jogar o mapa, usa o Play para permitir jogar)
eventListener keyinfo@(EventKey key Down _ _) (jogo, pic, m@(MapEdit (c1, c2, peca, states, 3, sec,n))) =
    case key of SpecialKey KeyTab -> undefined -- TODO
                Char 'p' -> return (jogo, pic, MapEdit (c1,c2,peca,states,0,sec,n))
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
eventListener (EventKey key Down _ _) e@(Jogo mapa (jogador@(Jogador (x,y) dir caixa)), pic, MapEdit ((x1,y1), (x2,y2), peca, (oRhor1,oRvert1,oRhor2,oRvert2,oRaction), mode, sec, n)) 
    | key == Char 'p' = if mode == 3 then do (jogo,gm) <- loadGameEditor (n-1)
                                             return (jogo,pic,MapEdit gm)
                                     else let mode3 | length mapa > 0 = 3 
                                                    | otherwise = 0
                                          in 
                                          do let str = show (compressMapa (desconstroiMapa mapa),jogador,(x1,y1),(x2,y2),peca)
                                             saveGame "SaveGameMapEditor.txt" str (n-1)
                                             return (Jogo mapa2 jogador2, pic, MapEdit ((x1,y1), (x2,y2), peca, (oRhor1,oRvert1,oRhor2,oRvert2,oRaction), mode3, sec, n))
    | otherwise = return (Jogo mapa2 jogador2, pic, MapEdit ((x3,y3),(x4,y4),peca2,(hor1,vert1,hor2,vert2,action),mode2,sec2,n))
    where (x3,hor1) | key == Char 'd' = (x1-1,-1)
                    | key == Char 'a' = (x1+1,1)
                    | otherwise = (x1,oRhor1)
          (y3,vert1) | key == Char 'w' = (y1+1,1)
                     | key == Char 's' = (y1-1,-1)
                     | otherwise = (y1,oRvert1)
          (x4,hor2) | key == SpecialKey KeyLeft = (x2-1,-1)
                    | key == SpecialKey KeyRight = (x2+1,1)
                    | otherwise = (x2,oRhor2)
          (y4,vert2) | key == SpecialKey KeyUp = (y2-1,-1)
                     | key == SpecialKey KeyDown = (y2+1,1)
                     | otherwise = (y2,oRvert2)
          (action,mapa2) | key == SpecialKey KeyEnter = (1,adicionaMapa (x2,y2) peca mapa)
                         | key == SpecialKey KeyDelete = (-1,deleteMapa (x2,y2) mapa)
                         | key == SpecialKey KeyCtrlL = (2,mapa)
                         | oRaction == 2 = (2,shiftedMap)
                         | otherwise = (oRaction,mapa)
                         where shiftedMap | mapa == [] = []
                                          | key == SpecialKey KeyUp = tail mapa
                                          | key == SpecialKey KeyDown = (replicate (length (head mapa)) Vazio):(mapa)
                                          | key == SpecialKey KeyRight = map (\linha -> Vazio:linha) mapa
                                          | key == SpecialKey KeyLeft = map (\linha -> tail linha) mapa
                                          | otherwise = mapa
          jogador2 | key == Char '5' = Jogador (x2,y2) dir caixa
                   | key == Char '6' = Jogador (x,y) dir (not caixa)
                   | key == Char '7' = Jogador (x,y) (if dir == Este then Oeste else Este) caixa
                   | otherwise = jogador
          peca2 | key == Char '1' = Bloco
                | key == Char '2' = Caixa
                | key == Char '3' = Porta
                | key == Char '4' = Picos
                | otherwise = peca 
          mode2 | key == Char 'v'= if mode == 1 then 0 else 1
                | key == SpecialKey KeySpace = if mode == 2 then 0 else 2
                -- | key == Char 'p' = if mode == 3 then 0 else 
                  --   if length mapa > 0 then 3 else 0
                | otherwise = mode
          sec2 | oRhor1 == 0 && oRhor2 == 0 && oRvert1 == 0 && oRvert2 == 0 = 0
               | otherwise = sec

-- MapEditor (tecla libertada)
eventListener (EventKey key Up _ _) (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, (oRhor1,oRvert1,oRhor2,oRvert2,oRaction), mode, sec, n))
    | key == SpecialKey KeyCtrlL = return (jogo,pic, MapEdit ((x1,y1),(x2,y2),peca,(oRhor1,oRvert1,oRhor2,oRvert2,0),mode,sec,n))
    | otherwise = return (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, mov, mode, 0.235,n))
    where mov | key == Char 'w'|| key == Char 's' = (oRhor1,0,oRhor2,oRvert2,oRaction)
              | key == Char 'a' || key == Char 'd' = (0,oRvert1,oRhor2,oRvert2,oRaction)
              | key == SpecialKey KeyUp || key == SpecialKey KeyDown = (oRhor1,oRvert1,oRhor2,0,oRaction)
              | key == SpecialKey KeyLeft || key == SpecialKey KeyRight = (oRhor1,oRvert1,0,oRvert2,oRaction)
              | key == SpecialKey KeyEnter || key == SpecialKey KeyDelete = (oRhor1,oRvert1,oRhor2,oRvert2,0)
              | otherwise = (oRhor1,oRvert1,oRhor2,oRvert2,oRaction)

-- Menu
eventListener (EventKey key Down _ _) e@(jogo, pic, Menu (lista,atual))
    | key /= SpecialKey KeyEnter = return (jogo2, pic, Menu (lista,atual))
    | atual == New = do let (Jogo mapa jogador,coords) = jogoInicial 1
                            str = show (compressMapa (desconstroiMapa mapa),jogador,(1,coords,0,0))
                        saveGame "SaveGamePlay.txt" str 0
                        return (estadoBase pic (Play (1,(0,0),0,0)))
    | atual == Continue = return e -- (estadoBase pic (SaveLoadScreen ((1,1),(loadFile "SaveGamePlay.txt"))))
    | atual == LevelSelector = return (estadoBase pic (MapSelector infoMapSelect))
    | atual == MapEdit1 = return (Jogo [] (Jogador (0,0) Este False),pic, MapEdit ((-13,-7),(0,0),Bloco,(0,0,0,0,0),0,0,1))
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
step time (jogo@(Jogo mapa (Jogador (x,y) _ _)), pic, gm@(Play (n,coords,sec,mov)))
    | (x,y) == coords =
        if n == numeroMapas
        then return (jogo, pic, Won (mov,sec))
        else do let (jogo2@(Jogo mapa jogador),coords2) = jogoInicial (n+1)
                    str = show (compressMapa (desconstroiMapa mapa),jogador,(n+1,coords2,sec+time,mov))
                saveGame "SaveGamePlay.txt" str 0
                return (jogo2, pic, Play (n+1,coords2,sec+time,mov))
    | (mapa !! y) !! x == Picos = do savedata <- readFile "SaveGamePlay.txt"
                                     let (_,_,info) = read (head (lines savedata))::([(Peca,[(Int,Int)])],Jogador,PlayInfo) -- especificar o tipo para o read não fazer conflito
                                         (jogo,_,_) = estadoBase pic gm
                                     return (jogo,pic,Play info)
    | otherwise = return (jogo, pic, Play (n,coords,sec+time,mov))

--Map edit com mode == 3 (jogar o mapa)
step time e@(Jogo mapa (Jogador (x,y) _ _), pic, MapEdit (_, _, _, _,3,_,n))
    | (pecaAtual == Picos || pecaAtual == Porta) = do (jogo,(c1,c2,peca,mov,_,sec,_)) <- loadGameEditor (n-1)
                                                      return (jogo, pic, MapEdit (c1,c2,peca,mov,3,sec,n))
    | otherwise = return e
    where pecaAtual = (mapa !! y) !! x


--Map edit
step time e@(jogo@(Jogo mapa jogador), pic, MapEdit ((x1,y1), (x2,y2), peca, mov@(hor1,vert1,hor2,vert2,action),mode,sec,n)) =
    if timeBetweenmodes < 250
    then return (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, mov,mode,sec+time,n))
    else return (Jogo mapa2 jogador, pic, MapEdit ((x1+hor1,y1+vert1), (x2+hor2,y2+vert2), peca, mov,mode,0.235,n))
    where mapa2 | action == 1 = adicionaMapa (x2,y2) peca mapa
                | action == -1 = deleteMapa (x2,y2) mapa
                | otherwise = mapa
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
    Just spikes <- loadJuicy "spikes.png"
    Just savescreen <- loadJuicy "savefile.png"
    Just overwrite <- loadJuicy "overwrite.png"
    Just loadquestion <- loadJuicy "load.png"
    Just arrowLeft <- loadJuicy "arrow-red-left.png"
    Just arrowRight <- loadJuicy "arrow-red-right.png"
    playIO window
           (white)
           fr
           (estadoBase (Pictures [(Scale 2.783 2.783 playerLeft),(Scale 2.783 2.783 playerRight), (Scale 0.186 0.186 brick), (Scale 2.0 2.0 crate), (Scale 0.674 0.451 door), (Scale 0.0762 0.0745 spikes),menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,overwrite,loadquestion,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]) (Menu infoMenu)) -- 64x64 px
           draw
           eventListener
           step