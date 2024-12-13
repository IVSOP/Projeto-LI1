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
import Niveis

-- * Data e tipos principais

-- | Estado usado pelo gloss, contendo um jogo, imagens e um modo de jogo 
type Estado = (Jogo, Picture, GameMode)

-- | Os diferentes modos de jogo com as suas respetivas informações
data GameMode = Menu MenuInfo -- ^ Menu (o menu inicial onde o jogador pode ser controlado)
                | MapSelector MapSelectInfo -- ^ Seletor de mapas
                | Play PlayInfo -- ^ O jogo propriamente dito
                | MapEdit MapEditInfo -- ^ Editor de mapas
                | Won WonInfo -- ^ Situação em que venceu o jogo
                | TabMenu TabMenuInfo -- ^ Menu interativo
                | Solver SolverInfo -- ^ Resolvedor do jogo
                | SaveLoadPlay SLPlayInfo -- ^ Menu para carregar e continuar jogos do Play
                | SaveLoadEditor SLEditorInfo -- ^ Menu para carregar e continuar jogos do Editor
    deriving (Eq,Show)

-- | (número de movimentos, tempo em segundos)
type WonInfo = (Int,Float)

-- | (número do mapa, tempo em segundos, número de movimentos)
type PlayInfo = (Int,Float,Int) -- mapa, coords da porta, segundos, movimentos

{- |  (lista com as coordenadas das portas no menu e os respetivos modos de jogo, modo de jogo à parte)

      O último valor permite distinguir se:

 * O jogador está em cima de uma porta

 * O jogador está em qualquer outro lugar no menu.

 Assim, é possível:

 *  criar o texto que aparece em cima de cada uma das portas, referente ao modo de jogo a que correspondem.

 * pressionar o "Enter" para entrar no modo de jogo, ou usar teclas de movimento par deslocar-se para outra porta. 
 -}
type MenuInfo = ([(Int,Int,MenuStates)],MenuStates)

{- | Indica a porta em que o jogador está atualmente, no menu.

Com isto presente no estado, a função 'step' seria a única responsável por determinar se o jogador estava numa porta do menu e em qual, de modo que as outras funções usam essa informação.
-}
data MenuStates = New -- ^ Novo jogo
                  | Continue -- ^ Continuar jogo
                  | LevelSelector -- ^ Seletor de níveis
                  | MapEdit1 -- ^ Editor de mapas (o 1 no nome é devido a MapEdit já existir)
                  | Normal -- ^ Nenhuma porta
    deriving (Eq,Show)

{- | (lista com as coordenadas de portas e nº dos níveis correspondentes, valor à parte)

De modo idêntico ao tipo 'MenuInfo', o último valor permite coordenar a posição em que o jogador está atualmente. 

É atualizado pela função 'step', para ser usada nas outras funções.

-}
type MapSelectInfo = ([(Int,Int,Int)],Int)

{- | (coordenadas da câmera, coordenadas do cursor, Peca selecionada, informações para ajudar no movimento contínuo, número que representa o modo, tempo em segundos, número do mapa)

As informações sobre o movimento contínuo representam (mover câmera verticalmente, mover câmera horizontalmente, mover cursor verticalmente, mover cursor horizontalmente, adicionar peça | tirar peça | ctrl está a ser pressionado)

Estas permitem guardar um valor que indica que uma tecla está a ser segurada

Os modos podem ser 0:normal | 1:validar o mapa | 2:zoom | 3:jogar o mapa
-}
type MapEditInfo = ((Int,Int), (Int,Int), Peca, (Int,Int,Int,Int,Int),Int,Float,Int)

{- | (posição atual no menu, estado do jogo antes de entrar no menu interativo)

O primeiro valor permite alternar entre as várias opções que surgem no menu interativo, a contar de cima para baixo. Atualizado pela função 'eventListener'.

O segundo valor permite guardar o estado antes de entrar no 'TabMenu' e recuperá-lo quando sair do 'TabMenu'
-}
type TabMenuInfo = (Int,Estado)

{- |  Os diferentes valores correspondem a:

* estado do menu interativo (antes de selecionar a opção 'Solver');
* lista de movimentos obtida de 'resolveJogo';
* lista para onde vão os movimentos já efetuados (caso o jogador queria repetir a solução);
* número que regista a opção de começar a solução do início ou do lugar atual; 
* tempo decorrido (para espaçar os movimentos da solução);
* número que distingue o menu em que se pergunta onde deve começar a solução e a própria animação da solução ;

-}
type SolverInfo = ((Estado, Maybe [Movimento], Maybe [Movimento], Int, Float),Int) 

{- | (primeiro Int: 1 -> load, 2-> save, segundo Int: número do savefile, terceiro Int: dados importados do "SaveGamePlay.txt")
-}

type SLPlayInfo = ((Int,Int),(Jogo,PlayInfo))

{- | (primeiro Int: 1 -> load, 2-> save, segundo Int: número do savefile, terceiro Int: dados importados do "SaveGameEditor.txt")
-}
type SLEditorInfo = ((Int,Int),(Jogo,MapEditInfo))

-- | Lista de jogos de cada mapa
jogos :: [Jogo]
jogos = [Jogo (makeMap map1) (Jogador (1,2) Este False),Jogo (makeMap map2) (Jogador (0,2) Este False),Jogo (makeMap map3) (Jogador (1,3) Este False),Jogo (makeMap map4) (Jogador (1,1) Este False),
        Jogo (makeMap map5) (Jogador (1,2) Este False),Jogo (makeMap map6) (Jogador (1,1) Este False), Jogo (makeMap map7) (Jogador (1,9) Este False), Jogo (makeMap map8) (Jogador (6,3) Oeste False),
        Jogo (makeMap map9) (Jogador (12,13) Este False),Jogo (makeMap map10) (Jogador (1,1) Este True),j1,j2,j3,j4,j5]

-- | Lista com as portas e os modos de jogo correspondentes no menu.
infoMenu :: ([(Int,Int,MenuStates)],MenuStates)
infoMenu = ([(3,8, New),(7,8,Continue),(11,8,LevelSelector),(15,8,MapEdit1)], Normal)

-- | Lista com as portas e os níveis correspondentes no seletor de mapas
infoMapSelect :: MapSelectInfo
infoMapSelect = ([(4,4,1),(6,4,2),(8,4,3),(10,4,4),(12,4,5),(21,1,6),(23,1,7),(25,1,8),(27,1,9),(29,1,10)],0)

numeroMapas :: Int
numeroMapas = length jogos

-- | converte o jogo para a resolução fullscreen
window :: Display
window = FullScreen

fr :: Int
fr = 50

-- * Funções auxiliares

{-| Dadas as imagens e um Gamemode, devolve o seu estado base

Por exemplo, para Play (1,...), sendo 1 o número do mapa, irá devolver um estado inicial com esse Mapa, mas com os segundos e movimentos zerados
-}
estadoBase :: Picture -- ^ Imagens
    -> GameMode -- ^ O gamemode que se pretende obter
    -> Estado -- ^ Estado coreresponente
estadoBase pics (Menu n) = (jogo, pics, Menu n) -- estado inicial, quando o jogador está no menu, obtido de infoMenu.
                         where jogo = Jogo (makeMap menuMap) (Jogador (9,4) Este False)
estadoBase pics (Play (n,sec,mov)) = (jogos !! (n-1), pics, Play (n,0,0)) -- !no futuro o mov não será 0 mas poderá ser carregado dos dados guardados -- estado nos níveis
estadoBase pics (MapSelector n) = (jogo, pics, MapSelector n) -- devo meter o tempo e os movimentos a contar já no seletor ?
                        where jogo = Jogo (makeMap mapSelectorMap) (Jogador (2,4) Este False)
estadoBase pics gm = (Jogo [] (Jogador (0,0) Este False), pics, gm)

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
    -> Int -- ^ numero da linha a gravar
    -> IO ()
saveGame file str n = do savedata <- readFile file
                         let l = lines (savedata)
                             len = length l
                         if n > len-1 
                         then appendFile file ((replicate (n-len) '\n')++str)
                         else do let (a,b) = splitAt n l
                                 writeFile file (unlines (a ++ (str:(tail b))))

-- | Carrega um estado do editor de mapas
loadGameEditor :: Int -- ^ Número da linha (número do mapa - 1)
    -> IO (Jogo,MapEditInfo)
loadGameEditor n = do txt <- readFile "SaveGameMapEditor.txt"
                      let contents = lines txt
                          (mapa,jogador,coords1,coords2,peca) | n+1 > length contents = ([],Jogador (0,0) Este False,(-13,-7),(0,0),Bloco) -- falta caso para quando a linha é vazia
                                                              | otherwise = do let linha = (contents !! n)
                                                                               if linha == [] then ([],Jogador (0,0) Este False,(-13,-7),(0,0),Bloco)
                                                                               else read linha :: ([(Peca,[(Int,Int)])],Jogador,(Int,Int),(Int,Int),Peca)
                      return (Jogo (makeMap mapa) jogador, (coords1,coords2,peca,(0,0,0,0,0),0,0,n+1))


-- | Carrega um estado do jogo propriamente dito
loadGamePlay :: Int -- ^ Número da linha (número do mapa - 1)
    -> IO (Jogo,PlayInfo)
loadGamePlay n = do txt <- readFile "SaveGamePlay.txt"
                    let contents = lines txt
                        (mapa,jogador,info) | n+1 > length contents = ([],Jogador (0,0) Este False, (0,0,0))
                                            | otherwise = do let linha = (contents !! n)
                                                             if linha == [] then ([],Jogador (0,0) Este False, (0,0,0))
                                                             else read linha :: ([(Peca,[(Int,Int)])],Jogador,PlayInfo)
                    return (Jogo (makeMap mapa) jogador, info)

-- | Transforma a lista de peças do mapa (já desconstruído) em listas da forma [(Peca,(lista das coordenadas em que ela ocorre))]
compressMapa :: [(Peca,(Int,Int))] -> [(Peca,[(Int,Int)])]
compressMapa [] = []
compressMapa mapa = [(Bloco,a),(Caixa,b),(Porta,c),(Picos,d)]
                    where (a,b,c,d) = foldr (\(peca,coords) (r1,r2,r3,r4) -> case peca of Bloco -> (coords:r1,r2,r3,r4)
                                                                                          Caixa -> (r1,coords:r2,r3,r4)
                                                                                          Porta -> (r1,r2,coords:r3,r4)
                                                                                          Picos -> (r1,r2,r3,coords:r4)) ([],[],[],[]) mapa
-- | Transforma o mapa comprimido num mapa desconstruído
decompressMapa :: [(Peca,[(Int,Int)])] -> [(Peca,(Int,Int))]
decompressMapa [] = []
decompressMapa [(Bloco,a),(Caixa,b),(Porta,c),(Picos,d)] = blocos ++ caixas ++ portas ++ picos
                    where blocos = map (\coords -> (Bloco,coords)) a
                          caixas = map (\coords -> (Caixa,coords)) b
                          portas = map (\coords -> (Porta,coords)) c
                          picos = map (\coords -> (Picos,coords)) d

-- | Simultaneamente faz a decompressão e a construção de um mapa na sua forma comprimida
makeMap :: [(Peca,[(Int,Int)])] -> Mapa
makeMap m = constroiMapa (decompressMapa m)

-- | Adiciona uma peça em dadas coordenadas a um mapa
adicionaMapa :: (Int,Int) -- ^ Coordenadas da peça
    -> Peca -- ^ Peça a colocar
    -> Mapa -- ^ Mapa original
    -> Mapa -- ^ Mapa resultante
adicionaMapa (x,y) peca mapa | x < 0 || y < 0 = mapa
                             | newMap /= [] = constroiMapa newMap
                             | otherwise = [] 
                             where newMap = ((peca,(x,y)):(desconstroiMapa mapa))

-- | Apaga qualquer peça de um mapa dadas as coordenadas
deleteMapa :: (Int,Int) -- ^ Coordenadas da peça a apagar
    -> Mapa -- ^ Mapa original
    -> Mapa -- ^ Mapa resultante
deleteMapa (x,y) mapa | x < 0 || y < 0 = mapa
                      | newMap /= [] = constroiMapa newMap
                      | otherwise = []
                      where newMap = (filter (\(p,c) -> c /= (x,y)) (desconstroiMapa mapa))

{- | Transforma o mapa dado numa lista de imagens

Esta começa a desenhar as peças aplicando-lhes uma translação Translate (offset x) (offset y), de seguida somando ao acumulador 64 (comprimento da peça) e repetindo o processo para a próxima peça, que agora será desenhada 64 píxeis à direita

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

{- | Percorre a lista das portas do menu e verifica correspondências da posição atual do jogador com as portas.

Utilizada na função 'step' para mudar o valor de MenuStates no estado e 

Deste modo as outras funções compreendem onde o jogador se encontra no menu.

-}
convert1 :: (Int,Int) -- ^ posição atual do jogador
    -> [(Int,Int,MenuStates)] -- ^ lista das portas do menu e respetivos modos de jogos
    -> MenuStates -- ^ mode de jogo final
convert1 (x,y) [] = Normal
convert1 (x,y) ((a,b,c):t)  | (a,b) == (x,y) = c
                            | otherwise = convert1 (x,y) t

{- | Percorre a lista das portas do seletor de mapas e verifica correspondências da posição atual do jogador com as portas.

Utilizada na função 'step' para mudar o valor de de um Int no estado. 

Deste modo as outras funções compreendem onde o jogador se encontra. 

Idêntica à função 'convert1'.

-}
convert2 :: (Int,Int) -- ^ posição atual do jogador
    -> [(Int,Int,Int)] -- ^ lista das portas do seletor de mapas e respetivos modos de jogos
    -> Int -- ^ mode de jogo final
convert2 (x,y) [] = 0
convert2 (x,y) ((a,b,c):t)  | (a,b) == (x,y) = c
                            | otherwise = convert2 (x,y) t


{- | Função auxiliar do draw para SaveLoad do Play e Editor

Faz a interface gráfica do jogo quando se carregam ou guardam jogos.

Se não houver jogo guardado numa dada janela, devolve um texto a dizer que está vazio. 

Se houver, desenha numa janela pequena o jogo guardado
-}

drawscreen ::  Picture -- ^ Recebe uma lista de todas as pictures no ficheiro do projeto
    -> Jogo -- ^ jogo para desenhar na janela de carregar ou guardar mapas
    -> Int -- ^ indica a posição atual nas janelas de SaveLoad
    -> Int -- ^ indica o modo de jogo: 1 -> Play, 2 -> Editor
    -> IO Picture -- ^ desenho final
drawscreen pics@(Pictures pic) (Jogo [] _) n gamemode = do let noLoadText = (Translate (-200) 0 (Scale 0.5 0.5 (Pictures [Text "Savefile Empty", Translate 0 (-120) (Text "Keep playing!")])))
                                                               functionpics = drop 17 pic
                                                           return (Pictures [(pic !! 16), noLoadText, loadpointers (Pictures functionpics) n gamemode]) -- se o jogador for para uma janela (das 3 possíveis) onde não consta um jogo guardado, desenha um texto en vez de um preview de jogo
drawscreen pics@(Pictures pic) jogo n gamemode = do previewMap <- draw (jogo,pics, MapSelector ([],0)) -- n importa o estado aqui, apenas o jogo, por isso peguei no estado que usa menos valores
                                                    let functionpics = drop 17 pic
                                                    return (Pictures [(pic !! 16),(Translate 0 (-(43.5)) (Scale 0.64 0.6 previewMap)), (loadpointers (Pictures functionpics) n gamemode)])

{- | Função auxiliar do draw para SaveLoad do Play e Editor

Desenha a par da função 'drawscreen', as setas nas janelas do SaveLoad, para orientar o jogador.
-}
                                      
loadpointers :: Picture 
    -> Int -- ^ posição atual no menu SaveLoad do Play ou Editor
    -> Int -- ^ modo de jogo (para saber o nº da janela máxima)
    -> Picture -- janela atual no menu Save, modo de jogo (para saber o nº da janela máxima)
loadpointers (Pictures [arrowLeft,arrowRight]) n gm  | gm == 1 && n == 3 || gm == 2 && n == 5 = Pictures [(Translate (-750) 0 arrowLeft)]
                                                     | n == 1 = Pictures [(Translate (750) 0 arrowRight)]
                                                     | otherwise = Pictures [(Translate (750) 0 arrowRight),(Translate (-750) 0 arrowLeft)]


-- | Converte o estado atual em imagens
draw :: Estado -> IO Picture

-- MapEditor transferido para play (se mode == 3) ---------------------
draw (jogo, pics, MapEdit ((x1,y1), (x2,y2), peca, _, 3, sec,_)) =
    draw (jogo, pics, Play (-1,0,0))

-- MapEditor ----------------------------------------------------------
-- desenha o mapa todo fazendo translate com as coordenadas da camera
draw ((Jogo mapa (Jogador (x,y) dir caixa)), (Pictures [playerLeft, playerRight, brick, crate, door, spikes,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]), MapEdit ((x1,y1), (x2,y2), peca, _, mode, _,_)) =
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

-- Won ----------------------------------------------------------------
draw ((Jogo mapa (Jogador (x,y) dir caixa)), Pictures pics, Won (mov,sec)) =
    return (Pictures [bg, Translate (-200) 0 (Scale 0.5 0.5 (Pictures [Text "You Won!!!", Translate 0 (-120) (Text ("Movements: " ++ (show mov))), Translate 0 (-250) (Text ("In " ++ show (round sec) ++ " seconds"))]))]) -- jogador chegou à porta final
    where bg = pics !! 14

-- TabMenu ------------------------------------------------------------
-- desenha de fundo o jogo do estado antes de abrir o menu interativo
-- desenha por cima uma janela com ponteiros para as diferentes opções, conforme navega
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door, spikes ,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]), (TabMenu (posMenu,e2@(_,_,gm)))) = 
        do drawjogo <- draw e2
           return $ case gm of 
                        Play n -> Pictures (drawjogo:menuplay:[tabpointer1])
                        MapSelector n -> Pictures (drawjogo:menuselector:[tabpointer2])
                        Solver (_,1) -> Pictures (drawjogo:menusolvertype:[tabpointer3])
                        Solver (_,2) -> Pictures (drawjogo:menusolver:[tabpointer4])
                        Solver (_,3) -> Pictures (drawjogo:menusolverend:[tabpointer5])
                        Solver (_,4) -> Pictures (drawjogo:menusolverimp:[tabpointer6])
                        MapEdit n -> Pictures (drawjogo:menueditor:[tabpointer7])
    where tabpointer1 = Pictures ((Translate (-250) (305-125*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (305-125*((fromIntegral (posMenu))-1)) playerLeft]) -- diferentes ponteiros que aparecem nos lados das opções do menu interativo, o seu Translate varia conforme o número de botões nesse menu
          tabpointer2 = Pictures ((Translate (-250) (240-156*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (240-156*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer3 = Pictures ((Translate (-250) (170-168*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (170-168*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer4 = Pictures ((Translate (-250) (250-165*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (250-165*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer5 = Pictures ((Translate (-250) (95-185*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (95-185*((fromIntegral (posMenu))-1)) playerLeft])
          tabpointer6 = Pictures ((Translate (-250) (-100) playerRight):[Translate 250 (-100) playerLeft])
          tabpointer7 = Pictures ((Translate (-250) (280-140*((fromIntegral (posMenu))-1)) playerRight):[Translate 250 (280-140*((fromIntegral (posMenu))-1)) playerLeft])

-- SaveLoad do Play ---------------------------------------------------
-- para os "previews" apenas é usado o jogo no primeiro valor do estado
-- chama a função 'drawscreen'
draw (jogo,pics, SaveLoadPlay ((_,n),_)) = 
    do  screendraw <- drawscreen pics jogo n 1 
        return screendraw

--SaveLoad do Editor --------------------------------------------------
-- para os "previews" apenas é usado o jogo no primeiro valor do estado
-- chama a função 'drawscreen'
draw (jogo, pics, SaveLoadEditor((_,n),_)) =
    do  screendraw <- drawscreen pics jogo n 2 
        return screendraw

-- Play, Menu, Map Selector e Solver-----------------------------------
-- permite uso de mapas grandes devido ao scroll
-- desenha de fundo uma imagem e desenha por cima o jogo em si
draw ((Jogo mapa (Jogador (x,y) dir caixa)), pics@(Pictures [playerLeft, playerRight, brick, crate, door,spikes,menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,arrowLeft,arrowRight]), gamemode)
    -- mapa com menos de 21 blocos, scrolling desnecessário mas temos de o centrar
    | xmax <= 21 = let offset = getOffset mapa
                       map3 = getLines y mapa
                       offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) ((-y2*64)+offsetY) picFinal]))
    -- jogador perto da parede (esquerda): mapa estático, só se move o jogador
    | x <= 10 = let map2 = map (\linha -> take 21 linha) mapa
                    map3 = getLines y map2
                    offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate ((xf*64)+offset) (((-y2*64))+offsetY) picFinal]))
    -- jogador perto da parede (direita): o mesmo que em cima
    | xf >= (xmax - 10) = let wallDistance = (round xmax)-20
                              map2 = map (\linha -> drop wallDistance linha) mapa
                              x2 = (xf-(fromIntegral wallDistance)) -- converte coordenadas do jogador em coordenadas no ecrã
                              map3 = getLines y map2
                              offsetY = getOffsetY map3 in
        --debug Scale 0.1 0.1 (Text (show map2))
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate ((x2*64)+offset) (((-y2*64))+offsetY) picFinal]))
    -- jogador a meio do mapa: jogador fica no centro do ecrã horizontalmente, e fazemos scroll do mapa
    | otherwise = let map2 = getLines y mapa
                      map3 = (map (\linha -> take 21 linha) (map (\linha -> drop (x-10) linha) map2))
                      offsetY = getOffsetY map3 in
        return (Pictures (sandbg:(getPictures [brick, crate, door, spikes] (offset,offset,offsetY) map3)++[Translate 0 (((-y2*64))+offsetY) picFinal]))
    where xmax = fromIntegral (length (head mapa))
          ymax = (length mapa)-1

          -- offset horizontal
          getOffset :: Mapa -> Float
          getOffset map3 = ((-64)*((fromIntegral (length (head map3)))/2))
          offset = -640 -- standard offsett para mapas com mais de 21 blocos

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
          y2 = fromIntegral (mod y 15) -- o mapa é dividido em secções de 15 blocos e é devolvida aquela que tem o jogador
          getLines :: Int -> Mapa -> Mapa
          getLines y l = drop (15*(section-1)) (take ((15*section)+1) l)
                       where section = (div y 15)+1

-- | Efetua uma ação consoante a tecla pressionada ou largada e o estado atual
eventListener :: Event -> Estado -> IO Estado
-- comando para exit
eventListener (EventKey (Char 'q') Down _ _) _ = exitSuccess
-- comandos debug -----------------------------------------------------
-- menu
eventListener (EventKey (SpecialKey KeyBackspace) Down _ _) (_, pic, _) = return (estadoBase pic (Menu infoMenu))
-- põe as informações do modo de jogo no terminal
eventListener (EventKey (Char 'I') Down _ _) e@(_,_,info) = do putStrLn (show info)
                                                               return e
 -- primeiro nível
eventListener (EventKey (SpecialKey KeyF1) Down _ _) (_, pic, _) = return (estadoBase pic (Play (1,0,0)))
-- editor de mapas vazio
eventListener (EventKey (SpecialKey KeyF2) Down _ _) (_, pic, _) = return ((Jogo [] (Jogador (0,0) Este False), pic, MapEdit ((-13,-7),(0,0),Bloco,(0,0,0,0,0),0,0,1)))
-- mapa alto para testar offset vertical
eventListener (EventKey (SpecialKey KeyF3) Down _ _) (_, pic, _) = return ((Jogo (makeMap stairMap) (Jogador (0,28) Este False), pic, Play (0,0,0)))
-- toggle da caixa do jogador
eventListener (EventKey (SpecialKey KeyF5) Down _ _) (Jogo mapa (Jogador pos dir caixa), pic, gm) = return (Jogo mapa (Jogador pos dir (not caixa)), pic, gm)
 -- mapa comprido para testar offset horizontal (é um dos mapa base)
eventListener (EventKey (SpecialKey KeyF6) Down _ _) (_, pic, _) = return (estadoBase pic (Play (7,0,0)))
-- load do primeiro mapa no ficheiro do editor (pode crashar se não houver nenhum (?))
eventListener (EventKey (SpecialKey KeyF7) Down _ _) (_, pic, _) = do (jogo,gm) <- loadGameEditor 0
                                                                      return (jogo, pic, MapEdit gm)
-- mapa anterior no play (crasha se já for o primeiro mapa)
eventListener (EventKey (SpecialKey KeyF8) Down _ _) (_, pic, Play (n,_,_)) = return (estadoBase pic (Play (n-1,0,0)))
-- próximo mapa no play (crasha se já for o último mapa)
eventListener (EventKey (SpecialKey KeyF9) Down _ _) (_, pic, Play (n,_,_)) = return (estadoBase pic (Play (n+1,0,0)))
-----------------------------------------------------------------------

-- Quick save e load --------------------------------------------------
eventListener (EventKey (Char 's') Down _ _) e@(Jogo mapa jogador, _, Play info@(n,_,_)) -- escrever estado Play
    = do let str = show ((compressMapa (desconstroiMapa mapa)),jogador,info)
         saveGame "SaveGamePlay.txt" str 0
         return e
eventListener (EventKey (Char 'l') Down _ _) (_, pic, Play (n,_,_)) -- carregar estado Play
    = do texto <- readFile "SaveGamePlay.txt"
         let (mapa,jogador,info) = read ((lines texto) !! 0)::([(Peca,[(Int,Int)])],Jogador,PlayInfo)
         return (Jogo (constroiMapa (decompressMapa mapa)) jogador, pic, Play info)
eventListener (EventKey (Char 'L') Down _ _) e@(_, pic, MapEdit (_,_,_,_,_,_,n)) = do (jogo,gm) <- loadGameEditor (n-1)
                                                                                      return (jogo, pic, MapEdit gm)
eventListener (EventKey (Char 'S') Down _ _) e@(jogo@(Jogo mapa jogador), pic, MapEdit gm@(c1,c2,peca,_,_,_,n)) =
    do let str = show (compressMapa (desconstroiMapa mapa),jogador,c1,c2,peca)
       saveGame "SaveGameMapEditor.txt" str (n-1)
       return (jogo, pic, MapEdit gm)

-- reset do nível -----------------------------------------------------
eventListener (EventKey (Char 'r') Down _ _) (jogo, pic, gm@(Play (a,sec,mov))) = do savedata <- readFile "SaveGamePlay.txt"
                                                                                     let (_,_,(_,sec2,mov2)) = read ((lines savedata) !! 0)::([(Peca,[(Int,Int)])],Jogador,PlayInfo) -- especificar o tipo para o read não fazer conflito
                                                                                         (jogo,_,_) = estadoBase pic gm
                                                                                     return (jogo,pic,Play (a,sec+sec2,mov+mov2))
eventListener (EventKey (Char 'r') Down _ _) e@(_, pic, MapEdit (_,_,_,_,_,_,n)) = do (jogo,gm) <- loadGameEditor (n-1) -- l e r fazem a mesma coisa, pelo menos por agora
                                                                                      return (jogo, pic, MapEdit gm)

-- Won (basta carregar em qualquer tecla para regressar ao menu) -----------------------------
eventListener (EventKey _ Down _ _) (_, pic, Won _) = return (estadoBase pic (Menu infoMenu))

-- MapEditor (mode == 3, jogar o mapa, usa o Play para permitir jogar)-
eventListener keyinfo@(EventKey key Down _ _) e@(jogo, pic, m@(MapEdit (c1, c2, peca, states, 3, sec,n))) =
    case key of SpecialKey KeyEsc -> return e -- TODO
                Char 'p' -> return (jogo, pic, MapEdit (c1,c2,peca,states,0,sec,n))
                _ -> do (jogo2,_,_) <- eventListener keyinfo (jogo, pic, Play (-1,0,0)) -- -1 para nao dar conflito (??)
                        return (jogo2,pic,m)

-- TabMenu e Solver ---------------------------------------------------
eventListener (EventKey (SpecialKey KeyEsc) Down _ _) e@(jogo, pics, gm) = return $ case gm of -- para entrar e sair do estado TabMenu
    Won _ -> e -- quando está na tela de vitória não deve poder abrir um menu interativo de opções
    Menu _ -> e -- TODO: Menu com a opção "exit"
    SaveLoadPlay _ -> (Jogo (makeMap menuMap) (Jogador (7,8) Este False), pics, Menu ([(3,8, New),(7,8,Continue),(11,8,LevelSelector),(15,8,MapEdit1)], Continue))
    SaveLoadEditor ((1,_),_) -> (Jogo [] (Jogador (0,0) Este False),pics, MapEdit ((-13,-7),(0,0),Bloco,(0,0,0,0,0),0,0,1)) -- se estiver a carregar um mapa do editor no SaveLoad e clicar no "esc", regressa a um mapa vazio do editor
    SaveLoadEditor ((2,_),(jogoeditor,infoeditor)) -> (jogoeditor,pics, MapEdit infoeditor) -- se estiver a guardar um mapa do editor no SaveLoad e clicar no "esc", regressa ao mapa por gravar (antes de abrir o menu interativo)
    TabMenu (_,(_,_,(Solver ((emenu,_,_,_,_),1)))) -> emenu -- no submenu de opções do Solver, se clicar no tab, volta ao menu interativo
    TabMenu (_,(_,_,(Solver (_,3)))) -> e -- previne o uso de "esc" no fim de Solver
    TabMenu (_,(_,_,(Solver (_,4)))) -> e -- previne o uso de "esc" no fim de Solver
    TabMenu (n,estAnterior) -> estAnterior -- se já estiver no menu interativo, sai do menu e retorna o estado guardado no segundo valor de TabMenu (correponde ao estado do jogo antes de abrir o TabMenu)
    otherwise -> (jogo, pics, TabMenu (1,e)) -- em qualque outro caso, entra no estado TabMenu e guarda no segundo valor do TabMenu o estado do jogo antes de abrir o TabMenu (para poder ser recuperado mais tarde)

eventListener (EventKey (SpecialKey KeyUp) Down _ _) e@(_,_,TabMenu (pos,e2@(_,_,gm))) = case gm of -- para mover no estado TabMenu
        Play n  -> return (tabMenuOrganizer e 6) -- o modo de jogo Play tem 6 opções no menu interativo
        MapSelector n -> return (tabMenuOrganizer e 4) -- o modo de jogo Play tem 4 opções no menu interativo
        MapEdit n -> return (tabMenuOrganizer e 5) -- o modo de jogo Play tem 5 opções no menu interativo
        Solver (_,n) | n == 1 -> return (tabMenuOrganizer e 3)
                     | n == 2 -> return (tabMenuOrganizer e 4)
                     | n == 3 -> return (tabMenuOrganizer e 2)
                     | n == 4 -> return e

    --coordena a interação com os menus interativos de cada modo de jogo, mudando o estado conforme o jogador se desloca para cima na lista das opções
    where   tabMenuOrganizer :: Estado -> Int -> Estado -- estado atual do menu interativo, número de botões no menu (para um gamemode específico) (permite clicar para cima no topo do menu e ir ter ao último elemento do mesmo)
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
       --coordena a interação com os menus interativos de cada modo de jogo, mudando o estado conforme o jogador se deslca para baixo na lista das opções
    where   tabMenuOrganizer2 :: Estado -> Int -> Estado -- igual a tabMenuOrganizer, mas para a teclaDown
            tabMenuOrganizer2 e@(jogo, pic, (TabMenu (pos,e2@(_,_,gm)))) x
                | pos == x = (jogo, pic, TabMenu (1,e2))
                | otherwise = (jogo, pic, (TabMenu ((pos+1),e2)))

    -- coordena a interação de Enter com os menus de pausa - clicar num dos botões do menu
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(_,pics, TabMenu (pos,e2@(jogo,_,gm))) = case gm of
    Play infoPlay   | pos == 1 -> return e2 -- volta ao jogo antes do menu inteativo
                    | pos == 2 -> return (estadoBase pics (Play infoPlay)) -- abre a janela do SaveLoad para guardar o mapa. Atualiza o jogo do estado, e o segundo par do SaveLoadEditor
                    | pos == 3 -> return e -- TODO: CONTROLS
                    | pos == 4 -> do loadedgame@(jogoplay,_) <- loadGamePlay 0
                                     return (jogoplay,pics, SaveLoadPlay ((2,1),(jogo,infoPlay))) -- abre a janela do SaveLoad para guardar o jogo. Atualiza o jogo do estado, e o segundo par do SaveLoadEditor 
                    | pos == 5 -> return (jogo,pics, TabMenu (1,(jogo,pics,Solver ((e,Nothing,Just [],0,0.0),1)))) -- retorna um estado de tabMenu em que o estado guardado é Solver (estadoPlay,1) - permite usar os mesmo controlos no menu do Solver
                    | pos == 6 -> return (estadoBase pics (Menu infoMenu)) -- volta ao menu inicial - TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    MapEdit infoEdit    | pos == 1 -> return e2 -- volta ao jogo antes do menu interativo
                        | pos == 2 -> return e -- TODO: CONTROLS
                        | pos == 3 -> do loadedgame@(jogoeditor,_) <- loadGameEditor 0
                                         return (jogoeditor,pics, SaveLoadEditor ((2,1),(jogo,infoEdit))) -- abre a janela do SaveLoad para guardar o mapa. Atualiza o jogo do estado, e o segundo par do SaveLoadEditor
                        | pos == 4 -> do loadedgame@(jogoeditor,_) <- loadGameEditor 0
                                         return (jogoeditor,pics, SaveLoadEditor ((1,1),loadedgame)) -- abre a janela do SaveLoad para carregar um mapa. Atualiza o jogo do estado, e o segundo par do SaveLoadEditor
                        | pos == 5 -> return (estadoBase pics (Menu infoMenu)) -- volta ao menu inicial - TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    MapSelector n   | pos == 1 -> return e2
                    | pos == 2 -> return e -- TODO
                    | pos == 3 -> return e -- TODO
                    | pos == 4 -> return (estadoBase pics (Menu infoMenu)) -- TODO: mensagem a perguntar se quer gravar, se não tiver gravado

    Solver ((emenu@(_,_,TabMenu (_,(ebefore@(jogo,_,Play (n,_,_))))),_,movelist,choice,_),state) -> case state of
            1   | pos == 1 -> return (jogos !! (n-1),pics,Solver ((emenu,getMoves1,getMoves1,1,0.1),2)) -- começa a solução do início do nível atual, o número 500 é arbitrariamente grande, para não haver um limite máximo de moves ao resolver o nível; o tempo é 0.1 (para dar tempo ao jogador para se habituar)
                | pos == 2 -> return (jogo,pics,Solver ((emenu,getMoves2, getMoves2, 2,0.1),2)) -- começa a solução da posição atual
                | pos == 3 -> return emenu -- volta para o menu interativo

            2   | pos == 1 -> return e2 -- retornar o jogo antes de abrir o tabmenu
                | pos == 2 -> return e -- TODO: CONTROLS
                | pos == 3 -> return emenu -- volta para o menu interativo
                | pos == 4 -> return (estadoBase pics (Menu infoMenu)) -- volta para o menu inicial

            3   | pos == 1 -> return $ case choice of -- caso em que o jogador acaba o solver e quer repetir
                                        1 -> (jogos !! (n-1),pics,Solver ((emenu,movelist, movelist,1,0.1),2)) -- se a solução escolhida for do início, começa do início outra vez
                                        2 -> (jogo,pics,Solver ((emenu, movelist, movelist, 2,0.1),2)) -- se a solução escolhida for da posição atual, começa da mesma posição outra vez
                | pos == 2 -> return emenu -- volta para  o menu interativo

            4   | pos == 1 -> return emenu -- volta para o menu interativo

             -- dá a lista de movimentos para a solução. Permite calcular apenas uma vez a solução, mesmo que o jogador queira repetir a solução.
              
        where getMoves1 = resolveJogo 500 (jogos !! (n-1))
              getMoves2 = resolveJogo 500 jogo

eventListener _ e@(_,_,TabMenu _) = return e -- previne qualquer outro input no menu interativo
eventListener _ e@(_,_,Solver (_,2)) = return e -- previne qualquer outro input no solver

-- MapEditor (tecla pressionada) --------------------------------------
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
                         | key == SpecialKey KeyShiftL = (2,mapa)
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
                | otherwise = mode
          sec2 | oRhor1 == 0 && oRhor2 == 0 && oRvert1 == 0 && oRvert2 == 0 = 0
               | otherwise = sec

-- MapEditor (tecla libertada) ----------------------------------------
eventListener (EventKey key Up _ _) (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, (oRhor1,oRvert1,oRhor2,oRvert2,oRaction), mode, sec, n))
    | key == SpecialKey KeyShiftL = return (jogo,pic, MapEdit ((x1,y1),(x2,y2),peca,(oRhor1,oRvert1,oRhor2,oRvert2,0),mode,sec,n))
    | otherwise = return (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, mov, mode, 0.235,n))
    where mov | key == Char 'w'|| key == Char 's' = (oRhor1,0,oRhor2,oRvert2,oRaction)
              | key == Char 'a' || key == Char 'd' = (0,oRvert1,oRhor2,oRvert2,oRaction)
              | key == SpecialKey KeyUp || key == SpecialKey KeyDown = (oRhor1,oRvert1,oRhor2,0,oRaction)
              | key == SpecialKey KeyLeft || key == SpecialKey KeyRight = (oRhor1,oRvert1,0,oRvert2,oRaction)
              | key == SpecialKey KeyEnter || key == SpecialKey KeyDelete = (oRhor1,oRvert1,oRhor2,oRvert2,0)
              | otherwise = (oRhor1,oRvert1,oRhor2,oRvert2,oRaction)

-- SaveLoadScreen -----------------------------------------------------
-- escolher o mapa no Load do Play
-- entra no jogo guardado no segundo par de SaveLoadPlay
-- se não houver jogo guardado nessa janela, não entra. Retorna o mesmo estado.
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(jogo,pics,SaveLoadPlay ((1,n),(jogoplay,infoplay))) =
       case jogoplay of 
            (Jogo [] _) -> return e
            _ -> return (jogoplay,pics,Play infoplay)

-- guardar o mapa no save do Play
-- guarda o jogo atual na linha x do ficheiro "SaveGamePlay.txt"
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(jogo,pics,SaveLoadPlay ((2,n),savegame@(jogoplay@(Jogo mapa jogador),infoplay))) = 
    do  let str = show (compressMapa (desconstroiMapa mapa),jogador,infoplay)
        saveGame "SaveGamePlay.txt" str (n-1)
        return (jogoplay,pics,SaveLoadPlay ((2,n),savegame))


-- escolher o mapa no SaveLoad do Editor
-- entra no jogo guardado no segundo par de SaveLoadPlay
-- se não houver jogo guardado nessa janela, entra num mapa vazio.
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(jogo,pics,SaveLoadEditor ((1,n),(jogoeditor,infoeditor))) =
        return (jogoeditor,pics,MapEdit infoeditor)

-- guardar o mapa no Save do Editor
-- guarda o jogo atual na linha x do ficheiro "SaveGameEditor.txt"
eventListener (EventKey (SpecialKey KeyEnter) Down _ _) e@(jogo,pics,SaveLoadEditor ((2,n),savegame@(jogoeditor@(Jogo mapa jogador),(i1,i2,i3,_,_,_,_)))) =
    do let str = show (compressMapa (desconstroiMapa mapa),jogador,i1,i2,i3)
       saveGame "SaveGameMapEditor.txt" str (n-1)
       return (jogoeditor,pics,SaveLoadEditor ((2,n),savegame))

-- interagir com o Load no Play 
-- 3 janelas possíveis
-- da 1 janela não dá para ir mais para a esquerda, e na 5 mais para a direita
-- ao mudar para a janela x, atualiza o jogo no primeiro valor do estado, para o jogo guardado na linha x do respetivo ficheiro (para Play ou Editor)
-- também atualiza os valores guardados no último par de SaveLoadEditor ou SaveLoadPlay
eventListener (EventKey key Down _ _) e@(jogo,pics,SaveLoadPlay ((1,n),savegame@(jogoplay,infoplay))) = case key of 
    SpecialKey KeyLeft -> if n == 1 then return e 
                         else do newgame@(newplay,newinfo) <- loadGamePlay (n-2)
                                 return (newplay,pics,SaveLoadPlay ((1,n-1),newgame))
    SpecialKey KeyRight -> if n == 3 then return e 
                          else do newgame@(neweditor,infoeditor) <- loadGamePlay n
                                  return (neweditor,pics,SaveLoadPlay ((1,n+1),newgame))
    _ -> return e
-- interagir com o Load no Editor 
-- 5 janelas possíveis
-- da 1 janela não dá para ir mais para a esquerda, e na 5 mais para a direita
-- ao mudar para a janela x, atualiza o jogo no primeiro valor do estado, para o jogo guardado na linha x do respetivo ficheiro (para Play ou Editor)
-- também atualiza os valores guardados no último par de SaveLoadEditor ou SaveLoadPlay
eventListener (EventKey key Down _ _) e@(jogo,pics,SaveLoadEditor ((1,n),savegame@(jogoeditor,infoeditor))) = case key of 
    SpecialKey KeyLeft -> if n == 1 then return e 
                         else do newgame@(newplay,newinfo) <- loadGameEditor (n-2)
                                 return (newplay,pics,SaveLoadEditor ((1,n-1),newgame))
    SpecialKey KeyRight -> if n == 5 then return e 
                          else do newgame@(neweditor,infoeditor) <- loadGameEditor n
                                  return (neweditor,pics,SaveLoadEditor ((1,n+1),newgame))
    _ -> return e
-- interagir com o Save no Play 
-- 3 janelas possíveis
-- da 1 janela não dá para ir mais para a esquerda, e na 3 mais para a direita
-- ao mudar para a janela x, atualiza o jogo no primeiro valor do estado, para o jogo guardado na linha x do respetivo ficheiro (para Play ou Editor)
-- não atualiza os valores guardados no último par de SaveLoadPlay. Essa informação continua a ser o estado do jogo quando se clicou na opção de guardar.
eventListener (EventKey key Down _ _) e@(jogo,pics,SaveLoadPlay ((2,n),savegame@(jogoplay,infoplay))) = case key of 
    SpecialKey KeyLeft -> if n == 1 then return e 
                         else do (newplay,newinfo) <- loadGamePlay (n-2)
                                 return (newplay,pics,SaveLoadPlay ((2,n-1),savegame))
    SpecialKey KeyRight -> if n == 3 then return e 
                          else do (neweditor,infoeditor) <- loadGamePlay n
                                  return (neweditor,pics,SaveLoadPlay ((2,n+1),savegame))
    _ -> return e

-- interagir com o Save no Play 
-- 3 janelas possíveis
-- da 1 janela não dá para ir mais para a esquerda, e na 3 mais para a direita
-- ao mudar para a janela x, atualiza o jogo no primeiro valor do estado, para o jogo guardado na linha x do respetivo ficheiro (para Play ou Editor)
-- não atualiza os valores guardados no último par de SaveLoadEditor. Essa informação continua a ser o estado do jogo quando se clicou na opção de guardar.
eventListener (EventKey key Down _ _) e@(jogo,pics,SaveLoadEditor ((2,n),savegame@(jogoeditor,infoeditor))) = case key of 
    SpecialKey KeyLeft -> if n == 1 then return e 
                         else do newgame@(newplay,newinfo) <- loadGameEditor (n-2)
                                 return (newplay,pics,SaveLoadEditor ((2,n-1),savegame))
    SpecialKey KeyRight -> if n == 5 then return e 
                          else do newgame@(neweditor,infoeditor) <- loadGameEditor n
                                  return (neweditor,pics,SaveLoadEditor ((2,n+1),savegame))
    _ -> return e

-- Menu ---------------------------------------------------------------
-- segue os controlos do Play
-- na primeira porta começa um jogo novo e guarda automaticamente
-- na segunda vai à lista de jogos guardados para continuar
-- abre o seletor de mapas
-- entra no editor de mapas
eventListener (EventKey key Down _ _) e@(jogo, pic, Menu (lista,atual))
    | key /= SpecialKey KeyEnter = return (jogo2, pic, Menu (lista,atual))
    | atual == New = do let (Jogo mapa jogador) = jogos !! 1
                            str = show (compressMapa (desconstroiMapa mapa),jogador,(1,0,0))
                        saveGame "SaveGamePlay.txt" str 0
                        return (estadoBase pic (Play (1,0,0))) -- retorna a primeira janela dos jogos guardados, guardando no último par de SaveLoadPlay o jogo guardado na primeira linha do ficheiro
    | atual == Continue = do  loadedgame@(jogoplay,infoplay) <- loadGamePlay 0
                              return (jogoplay,pic, SaveLoadPlay ((1,1),loadedgame)) -- retorna a primeira janela do Load do Play
    | atual == LevelSelector = return (estadoBase pic (MapSelector infoMapSelect)) -- retorna a informação base do seletor de mapas
    | atual == MapEdit1 = return (Jogo [] (Jogador (0,0) Este False),pic, MapEdit ((-13,-7),(0,0),Bloco,(0,0,0,0,0),0,0,1)) -- retorna a informação base do do editor (mapa vazio)
    | otherwise = return e
    where jogo2 | key == SpecialKey KeyUp = moveJogador jogo Trepar
                | key == SpecialKey KeyDown = moveJogador jogo InterageCaixa
                | key == SpecialKey KeyLeft = moveJogador jogo AndarEsquerda
                | key == SpecialKey KeyRight = moveJogador jogo AndarDireita
                | otherwise = jogo

-- Map selector e Play ------------------------------------------------
-- controlos do seletor de mapas idênticos aos do play
-- ao entrar numa porta, entra no Play respetivo
eventListener (EventKey key Down _ _) e@(jogo, pic, gamemode) = case gamemode of 
        Play (mapa,sec,mov) -> return (jogo2, pic, Play (mapa,sec,mov+1))
        MapSelector (lista,atual) -> 
            let novoEstado2 | atual == 0 =  e
                            | otherwise = (estadoBase pic (Play (atual,0,0)))
            in  if key == SpecialKey KeyEnter
                then return novoEstado2
                else return (jogo2, pic, MapSelector (lista,atual)) 

    where jogo2     | key == SpecialKey KeyUp = moveJogador jogo Trepar
                    | key == SpecialKey KeyDown = moveJogador jogo InterageCaixa
                    | key == SpecialKey KeyLeft = moveJogador jogo AndarEsquerda
                    | key == SpecialKey KeyRight = moveJogador jogo AndarDireita
                    | otherwise = jogo

eventListener _ s = return s

{- | Função de update: 

*permite guardar a passagem do tempo no Play (para o highscore) e Solver (para espaçar os movimentos da solução)

*permite atualizar os estados do jogo, de modo a orientar as outras funções da posição atual do jogador.
Isto é:  

**atualiza o estado quando o jogador se coloca à frente de uma porta no menu, seletor de mapas. 

**atualiza o estado quando o jogador termina o nível no Play, ou quando termina a solução no Solver,...
-}

step :: Float -> Estado -> IO Estado
--Play ----------------------------------------------------------------
step time (jogo@(Jogo mapa (Jogador (x,y) _ _)), pic, gm@(Play (n,sec,mov)))
    | pecaAtual == Porta =
        if n == numeroMapas
        then return (jogo, pic, Won (mov,sec))
        else do let (jogo2@(Jogo mapa jogador)) = jogos !! n
                    str = show (compressMapa (desconstroiMapa mapa),jogador,(n+1,sec+time,mov))
                saveGame "SaveGamePlay.txt" str 0
                return (jogo2, pic, Play (n+1,sec+time,mov))
    | pecaAtual == Picos = do savedata <- readFile "SaveGamePlay.txt"
                              let (_,_,info) = read (head (lines savedata))::([(Peca,[(Int,Int)])],Jogador,PlayInfo) -- especificar o tipo para o read não fazer conflito
                                  (jogo,_,_) = estadoBase pic gm
                              return (jogo,pic,Play info)
    | otherwise = return (jogo, pic, Play (n,sec+time,mov))
    where pecaAtual = (mapa !! y) !! x

--Map edit com mode == 3 (jogar o mapa) -------------------------------
step time e@(Jogo mapa (Jogador (x,y) _ _), pic, MapEdit (_, _, _, _,3,_,n))
    | (pecaAtual == Picos || pecaAtual == Porta) = do (jogo,(c1,c2,peca,mov,_,sec,_)) <- loadGameEditor (n-1)
                                                      return (jogo, pic, MapEdit (c1,c2,peca,mov,3,sec,n))
    | otherwise = return e
    where pecaAtual = (mapa !! y) !! x


--Map edit ------------------------------------------------------------
step time e@(jogo@(Jogo mapa jogador), pic, MapEdit ((x1,y1), (x2,y2), peca, mov@(hor1,vert1,hor2,vert2,action),mode,sec,n)) =
    if timeBetweenmodes < 250
    then return (jogo, pic, MapEdit ((x1,y1), (x2,y2), peca, mov,mode,sec+time,n))
    else return (Jogo mapa2 jogador, pic, MapEdit ((x1+hor1,y1+vert1), (x2+hor2,y2+vert2), peca, mov,mode,0.235,n))
    where mapa2 | action == 1 = adicionaMapa (x2,y2) peca mapa
                | action == -1 = deleteMapa (x2,y2) mapa
                | otherwise = mapa
          timeBetweenmodes = mod (round (sec*1000)) 500

-- Menu ---------------------------------------------------------------
-- confere constantemente se a posição do jogador é a de alguma porta para alterar o estado
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, Menu (lista,antigo)) = 
    return (jogo, pic, Menu (lista,atual))
    where atual = convert1 (x,y) lista

-- Map selector -------------------------------------------------------
-- confere constantemente se a posição do jogador é a de alguma porta para alterar o estado
step time (jogo@(Jogo _ (Jogador (x,y) _ _)), pic, MapSelector (lista,antigo)) = 
    return (jogo, pic, MapSelector (lista,atual))
    where atual = convert2 (x,y) lista

-- Solver -------------------------------------------------------------
step time e@(jogo, pic, Solver ((prevE, solution, movelist,choice,sec),2)) = 
    return $ case solution of  -- prevE será sempre tabMenu porque o Solver é aberto no tabMenu
                Nothing -> (jogo,pic,TabMenu (1,(jogo, pic, Solver ((prevE, Nothing, Nothing ,choice,0),4)))) -- no caso de a solução para o nível dar vazia, devolve uma janela de erro -- os valores dentro deste Solver são na maioria arbitrários (_,4)
                Just l  | null l -> (jogo,pic,TabMenu (1,(jogo, pic, Solver ((prevE, Nothing, movelist,choice,0),3)))) -- quando a lista de movimentos terminar, ou seja, o jogador chegar à porta, retorna o jogador para uma tela de conclusão do solver
                        | otherwise ->  if timeBetweenMoves <= 450                                              -- permite espaçar os movimentos do jogador enquanto este não chega á porta
                                        then (jogo, pic, Solver ((prevE, Just l, movelist,choice,sec+time),2))
                                        else (jogo2, pic, Solver ((prevE, Just (tail l),movelist,choice,0),2))
                    where jogo2 = moveJogador jogo (head l)
                          timeBetweenMoves = mod (round (sec*1000)) 700 -- os movimentos serão efetuados espaçadamente

-- Para outras situações
step _ e = return e

-- | Função main, que carrega as imagens e inicia o jogo no estado inicial do menu
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
    Just spikes <- loadJuicy "spike.png"
    Just savescreen <- loadJuicy "savefile.png"
    Just arrowLeft <- loadJuicy "arrow-red-left.png"
    Just arrowRight <- loadJuicy "arrow-red-right.png"
    playIO window
           (white)
           fr
           (estadoBase (Pictures [(Scale 2.783 2.783 playerLeft),(Scale 2.783 2.783 playerRight), (Scale 0.186 0.186 brick), (Scale 2.0 2.0 crate), (Scale 0.674 0.451 door), (Scale 0.0762 0.0745 spikes),menuplay,menuselector,menueditor,menusolvertype,menusolver,menusolverend,menusolverimp,snowbg,grassbg,sandbg,savescreen,(Scale 0.5 0.5 arrowLeft),(Scale 0.3 0.3 arrowRight)]) (Menu infoMenu)) -- 64x64 px
           draw
           eventListener
           step

{- Notas e outros comentários

Imagens:
A textura do "red-brick.png" foi descarregada de "http://minecraft.novaskin.me/gallery/tag/texture:concrete_red?next=CjoKFAoHaG90bmVzcxIJIe_Jw0JtQ4NAEh5qDHN-c2tpbmVkaXRvcnIOCxIEU2tpbhj4qY_BBQwYACAB"
Os restantes blocos e sprite do jogador foram descarregados de diferentes packs no "itch.io" (não sabemos qual ao certo)
Todos os menus interativos e associados foram construídos manualmente no "canva.com"

Controlos:
 Play: 
  setas- Fazer o jogador mover e interagir com caixas
  Esc- Menu pequeno
  s- Gravar o jogo (irá ser sempre na primeira linha do ficheiro, ou seja, a primeira save)
  l- Carregar (o mesmo de cima)
  r- Reset do nível

 MapEditor:
  setas- Mover o cursor   \
  WASD- Mover a camera     \ 
                            > estes quatro podem ser segurados para ações contínuas
  Enter- Colocar uma peça  /
  Delete- Apagar uma peça /
  Espaço- Zoom
  p- Jogar o mapa
  v- Validar o mapa
  1-Seleciona bloco
  2-Seleciona caixa
  3-Seleciona porta
  4-Seleciona picos
  5-Move o jogador para a posição do cursor
  6-Toggle da caixa do jogador
  7-Toggle da orientação do jogador
  S (shift + s)- Gravar jogo
  L (shift + l)- Carregar jogo (estas poderao criar alguns problemas)
  Shift + setas- Mover o mapa inteiro

 Menu:
  q - sair do jogo
  setas tal como um jogo normal
  enter para entrar na porta

 TabMenu (menu pequeno):
  setas e enter para mover e interagir

 Debug:
  KeyBackspace- Menu
  I (shift + i)- põe as informações do gamemode no terminal
  F1- Play no primeiro mapa
  F2- MapEditor com mapa vazio
  F3- Mapa alto para testar offset vertical
  F5- Toggle da caixa do jogador 
  F6- Mapa comprido para testar scroll e offset (é um dos mapa base)
  F7- Load do primeiro mapa no ficheiro do editor de mapas
  F8- Mapa anterior no Play (erro se já estiver no primeiro mapa)
  F9- Mapa seguinte no Play (erro se já estiver no último mapa)



Tivemos alguns problemas no save e load, nomeadamente problemas com o read em linhas vazias (parse error por várias razões)
Também surgiram problemas com o facto de o read e writeFile serem lazy, nomeadamente ao tentar escrever no meio de um ficheiro preservando as linhas restantes
Devido a isto, por vezes as funcionalidades de save e load (por exemplo ao entrar na porta Continue do menu) poderão dar problemas.

Outro problema foi também não termos noção no início daquilo que queríamos fazer e de como organizar,
o que fez com que os tipos de dados e o estado se fossem tornando cada vez mais complexos à medida que desenvolvíamos esta tarefa.
Dividimos tudo em Gamemodes para tentar simplificar um pouco, mas ainda assim há muita coisa que é desnecessária, ineficiente ou confusa

Para além idsso, a introdução dos picos veio também complicar o trabalho das tarefas anteriores, pois muitas funções que precisavam de verificar
uma peça não iriam saber lidar com uma nova peça ou iriam fazê-lo mal, mas penso que conseguimos criar um jogo funcional e corrigir estas tarefas.

Não chegamos a conseguir implementar, por falta de tempo: 
- secção "controls" nos menus interativos
- menu interativo no menu para sair do jogo ou resumir
-}