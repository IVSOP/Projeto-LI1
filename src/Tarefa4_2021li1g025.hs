{- |
Module      : Tarefa4_2021li1g025
Description : Movimentação do personagem
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g025 where

import LI12122
import Tarefa3_2021li1g025 -- para dar show ao jogo

-- dados que permitem comparar a posição relativa do jogador e as peças que o circundam
data PontosCardeais = NO | N | NE | O | E | SO | S | SE 
                    deriving (Show, Eq, Read)

-- * Funções Principais


-- | Corre um único movimento, devolvendo o jogo no estado após esse movimento
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo@(Jogo mapa jogador@(Jogador coord dir caixa)) movimento 
    | movimento == Trepar = trepar jogo movimento
    | movimento == InterageCaixa = undefined
    | movimento == AndarEsquerda = Jogo mapa (Jogador (andar jogo movimento) Oeste caixa) -- estes dois casos sao necessarios para garantir que se troca de direcao sempre
    | movimento == AndarDireita = Jogo mapa (Jogador (andar jogo movimento) Este caixa)

-- | Corre uma lista de movimentos, devolvendo o jogo no estado após esses movimentos
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos x [] = x
correrMovimentos jogo (mov:l) = correrMovimentos (moveJogador jogo mov) l

-- * Funcções auxiliares

-- Retorna as coordendas (x,y) de uma lista do tipo [[Peca]]
getpeca :: Mapa -> Coordenadas -> Peca
getpeca m (x,y) = (m !! y) !! x

-- retorna as peças que circundam o jogador (num raio 1) , pela ordem (NO,N,NE,O,E,SO,S,SE), segundo os pontos cardeais
-- Nas segundas coordendas é para por a coordenda a noroeste (ou seja, NO) do jogador (para guiar o funcionamento da função)
mapAround :: Mapa -> Coordenadas -> Coordenadas -> [Peca]
mapAround [] _ _ = []
mapAround m player@(x1,y1) (x2,y2) | player == (x2,y2) = mapAround m player (x2+1,y2) -- ignora a peça em que o jogador está (já se sabe que é vazio)
                                   | abs (y1-y2) > 1 = []
                                   | abs (x1-x2) > 1 = mapAround m player (x1-1,y2+1)
                                   | otherwise = ((m !! y2) !! x2 ) : mapAround m player (x2+1,y2)

-- Testa se um bloco numa posicao cardeal relativa ao jogador é bloco ou caixa
-- Supõe-se que a lista em que atua é resultante de "mapAround"
isBlockorBox :: PontosCardeais -> [Peca] -> Bool 
isBlockorBox direcao l = case direcao of 
                                NO -> head l == Bloco || head l == Caixa
                                N  -> (l !! 1) == Bloco || (l!! 1) == Caixa
                                NE -> (l !! 2) == Bloco || (l!! 2) == Caixa
                                O  -> (l !! 3) == Bloco || (l!! 3) == Caixa
                                E  -> (l !! 4) == Bloco || (l!! 4) == Caixa
                                SO -> (l !! 5) == Bloco || (l!! 5) == Caixa
                                S  -> (l !! 6) == Bloco || (l!! 6) == Caixa
                                SE -> last l == Bloco || last l == Caixa



-- CRITÉRIO TREPAR ----


-- TODO: Implementar a função "trepar" para o formato da Tarefa 3


-- Função que faz com que o jogador trepe (se possível) o obstáculo imediatamente à sua frente
trepar :: Jogo -> Movimento -> Jogo
trepar (Jogo [] player) _ = (Jogo [] player) -- TODO: devo meter uma mensagem de erro aqui?
trepar j movimento | movimento /= Trepar = j
trepar j@(Jogo m@(mapa) (Jogador (x,y) direc carryBox)) _
    | carryBox == True = j 
    | otherwise = treparAux j (mapAround m (x,y) (x-1,y-1))


-- Função que verifica se existem blocos à frente do jogador que o impedem de trepar

treparAux :: Jogo -> [Peca] -> Jogo
treparAux j@(Jogo m (Jogador (x,y) _ _)) circle | isBlockorBox N circle = j
treparAux j@(Jogo m@(mapa) player@(Jogador (x,y) direc _)) circle = case direc of 
    Oeste   | isBlockorBox O circle && not((isBlockorBox NO circle)) -> Jogo m (Jogador (x-1,y-1) Oeste False)
            | otherwise -> j

    Este    | isBlockorBox O circle && not((isBlockorBox NO circle)) -> Jogo m (Jogador (x+1,y-1) Este False)
            | otherwise -> j

--

-- | Retorna as coordenadas resultantes de andar para a esquerda ou direita, com ou sem caixa

-- | Assim, é possível, por exemplo, cair vários blocos de uma só vez ou não sair do lugar se o movimento não for possível
andar :: Jogo -> Movimento -> Coordenadas
andar (Jogo [] (Jogador c _ _)) _ = c
andar (Jogo mapa (Jogador (x,y) dir True)) movimento -- com caixa, valida a caixa e depois valida outra vez sem caixa
    | movimento == AndarEsquerda =
        case (last a) of Vazio -> andar (Jogo mapa (Jogador (x,y) dir False)) AndarEsquerda 
                         _ -> (x,y)
    | otherwise = 
        case (head b) of Vazio -> andar (Jogo mapa (Jogador (x,y) dir False)) AndarDireita
                         _ -> (x,y)
    where (l1,linha:l2) = splitAt (y-1) mapa
          (a,posicao:b) = splitAt x linha

andar (Jogo mapa (Jogador (x,y) dir _)) movimento -- sem caixa
    | movimento == AndarEsquerda = -- por alguma razao nao funcionou sem nested guards
        case (a,posicao:b) of
          (a,posicao:b) | last a /= Vazio -> (x,y)
                        | otherwise -> (x2,y2-1) 
                        where (x2,y2) = getNext l2 (x-1,y+1)

    | otherwise = case (a,posicao:b) of
        (a,posicao:b) | head b /= Vazio -> (x,y)
                      | otherwise -> (x2,y2-1)
                      where (x2,y2) = getNext l2 (x+1,y+1)
    where (l1,(linha:l2)) = splitAt y mapa
          (a,posicao:b) = splitAt x linha

-- | Devolve as coordenadas bloco mais próximo abaixo de certas coordenadas, se existir

-- | É usada, por exemplo, para encontrar onde o jogador irá cair após andar num local sem "chão" imediatamente à sua frente

-- | Como esta mostra o bloco e não o último vazio, é importante subtrair 1 ao y do resultado final ao utilizar esta função
getNext :: Mapa -> Coordenadas -> Coordenadas
getNext [] c = c
getNext (linha:l) (x,y) | (posicao == Bloco || posicao == Caixa) = (x,y)
                        | otherwise = getNext l (x,y+1)
                        where (a,posicao:b) = splitAt x linha