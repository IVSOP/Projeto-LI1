{- |
Module      : Tarefa4_2021li1g025
Description : Movimentação do personagem
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g025 where

import LI12122
import Tarefa2_2021li1g025 (desconstroiMapa)
import Tarefa1_2021li1g025 (getPeca)

-- dados que permitem comparar a posição relativa do jogador e as peças que o circundam
data PontosCardeais = NO | N | NE | O | E | SO | S | SE 
                    deriving (Show, Eq, Read)

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo movimento = undefined

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos x [] = x
correrMovimentos jogo movimentos = undefined

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














































                                          
