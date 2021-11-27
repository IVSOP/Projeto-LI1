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

import Tarefa3_2021li1g025

-- datatype que permite comparar a posição relativa do jogador com as peças que o circundam
data PontosCardeais = NO | N | NE | O | E | SO | S | SE 
                    deriving (Show, Eq, Read)

-- * Funções Principais


-- | Corre um único movimento, devolvendo o jogo no estado após esse movimento
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo@(Jogo mapa (Jogador coord dir caixa)) movimento 
    | movimento == Trepar = Jogo mapa (Jogador (trepar jogo movimento) dir caixa)
    | movimento == InterageCaixa = Jogo mapa (Jogador coord dir (interagirCaixa jogo movimento))
    | movimento == AndarEsquerda = Jogo mapa (Jogador (andar jogo movimento) Oeste caixa) -- estes dois casos sao necessarios para garantir que se troca de direcao sempre
    | movimento == AndarDireita = Jogo mapa (Jogador (andar jogo movimento) Este caixa)

-- | Corre uma lista de movimentos, devolvendo o jogo no estado após esses movimentos
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j
correrMovimentos jogo (mov:l) = correrMovimentos (moveJogador jogo mov) l

-- * Funcções auxiliares

-- acho que se pode tirar. Não está a ser usada ?
-- | Retorna as coordendas (x,y) de uma peça do mapa
getpeca :: Mapa -> Coordenadas -> Peca
getpeca m (x,y) = (m !! y) !! x

-- | Retorna as peças que circundam o jogador (num raio 1) , pela ordem (NO,N,NE,O,E,SO,S,SE), segundo os pontos cardeais
-- | Supõe-se que o jogador está sempre rodeado de peças (de qualquer tipo)
mapAround :: Mapa -> Coordenadas -> [Peca]
mapAround [] _ = []
mapAround m (x,y) = mapAroundAux m (x,y) (x-1,y-1)
    where mapAroundAux :: Mapa -> Coordenadas -> Coordenadas -> [Peca]
          mapAroundAux m player@(x1,y1) (x2,y2) | player == (x2,y2) = mapAroundAux m player (x2+1,y2) -- ignora a peça em que o jogador está (já se sabe que é vazio)
                                                | abs (y1-y2) > 1 = []
                                                | abs (x1-x2) > 1 = mapAroundAux m player (x1-1,y2+1)
                                                | otherwise = ((m !! y2) !! x2 ) : mapAroundAux m player (x2+1,y2)

-- | Testa se um bloco numa posicao cardeal relativa ao jogador é bloco ou caixa
-- | A lista em que atua tem de vir da função "mapAround"
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

-- CRITÉRIO ANDAR PARA A ESQUERDA E DIREITA -- 

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

-- CRITÉRIO TREPAR --

-- | Retorna as coordenadas resultantes de tentar trepar o obstáculo imediatamente à sua frente

-- | Verifica inicialemnte se o jogador transporta uma caixa e redireciona os dados para auxiliares ("treparsemCaixa" e "treparcomCaixa")
trepar :: Jogo -> Movimento -> Coordenadas
trepar (Jogo [] (Jogador c _ _)) _ = c
trepar j@(Jogo m (Jogador (x,y) _ carryBox)) _ = case carryBox of 
    True  -> treparcomCaixa j (mapAround m (x,y)) (mapAround m (x,y-1))
    False -> treparsemCaixa j (mapAround m (x,y))


-- | Quando o jogador não segura uma caixa, @treparsemCaixa@ verifica se existe simultaneamente um bloco ou caixa em frente ao jogador e uma peça vazio (ou porta) em cima-frente dele

-- | Impede o jogador de trepar portas, peças de vazio, obstáculos com mais de um bloco de altura, entre outras ocorrências
treparsemCaixa :: Jogo -> [Peca] -> Coordenadas
treparsemCaixa j@(Jogo _ (Jogador (x,y) _ _)) circle | isBlockorBox N circle = (x,y)
treparsemCaixa j@(Jogo m (Jogador (x,y) direc _)) circle = case direc of 
    Oeste   | isBlockorBox O circle && not(isBlockorBox NO circle) -> (x-1,y-1)
            | otherwise -> (x,y)

    Este    | isBlockorBox E circle && not(isBlockorBox NE circle) -> (x+1,y-1)
            | otherwise -> (x,y)

-- | Quando o jogador segura uma caixa, @treparcomCaixa@ verifica se existe um bloco ou caixa na posição 1 telha à frente e 2 telhas a cima do jogador (imagine-se uma forma de "L") 

-- | Impede o jogador de trepar para espaços apertados em que o teto não permite o transporte de uma caixa.

-- | Depois valida a ação de trepar sem caixa, chamando a função @treparsemCaixa@
treparcomCaixa :: Jogo -> [Peca] -> [Peca] ->  Coordenadas
treparcomCaixa j@(Jogo m (Jogador c@(x,y) direc _)) circlejogador circlecaixa = case direc of 
    Oeste   | isBlockorBox NO circlecaixa -> c
            | otherwise -> treparsemCaixa j circlejogador

    Este    | isBlockorBox NE circlecaixa -> c
            | otherwise -> treparsemCaixa j circlejogador


-- CRITÉRIO INTERAGIR COM CAIXA --

-- | Faz com que o jogador carregue/largue uma caixa (se possível)
interagirCaixa :: Jogo -> Movimento -> Bool
interagirCaixa (Jogo [] (Jogador _ _ caixa)) _ = caixa
interagirCaixa j@(Jogo m (Jogador (x,y) _ carryBox)) _ = case carryBox of 
    True -> undefined
    False -> pegarCaixa j (mapAround m (x,y))

-- | Quando o jogador não segura uma caixa, @pegarCaixa@ testa a possiblidade de pegar numa caixa

-- | Verifica simultaneamente se existe uma caixa na posição à frente do jogador e, caso haja, verifica se nenhuma caixa ou bloco se encontra por cima dela

-- | Impede o jogador de pegar em caixas quando existem objetos em cima da caixa ou do jogador
pegarCaixa  :: Jogo -> [Peca] -> Bool
pegarCaixa (Jogo _ (Jogador (x,y) direc _)) circle | isBlockorBox N circle = False
pegarCaixa (Jogo _ (Jogador (x,y) direc _)) circle = case direc of 
    Oeste   | (circle !! 3) == Caixa && not(isBlockorBox NO circle) -> True
            | otherwise -> False

    Este    | (circle !! 4) == Caixa && not(isBlockorBox NE circle) -> True
            | otherwise -> False



