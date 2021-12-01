{- |
Module      : Tarefa4_2021li1g025
Description : Movimentação do personagem
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.

Constituído por duas funções principais 'moveJogador' e 'correrMovimentos'.

O mesmo mapa da Tarefa1 continua a ser usado para os exemplos neste módulo.

-}
module Tarefa4_2021li1g025 where

import LI12122
import Tarefa3_2021li1g025 -- para dar show ao jogo

-- |  Permite comparar a posição relativa do jogador com as peças que o circundam. Utilizado na função 'mapAround'.
data PontosCardeais 
 = NO -- ^ Noroeste
 | N -- ^ Norte
 | NE -- ^ Nordeste
 | O -- ^ Oeste
 | E -- ^ Este
 | SO -- ^ Sudoeste
 | S -- ^ Sul
 | SE -- ^ Sudeste
   deriving (Show, Eq, Read)

-- * Funções Principais

{- | Corre um único movimento, devolvendo o jogo no estado após esse movimento. 

Estuda vários critérios, desenvolvidos nas funções 'andar','trepar','largarCaixa','pegarCaixa'.

-} 

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador jogo@(Jogo mapa (Jogador coord dir caixa)) movimento
    | movimento == Trepar = Jogo mapa (Jogador (trepar jogo movimento) dir caixa)
    | movimento == InterageCaixa = 
        case caixa of True -> largarCaixa jogo
                      False -> pegarCaixa jogo (mapAround mapa coord)
    | movimento == AndarEsquerda = Jogo mapa (Jogador (andar jogo movimento) Oeste caixa) -- estes dois casos sao necessarios para garantir que se troca de direcao sempre
    | movimento == AndarDireita = Jogo mapa (Jogador (andar jogo movimento) Este caixa)

{- | Corre uma lista de movimentos, devolvendo o jogo no estado após esses movimentos.

Chama recursivamente a função 'moveJogador'.

-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j
correrMovimentos jogo (mov:l) = correrMovimentos (moveJogador jogo mov) l

-- * Funções auxiliares

{- | Retorna as peças que circundam o jogador (num raio 1) pela ordem ('NO','N','NE','O','E','SO','S','SE'), segundo os pontos cardeais.

Supõe-se que o jogador está sempre rodeado de peças (de qualquer tipo).

=== Exemplo: 

>>> mapAround [[Bloco,Caixa,Bloco,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Vazio,Porta,Bloco],[Bloco,Vazio,Vazio,Bloco,Bloco,Bloco],[Bloco,Vazio,Caixa,Bloco,Bloco,Vazio],[Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]] (3,1)
[Bloco,Vazio,Vazio,Vazio,Porta,Vazio,Bloco,Bloco]

Note-se:

* O resultado corresponde aos blocos que rodeiam o jogador quando este se encontra na posição (3,1).
-}

mapAround :: Mapa -- ^ Mapa original
    -> Coordenadas -- ^ Coordenadas do jogador no mapa
    -> [Peca]
mapAround [] _ = []
mapAround m (x,y) = mapAroundAux m (x,y) (x-1,y-1)
    where mapAroundAux :: Mapa -> Coordenadas -> Coordenadas -> [Peca]
          mapAroundAux m player@(x1,y1) (x2,y2) | player == (x2,y2) = mapAroundAux m player (x2+1,y2) -- ignora a peça em que o jogador está (já se sabe que é vazio)
                                                | abs (y1-y2) > 1 = []
                                                | abs (x1-x2) > 1 = mapAroundAux m player (x1-1,y2+1)
                                                | otherwise = ((m !! y2) !! x2 ) : mapAroundAux m player (x2+1,y2)

{- | Testa se um bloco numa posição cardeal relativa ao jogador é bloco ou caixa.

A lista em que atua tem obrigatoriamente de vir da função 'mapAround'.

=== Exemplo: 

>>> isBlockorBox NO [Bloco,Vazio,Vazio,Vazio,Porta,Vazio,Bloco,Bloco]
True

>>> isBlockorBox E [Bloco,Vazio,Vazio,Vazio,Porta,Vazio,Bloco,Bloco]
False

Usando o mapa apresentado anteriormente na função 'mapAround' verifica-se:

* A noroeste (NO) do jogador existe um Bloco.

* A este (E) do jogador não existe um Bloco ou Caixa.

-}
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

-- ** Critério andar para a esquerda e direita

{- | Retorna as coordenadas resultantes de andar para a esquerda ou direita, com ou sem caixa

Assim, é possível, por exemplo, cair vários blocos de uma só vez ou não sair do lugar se o movimento não for possível

Neste exemplo temos Jogador (6,1) Este True

>>> andar jogo AndarDireita
(7,5)

-}

-- os casos para esquerda e direita são separados para evitar erros (como coordenadas negativas)
andar :: Jogo -> Movimento -> Coordenadas
andar (Jogo [] (Jogador c _ _)) _ = c
andar (Jogo mapa (Jogador (x,y) _ True)) AndarEsquerda -- com caixa
    | (posj == Vazio || posj == Porta) && posc == Vazio = (x2,y2-1) 
    | otherwise = (x,y)
    where linhac:linhaj:l = drop (y-1) mapa
          posj = linhaj !! (x-1)
          posc = linhac !! (x-1)
          (x2,y2) = getNext l (x-1,y+1)
andar (Jogo mapa (Jogador (x,y) _ True)) _
    | (posj == Vazio || posj == Porta) && posc == Vazio = (x2,y2-1)
    | otherwise = (x,y)
    where linhac:linhaj:l = drop (y-1) mapa
          posj = linhaj !! (x+1)
          posc = linhac !! (x+1)
          (x2,y2) = getNext l (x+1,y+1)

andar (Jogo mapa (Jogador (x,y) _ _)) AndarEsquerda
    | (posj == Vazio || posj == Porta) = (x2,y2-1)
    | otherwise = (x,y)
    where linhaj:l = drop y mapa 
          posj = linhaj !! (x-1)
          (x2,y2) = getNext l (x-1,y+1)
andar (Jogo mapa (Jogador (x,y) _ _)) _
    | (posj == Vazio || posj == Porta) = (x2,y2-1)
    | otherwise = (x,y)
    where linhaj:l = drop y mapa
          posj = linhaj !! (x+1)
          (x2,y2) = getNext l (x+1,y+1)

{- | Devolve as coordenadas bloco mais próximo abaixo de certas coordenadas, se existir

É usada, por exemplo, para encontrar onde o jogador irá cair após andar num local sem "chão" imediatamente à sua frente

Como esta devolve as coordenadas do bloco ou caixa e não o último vazio, é importante subtrair 1 ao y do resultado final ao utilizar esta função

Assume-se que o mapa será aquele sem as linhas "inúteis", ou seja, apenas inclui as linhas diretamente abaixo do jogador

>>> getNext mapa (7,2)
(7,8), o que estaria errado
>>> getNext (drop 2 mapa) (7,2)
(7,6) o que está certo, após remover as linhas que não interessam
-}
getNext :: Mapa -> Coordenadas -> Coordenadas
getNext [] c = c
getNext (linha:l) (x,y) | (posicao == Bloco || posicao == Caixa) = (x,y)
                        | otherwise = getNext l (x,y+1)
                        where (a,posicao:b) = splitAt x linha

-- ** Critério trepar

{- | Retorna as coordenadas resultantes de (tentar) trepar o obstáculo imediatamente à sua frente.

Verifica inicialmente se o jogador transporta uma caixa e redireciona os dados para as auxiliares 'treparsemCaixa' e 'treparcomCaixa'.

=== Exemplos: 

Quando o jogador não segura uma caixa e tenta trepar para um espaço com um bloco por cima, consegue fazê-lo:

>>> trepar (Jogo [[Bloco,Vazio,Bloco,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False)) Trepar
(2,1)

Quando o jogador segura uma caixa e tenta trepar para um espaço com um bloco por cima, não o consegue fazer:

>>> trepar (Jogo [[Bloco,Vazio,Bloco,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Bloco,Vazio,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste True)) Trepar
(3,2)

-}

trepar :: Jogo -> Movimento -> Coordenadas
trepar (Jogo [] (Jogador c _ _)) _ = c
trepar j@(Jogo m (Jogador (x,y) _ carryBox)) _ = case carryBox of 
    True  -> treparcomCaixa j (mapAround m (x,y)) (mapAround m (x,y-1))
    False -> treparsemCaixa j (mapAround m (x,y))

{- | Quando o jogador não segura uma caixa, 'treparsemCaixa' verifica se existe simultaneamente um bloco ou caixa em frente ao jogador e uma peça vazio (ou porta) em cima desse bloco/caixa.

=== Propriedades:

prop> Impede o jogador de trepar portas, peças do tipo 'Vazio' , obstáculos com mais de um bloco de altura, entre outras ocorrências.

-}

treparsemCaixa :: Jogo -> [Peca] -> Coordenadas
treparsemCaixa j@(Jogo _ (Jogador (x,y) _ _)) circle | isBlockorBox N circle = (x,y)
treparsemCaixa j@(Jogo m (Jogador (x,y) direc _)) circle = case direc of 
    Oeste   | isBlockorBox O circle && not(isBlockorBox NO circle) -> (x-1,y-1)
            | otherwise -> (x,y)

    Este    | isBlockorBox E circle && not(isBlockorBox NE circle) -> (x+1,y-1)
            | otherwise -> (x,y)

{- | Quando o jogador segura uma caixa, 'treparcomCaixa' verifica se existe um bloco ou caixa na posição 1 telha à frente e 2 telhas a cima do jogador.

Depois valida a ação de trepar sem essa caixa, chamando a função 'treparsemCaixa'.

=== Propriedades:

prop> Impede o jogador de trepar para espaços apertados em que o teto não permite o transporte de uma caixa.


-}

treparcomCaixa :: Jogo -> [Peca] -> [Peca] ->  Coordenadas
treparcomCaixa j@(Jogo m (Jogador c@(x,y) direc _)) circlejogador circlecaixa = case direc of 
    Oeste   | isBlockorBox NO circlecaixa -> c
            | otherwise -> treparsemCaixa j circlejogador

    Este    | isBlockorBox NE circlecaixa -> c
            | otherwise -> treparsemCaixa j circlejogador

-- ** Critério InteragirCaixa

{- | Retorna o 'Jogo' resultantes de (tentar) pegar ou largar caixas no Mapa.

Chama as funções 'pegarCaixa' e 'largarCaixa'

-}

interageCaixa :: Jogo -> Jogo
interageCaixa _ = undefined

{- | Quando o jogador não segura uma caixa, 'pegarCaixa' testa a possiblidade de pegar numa caixa (e consequentemente removê-la do mapa).

Verifica simultaneamente se existe uma caixa na posição à frente do jogador e, caso haja, verifica se nenhuma caixa ou bloco se encontra por cima dela.

Impede o jogador de pegar em caixas quando existem objetos em cima da caixa ou do jogador.

=== Exemplo:

Neste exemplo temos Jogador (4,3) Este False

>>> pegarCaixa jogo
X           X
X           X
X   * X     X
X   > X     X
X   XXX X   X
X CX  X*X  PX
XXX    XXXXXX
* é a caixa recebida da coordenada (5,3)

-}

pegarCaixa  :: Jogo -- ^ Mapa e caraterísticas do jogador.
    -> [Peca] -- ^ Lista com as peças que rodeiam o jogador (obtido de 'mapAround').
    -> Jogo
pegarCaixa j@(Jogo m (Jogador (x,y) direc _)) circle | isBlockorBox N circle = j
pegarCaixa j@(Jogo m (Jogador (x,y) direc _)) circle = case direc of 
    Oeste   | (circle !! 3) == Caixa && not(isBlockorBox NO circle) -> (Jogo (mapaAntes ++ [antesNaLinha ++ [Vazio] ++ depoisNaLinha] ++ mapaDepois) (Jogador (x,y) direc True))
            | otherwise -> j

        where   (mapaAntes,linhaFoco: mapaDepois) = splitAt y m 
                (antesNaLinha, pecaEsquerdadoJogador:depoisNaLinha) = splitAt (x-1) linhaFoco

    Este    | (circle !! 4) == Caixa && not(isBlockorBox NE circle) -> (Jogo (mapaAntes ++ [antesNaLinha ++ [Vazio] ++ depoisNaLinha] ++ mapaDepois) (Jogador (x,y) direc True))
            | otherwise -> j

        where   (mapaAntes,linhaFoco: mapaDepois) = splitAt y m 
                (antesNaLinha, pecaDepoisdoJogador:depoisNaLinha) = splitAt (x+1) linhaFoco

{- | Devolve um jogo com um mapa em que foi (ou não) inserida a caixa largada pelo jogador

Neste exemplo, temos Jogador (6,1) Este True

>>> largarCaixa jogo
X           X
X     >     X
X     X     X
X    CX     X
X   XXX X   X
X CX  X*X  PX
XXX    XXXXXX
* é a caixa largada

-}

largarCaixa :: Jogo -> Jogo
largarCaixa jogo@(Jogo mapa (Jogador (x,y) Oeste _))
    | pos1 /= Vazio = jogo
    | pos2 /= Vazio = Jogo (l1 ++ [c1 ++ [Caixa] ++ c1l] ++ [linha2] ++ l2) (Jogador (x,y) Oeste False)
    | otherwise = let (x2,y2) = getNext l2 (x-1,y+1)
                      (l3,linhac:l4) = splitAt (y2-1) mapa
                      (c3,posc:c4) = splitAt x2 linhac in
                  Jogo (l3 ++ [c3 ++ [Caixa] ++ c4] ++ l4) (Jogador (x,y) Oeste False) 
    where (l1,linha1:linha2:l2) = splitAt (y-1) mapa
          (c1,pos1:c1l) = splitAt (x-1) linha1
          (c2,pos2:c2l) = splitAt (x-1) linha2
largarCaixa jogo@(Jogo mapa (Jogador (x,y) _ _))
    | pos1 /= Vazio = jogo
    | pos2 /= Vazio = Jogo (l1 ++ [c1 ++ [Caixa] ++ c1l] ++ [linha2] ++ l2) (Jogador (x,y) Este False)
    | otherwise = let (x2,y2) = getNext l2 (x+1,y+1)
                      (l3,linhac:l4) = splitAt (y2-1) mapa
                      (c3,posc:c4) = splitAt x2 linhac in
                  Jogo (l3 ++ [c3 ++ [Caixa] ++ c4] ++ l4) (Jogador (x,y) Este False)
    where (l1,linha1:linha2:l2) = splitAt (y-1) mapa
          (c1,pos1:c1l) = splitAt (x+1) linha1
          (c2,pos2:c2l) = splitAt (x+1) linha2