{- |
Module      : Tarefa1_2021li1g025
Description : Validação de um potencial mapa
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.

Para os exemplos, será utilizado o seguinta mapa:

<<example.png>>

Onde a porta é representada a azul, os blocos a vermelho e as caixas a laranja
-}
module Tarefa1_2021li1g025 where

import LI12122
-- * Função principal
{- | Testa todos os critérios, recebendo o mapa sob o nome "pecas".
 
 ====Para tal, é necessário armazenar estes valores:

 * Ordenar o mapa

     > mapa = ordena pecas

 * Obter o maior valor de coluna e linha (x,y)

     @
     xmax = xMax mapa
     ymax = yMax mapa
     @

 * Criar uma lista das colunas

     > colunas = getColunas mapa xmax

 * Obter a "base" do mapa (últimos blocos de cada coluna)

     > base = getBase colunas

 * Obter o número de cada peça (exceto do Vazio)

     > (portas,blocos,caixas) = contaPecas mapa

 ====Com isto, podemos testar todos os critérios

 Se algum destes for __True__, então o mapa __não__ é válido:

 * Não haver mais que uma declaração de peça para a mesma posição

     > not (pecaSingular mapa) 

 * Declarar exatamente uma porta

     > portas /= 1

 * Todas as caixas devem estar posicionadas em cima de outra caixa ou bloco

     > not (validaCaixas mapa)

 * Deve existir pelo menos um expaço vazio 

     Número total de blocos /= número de casas totais no mapa

     > (xmax+1)*(ymax+1) == (portas+blocos+caixas)

 * A base do mapa deve ser composta por blocos

     Não podem haver colunas sem blocos

     > elem [] colunas

     Os blocos da base têm de ser "válidos"

     > not (validaBase base colunas)
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas | elem [] colunas = False
                          | not (pecaSingular mapa) = False
                          | not (validaBase base colunas) = False 
                          | portas /= 1 = False
                          | not (validaCaixas mapa) = False
                          | (xmax+1)*(ymax+1) == portas+blocos+caixas = False
                          | otherwise = True
                          where mapa = ordena pecas
                                xmax = xMax mapa
                                colunas = getColunas mapa xmax
                                base = getBase colunas
                                ymax = yMax mapa
                                (portas,blocos,caixas) = contaPecas mapa

-- * Funções auxiliares

-- | Ordena o mapa segundo x e y crescente
ordena :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordena [] = []
ordena (x:l) = inserePeca x (ordena l)

-- | Insere uma peça numa lista (ordenada) de peças
inserePeca :: (Peca, Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
inserePeca x [] = [x]
inserePeca p1@(_,(x1,y1)) (p2@(_,(x2,y2)):l) | x1 > x2 = p2:(inserePeca p1 l)
                                             | y1 > y2 = p2:(inserePeca p1 l)
                                             | y1 < y2 = p1:p2:l
                                             | x1 < x2 = p1:p2:l

-- | Devolve o y maximo do mapa
yMax :: [(Peca,Coordenadas)] -> Int
yMax [] = 0
yMax l = case (last l) of (_,(_,y)) -> y

-- | Devolve o x maximo do mapa
xMax :: [(Peca,Coordenadas)] -> Int
xMax [] = 0
xMax [(_,(x,_))] = x
xMax ((p1@(_,(x,_))):l) | x > x2 = x
                        | otherwise = x2
                        where x2 = xMax l

-- | Conta o numero de portas, caixas e blocos
contaPecas :: [(Peca,Coordenadas)] -> (Int,Int,Int)
contaPecas [] = (0,0,0)
contaPecas ((x,_):l) | x == Porta = (1+a,b,c)
                     | x == Bloco = (a,b+1,c)
                     | x == Caixa = (a,b,c+1)
                     | otherwise = (a,b,c)
                     where (a,b,c) = contaPecas l

-- | Devolve pecas dada as suas coordenadas. Devolve uma lista para que possamos verificar repetições ou não existência de peças
getPeca :: [(Peca,Coordenadas)] -> Coordenadas -> [(Peca,Coordenadas)]
getPeca [] _ = []
getPeca ((p,c1):l) c2 | c1 == c2 = (p,c1):(getPeca l c2)
                      | otherwise = getPeca l c2

{- | Devolve uma coluna de blocos e o mapa sem essa coluna, para maior eficiência na função 'getColunas'

>>> getColuna mapa 1
([Bloco,(1,6)], [(Bloco,(0,0)),...(restante mapa))
-}
getColuna :: [(Peca,Coordenadas)] -- ^ Mapa
 -> Int -- ^ Índice da coluna (x)
  -> ([(Peca,Coordenadas)],[(Peca,Coordenadas)]) -- ^ (coluna,mapa sem coluna)
getColuna [] _ = ([],[])
getColuna (p@(peca,(x,_)):l) n | peca == Bloco = if x == n
                                                 then (p:a,b)
                                                 else (a,p:b)
                               | otherwise = (a,b)
                               where (a,b) = getColuna l n

{- | Devolve uma lista de listas em que cada lista corresponde a uma coluna

As colunas ficam ordenadas segundo y crescente, mas segundo x decrescente

>>> getColunas mapa 12
[[(Bloco,(12,0)),(Bloco,(12,1)),(Bloco,(12,2)),(Bloco,(12,3)),(Bloco,(12,4)),(Bloco,(12,5)),(Bloco,(12,6))],...,[(Bloco,(8,4)),(Bloco,(8,5)),(Bloco,(8,6))]...
-}
getColunas :: [(Peca,Coordenadas)] -- ^ Mapa
 -> Int -- ^ Maior índice de coluna (xmax)
  -> [[(Peca,Coordenadas)]] -- ^ Lista de colunas
getColunas l n | n < 0 = []
               | otherwise = a:(getColunas b (n-1))
               where (a,b) = getColuna l n

{- | Devolve blocos mais "baixos" de cada coluna (dada a lista de colunas)

>>> getBase (getColunas mapa 12)
[(Bloco,(12,6)),(Bloco,(11,6)),(Bloco,(10,6)),(Bloco,(9,6)),(Bloco,(8,6)),(Bloco,(7,6)),(Bloco,(6,5)),(Bloco,(5,4)),...
-}
getBase :: [[(Peca,Coordenadas)]] -> [(Peca,Coordenadas)]
getBase [] = []
getBase (x:l) | x == [] = getBase l
              | otherwise = (last x):(getBase l)

{- | Verifica se uma dada peça está "suportada" por outra peça numa coluna adjacente.

Para isso, sendo dada essa peça e a coluna à sua esquerda, testa se existem peças adjacentes à peça
-}
validaPeca :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
validaPeca _ [] = False
validaPeca (peca,(x,y)) l | getPeca l (x-1,y-1) /= [] = True
                          | getPeca l (x-1,y) /= [] = True
                          | getPeca l (x-1,y+1) /= [] = True
                          | otherwise = False

{- | Verifica se a base do mapa é válida
Estes esquemas ilustram como a função valida uma base:

Primeiro, é dada a base do mapa como argumento

<<map1.png>> <<map2.png>> <<map3.png>>

<<map1.2.png>> <<map2.2.png>> <<map3.2.png>>

Depois, da direita para a esqueda, cada peça da base é validada, através da 'validaPeca' que recebe essa peca e a coluna à sua esquerda

<<map1.3.png>> <<map2.3.png>> <<map3.3.png>>

No caso do primeiro mapa, a primeira peça não é válida.

Quando isto acontece, é verificado se existe uma peça acima

<<map1.4.png>>

Se não existir, a base não é válida.

Neste caso, como existe, esta passa a ser a peça que tem de ser validada

<<map1.5.png>>

Este processo é repetido até percorrer toda a base ou chegar a um caso não válido.
-}
validaBase :: [(Peca,Coordenadas)] -- ^ Base do mapa
 -> [[(Peca,Coordenadas)]]-- ^ Lista de colunas do mapa
  -> Bool -- ^ True se a base for válida
validaBase [_] _ = True
validaBase (p:base) ct@(c1:c2:colunas) | validaPeca p c2 = continua
                                       | peca /= [] = validaBase ((head peca):base) ct -- se fosse caixa, daria erro noutro critério
                                       | otherwise = False
                                       where continua = validaBase base (c2:colunas)
                                             (_,(x,y)) = p
                                             peca = getPeca c1 (x,y-1)
validaBase _ _ = False

-- | Função que testa se existem declarações de peças no mesmo síto, devolvendo False se uma peca se repetir
pecaSingular :: [(Peca, Coordenadas)] -> Bool
pecaSingular [] = True
pecaSingular ((_,coord):t) | length (getPeca t coord) > 0 = False
                           | otherwise = pecaSingular t


-- | Verifica se todas as caixas do mapa têm um bloco ou uma caixa na posição diretamente abaixo
validaCaixas :: [(Peca,Coordenadas)] -> Bool
validaCaixas [] = True
validaCaixas ((peca,(x,y)):l) | peca == Caixa = (case getPeca l (x,y+1) of [] -> False
                                                                           [(Vazio,_)] -> False
                                                                           [(Porta,_)] -> False
                                                                           _ -> validaCaixas l )
                              | otherwise = validaCaixas l