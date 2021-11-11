{- |
Module      : Tarefa1_2021li1g025
Description : Validação de um potencial mapa
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g025 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas | length colunas /= xmax + 1 = False
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

-- ordena a lista de pontos segundo x e y crescente
ordena :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordena [] = []
ordena (x:l) = inserePeca x (ordena l)

inserePeca :: (Peca, Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
inserePeca x [] = [x]
inserePeca p1@(_,(x1,y1)) (p2@(_,(x2,y2)):l) | x1 > x2 = p2:(inserePeca p1 l)
                                             | y1 > y2 = p2:(inserePeca p1 l)
                                             | otherwise = p1:p2:l-- assume-se que nunca pode ser igual a uma peca existente

-- devolve a altura maxima
yMax :: [(Peca,Coordenadas)] -> Int
yMax [] = 0
yMax l = case (last l) of (_,(_,y)) -> y

-- devolve o comprimento maximo, sendo dados os ultimos elementos de cada linha
xMax :: [(Peca,Coordenadas)] -> Int
xMax [] = 0
xMax [(_,(x,_))] = x
xMax ((p1@(_,(x,_))):l) | x > x2 = x
                        | otherwise = x2
                        where x2 = xMax l

-- devolve uma linha, dada a sua altura (yMax para ultima linha)
getLinha :: [(Peca,Coordenadas)] -> Int -> [(Peca,Coordenadas)]
getLinha [] _ = []
getLinha ((e@(_,(_,y))):l) n | y == n = e:(getLinha l n)
                             | otherwise = getLinha l n

-- conta o numero de portas, caixas e blocos
contaPecas :: [(Peca,Coordenadas)] -> (Int,Int,Int)
contaPecas [] = (0,0,0)
contaPecas ((x,_):l) | x == Porta = (1+a,b,c)
                     | x == Bloco = (a,b+1,c)
                     | x == Caixa = (a,b,c+1)
                     | otherwise = (a,b,c)
                     where (a,b,c) = contaPecas l

-- devolve pecas dada as coordenadas ( devolve lista para verificar repetencias ou nao existencia de pecas)
getPeca :: [(Peca,Coordenadas)] -> Coordenadas -> [(Peca,Coordenadas)]
getPeca [] _ = []
getPeca ((p,c1):l) c2 | c1 == c2 = (p,c1):(getPeca l c2)
                      | otherwise = getPeca l c2

--devolve coluna DE BLOCOS e o mapa sem essa coluna (assim quando uma coluna está calculada podemos ignora la)
-- funciona
getColuna :: [(Peca,Coordenadas)] -> Int -> ([(Peca,Coordenadas)],[(Peca,Coordenadas)])
getColuna [] _ = ([],[])
getColuna (p@(peca,(x,_)):l) n | peca == Bloco = if x == n
                                                 then (p:a,b)
                                                 else (a,p:b)
                               | otherwise = (a,b)
                               where (a,b) = getColuna l n

-- devolve uma lista de listas em que cada lista são as pecas de uma coluna (n = xMax do mapa)
-- cuidado com a ordem das colunas
getColunas :: [(Peca,Coordenadas)] -> Int -> [[(Peca,Coordenadas)]] 
getColunas l n | n < 0 = []
               | otherwise = a:(getColunas b (n-1))
               where (a,b) = getColuna l n

-- devolve blocos mais "baixos" de cada coluna
getBase :: [[(Peca,Coordenadas)]] -> [(Peca,Coordenadas)]
getBase [] = []
getBase (x:l) | x == [] = getBase l
              | otherwise = (last x):(getBase l)


validaPeca :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> Bool
validaPeca _ [] = False
validaPeca (peca,(x,y)) l | getPeca l (x-1,y-1) /= [] = True
                          | getPeca l (x-1,y) /= [] = True
                          | getPeca l (x-1,y+1) /= [] = True
                          | otherwise = False

validaBase :: [(Peca,Coordenadas)] -> [[(Peca,Coordenadas)]] -> Bool
validaBase [_] _ = True
validaBase (p:base) ct@(c1:c2:colunas) | validaPeca p c2 = continua
                                       | peca /= [] = if validaPeca (head peca) c2
                                                      then continua
                                                      else False
                                       | otherwise = False
                                       where continua = validaBase base (c2:colunas)
                                             (_,(x,y)) = p
                                             peca = getPeca c1 (x,y-1)
validaBase _ _ = False 

-- let mapa = [(Bloco,(0,0)),(Bloco,(0,2)),(Bloco,(1,0)),(Bloco,(2,2))]
-- let mapa2 = [(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,1))] (tem de dar certo)
-- let mapa3 = [(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(1,1)),(Bloco,(0,2))] (tem de dar certo)
-- let mapa4 = [(Caixa,(0,2)),(Bloco,(0,3)),(Bloco,(1,1)),(Bloco,(1,2)),(Bloco,(2,2)),(Porta,(3,2)),(Bloco,(3,3))]
{-
x1 = ordena mapa2
x2 = xMax x1
x3 = getColunas x1 x2
x4 = getBase x3
validaBase x4 x3
-}
-- 2.1.1 - Função que testa se existem declarações de peças no mesmo síto -- dois processos para exeplorar
-- assume-se que a lista não é vazia
pecaSingular :: [(Peca, Coordenadas)] -> Bool
pecaSingular [x] = True
pecaSingular ((_,p):t) = not(p `elem` (converSecond t)) && pecaSingular t 

 -- Função auxiliar que retorna apenas as "Coordenadas" de qualquer lista de pares ("Peca", "Coordenadas")
converSecond :: [(Peca, Coordenadas)] -> [(Coordenadas)]
converSecond [] = []
converSecond ((_,(x,y)):t) = (x,y) : (converSecond t)

-- TO DO -- Outra forma de resolução da 2.1.1 por explorar
{-
pecaSingular :: [(Peca, Coordenadas)] -> Bool
pecaSingular [x] = True
pecaSingular l@(h:t) = length (getPeca l h) <= 1 && pecaSingular t
-}

--acho que esta se pode apagar
{- -- 2.1.2 - Função que declara exatamente uma porta

declarePorta :: [(Peca,Coordenadas)] -> Bool
declarePorta l = contaPortas l == 1

 -- TO DO -- 2.1.3 - Função que verifica se as caixas estão em posições permitidas 
 
 posCaixa :: [(Peca, Coordenadas)] -> Bool
 posCaixa [] = True
 posCaixa l1@((obj1, (x,y)):t) | obj1 == Caixa && obj2 == Caixa = posCaixa ((obj2, cords):t)
                               | obj1 == Caixa && obj2 /= Bloco = False -- caso a caixa esteja posicionada em cima de uma porta
                               | obj1 == Caixa = length l1' == 1
                               | otherwise = posCaixa t
            where l1'@[(obj2, cords)] = getPeca l1 (x,y-1)
-}

validaCaixas :: [(Peca,Coordenadas)] -> Bool
validaCaixas [] = True
validaCaixas ((peca,(x,y)):l) | peca == Caixa = (case getPeca l (x,y+1) of [] -> False
                                                                           [(Vazio,_)] -> False
                                                                           [(Porta,_)] -> False
                                                                           _ -> validaCaixas l )
                              | otherwise = validaCaixas l