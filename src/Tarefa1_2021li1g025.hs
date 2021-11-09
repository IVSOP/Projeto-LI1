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
validaPotencialMapa pecas = undefined

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

-- conta o numero de portas
contaPortas :: [(Peca,Coordenadas)] -> Int
contaPortas [] = 0
contaPortas ((x,_):l) | x == Porta = 1 + (contaPortas l)
                     | otherwise = contaPortas l

-- devolve pecas dada as coordenadas
getPeca :: [(Peca,Coordenadas)] -> Coordenadas -> [(Peca,Coordenadas)]
getPeca [] _ = error "nenhuma peca encontrada"
getPeca ((p,c1):l) c2 | c1 == c2 = (p,c1):(getPeca l c2)
                      | otherwise = getPeca l c2


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

 -- 2.1.2 - Função que declara exatamente uma porta
declarePorta :: [(Peca,Coordenadas)] -> Bool
declarePorta l = contaPortas l == 1

 -- TO DO -- 2.1.3 - Função que verifica se as caixas estão em posições permitidas 
 {-
 posCaixa :: [(Peca, Coordenadas)] -> Bool
 posCaixa [] = True
 posCaixa l1@((obj1, (x,y)):t) | obj1 == Caixa && obj2 == Caixa = posCaixa ((obj2, cords):t)
                               | obj1 == Caixa && obj2 /= Bloco = False -- caso a caixa esteja posicionada em cima de uma porta
                               | obj1 == Caixa = length l1' == 1
                               | otherwise = posCaixa t
            where l1'@[(obj2, cords)] = getPeca l1 (x,y-1)
-}