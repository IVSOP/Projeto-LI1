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


-- testa se a última linha tem vazios, ou seja, se o x max do mapa é igual ao x max da linha, e se não há espacos vazios na linha
validaBase :: [(Peca,Coordenadas)] -> Bool
validaBase [] = True
validaBase l | xMax l <= xMax ult = if (length [(peca,(x,y)) | (peca,(x,y)) <- ult, peca == Bloco]) == (xMax ult)+1 -- se o numero de blocos (length ...) for igual ao xMax+1, todas as colunas têm bloco por baixo
                                    then True
                                    else False

             | otherwise = False
             where ult = ultLinha l (yMax l)

-- devolve a altura maxima
yMax :: [(Peca,Coordenadas)] -> Int
yMax [] = 0
yMax ((_,(x,y)):l) | y >= yMax l = y
                   | otherwise = yMax l

-- devolve o comprimento maximo
xMax :: [(Peca,Coordenadas)] -> Int
xMax [] = 0
xMax ((_,(x,y)):l) | x >= xMax l = x
                   | otherwise = xMax l

-- devolve a ultima linha, dada a sua altura (yMax)
ultLinha :: [(Peca,Coordenadas)] -> Int -> [(Peca,Coordenadas)]
ultLinha [] _ = []
ultLinha ((e@(_,(_,y))):l) n | y == n = e:(ultLinha l n)  
                             | otherwise = ultLinha l n