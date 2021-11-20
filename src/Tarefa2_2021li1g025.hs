{- |
Module      : Tarefa2_2021li1g025
Description : Construção/Desconstrução do mapa
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g025 where

import LI12122


-- devolve o indice maximo de linha numa lista
yMax :: [(Peca,Coordenadas)] -> Int
yMax [] = 0
yMax ((_,(_,y)):t) | y > (yMax t) = y
                   | otherwise = yMax t

-- devolve o indice maximo de coluna numa lista
xMax :: [(Peca,Coordenadas)] -> Int
xMax [] = 0
xMax ((_,(x,_)):t) | x > (xMax t) = x
                   | otherwise = xMax t

-- devolve pecas juntamente com as respetivas coordenadas, dando apenas coordenadas
getPeca :: [(Peca,Coordenadas)] -> Coordenadas -> [(Peca,Coordenadas)]
getPeca [] _ = []
getPeca ((p,c1):l) c2 | c1 == c2 = (p,c1):(getPeca l c2)
                      | otherwise = getPeca l c2

-- devolve uma linha de elementos, dado o índice da linha pretendido
getLinha :: [(Peca,Coordenadas)] -> Int -> [(Peca,Coordenadas)]
getLinha [] _ = []
getLinha ((h@(_,(_,y))):t) linha | y == linha = h:(getLinha t linha)
                                 | otherwise = getLinha t linha




-- função que devolve um mapa (esquema) de peças nas suas posições respetivas, incluindo espaços vazios
-- supõe-se que a lista já está organizada

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = [ (constroiLinha l y) | y <- [0..(yMax l)]]

-- função principal que constroi uma linha de peças pela sua ordem respetiva, incluindo espaços vazios
constroiLinha :: [(Peca,Coordenadas)] -> Int -> [Peca] 
constroiLinha l y = constroiLinhaAux (getLinha l y) 0 y (xMax l)
-- aplicando constroiLinha a uma lista com apenas os elementos da linha que quero ( através da função "getLinha"), reduzo o trabalho da função.
-- xMax guarda num acumulador o número maximo de colunas da lsita original


-- função auxiliar que lê uma linha de peças, e preenche os espaços vazios da lista com "Vazio", devolvendo as peças originais e os espaços vazios adicionados.
-- o input "cnst" permite ler a lista da esquerda para a direita
constroiLinhaAux :: [(Peca,Coordenadas)] -> Int -> Int -> Int -> [Peca]
constroiLinhaAux [] coluna linha cnst | coluna == cnst = [Vazio]
                                      | otherwise = Vazio : constroiLinhaAux [] (coluna+1) linha cnst

constroiLinhaAux l@(h:t) coluna linha cnst | coluna == cnst = [(fst h)] -- quando a função atinge o ultimo elemento da linha, fecha a lista
                                           | null (getPeca l (coluna,linha)) = Vazio: constroiLinhaAux l (coluna+1) linha cnst
                                           | otherwise = (fst h) : constroiLinhaAux t (coluna+1) linha cnst






-- Outras funções auxiliares aplicadas a listas de peças (SEM COORDENADAS) para "DescontroiMapa"

-- devolve o indice maximo de linha numa lista
yMax2 :: [[Peca]] -> Int
yMax2 [x] = 0     -- a linha 0 também existe
yMax2 (h:t) = 1 + yMax2 t

-- devolve uma linha de elementos, dado o seu índice de linha
getLinha2 :: [[Peca]] -> Int -> [Peca]
getLinha2 [] _ = []
getLinha2 l linha = (l !! linha)



-- função inversa de "constroiMapa" que transforma uma lista de peças numa lista com as peças que não "Vazio", juntamente com as suas respetivas coordenadas
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa l = agrupaLinhas l 0

-- função que agrupa várias linhas já desconstruídas numa lista global
agrupaLinhas :: Mapa -> Int -> [(Peca, Coordenadas)]
agrupaLinhas l cnst | cnst == yMax2 l = desconstroiLinha l cnst
                    | otherwise = desconstroiLinha l cnst ++ agrupaLinhas l (cnst+1)

-- função principal que constroi uma linha de peças. Antes de passar pela função auxiliar, restringe a lista com que a Aux trabalha a apenas uma lista com os elementos da linha
desconstroiLinha :: [[Peca]] -> Int -> [(Peca, Coordenadas)]
desconstroiLinha l y = desconstroiLinhaAux (getLinha2 l y) 0 y

-- função auxiliar que constroi uma lista de peças ignorando os espaços vazios
desconstroiLinhaAux :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiLinhaAux [] _ _ = []
desconstroiLinhaAux  (h:t) coluna linha  | h == Vazio = desconstroiLinhaAux t (coluna+1) linha
                                         | otherwise = (h,(coluna,linha)): (desconstroiLinhaAux t (coluna+1) linha)


