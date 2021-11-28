{- |
Module      : Tarefa2_2021li1g025
Description : Construção/Desconstrução do mapa
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22. É constituído por duas funções principais: 'constroiMapa' e 'desconstroiMapa'. Permitem convertar mapas entre o formato [Peca,Coordenadas] (sem indicação de espaços vazios) e [Peca]( com indicação de espaços vazios).
A abordagem seguida em ambas as funções foi:
1. Converter a primeira linha do mapa fornecido
2. Agrupar as sucessivas linhas convertidas do mapa, por ordem crescente
3. Obter um mapa final convertido

-}

module Tarefa2_2021li1g025 where

import LI12122
import Tarefa1_2021li1g025 (yMax, xMax,validaPotencialMapa)

-- | = Função constroiMapa

{- |  Dada uma lista de peças e respetivas coordenadas constrói um mapa só de peças (a grelha propriamente dita), incluindo a referência a peças do tipo 'Vazio'

Só funciona se a lista de peças já tiver sido validada (cf. Tarefa 1)

Chama a função 'constroiMapaAux', que irá atuar sobre uma lista ordenada (através de 'ordena')

 === Exemplo

 >>> constroiMapa [(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(0,1)),(Bloco,(4,1)),(Porta,(2,1)),(Bloco,(4,0)),(Bloco,(3,2)),(Bloco,(4,2)),(Bloco,(0,0)),(Bloco,(2,2)),(Caixa,(3,1))]
[[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Porta,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

-}

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l | validaPotencialMapa l == False = []
constroiMapa l = constroiMapaAux list 0 (yMax list) (xMax l)
    where list = ordena l 

{- | Agrupa várias linhas construídas numa lista global. Para isso, chama recursivamente a função 'constroiLinha' aumentando em cada passagem, o índice de linha construída
Supõe-se que a lista já foi ordenada.

 === Propriedades:

prop> Em cada passagem, a linha de menor índice da lista é convertida e é removida do mapa inicial ( através de 'getLinha') para tornar mais eficientes as passagens seguintes. 
Quando 'getLinha' procura os elementos da linha seguinte do mapa, já não tem que passar pelos elementos da linha anterior.

prop> Quando o valor do índice de linha (valor y) , guardado no acumulador, atinge o valor máximo y do mapa (obtido através de 'yMax', a função percebe que chegou à última linha e fecha a lista.

=== Exemplo:

>>> constroiMapaAux [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Porta,(2,2)),(Bloco,(3,2)),(Bloco,(4,2)),(Bloco,(1,3)),(Caixa,(2,3))] 0 3 4
[[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Porta,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]

Para além da lista de peças, note-se:

* 0 é o índice da linha que a função irá converter primeiro. 
Como a função percorre a lista da cima para baixo, para o funcionamento correto da função é necessário introduzir o 0 neste parâmetro.

* 3 é o índice da linha máximo (valor y) do mapa exemplificado. Como referido, permite detetar a última linha do mapa.

* 4 é o índice máximo de colunas (valor x) do mapa exemplificado
-}


constroiMapaAux :: [(Peca, Coordenadas)] -- ^ Mapa
 -> Int -- ^ acumulador que guarda o valor y da linha em que a função está a trabalhar (0 no início, obrigatoriamente).
 -> Int -- ^ constante com o valor máximo y da lista (através de 'yMax'). 
 -> Int -- ^ constante com o valor máximo x da lista (através de 'xMax'). Será necessário para a função 'constroiLinha'
 -> Mapa

constroiMapaAux l yAtual yMax  xMax | yAtual == yMax = [constroiLinha a 0 yAtual xMax]
                                    | otherwise = constroiLinha a 0 yAtual xMax : (constroiMapaAux b (yAtual+1) yMax xMax)
        where (a,b) = getLinha l yAtual


{- | == constroiLinha

Dada uma lista válida de peças e as suas respetivas coordenadas, constrói o mapa de uma linha.

Devolve as peças originais da lista e preenche os espaços da lista omitidos com 'Vazio'

=== Propriedades:

prop> Quando uma peça de uma linha dada tem um índice de coluna (valor x), guardado no acumulador, igual ao valor máximo x do mapa, a função percebe que chegou à última coluna e fecha a lista.

prop> Como a função trabalha apenas com uma lista de elementos da própria linha, é mais eficiente a construção da linha.

=== Exemplos:

>>> constroiLinha [(Porta,(2,3)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco,(6,3)),(Caixa,(7,3))] 0 3 10
[Vazio,Vazio,Porta,Vazio,Bloco,Bloco,Bloco,Caixa,Vazio,Vazio,Vazio]

Para além da lista de peças, note-se:

* 0 é o índice de coluna da peça que a função irá converter primeiro. 
Como a função percorre a lista da esquerda para a direita, para o funcionamento correto da função é necessário introduzir o 0 neste parâmetro

* 3 é o índice da linha ( valor y) que pretendemos construir

*10 é o índice máximo de colunas (valor x) do mapa específico que pretendemos

-}
constroiLinha :: [(Peca,Coordenadas)] -- ^ Mapa com apenas as peças da linha (através de 'getLinha')
 -> Int -- ^ Acumulador que guarda o índice da coluna em que a função está a trabalhar.
 -> Int -- ^ Constante com o valor y da linha que pretendemos construir.
 -> Int -- ^ Constante com o valor máximo x do mapa original (através de 'xMax' (importada da Tarefa 1)). 
 -> [Peca]
constroiLinha [] xAtual linha xMax  | xAtual == xMax = [Vazio]
                                    | otherwise = Vazio : constroiLinha [] (xAtual+1) linha xMax

constroiLinha l@(h@(_,(x,_)):t) xAtual linha xMax   | xAtual == xMax = [(fst h)] -- quando a função atinge o ultimo elemento da linha, fecha a lista
                                                    | x == xAtual = (fst h) : constroiLinha t (xAtual+1) linha xMax
                                                    | otherwise = Vazio: constroiLinha l (xAtual+1) linha xMax


-- * Funções Auxiliares de 'constroiMapa'

{- | Ordena o mapa segundo x e y crescentes (com prioridade para y)
     Algoritmo de Insertion sort

== Exemplos:

>>> ordena [(Caixa,(5,4)),(Porta,(7,5)),(Caixa,(4,5)),(Bloco,(1,4))]
[(Bloco,(1,4)),(Caixa,(5,4)),(Caixa,(4,5)),(Porta,(7,5))]

>>> ordena []
[]
-}

ordena :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordena [] = []
ordena (h:t) = inserePeca h (ordena t)


{- | Insere uma peça numa lista (ordenada) de peças

Exemplo:

>>> inserePeca (Caixa,(4,5)) [(Bloco,(1,4)),(Caixa,(5,4)),(Porta,(7,5))]
[(Bloco,(1,4)),(Caixa,(4,5)),(Caixa,(5,4)),(Porta,(7,5))]
-}
inserePeca :: (Peca, Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
inserePeca x [] = [x]
inserePeca p1@(_,(x1,y1)) (p2@(_,(x2,y2)):t)    | y1 > y2 = p2:(inserePeca p1 t)
                                                | y1 < y2 = p1:p2:t
                                                | x1 > x2 = p2:(inserePeca p1 t)
                                                | x1 < x2 = p1:p2:t -- assume-se que nunca pode ser igual a uma peca existente

{- |  Dado o índice da linha pretendido, devolve uma par com uma lista das peças da linha pretendida e uma lista das restantes peças, para maior eficiência de 'constroiMapa'

Supõe-se que a lista fornecida já está organizada.

 === Propriedades:

* Como esta função é usada em conjunto com 'constroiLinha', o índice da linha pretendido será sempre correspondente ao da primeira linha da tabela fornecida a esta função.

== Exemplo:

>>> getLinha [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(4,3)),(Bloco,(2,4)),(Bloco,(2,5)),(Bloco,(3,6))] 3
( [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(4,3))] , [(Bloco,(2,4)),(Bloco,(2,5)),(Bloco,(3,6))] )

Para além da lista de peças, note-se:

* 3 é o índice da linha (valor y) que pretendemos construir

-}
getLinha :: [(Peca,Coordenadas)] -- ^ Mapa em que a primeira linha constitui a linha de peças que pretendemos separar
    -> Int -- ^ Índice da linha que pretendemos
    -> ([(Peca,Coordenadas)],[(Peca,Coordenadas)])
getLinha [] _ = ([],[])
getLinha l@(h@(_,(_,y)):t) yLinha = span ((==yLinha).snd.snd) l -- ^ (==yLinha).snd.snd) é uma função composta que verifica se a coordenada 


-- | = Função desconstroiMapa

{- | Dada uma mapa de peças de todos os tipos, converte-o numa listagem das suas peças e respectivas coordenadas, excluindo as peças do tipo 'Vazio'.

Função inversa de 'constroiMapa', com estrutura idêntica.

Chama a função 'desconstroiMapaAux'

 === Exemplo

 >>> desconstroiMapa [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Porta,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]]
[(Bloco,(0,0)),(Bloco,(4,0)),(Bloco,(0,1)),(Porta,(2,1)),(Caixa,(3,1)),(Bloco,(4,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(3,2)),(Bloco,(4,2))]


-}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa l = desconstroiMapaAux l 0 (yMax2 l)

{-  | Agrupa várias linhas já desconstruídas numa lista global. Para isso, chama recursivamente a função 'desconstroiLinha' aumentando em cada passagem, o índice de linha desconstruída.

 === Propriedades:

prop> Em cada passagem, a linha de menor índice da lista é desconstruída e é removida do mapa inicial ( através de 'getLinha2') para tornar mais eficientes as passagens seguintes. 
Quando 'getLinha2' pegar na lista de elementos da linha seguinte do mapa, já não tem que passar pelos elementos da linha anterior.

prop> Quando o valor do índice de linha (valor y) , guardado no acumulador, atinge o valor máximo y do mapa (obtido através de 'yMax2', a função percebe que chegou à última linha e fecha a lista.

=== Exemplo:

>>> desconstroiMapaAux [[Bloco,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Porta,Caixa,Bloco],[Bloco,Bloco,Bloco,Bloco,Bloco]] 0 2
[(Bloco,(0,0)),(Bloco,(4,0)),(Bloco,(0,1)),(Porta,(2,1)),(Caixa,(3,1)),(Bloco,(4,1)),(Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(3,2)),(Bloco,(4,2))]

Para além da lista de peças, note-se:

* 0 é o índice da linha que a função irá converter primeiro. 
Como a função percorre a lista da cima para baixo, para o funcionamento correto da função é necessário introduzir o 0 neste parâmetro.

* 2 é o índice de linha máximo (valor y) do mapa exemplificado. Como referido, permite detetar a última linha do mapa.

-}
desconstroiMapaAux :: Mapa -- ^ Mapa
    -> Int -- ^ acumulador que guarda o valor y da linha em que a função está a trabalhar.
    -> Int -- ^ constante com o valor máximo y da lista (através de 'yMax2'). 
    -> [(Peca, Coordenadas)]
desconstroiMapaAux l yAtual yMax | yAtual == yMax = desconstroiLinha a 0 yAtual
                               | otherwise = desconstroiLinha a 0 yAtual ++ desconstroiMapaAux b (yAtual+1) yMax
        where (a,b) = getLinha2 l yAtual                      

{-- | == desconstroiLinha

Dada uma linha de peças de todos os tipos, converte-a numa listagem das peças e respectivas coordenadas, excluindo as peças do tipo 'Vazio'.

=== Propriedades:

prop> Quando uma peça de uma linha dada tem um índice de coluna (valor x), guardado no acumulador, igual ao valor máximo x do mapa, a função percebe que chegou à última coluna e fecha a lista.

prop> Como a função trabalha apenas com uma lista de elementos da própria linha, é mais eficiente a desconstrução da linha.

=== Exemplo:

>>> desconstroiLinha [Vazio,Vazio,Porta,Vazio,Bloco,Bloco,Bloco,Caixa] 0 3
[(Porta,(2,3)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco,(6,3)),(Caixa,(7,3))]

Para além da lista de peças, note-se:

* 0 é o índice de coluna da peça que a função irá converter primeiro. 
Como a função percorre a lista da esquerda para a direita, para o funcionamento correto da função é necessário introduzir o 0 neste parâmetro

* 3 é o índice da linha (valor y) que pretendemos construir

-}
desconstroiLinha :: [Peca] -- ^ Mapa com apenas as peças da linha (através de 'getLinha')
    -> Int -- ^ Acumulador que guarda o índice da coluna em que a função está a trabalhar.
    -> Int -- ^ Constante com o valor y da linha que pretendemos construir.
    -> [(Peca, Coordenadas)]

desconstroiLinha [] _ _ = []
desconstroiLinha (h:t) xAtual linha | h == Vazio = desconstroiLinha t (xAtual+1) linha
                                    | otherwise = (h,(xAtual,linha)): (desconstroiLinha t (xAtual+1) linha)

-- * Funções auxiliares de 'desconstroiMapa'

{-- | Dada uma lista de peças (sem coordenadas), devolve o índice maximo de linha (valor y) de uma lista
-}
yMax2 :: [[Peca]] -> Int
yMax2 [x] = 0     -- a linha 0 também existe
yMax2 (h:t) = 1 + yMax2 t

{-- |Dado o índice de linha pretendido de uma mapa, devolve uma par com uma lista das peças da linha pretendida e uma lista das restantes peças, para maior eficiência de 'constroiMapa'

 === Propriedades:

* Como esta função é usada em conjunto com 'desconstroiLinha', o índice da linha pretendido será sempre correspondente ao da primeira linha da tabela fornecida a esta função.

== Exemplo:

>>> getLinha2 [[Bloco,Bloco,Vazio,Vazio,Bloco],[Vazio,Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio]] 3
([Bloco,Bloco,Vazio,Vazio,Bloco],[[Vazio,Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Bloco,Vazio,Vazio],[Vazio,Vazio,Vazio,Bloco,Vazio]])

Para além da lista de peças, note-se:

* 3 é o índice da linha (valor y) que pretendemos construir
-}

getLinha2 :: [[Peca]] -- ^ Mapa em que a primeira linha constitui a linha de peças que pretendemos separar
    -> Int -- ^ Índice da linha que pretendemos
    -> ([Peca],[[Peca]])

getLinha2 [] _ = ([],[])
getLinha2 l@(h:t) linha = (h,t)





