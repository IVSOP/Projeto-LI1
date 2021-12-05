module MyTests where

import LI12122
import Tarefa1_2021li1g025
import Tarefa2_2021li1g025
import Tarefa3_2021li1g025
import Tarefa4_2021li1g025
import Test.HUnit
import Fixtures
import Data.List (sort)

map1 = [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),
        (Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),
        (Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),
        (Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]

map1result = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
              [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
              [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
              [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

j1 = Jogador (5,2) Oeste False

showresult = "      X\n      X\nP   C<X\nXXXXXXX"


map1'2 =  [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),
           (Bloco,(0,3)),(Bloco,(1,4)),(Bloco,(2,3)),
           (Bloco,(3,2)),(Bloco,(4,1)),(Bloco,(5,2)),
           (Bloco,(6,3)),(Bloco,(7,4)),(Porta,(7,3)),
           (Bloco,(8,3)),(Bloco,(8,2)),(Bloco,(8,1)),
           (Bloco,(8,0))]

map1'3 = [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),
          (Bloco,(0,1)),(Bloco,(2,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2))]

map1'4 = [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),
          (Bloco,(0,1)),(Porta,(1,1)),(Bloco,(2,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2))]

map1'5 = [(Porta,(0,1)),(Porta,(1,1)),(Porta,(2,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2))]

map1'6 = [(Caixa,(0,0)),(Bloco,(3,0)),
          (Caixa,(0,1)),(Porta,(1,1)),(Bloco,(3,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(3,2))]

map1'7 = [(Caixa,(1,0)),(Bloco,(3,0)),
          (Caixa,(0,1)),(Porta,(1,1)),(Bloco,(3,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(3,2))]

map1'8 = [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),
          (Bloco,(1,5)),(Caixa,(1,4)),(Bloco,(2,3)),(Bloco,(2,4)),
          (Bloco,(3,2)),(Bloco,(4,1)),(Bloco,(5,2)),(Bloco,(5,3)),
          (Bloco,(5,4)),(Bloco,(5,5)),(Bloco,(6,5)),(Bloco,(7,5)),
          (Porta,(7,4)),(Bloco,(8,3)),(Bloco,(8,4))]


map2 = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
        [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
        [Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio],
        [Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

map2'1'1 =     [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
                [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
                [Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio],
                [Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

j2 = Jogador (0,2) Este False

resultT4 = Jogo map2'1'1 (Jogador (6,2) Este False)

map2'1 = [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(5,2)),(Porta,(4,1)),(Bloco,(4,3)),
          (Bloco,(3,4)),(Bloco,(2,4)),(Bloco,(1,4)),(Bloco,(3,3)),(Bloco,(3,2)),
          (Bloco,(4,2)),(Bloco,(5,1)),(Bloco,(2,0)),(Bloco,(1,1)),(Caixa,(1,0)),
          (Bloco,(0,4)),(Bloco,(0,3)),(Bloco,(0,2)),(Bloco,(5,0)),(Caixa,(2,3))]

map2'1r = [[Bloco,Caixa,Bloco,Vazio,Vazio,Bloco],
           [Bloco,Bloco,Vazio,Vazio,Porta,Bloco],
           [Bloco,Vazio,Vazio,Bloco,Bloco,Bloco],
           [Bloco,Vazio,Caixa,Bloco,Bloco,Vazio],
           [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]]

map4'1 = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
          [Bloco,Bloco,Vazio,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio],
          [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio]]

map4'2 = [[Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco],
          [Bloco,Caixa,Vazio,Vazio,Caixa,Caixa,Vazio,Porta,Bloco],
          [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

map4'2r = [[Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco],
           [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco],
           [Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco],
           [Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Vazio,Porta,Bloco],
           [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]


map5 = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Caixa,Bloco,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Porta,Bloco],
        [Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

-- uma sequencia que, com m2 e j2, testa quase todos os casos "especiais" possiveis
movementListM2J2 = [Trepar,AndarDireita,AndarDireita,AndarDireita,
  AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,
  AndarDireita,AndarDireita,AndarDireita,InterageCaixa,
  AndarEsquerda,AndarEsquerda,AndarDireita,InterageCaixa,
  Trepar,Trepar,AndarDireita,AndarEsquerda,AndarEsquerda,
  Trepar,AndarDireita]


testsT1 = TestList ["Tarefa 1 - base em forma de triângulo" ~: validaPotencialMapa map1'2 ~=? True,
                    "Tarefa 1 - mapa sem porta" ~: validaPotencialMapa map1'3 ~=? False,
                    "Tarefa 1 - mapa sem espaço vazio" ~: validaPotencialMapa map1'4 ~=? False, 
                    "Tarefa 1 - mapa com 3 portas" ~: validaPotencialMapa map1'5 ~=? False,
                    "Tarefa 1 - mapa com torre de caixas" ~: validaPotencialMapa map1'6 ~=? True,
                    "Tarefa 1 - mapa com caixa sobre porta" ~: validaPotencialMapa map1'7 ~=? False, 
                    "Tarefa 1 - mapa com descida abrupta" ~: validaPotencialMapa map1'8 ~=? True]

testsT2 = TestList ["Valida construção do mapa da figura 6" ~: constroiMapa map1 ~=? map1result,
                    "Tarefa 2 - Constroi mapa com pecas desordenadas" ~: constroiMapa map2'1 ~=? map2'1r,
                    "Tarefa 2 - Desconstroi mapa2'1" ~: sort (desconstroiMapa map2'1r) ~=? sort (map2'1)]

testsT3 = TestList ["Valida tarefa 3 (o HUnit espera um resultado diferente do pedido???)" ~: show (Jogo map1result j1) ~=? showresult]

testsT4 = TestList ["Valida tarefa 4" ~: correrMovimentos (Jogo map2 j2) movementListM2J2 ~=? resultT4,
                    "Tarefa 4 - Lista de movimentos nula" ~: correrMovimentos resultT4 [] ~=? resultT4,
                    "Tarefa 4 - pegar caixa" ~: moveJogador (Jogo map4'2 (Jogador (2,3) Oeste False)) InterageCaixa ~=? Jogo map4'2r (Jogador (2,3) Oeste True),
                    "Tarefa 4 - pegar caixa de costas" ~: moveJogador (Jogo map4'2 (Jogador (2,3) Este False)) InterageCaixa ~=? Jogo map4'2 (Jogador (2,3) Este False),
                    "Tarefa 4 - pegar caixa com bloco em cima da caixa" ~: moveJogador (Jogo map4'2 (Jogador (3,3) Este False)) InterageCaixa ~=? Jogo map4'2 (Jogador (3,3) Este False),
                    "Tarefa 4 - pegar caixa e bloco em cima do jogador" ~: moveJogador (Jogo map4'2 (Jogador (4,1) Este False)) InterageCaixa ~=? Jogo map4'2 (Jogador (4,1) Este False),
                    "Tarefa 4 - pegar caixa de torre de caixas" ~: moveJogador (Jogo map4'2 (Jogador (6,3) Oeste False)) InterageCaixa ~=? Jogo map4'2 (Jogador (6,3) Oeste False),
                    "Tarefa 4 - pegar bloco" ~: moveJogador (Jogo map4'2 (Jogador (1,2) Oeste False)) InterageCaixa ~=? Jogo map4'2 (Jogador (1,2) Oeste False),
                    "Tarefa 4 - pegar porta" ~: moveJogador (Jogo map4'2 (Jogador (6,3) Este False)) InterageCaixa ~=? Jogo map4'2 (Jogador (6,3) Este False),
                    "Tarefa 4 - pegar ar" ~: moveJogador (Jogo map4'2 (Jogador (1,2) Este False)) InterageCaixa ~=? Jogo map4'2 (Jogador (1,2) Este False)]

-- testa as auxiliares especificamente
testsT4Aux = TestList  ["Testa andar (1)" ~: andar (Jogo map2 (Jogador (5,0) Este False)) AndarDireita ~=? (6,2),
                        "Testa andar (2)" ~: andar (Jogo map2 (Jogador (2,2) Este False)) AndarEsquerda ~=? (2,2),
                        "Testa andar (3)" ~: andar (Jogo map2 (Jogador (3,1) Este True)) AndarDireita ~=? (4,2),
                        "Testa andar (4)" ~: andar (Jogo map2 (Jogador (4,2) Este True)) AndarDireita ~=? (4,2),
                        "Tarefa 4 - trepar para fresta sem transportar caixa" ~: trepar (Jogo map4'1 (Jogador (7,2) Oeste False)) Trepar ~=? (6,1),
                        "Tarefa 4 - trepar para fresta a transportar caixa" ~: trepar (Jogo map4'1 (Jogador (7,2) Oeste True)) Trepar ~=? (7,2),
                        "Tarefa 4 - trepar caixa a transportar caixa" ~: trepar (Jogo map4'1 (Jogador (5,4) Oeste True)) Trepar ~=? (4,3),
                        "Tarefa 4 - trepar muro com vários blocos de altura" ~: trepar (Jogo map4'1 (Jogador (5,4) Este False)) Trepar ~=? (5,4),
                        "Tarefa 4 - trepar o vazio" ~: trepar (Jogo map4'1 (Jogador (2,2) Oeste False)) Trepar ~=? (2,2),
                        "Tarefa 4 - trepar porta" ~: trepar (Jogo map4'1 (Jogador (2,2) Este False)) Trepar ~=? (2,2),
                        "Tarefa 4 - trepar com bloco por cima" ~: trepar (Jogo map4'1 (Jogador (2,4) Oeste False)) Trepar ~=? (2,4),
                        "Tarefa 4 - trepar bloco de costas" ~: trepar (Jogo map4'1 (Jogador (7,2) Este False)) Trepar ~=? (7,2),
                        "Tarefa 4 - trepar para a mesma posição que a porta" ~: trepar (Jogo map4'1 (Jogador (7,2) Este False)) Trepar ~=? (7,2)] -- assumo que o jogador poderá trepar para a posição da porta

runTestsT1 = runTestTT testsT1
runTestsT2 = runTestTT testsT2
runTestsT3 = runTestTT testsT3
runTestsT4 = runTestTT testsT4
runTestsT4Aux = runTestTT testsT4Aux

runAllTests = runTestTT $ TestList [testsT1, testsT2, testsT3, testsT4,testsT4Aux]
-- ghci -i="src" -i="tests" tests/myTests.hs