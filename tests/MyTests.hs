module MyTests where

import LI12122
import Tarefa1_2021li1g025
import Tarefa2_2021li1g025
import Tarefa3_2021li1g025
import Tarefa4_2021li1g025
import Tarefa6_2021li1g025
import Maps
import Test.HUnit
import Fixtures
import Data.List (sort)

maptest1 = [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),
        (Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),
        (Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),
        (Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]

maptest1result = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
              [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
              [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
              [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

j1 = Jogador (5,2) Oeste False

showresult = "      X\n      X\nP   C<X\nXXXXXXX"


maptest1'2 =  [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),
           (Bloco,(0,3)),(Bloco,(1,4)),(Bloco,(2,3)),
           (Bloco,(3,2)),(Bloco,(4,1)),(Bloco,(5,2)),
           (Bloco,(6,3)),(Bloco,(7,4)),(Porta,(7,3)),
           (Bloco,(8,3)),(Bloco,(8,2)),(Bloco,(8,1)),
           (Bloco,(8,0))]

maptest1'3 = [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),
          (Bloco,(0,1)),(Bloco,(2,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2))]

maptest1'4 = [(Bloco,(0,0)),(Bloco,(1,0)),(Bloco,(2,0)),
          (Bloco,(0,1)),(Porta,(1,1)),(Bloco,(2,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2))]

maptest1'5 = [(Porta,(0,1)),(Porta,(1,1)),(Porta,(2,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2))]

maptest1'6 = [(Caixa,(0,0)),(Bloco,(3,0)),
          (Caixa,(0,1)),(Porta,(1,1)),(Bloco,(3,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(3,2))]

maptest1'7 = [(Caixa,(1,0)),(Bloco,(3,0)),
          (Caixa,(0,1)),(Porta,(1,1)),(Bloco,(3,1)),
          (Bloco,(0,2)),(Bloco,(1,2)),(Bloco,(2,2)),(Bloco,(3,2))]

maptest1'8 = [(Bloco,(0,2)),(Bloco,(0,3)),(Bloco,(0,4)),(Bloco,(0,5)),
          (Bloco,(1,5)),(Caixa,(1,4)),(Bloco,(2,3)),(Bloco,(2,4)),
          (Bloco,(3,2)),(Bloco,(4,1)),(Bloco,(5,2)),(Bloco,(5,3)),
          (Bloco,(5,4)),(Bloco,(5,5)),(Bloco,(6,5)),(Bloco,(7,5)),
          (Porta,(7,4)),(Bloco,(8,3)),(Bloco,(8,4))]


maptest2 = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
        [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
        [Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio],
        [Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

maptest2'1'1 =     [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
                [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
                [Vazio,Bloco,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio],
                [Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

j2 = Jogador (0,2) Este False

resultT4 = Jogo maptest2'1'1 (Jogador (6,2) Este False)

maptest2'1 = [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(5,2)),(Porta,(4,1)),(Bloco,(4,3)),
          (Bloco,(3,4)),(Bloco,(2,4)),(Bloco,(1,4)),(Bloco,(3,3)),(Bloco,(3,2)),
          (Bloco,(4,2)),(Bloco,(5,1)),(Bloco,(2,0)),(Bloco,(1,1)),(Caixa,(1,0)),
          (Bloco,(0,4)),(Bloco,(0,3)),(Bloco,(0,2)),(Bloco,(5,0)),(Caixa,(2,3))]

maptest2'1r = [[Bloco,Caixa,Bloco,Vazio,Vazio,Bloco],
           [Bloco,Bloco,Vazio,Vazio,Porta,Bloco],
           [Bloco,Vazio,Vazio,Bloco,Bloco,Bloco],
           [Bloco,Vazio,Caixa,Bloco,Bloco,Vazio],
           [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio]]

maptest4'1 = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
          [Bloco,Bloco,Vazio,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio],
          [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio]]

maptest4'2 = [[Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco],
          [Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco],
          [Bloco,Caixa,Vazio,Vazio,Caixa,Caixa,Vazio,Porta,Bloco],
          [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

maptest4'2r = [[Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco],
           [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco],
           [Bloco,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Vazio,Bloco],
           [Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Vazio,Porta,Bloco],
           [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]


maptest5 = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
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

-- lista de jogos e pos da porta
mapTests6 = [(Jogo map1 (Jogador (1,2) Este False)),(Jogo map2 (Jogador (1,3) Este False)),(Jogo map3 (Jogador (1,1) Este False)),(Jogo map4 (Jogador (1,1) Este False)), (Jogo hugeMap (Jogador (1,9) Este False))]





testsT1 = TestList ["Tarefa 1 - base em forma de triângulo" ~: validaPotencialMapa maptest1'2 ~=? True,
                    "Tarefa 1 - maptesta sem porta" ~: validaPotencialMapa maptest1'3 ~=? False,
                    "Tarefa 1 - maptesta sem espaço vazio" ~: validaPotencialMapa maptest1'4 ~=? False, 
                    "Tarefa 1 - maptesta com 3 portas" ~: validaPotencialMapa maptest1'5 ~=? False,
                    "Tarefa 1 - maptesta com torre de caixas" ~: validaPotencialMapa maptest1'6 ~=? True,
                    "Tarefa 1 - maptesta com caixa sobre porta" ~: validaPotencialMapa maptest1'7 ~=? False, 
                    "Tarefa 1 - maptesta com descida abrupta" ~: validaPotencialMapa maptest1'8 ~=? True]

testsT2 = TestList ["Valida construção do maptesta da figura 6" ~: constroiMapa maptest1 ~=? maptest1result,
                    "Tarefa 2 - Constroi maptesta com pecas desordenadas" ~: constroiMapa maptest2'1 ~=? maptest2'1r,
                    "Tarefa 2 - Desconstroi maptesta2'1" ~: sort (desconstroiMapa maptest2'1r) ~=? sort (maptest2'1)]

testsT3 = TestList ["Valida tarefa 3" ~: show (Jogo maptest1result j1) ~=? showresult]

testsT4 = TestList ["Valida tarefa 4" ~: correrMovimentos (Jogo maptest2 j2) movementListM2J2 ~=? resultT4,
                    "Tarefa 4 - Lista de movimentos nula" ~: correrMovimentos resultT4 [] ~=? resultT4,
                    "Tarefa 4 - pegar caixa" ~: moveJogador (Jogo maptest4'2 (Jogador (2,3) Oeste False)) InterageCaixa ~=? Jogo maptest4'2r (Jogador (2,3) Oeste True),
                    "Tarefa 4 - pegar caixa de costas" ~: moveJogador (Jogo maptest4'2 (Jogador (2,3) Este False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (2,3) Este False),
                    "Tarefa 4 - pegar caixa com bloco em cima da caixa" ~: moveJogador (Jogo maptest4'2 (Jogador (3,3) Este False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (3,3) Este False),
                    "Tarefa 4 - pegar caixa e bloco em cima do jogador" ~: moveJogador (Jogo maptest4'2 (Jogador (4,1) Este False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (4,1) Este False),
                    "Tarefa 4 - pegar caixa de torre de caixas" ~: moveJogador (Jogo maptest4'2 (Jogador (6,3) Oeste False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (6,3) Oeste False),
                    "Tarefa 4 - pegar bloco" ~: moveJogador (Jogo maptest4'2 (Jogador (1,2) Oeste False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (1,2) Oeste False),
                    "Tarefa 4 - pegar porta" ~: moveJogador (Jogo maptest4'2 (Jogador (6,3) Este False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (6,3) Este False),
                    "Tarefa 4 - pegar ar" ~: moveJogador (Jogo maptest4'2 (Jogador (1,2) Este False)) InterageCaixa ~=? Jogo maptest4'2 (Jogador (1,2) Este False)]

-- testa as auxiliares especificamente
testsT4Aux = TestList  ["Testa andar (1)" ~: andar (Jogo maptest2 (Jogador (5,0) Este False)) AndarDireita ~=? (6,2),
                        "Testa andar (2)" ~: andar (Jogo maptest2 (Jogador (2,2) Este False)) AndarEsquerda ~=? (2,2),
                        "Testa andar (3)" ~: andar (Jogo maptest2 (Jogador (3,1) Este True)) AndarDireita ~=? (4,2),
                        "Testa andar (4)" ~: andar (Jogo maptest2 (Jogador (4,2) Este True)) AndarDireita ~=? (4,2),
                        "Tarefa 4 - trepar para fresta sem transportar caixa" ~: trepar (Jogo maptest4'1 (Jogador (7,2) Oeste False)) Trepar ~=? (6,1),
                        "Tarefa 4 - trepar para fresta a transportar caixa" ~: trepar (Jogo maptest4'1 (Jogador (7,2) Oeste True)) Trepar ~=? (7,2),
                        "Tarefa 4 - trepar caixa a transportar caixa" ~: trepar (Jogo maptest4'1 (Jogador (5,4) Oeste True)) Trepar ~=? (4,3),
                        "Tarefa 4 - trepar muro com vários blocos de altura" ~: trepar (Jogo maptest4'1 (Jogador (5,4) Este False)) Trepar ~=? (5,4),
                        "Tarefa 4 - trepar o vazio" ~: trepar (Jogo maptest4'1 (Jogador (2,2) Oeste False)) Trepar ~=? (2,2),
                        "Tarefa 4 - trepar porta" ~: trepar (Jogo maptest4'1 (Jogador (2,2) Este False)) Trepar ~=? (2,2),
                        "Tarefa 4 - trepar com bloco por cima" ~: trepar (Jogo maptest4'1 (Jogador (2,4) Oeste False)) Trepar ~=? (2,4),
                        "Tarefa 4 - trepar bloco de costas" ~: trepar (Jogo maptest4'1 (Jogador (7,2) Este False)) Trepar ~=? (7,2),
                        "Tarefa 4 - trepar para a mesma posição que a porta" ~: trepar (Jogo maptest4'1 (Jogador (7,2) Este False)) Trepar ~=? (7,2)] -- assumo que o jogador poderá trepar para a posição da porta

testsT6 = TestList     ["Teste movimentos insuficientes" ~: (resolveJogo 6 (mapTests6 !! 0)) ~=? Nothing,
                        "Teste nível 1 jogo" ~: (resolveJogo 50 (mapTests6 !! 0)) ~=? Just [InterageCaixa,AndarDireita,InterageCaixa,Trepar,Trepar,AndarDireita,AndarDireita],
                        "Teste nível 2 jogo" ~: (resolveJogo 50 (mapTests6 !! 1)) ~=? Just [InterageCaixa,AndarDireita,Trepar,InterageCaixa,AndarDireita,Trepar,Trepar,AndarDireita,AndarDireita,AndarDireita],
                        "Teste nível 3 jogo" ~: (resolveJogo 50 (mapTests6 !! 2)) ~=? Just [AndarDireita,AndarDireita,Trepar,Trepar,AndarDireita,InterageCaixa,AndarDireita,AndarDireita,InterageCaixa,Trepar,Trepar,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,AndarEsquerda,Trepar,Trepar,AndarEsquerda,AndarEsquerda,InterageCaixa,Trepar,Trepar],
                        "Teste nível 4 jogo" ~: (resolveJogo 50 (mapTests6 !! 3)) ~=? Just [AndarDireita,AndarEsquerda,InterageCaixa,AndarDireita,AndarDireita,AndarDireita,AndarDireita,AndarDireita,Trepar,Trepar,InterageCaixa,Trepar,Trepar,InterageCaixa,AndarDireita,AndarEsquerda,InterageCaixa,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,Trepar],
                        "Teste nível 5 jogo" ~: (resolveJogo 50 (mapTests6 !! 4)) ~=? Just [Trepar,AndarDireita,Trepar,InterageCaixa,AndarDireita,InterageCaixa,AndarEsquerda,AndarEsquerda,AndarDireita,InterageCaixa,AndarDireita,Trepar,Trepar,InterageCaixa,Trepar,Trepar,AndarDireita,AndarEsquerda,InterageCaixa,AndarDireita,Trepar,Trepar,Trepar,Trepar,AndarDireita,InterageCaixa,AndarDireita,Trepar,Trepar,Trepar,InterageCaixa,AndarDireita,InterageCaixa,AndarDireita,Trepar,Trepar,Trepar,InterageCaixa,AndarDireita,AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,AndarEsquerda,InterageCaixa,Trepar,Trepar]
                        ]

runTestsT1 = runTestTT testsT1
runTestsT2 = runTestTT testsT2
runTestsT3 = runTestTT testsT3
runTestsT4 = runTestTT testsT4
runTestsT4Aux = runTestTT testsT4Aux
runTestsT6 = runTestTT testsT6

runAllTests = runTestTT $ TestList [testsT1, testsT2, testsT3, testsT4,testsT4Aux,testsT6]
-- ghci -i="src" -i="tests" tests/myTests.hs