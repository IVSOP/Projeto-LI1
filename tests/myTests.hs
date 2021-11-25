module CustomTests where

import LI12122
import Tarefa2_2021li1g025
import Tarefa3_2021li1g025
import Tarefa4_2021li1g025
import Test.HUnit
import Fixtures

map1 = [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),
        (Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),
        (Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),
        (Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]

map1result = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
              [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
              [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
              [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

j1 = Jogador (5,2) Oeste False

showresult = "      X\n      X\nP    C<P\nXXXXXXX"

map2 = [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
        [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
        [Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio],
        [Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

j2 = Jogador (0,2) Este False

-- uma sequencia que, com m2 e j2, testa quase todos os casos "especiais" possiveis
movementListM2J2 = [Trepar,AndarDireita,AndarDireita,AndarDireita,
  AndarEsquerda,InterageCaixa,AndarDireita,InterageCaixa,
  AndarDireita,AndarDireita,AndarDireita,InterageCaixa,
  AndarEsquerda,AndarEsquerda,AndarDireita,InterageCaixa,
  Trepar,Trepar,AndarDireita]

resultT4 = Jogo map2 (Jogador (6,2) Este False)

map3 = [[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco],
        [Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco],
        [Bloco,Bloco,Vazio,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio],
        [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio]]




testsT2 = TestList ["Valida construção do mapa da figura 6" ~: constroiMapa map1 ~=? map1result]

testsT3 = TestList ["Valida tarefa 3" ~: show (Jogo map1result j1) ~=? showresult]

testsT4 = TestList ["Valida tarefa 4" ~: correrMovimentos (Jogo map2 j2) movementListM2J2 ~=? resultT4,
                    "Tarefa 4 - Lista de movimentos nula" ~: correrMovimentos resultT4 [] ~=? resultT4]

-- testa as auxiliares especificamente
testsT4Aux = TestList ["Valida andar (1)" ~: andar (Jogo map2 (Jogador (5,0) Este False)) AndarDireita ~=? (6,2),
                       "Valida andar (2)" ~: andar (Jogo map2 (Jogador (2,2) Este False)) AndarEsquerda ~=? (2,2),
                       "Valida andar (3)" ~: andar (Jogo map2 (Jogador (3,1) Este True)) AndarDireita ~=? (4,2),
                       "Valida andar (4)" ~: andar (Jogo map2 (Jogador (4,2) Este True)) AndarDireita ~=? (4,2),
                       "Tarefa 4 - trepar para fresta sem transportar caixa" ~: trepar (Jogo map3 (Jogador (7,2) Oeste False)) Trepar ~=? (6,1),
                       "Tarefa 4 - trepar para fresta a transportar caixa" ~: trepar (Jogo map3 (Jogador (7,2) Oeste True)) Trepar ~=? (7,2),
                       "Tarefa 4 - trepar caixa a transportar caixa" ~: trepar (Jogo map3 (Jogador (5,4) Oeste True)) Trepar ~=? (4,3),
                       "Tarefa 4 - trepar muro com vários blocos de altura" ~: trepar (Jogo map3 (Jogador (5,4) Este False)) Trepar ~=? (5,4),
                       "Tarefa 4 - trepar o vazio" ~: trepar (Jogo map3 (Jogador (2,2) Oeste False)) Trepar ~=? (2,2),
                       "Tarefa 4 - trepar porta" ~: trepar (Jogo map3 (Jogador (2,2) Este False)) Trepar ~=? (2,2),
                       "Tarefa 4 - trepar com bloco por cima" ~: trepar (Jogo map3 (Jogador (2,4) Oeste False)) Trepar ~=? (2,4),
                       "Tarefa 4 - trepar bloco de costas" ~: trepar (Jogo map3 (Jogador (7,2) Este False)) Trepar ~=? (7,2)]

runTestsT4 = runTestTT testsT4
runTestsT4Aux = runTestTT testsT4Aux

-- ghci -i="src" -i="tests" tests/myTests.hs