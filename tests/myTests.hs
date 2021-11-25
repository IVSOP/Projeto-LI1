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

m1result = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
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

testsT2 = TestList ["Valida construção do mapa da figura 6" ~: constroiMapa map1 ~=? m1result]

testsT3 = TestList ["Valida tarefa 3" ~: show (Jogo m1result j1) ~=? showresult]

testsT4 = TestList ["Valida tarefa 4" ~: correrMovimentos (Jogo map2 j2) movementListM2J2 ~=? resultT4]

-- testa as auxiliares especificamente
testsT4Aux = TestList ["Valida andar (1)" ~: andar (Jogo map2 (Jogador (5,0) Este False)) AndarDireita ~=? (6,2),
                       "Valida andar (2)" ~: andar (Jogo map2 (Jogador (2,2) Este False)) AndarEsquerda ~=? (2,2),
                       "Valida andar (3)" ~: andar (Jogo map2 (Jogador (3,1) Este True)) AndarDireita ~=? (4,2),
                       "Valida andar (4)" ~: andar (Jogo map2 (Jogador (4,2) Este True)) AndarDireita ~=? (4,2)]

runTestsT4 = runTestTT testsT4
runTestsT4Aux = runTestTT testsT4Aux

-- ghci -i="src" -i="tests" tests/myTests.hs