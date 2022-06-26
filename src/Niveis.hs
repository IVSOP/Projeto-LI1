module Niveis where

import LI12122


j1 = Jogo mapa1 (Jogador (6,4) Oeste False)
j2 = Jogo mapa2 (Jogador (8,5) Oeste False)
j3 = Jogo mapa3 (Jogador (17, 7) Oeste False)
j4 = Jogo mapa4 (Jogador (1,7) Este True)
j5 = Jogo mapa5 (Jogador (19,7) Oeste False)


mapa1 :: Mapa
mapa1 = [
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

mapa2 :: Mapa
mapa2 = [
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

mapa3 :: Mapa
mapa3 = [ 
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]


mapa4 :: Mapa
mapa4 = [ 
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco, Vazio, Caixa, Bloco, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]



mapa5 :: Mapa
mapa5 = [ 
      [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco]
    , [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Caixa, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Caixa, Caixa, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Caixa, Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Bloco, Bloco]
    , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]
