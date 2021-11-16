{- |
Module      : Tarefa3_2021li1g025
Description : Representação textual do jogo
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g025 where

import LI12122

-- * Expressão principal

{- | Esta expressão será usada para transformar um jogo em texto, transformando-o numa instância da classe Show.

Para tal, utilizamos as funções 'insereJogador' e 'jogoToText'

> show (Jogo mapa jogador) = insereJogador jogador (jogoToText mapa)
-}

-- como é que fazemos documentação para a instância???
instance Show Jogo where
  show (Jogo mapa jogador) = insereJogador jogador (jogoToText mapa)

-- * Funções utilizadas

-- | Aplica 'jogoToTextAux' a cada linha, recursivamente, adicionando ao fim da linha "\n".

-- | Quando o mapa tiver apenas uma lista, não adiciona "\n" para que este não fique no final da expressão
jogoToText :: Mapa -- ^ Lista de listas em que cada lista é uma linha de peças
 -> [String] -- ^ Igual ao mapa, mas as peças estão transformadas nos seus correspondentes textuais
jogoToText [] = []
jogoToText [linha] = [jogoToTextAux linha]
jogoToText (linha:l) = ((jogoToTextAux linha)++"\n"):(jogoToText l)

{- | Transforma todos os elementos de uma linha nos seus correspondentes textuais

@case x of Bloco -> 'X'
           Porta -> 'P'
           Vazio -> ' '
           Caixa -> 'C'
@
-}
jogoToTextAux :: [Peca] -> String
jogoToTextAux [] = []
jogoToTextAux (x:linha) =
      (case x of Bloco -> 'X'
                 Porta -> 'P'
                 Vazio -> ' '
                 Caixa -> 'C'):(jogoToTextAux linha)

{- | Insere o jogador na lista de Strings criada pelo jogoToText

Dependendo da sua orientação, este corresponderá a '>' ou '<'

A função funciona dividindo a lista de strings segundo o valor de y, obtendo assim a linha que queremos e as linhas antes e após

De seguida, a string obtida é dividida segundo o valor de x, permitindo obter a 'casa' que queremos, bem como as casas antes e após

Se o jogador puder ocupar essa casa, ela será substituída pela sua representação e todas as Strings resultantes são concatenadas numa só 
-}
-- é suposto representar a caixa em cima do jogador??
insereJogador :: Jogador -> [String] -> String
insereJogador _ [] = error "posição inválida"
insereJogador (Jogador (x,y) dir caixa) l | posicao == ' ' = (concat l1) ++ ((a ++ [(case dir of Oeste -> '<'
                                                                                                 Este -> '>')]) ++ b) ++ (concat l2)
                                          | otherwise = error "posição ocupada"
                                          where (l1,(linha:l2)) = splitAt y l
                                                (a,posicao:b) = splitAt x linha


-- incompleto l22 l63