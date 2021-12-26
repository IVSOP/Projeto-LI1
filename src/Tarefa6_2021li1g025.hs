module Tarefa6_2021li1g025 where

import LI12122
import Maps
import Tarefa4_2021li1g025
import Tarefa2_2021li1g025

maps1 = [(Jogo map1 (Jogador (1,2) Este False)),(Jogo map2 (Jogador (1,3) Este False)),(Jogo map3 (Jogador (1,1) Este False)),(Jogo map4 (Jogador (1,1) Este False)), (Jogo map5 (Jogador (1,9) Este False)), (Jogo map6 (Jogador (6,3) Oeste False)), (Jogo map7 (Jogador (12,14) Este False))]

data MTree a = Empty 
                | Node a (MTree a) (MTree a) (MTree a) (MTree a) -- árvore em que cada node é pai de quatro nodes que contêm movimentos possíveis. Por ordem: InterageCaixa, AndarEsquerda,AndarDireita,Trepar.
     deriving (Show,Eq)

type CordsPorta = (Int,Int)

-- Recebe uma lista no formato de números, proveniente de SequenceBuilder ( 1 corresponde a InterageCaixa, 2 a AndarEsquerda, 3 a AndarDireita, 4 a Trepar) 
-- Devolve uma lista de movimentos (após traduzida a lista de números)

--TODO : testar se a função é mais eficiente se receber [Movimento] ou converter [Int]
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 j@(Jogo m player@(Jogador cords _ _)) | (portaFinder (desconstroiMapa m)) == cords = Just []
                                                    | otherwise =  Nothing
resolveJogo n j@(Jogo m _) = fmap decode numberseq -- permite retornar 'Nothing' ou fazer a tradução de 'Just [Int]' com 'decode' dependendo do resultado de numberseq
    where numberseq = (sequenceBuilder (n+1) (portaFinder (desconstroiMapa m)) [(Node (j,[]) Empty Empty Empty Empty)] [] [])

-- A maneira mais eficiente de inverter o resultado obtido de sequenceBuilder parece ser reverter o resultado final de dec
decode :: [Int] -> [Movimento]
decode n = reverse (decodeAux n)
    where   decodeAux :: [Int] -> [Movimento]
            decodeAux [] = []
            decodeAux l@(h:t) =   case h of 
                        1 -> InterageCaixa: (decodeAux t)
                        2 -> AndarEsquerda: (decodeAux t)
                        3 -> AndarDireita: (decodeAux t)
                        4 -> Trepar: (decodeAux t)


-- o tipo da função corresponde a: nº de moves limite, cords da porta, árvore incial (contém a posição inicial do jogador), lista de acumulação de folhas que serão aprofundados (após terem sido lidas e comparadas com a pos. da porta) e sequência de jogos obtidos antes (que não podem voltar a surgir)
-- o primeiro Int quea função recebe tem de ser igual a (nº de moves possíveis + 1) para funcionar

sequenceBuilder :: Int -> CordsPorta -> [(MTree (Jogo,[Int]))] -> [(MTree (Jogo,[Int]))] -> [Jogo] -> (Maybe [Int])
sequenceBuilder 0 _ tree _ _ = Nothing
sequenceBuilder depth porta [] accgames prevjogos = sequenceBuilder (depth-1) porta (newDepth accgames prevjogos) [] prevjogos
sequenceBuilder depth porta (h@(Node (jogoresult@(Jogo _ (Jogador cords _ _)),moves) _ _ _ _):t) accgames prevjogos 
    | cords == porta = Just moves
    | cords /= porta = sequenceBuilder depth porta t (h:accgames) (saveGames jogoresult prevjogos)

-- constroi uma lista de árvores, que têm como raiz nodes com jogos resultantes de cada um dos movimentos aplicados ao seu nodo pai (da profunidade anterior), respetivamente.
newDepth :: [(MTree (Jogo,[Int]))] -> [Jogo] -> [(MTree (Jogo,[Int]))]
newDepth [] _ = []         --- má resolução, ver melhor eficiência na formação de listas
newDepth (h:t) prevjogos = filter (/= Empty) (a:b:c:d:rest)
    where branches@(Node _ a b c d) = addDepth h prevjogos
          rest = newDepth t prevjogos

-- adiciona um de profunidade a folhas de uma árvore
addDepth :: MTree (Jogo,[Int]) -> [Jogo] -> MTree (Jogo,[Int])
addDepth Empty _ = Empty                                                                -- necessário?
addDepth (Node i@(jogoresult,moves) Empty Empty Empty Empty) prevjogos =  Node i a b c d

            where   a = if elem (moveJogador jogoresult InterageCaixa)  prevjogos
                        then Empty 
                        else Node ((moveJogador jogoresult InterageCaixa), (1:moves)) Empty Empty Empty Empty -- TODO: tornar  mais eficiente a inversão nos ramos

                    b = if elem (moveJogador jogoresult AndarEsquerda) prevjogos
                        then Empty 
                        else Node ((moveJogador jogoresult AndarEsquerda), (2:moves)) Empty Empty Empty Empty

                    c = if elem (moveJogador jogoresult AndarDireita) prevjogos
                        then Empty 
                        else Node ((moveJogador jogoresult AndarDireita), (3:moves)) Empty Empty Empty Empty

                    d = if elem (moveJogador jogoresult Trepar) prevjogos
                        then Empty 
                        else Node ((moveJogador jogoresult Trepar), (4:moves)) Empty Empty Empty Empty

-- verifica se um jogo já existe numa lista de jogos. Se não existir, é adicionado à lista; caso já exista não é.
saveGames :: Jogo -> [Jogo] -> [Jogo]
saveGames x [] = [x]
saveGames x l = if x `elem` l then l else (x:l)

--TODO: tornar mais eficiente
-- encontra as coordenadas da porta de um jogo (para usar na função sequenceBuilder)
portaFinder :: [(Peca,Coordenadas)] -> CordsPorta
portaFinder l@(h@(p,cords):t) = if p == Porta then cords else portaFinder t 






