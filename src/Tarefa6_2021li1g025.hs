{- |
Module      : Tarefa5_2021li1g025
Description : Interface gráfica do jogo
Copyright   : Pedro Miguel Meruge Ferreira <a100709@alunos.uminho.pt>;
            : Ivan Sérgio Rocha Ribeiro <a100538@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.

A abordagem seguida foi construir uma árvore quaternária de movimentos.

Cada Node está ligado a (potenciais) 4 Nodes filhos.

O Node raiz contém o jogo inicial e uma lista vazia.

Cada um dos 4 Nodes subsequentes guarda dois dados: 

 * o jogo resultantes de aplicar, respetivamente, um dos 4 comandos de movimento ('InterageCaixa', 'AndarEsquerda', 'AndarDireita', 'Trepar') ao jogo do Node pai; 

 * uma lista dos movimentos necessários para chegar a esse jogo, desde o jogo incial guardado no Node raiz (esta lista é continuamente expandida).

Aumentando a profunidade, eventualmente, um dos Nodes contém um jogo em que o jogador chega à porta. Na menor profunidade em que isso se verificar, é extraída a lista de movimentos guardada nesse node.
-}

module Tarefa6_2021li1g025 where

import LI12122
import Maps
import Tarefa4_2021li1g025
import Tarefa2_2021li1g025

-- * Data e tipos principais

{- | Árvore quaternária, em que cada node é um dos dois:

* Folha; 

* Pai de quatro Nodes, associados aos quatro comandos de movimento possíveis.
  Por ordem, da esquerda para direita, correspondem aos movimentos: 'InterageCaixa', 'AndarEsquerda', 'AndarDireita' ou 'Trepar'.
-}
data MTree a = Empty 
                | Node a (MTree a) (MTree a) (MTree a) (MTree a) -- árvore em que cada node é pai de quatro nodes que contêm movimentos possíveis. Por ordem: InterageCaixa, AndarEsquerda,AndarDireita,Trepar.
     deriving (Show,Eq)

-- | Par que contém as coordenadas da porta
type CordsPorta = (Int,Int)

-- * Função principal 

{- | Tenta resolver um jogo no menor número de movimentos, dado um valor máximo destes.

     Caso o número de movimentos dado seja 0, testa se a posição atual do jogador é a porte e devolve "Just []" ou "Nothing".

     Caso não seja, recebe uma lista de movimentos no formato de números, proveniente de 'sequenceBuilder'.

     Após conversão da lista na função auxiliar 'decode' , devolve uma lista de movimentos (ou "Nothing").
-}
resolveJogo :: Int -- ^ Número de movimentos máximo para a solução
    -> Jogo -- ^ Jogo em questão
    -> Maybe [Movimento] -- ^ Lista dos movimentos para a solução ou Nothing
resolveJogo 0 j@(Jogo m player@(Jogador cords _ _)) | (portaFinder (desconstroiMapa m)) == cords = Just []
                                                    | otherwise =  Nothing
resolveJogo n j@(Jogo m _) = fmap decode numberseq -- permite retornar 'Nothing' ou fazer a tradução de 'Just [Int]' com 'decode' dependendo do resultado de numberseq
    where numberseq = (sequenceBuilder (n+1) (portaFinder (desconstroiMapa m)) [(Node (j,[]) Empty Empty Empty Empty)] [] [])

-- * Funções auxiliares

{- | Em cada passagem, 'sequenceBuilder' recebe uma lista de nodes da mesma profundidade, provenientes da função 'newDepth'.

     Compara os jogos guardados nesses nodes com a posição da porta (recebe as coordenadas como argumento) e de picos.

     Se encontrar um jogo correspondente à posição da porta (significa que encontrou a solução), extrai a lista de movimentos desse Node.

     Se encontrar um jogo em que a posição do jogador corresponde à posição de picos, corta esse ramo (ignora-o).

     Senão, adiciona os jogos que vai comparando a uma lista (acumulador), de modo a que não se possam voltar a repetir esses jogos em nodes subsequentes, e reinica a procura nos nodes da profunidade seguinte da árvore de movimentos.

     O primeiro Int que a função recebe tem de ser (nº de moves possíveis + 1) para funcionar

     O facto de receber as coordenadas da porta torna a função mais eficiente, por não ter de as calcular repetidamente.
-}
sequenceBuilder :: Int -- ^ Número limite de movimentos
    -> CordsPorta -- ^ Coordenadas da porta 
    -> [(MTree (Jogo,[Int]))] -- ^ Lista de nodes por comparar. Inicialmente, tem de ter o formato "[(Node (j,[]) Empty Empty Empty Empty)]", sendo 'j' o jogo inicial dado. 
    -> [(MTree (Jogo,[Int]))] -- ^ Lista de acumulação dos nodes processados. Os nodes são todos folhas de uma certa profunidade da árvore.
    -> [Jogo] -- ^ Lista de jogos que o jogador já experimentou
    -> (Maybe [Int]) -- ^ Lista de movimentos no formato de números, "Just [Int] ou "Nothing"
sequenceBuilder 0 _ tree _ _ = Nothing
sequenceBuilder depth porta [] accgames prevjogos = sequenceBuilder (depth-1) porta (newDepth accgames prevjogos) [] prevjogos
sequenceBuilder depth porta (h@(Node (jogoresult@(Jogo mapa (Jogador cords@(x,y) _ _)),moves) _ _ _ _):t) accgames prevjogos 
    | cords == porta = Just moves
    | (mapa !! y) !! x == Picos = sequenceBuilder depth porta t (accgames) (saveGames jogoresult prevjogos)
    | otherwise = sequenceBuilder depth porta t (h:accgames) (saveGames jogoresult prevjogos)

{- | Obtém o conjunto de nodes da profunidade seguinte na árvore de movimentos.

     Introduz os nodes pais (da lista que recebe) na função 'addDepth', e extrai todos os nodes filhos que contenham jogos inexplorados.

     Se receber algum node 'Empty' , filtra-o da lista resultante.
-}

newDepth :: [(MTree (Jogo,[Int]))] -- ^ Lista de nodes (folha) de uma certa profundidade
    -> [Jogo] -- ^ Lista de jogos que o jogador já experimentou
    -> [(MTree (Jogo,[Int]))] -- ^ Lista de nodes folha da profunidade seguinte
newDepth [] _ = [] 
newDepth (h:t) prevjogos = filter (/= Empty) (a:b:c:d:rest)
    where branches@(Node _ a b c d) = addDepth h prevjogos
          rest = newDepth t prevjogos

{- | Adiciona um de profundidade a folhas.

     Recebe um Node folha e expande esse Node, conectando-o a quatro Nodes filhos. 

     Cada um destes filhos guarda o jogo resultante de aplicar, respetivamente, o movimento 'InterageCaixa', 'AndarEsquerda', 'AndarDireita' ou 'Trepar' ao jogo do Node pai (node original). 

     Simultaneamente, ao aplicar um movimento, também adiciona um número correspondente ao movimento (1 ->'InterageCaixa', 2 -> 'AndarEsquerda', 3 -> 'AndarDireita' ou 4 -> 'Trepar') à lista que o respetivo Node filho guarda.

     No entanto, se ao expandir um dos ramos, se detetar que o jogo guardado num dos 4 novos nodes já consta na lista de jogos que a função recebe, esse ramo devolve 'Empty', em vez de uma folha. 

     Ou seja, a função só cria novos nodes que guardem jogos inexplorados antes, aumentando a eficiência.
-}   

addDepth :: MTree (Jogo,[Int]) -- ^ Node folha que contém um jogo e uma lista de movimentos no formato de números
    -> [Jogo] -- ^ Lista de jogos que o jogador já experimentou
    -> MTree (Jogo,[Int]) -- ^ Node pai que liga a quatro Nodes filhos (folhas) 
addDepth Empty _ = Empty -- prevenção de bug
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

{- | Verifica se um jogo já consta numa lista de jogos. Se não existir, é adicionado à lista; caso já exista, não é.
     Usada na função 'sequenceBuilder'
-}

saveGames :: Jogo -- ^ Jogo que irá ser comparado
    -> [Jogo] -- ^ Lista de jogos 
    -> [Jogo] -- ^ Lista de jogos (todos diferentes)
saveGames x [] = [x]
saveGames x l = if x `elem` l then l else (x:l)

{- | Descodifica uma lista de números inteiros, devolvendo uma lista de movimentos. 

    Segue a chave da função 'addDepth', ou seja, os números corresondem a: 1 ->'InterageCaixa', 2 -> 'AndarEsquerda', 3 -> 'AndarDireita', 4 -> 'Trepar'.

    Também inverte os elementos da lista, porque recebe, da função 'addDepth', os elementos na ordem contrária.

    Usada na função 'resolveJogo'.
-}
decode :: [Int] -- ^ Lista de inteiros
    -> [Movimento] -- ^ Lista de movimentos correspondentes
decode n = reverse (decodeAux n)
    where   decodeAux :: [Int] -> [Movimento]
            decodeAux [] = []
            decodeAux l@(h:t) =   case h of 
                        1 -> InterageCaixa: (decodeAux t)
                        2 -> AndarEsquerda: (decodeAux t)
                        3 -> AndarDireita: (decodeAux t)
                        4 -> Trepar: (decodeAux t)

{- | Encontra as coordenadas da porta de um jogo.

    Usada na função 'sequenceBuilder'.

=== Notas sobre eficiência

Foi utilizado o comando ":set +s" no ghci para as seguintes afirmações:

== Sobre a obtenção da lista de movimentos :

Foi testado devolver diretamente na função 'addDepth' uma lista de movimentos (em vez de uma lista de números).

Foi testado adicionar diretamente na função 'addDepth' os sucessivos movimentos ao fim da lista guardada nos Nodes (evitando um 'reverse' posterior).

Mas, estas alternativas mostraram-se menos eficientes.

A maneira mais eficiente de obter o resultado pareceu ser codificar os movimentos, descodificar a lista de números final e depois reverter o resultado.

No entanto, a escala de profunidade das árvores usada para os testes foi pequena.

== Sobre a acumulação dos sucessivos jogos numa lista, na função 'SequenceBuilder':

A acumulação de uma grande quantidade de jogos na lista tornava a função ineficiente. 

No entanto, foi a melhor forma que conseguimos garantir que não eram cortados certos ramos incorretamente. 

Se não guardássemos o mapa, não se tinha em conta caixas colocadas após o início, cortando ramos.

Se não guardássemos a informação do jogador, ao atravessar a mesma posição com 'Direcao'  ou "Bool" diferente, o ramo era cortado.

Assim, guardar o jogo todo foi a melhor abordagem que encontramos.

-}

portaFinder :: [(Peca,Coordenadas)] -> CordsPorta
portaFinder l@(h@(p,cords):t) = if p == Porta then cords else portaFinder t 







