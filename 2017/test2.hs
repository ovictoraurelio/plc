-- Primeira Prova de Paradigmas de Linguagens Computacionais 
-- 1/2017 - 01/05/2017
--
-- Nome: 
--
{- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo: isSorted [1,6,8,9,9] ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}

isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (a:(b:bs)) | (a <= b)     = (True && (isSorted (b:bs)))
                    | otherwise    = False


{- 2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
   cada elemento da lista de entrada é comparado com o seguinte, 
   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
   (nenhuma troca seja mais necessária).
   exemplo, passo a passo: 
       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
                                   ----> [4,3,6,1,8,8]
       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
Implemente a função bSort.
Dica 1: use funções auxiliares, que façam parte do processo;
Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.
-}

troca :: Ord t => [t] -> [t]
troca []  = []
troca [x] = [x]
troca (a:b:bs) | (a >= b)   = b : troca (a:bs)
               | otherwise  = a : troca (b:bs)

bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort [x] = [x]
bSort list | (isSorted list) = list
           | otherwise       = (bSort (troca list))

{- 3) (2.5) explique como funciona e informe qual o resultado da execução das 
   seguintes expressões. Caso estejam erradas explique por que.
a) map (\x -> x + x) [3,5,7,9]
b) filter (\x -> x < 7) [5,7,9,11]
c) foldr1 (*) [-2,0,2,4]
d) foldr (+) 20 [-2,0,2,4]
e) (map (+2) . filter (<7)) [5,7,9,11]
-}

-- a) A função map realiza a operação (de soma) sobre cada elemento do vetor
-- b) A função filter cria uma nova lista com os elementos que se aplicam a regra (de ser menor que 7)
-- c) a função foldr1 realiza a operação sobre os elementos da lista e os juntas r1 é porque realiza pela direita (right)
-- d) a função foldr (+) realiza a operação (+) e junta os elementos e unicia com  elemento 20
-- e) é aplicada uma função composta de map e filter, no caso filter é aplicado primeiro então é criada uma lista que contém apenas o 5
--     posteriormente a lista é mapeada e fica com o valor 7 pois 5 é somado a 2.

{- 4) (2.5) Dada o tipo de dados Tree t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada,
ou seja, os valores em nós ou folhas na sub-àrvore à esquerda são sempre menores ou iguais ao valor do nó, 
e os da sub-árvore à direita sempre maiores ou iguais.
-}

data Tree t = Node t (Tree t) (Tree t) 
            | Leaf t

testeOrdenado :: Tree Int
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))

testeNaoOrdenado :: Tree Int
testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))


flatten :: Tree t -> [t]
flatten (Leaf x) = [x]
flatten (Node x a b) = flatten a ++ [x] ++ flatten b

isSortedTree :: Ord t => Tree t -> Bool
isSortedTree x = isSorted (flatten x)

-- isSortedTree testeOrdenado ----> True
-- isSortedTree testeNaoOrdenado ----> False
-- isSortedTree :: Ord t => Tree t -> Bool
-- isSortedTree (Leaf x) = True
-- isSortedTree (Node x (Leaf l) (Leaf r)) | (x) (l < r) = True
-- isSortedTree (Node x (Node l _ _) (Leaf r)) = True
-- isSortedTree (Node x (Leaf l) (Node r _ _)) = False
-- isSortedTree (Node x (Node l a b) (Node r c d)) | (l <= r)  = ((isSortedTree (Node l a b)) && (isSortedTree (Node r c d)))
--                                                 | otherwise = False