--
--  PLC 2018.1 Prova 1
--  Haskell
--  
--  Professor: André Luís
--  Aluno: Victor Aurélio Guimarães Silva
--  Email: vags@cin.ufpe.br
-- 

data Time =  Australia | Dinamarca | Franca | Peru | Argentina | Croacia | Islandia | Nigeria
    deriving (Eq,Show)

type Jogo = (Time, Int, Time, Int)

jogos1 :: [Jogo]
jogos1 = [(Australia, 1, Dinamarca, 3), (Franca, 2, Peru, 0),
          (Australia, 0, Franca, 2), (Dinamarca, 0, Peru, 0),
          (Dinamarca, 0, Franca, 1), (Australia, 0, Peru, 0),
          (Argentina, 1, Croacia, 0), (Islandia, 0, Nigeria, 1),
          (Argentina, 1, Islandia, 0), (Argentina, 1, Nigeria, 1),
          (Croacia, 0, Islandia, 0), (Croacia, 1, Nigeria, 4)]

-- A )

-- Exemplo: gols Croacia jogos1

gols :: Time -> [Jogo] -> Int
gols _ [] = 0
gols x (a:as) = (golPorJogo x a) + (gols x as)
    where golPorJogo time (a,b,c,d) | time == a = b
                                    | time == c = d      
                                    | otherwise = 0        

-- B )

-- Exemplo: saldo Croacia jogos1

saldo :: Time -> [Jogo] -> Int
saldo _ [] = 0
saldo x (a:as) = (saldoPorJogo x a) + (saldo x as)
    where saldoPorJogo time (a,b,c,d) | time == a = (b-d)
                                      | time == c = (d-b)
                                      | otherwise = 0 -- saldo de gols quando o time não jogou é 0, apenas para ignorar...

-- C )

-- Exemplo: pontos Croacia jogos1

pontos :: Time -> [Jogo] -> Int
pontos _ [] = 0
pontos time jogos = foldr1 (+) ([ 3 | jogo <- jogos, (saldoPorJogo' time jogo) > 0] ++ [ 1 | jogo <- jogos, (saldoPorJogo' time jogo) == 0])
    where saldoPorJogo' time (a,b,c,d)  | time == a = (b-d)
                                        | time == c = (d-b)
                                        | otherwise = (-1) -- porque caso o time não tenha jogado o saldo 0 seria empate e daria 1 ponto...

-- D )                                        

type TimesPorGrupo = (Time, Time, Time, Time)
data Grupo = A TimesPorGrupo | B TimesPorGrupo | C TimesPorGrupo | D TimesPorGrupo | E TimesPorGrupo | F TimesPorGrupo | G TimesPorGrupo | H TimesPorGrupo
    deriving (Show)

-- E )

grupos :: [Grupo]
grupos = [(C (Australia, Dinamarca, Franca, Peru)), (D (Argentina, Croacia, Islandia, Nigeria))]  

--
--  Para testar essa função pode ser utilizado:  classificados (A (Australia, Dinamarca, Croacia, Islandia)) jogos1
--  Definição de Tipo 
--

classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados _ [] = error "Não é possível definir os classificados se não houveram jogos"
classificados (A (a,b,c,d)) jogos = pegarTupla (take 2 (ordenaListaDeTimes ([a]++[b]++[c]++[d]) jogos))
    where pegarTupla (a:b:[]) = (a,b)
          pegarTupla _ = error "Impossível tupla inválida"

ordenaListaDeTimes :: [Time] -> [Jogo] -> [Time]
ordenaListaDeTimes [] _ = []
ordenaListaDeTimes _ [] = error "Não é possível ordenar os times se não houve jogo."
ordenaListaDeTimes (x:xs) jogos = (ordenaListaDeTimes [time | time <- xs, (timeMenor x time jogos) ] jogos) ++ [x] ++ (ordenaListaDeTimes [time | time <- xs, (timeMaior x time jogos) ] jogos)
  where timeMenor x time jogos  | (pontos time jogos) > (pontos x jogos) = True
                                | (pontos time jogos) == (pontos x jogos) && (saldo time jogos) > (saldo x jogos) = True
                                | otherwise = False
        timeMaior x time jogos  | (pontos time jogos) < (pontos x jogos) = True
                                | (pontos time jogos) == (pontos x jogos) && (saldo time jogos) < (saldo x jogos) = True
                                | otherwise = False