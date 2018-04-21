-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥
-- ♥			        Third exercise list of 2018.1 PLC
-- ♥
-- ♥			        @author ovictoraurelio
-- ♥			        @github http://github.com/ovictoraurelio
-- ♥			        @website http://victoraurelio.com
-- ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ 

-- Q3
-- 
-- Dado, abaixo, os tipos algébricos polimórficos LQueue e RQueue que
-- representam filas, crie uma classe OprQueue que contenha as funções enqueue,
-- dequeue, peek e isEmpty.
-- data LQueue t = LQ [t] deriving (Show)
-- data RQueue t = Empty | RQ t (RQueue t) deriving (Show)
-- Crie também instâncias da classe OprQueue para os dois tipos algébricos acima.
-- Caso a fila esteja vazia e seja utilizada alguma das operações dequeue ou peek,
-- encerre a execução do programa e exiba a mensagem de erro: “Empty Queue!”.
-- Exemplos:
-- Main> peek (dequeue (enqueue (LQ [3, 4, 5]) 7))
-- 4
-- Main> isEmpty (enqueue (enqueue Empty (>5)) (==0))
-- False
-- 
-- Main> enqueue (LQ []) (RQ [12.96, 0.1225] Empty)
-- LQ [RQ [12.96, 0.1225] Empty]

data LQueue t = LQ [t] deriving (Show)
data RQueue t = Empty | RQ t (RQueue t) deriving (Show)

class OprQueue q where
    enqueue :: q t -> t -> q t
    dequeue :: q t -> q t
    peek    :: q t -> t
    isEmpty :: q t -> Bool

instance OprQueue LQueue where
    isEmpty (LQ [])     = True
    isEmpty (LQ (a:as)) = False
    peek    (LQ [])     = error "Empty Queue!"
    peek    (LQ (a:as)) = a
    dequeue (LQ [])     = error "Empty Queue!"
    dequeue (LQ (a:as)) = (LQ as)    
    enqueue (LQ []) x   = (LQ [x])
    enqueue (LQ d)  x   = (LQ (d ++ [x]))

instance OprQueue RQueue where
    isEmpty (Empty)  = True
    isEmpty (RQ x y) = False
    peek    (Empty)  = error "Empty Queue!"
    peek    (RQ x z) = x   
    dequeue (Empty)             = error "Empty Queue!"
    dequeue (RQ x Empty)        = (Empty)
    dequeue (RQ x (RQ y z))     = (RQ y z)         
    enqueue (Empty) x           = (RQ x Empty)
    enqueue (RQ a b) x          = (RQ a (enqueue b x))