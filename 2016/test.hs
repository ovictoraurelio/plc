type Dia = String
type Hora = String
type Usuario = String
data LogEntry = Permitido Dia Hora Usuario | Negado Dia Hora Usuario
     deriving (Show)

logSetembro = "2016-09-27;19:31:52;Normal;208772;\n2016-09-27;18:12:02;Normal;155759;\n2016-09-26;17:12:02;Normal;155759;\n2016-09-26;16:11:02;Denied;188758;\n2016-09-25;19:12:02;Normal;155759;"

-- Função dada na questão
strToInt :: String -> Int
strToInt str = read str

--
--
--      Q4
-- 
--

convertLog :: String -> [LogEntry]
convertLog [] = []
convertLog str = toLogEntry (take 34 str) : convertLog (drop 35 str)


toLogEntry :: String -> LogEntry
toLogEntry "" = error "Cannot convert"
toLogEntry str | ((slice str 20 6)) == "Normal" = (Permitido (slice str 8 2) (slice str 11 8) (slice str 27 6))
               | otherwise = (Negado (slice str 8 2) (slice str 11 8) (slice str 27 6))

slice :: String -> Int -> Int -> String
slice [] _ _ = []
slice str n m = take m (drop n str) 


--
--      Q1
--
(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (x,y) (a,b) = ((x+a), (y+b))

countAll :: [LogEntry] -> (Int,Int)
countAll [] = (0,0)
countAll ((Permitido x y z):as) = (1,0) +++ (countAll as)
countAll ((Negado x y z):as) = (0,1) +++ (countAll as)

-- Just for be THE SAME OF QUESTION WANT
tiposDeAcesso :: String -> (Int, Int)
tiposDeAcesso str = countAll (convertLog str)

--
-- Functions below are useful to Q2 and Q3
--

-- Get value of prop in LogEntry data
sProp :: String -> LogEntry -> String
sProp [] _ = error "You must passed a valid prop"
sProp "Dia" (Permitido x y z)        = x
sProp "Hora" (Permitido x y z)       = y
sProp "Usuario" (Permitido x y z)    = z
sProp "Dia" (Negado x y z)           = x
sProp "Hora" (Negado x y z)          = y
sProp "Usuario" (Negado x y z)       = z

countDays :: String -> String -> [LogEntry] -> [(Int,Int)]
countDays _ _ [] = []
countDays p beforeSearch (a:as) | beforeSearch == (sProp p a)  = (countDays p (sProp p a) as) 
                                | otherwise                    = ( (strToInt (sProp p a)), (countDay p (sProp p a) (a:as))) : (countDays p (sProp p a) as) 

countDay :: String -> String -> [LogEntry] -> Int
countDay _ _ [] = 0
countDay p x (a:as) | x == (sProp p a) = 1 + (countDay p x as)
                    | otherwise       = (countDay p x as)

--
--      Q2
--

-- Just for be THE SAME OF QUESTION WANT
acessoPorDia :: String -> [(Int,Int)]
acessoPorDia str = (countDays "Dia" "0" (convertLog str))

--
--     Q3
--
acessosPorUsuario :: String -> [(Int, Int)]
acessosPorUsuario str = (countDays "Usuario" "0" (convertLog str))



--
--
--
-- Q2 or Q3 on HARD way you need to set X, Y or Z that you want to Compare!
--
--  In hard way you need repeat functions below for each "query", Usuario or Day
--
countDaysWithoutSProp :: String -> [LogEntry] -> [(Int,Int)]
countDaysWithoutSProp _ [] = []
countDaysWithoutSProp beforeSearch ((Permitido x y z):as) | beforeSearch == x    = (countDaysWithoutSProp x as) 
                                              | otherwise         = ((strToInt x), (countDayWithoutSp x ((Permitido x y z):as))) : (countDaysWithoutSProp x as) 

countDaysWithoutSProp beforeSearch ((Negado x y z):as)    | beforeSearch == x    = (countDaysWithoutSProp x as) 
                                              | otherwise         = ((strToInt x), (countDayWithoutSp x ((Negado x y z):as))) : (countDaysWithoutSProp x as) 

countDayWithoutSp :: String -> [LogEntry] -> Int
countDayWithoutSp x [] = 0
countDayWithoutSp x ((Permitido a y z):as)   | x == a      = 1 + (countDayWithoutSp x as)
                                    | otherwise   = (countDayWithoutSp x as)
countDayWithoutSp x ((Negado a y z):as)      | x == a      = 1 + (countDayWithoutSp x as)
                                    | otherwise   = (countDayWithoutSp x as)