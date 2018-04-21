-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥
-- ♥
-- ♥			        Third exercise list of 2018.1 PLC
-- ♥
-- ♥			        @author ovictoraurelio
-- ♥			        @github http://github.com/ovictoraurelio
-- ♥			        @website http://victoraurelio.com
-- ♥
-- ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ ♥ 

-- Q2
--
-- Defina um tipo algébrico Temperatura com 3 construtores (Celsius, Fahrenheit e
-- Kelvin) que terão valores (Float) representando temperaturas nas escalas indicadas.
-- Em seguida, crie instâncias das classes Ord, Eq e Show para Temperatura (leve em
-- conta que TemperaturaCelsius/5 = (TemperaturaFahrenheit-32)/9 =
-- (TemperaturaKelvin-273)/5) e depois crie uma função minMax, que recebe um lista
-- de temperaturas e retorna um par em que o primeiro elemento é a menor
-- temperatura da lista e o segundo elemento a maior.
--
data Temperature = Celsius Float | Fahrenheit Float | Kelvin Float   

showTmp :: Temperature -> String
showTmp (Celsius x)      = "Temp: " ++ show x ++ "º C"
showTmp (Fahrenheit x)   = "Temp: " ++ show x ++ "° F"
showTmp (Kelvin x)       = "Temp: " ++ show x ++ " K"

toKelvin :: Temperature -> Float
toKelvin (Celsius x)        = (x + 273)
toKelvin (Fahrenheit x)     = ((((x - 32) / 9) * 5) + 273)
toKelvin (Kelvin x)         = x

cmpTmp :: (Float -> Float -> Bool) -> Temperature -> Temperature -> Bool
cmpTmp f (Kelvin x) (Kelvin y)             = f x y
cmpTmp f (Celsius x) (Celsius y)           = f x y
cmpTmp f (Fahrenheit x) (Fahrenheit y)     = f x y 
cmpTmp f (Kelvin x) (Celsius y)            = f x (toKelvin (Celsius y))
cmpTmp f (Kelvin x) (Fahrenheit y)         = f x (toKelvin (Fahrenheit y))
cmpTmp f (Celsius x) (Fahrenheit y)        = f (toKelvin (Celsius x)) (toKelvin (Fahrenheit y))
cmpTmp f (Celsius x) (Kelvin y)            = cmpTmp f (Kelvin y) (Celsius x)
cmpTmp f (Fahrenheit x) (Kelvin y)         = cmpTmp f (Kelvin y) (Fahrenheit x) 
cmpTmp f (Fahrenheit x) (Celsius y)        = cmpTmp f (Celsius y) (Fahrenheit x)

instance Show Temperature where
        show = showTmp

instance Eq Temperature where
        (==) = (cmpTmp (==))

instance Ord Temperature where 
        (<=)    = (cmpTmp (<=))
        (>) x y = (cmpTmp (>)) y x

minMax :: [Temperature] -> (Temperature, Temperature)
minMax [] = error "There\'s no min or max to a Empty list."
minMax l = (minimum l, maximum l)