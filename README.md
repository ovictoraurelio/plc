# PLC - Paradigmas de Linguagens computacionais | Programming Pparadigms

## My lessons or whatever to discipline Programming Paradigms at CIn - UFPE.

### Tutorial

Open GHCI inside workspace folder typing ghci on terminal
    `$ cd /myworkspace && ghci`
To load a file type: 
    `$ :load file.hs`
Now you just need call a function in file.hs
    `$ myFunc`


### List generators

[0..5]  ---> [0,1,2,3,4,5]

### List functions

## take
    take 3 [1,2,3,4,5,6]   --> [1,2,3]

## drop    
    drop 3 [1,2,3,4,5,6]   --> [4,5,6]

## map
    map func [1,2,3]       -->  [func 1, func 2, func 3]

## filter
    filter func [1,2,3]    --> [1,2] if satf func
    filter (>=2) [1,2,3]   --> [2,3]

## reverse
    reverses [1,2,3,4]       --> [4,3,2,1]

## zip
    zip [a,b] [c,d]        --> [(a,c), (c,d)]

## fmap 
    fmap (*2)  [1,2,3]

## foldr or foldl (right or left)
    foldr (+) 0 (fmap (+2) [1,2,3])    --> 12

## foldr1 no have base case
    foldr1 (+) [1,2,3]      --> 6

## elem
    elem 4 [1,2,3,4,5,6]      --> True
    elem 9 [1,2,3,4,5,6]      --> False

## maximum and minimum
    maximum [1,2,3,4,5,6]      --> 6
    minimum [1,2,3,4,5,6]      --> 1

### List Comprehension

    [ ( op item ) | item <- [1,2,3], conditions]           ---> [op 1, op 2, op 3] if condition
    [ (i1, i2) | i1 <- [1,2], i2 <- [1,2], conditions]     ---> [(1,1), (1,2), (2,1), (2,2)] if condition

### Where

    func :: t -> t
    func _ = _
    func x = anotherFunc x
        where anotherFunc _ = _
              anotherFunc x = x

### Let
    can be used to generate var 


# Class

### Instance

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