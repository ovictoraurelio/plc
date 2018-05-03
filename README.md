# PLC - Paradigmas de Linguagens computacionais | Programming Models (paradigms)

## My lessons or whatever to discipline Programming Models (paradigms) at CIn - UFPE.

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

[ ( op item ) | item <- [1,2,3], conditions]        ---> [op 1, op 2, op 3] if condition
[ (i1, i2) | i1 <- [1,2], i2 <- [1,2], conditions]     ---> [(1,1), (1,2), (2,1), (2,2)] if condition

### Let
    can be used to generate var x exprs in exprs