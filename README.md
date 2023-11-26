# Haskell
Problems and simulators made in Haskell. The problem descriptions are the following:  

1. Implement the Extended Euclidean Algorithm in Haskell in its classic version in two ways: the first would be with an iterative style, specifying and providing, let's say, the function `ixeuc(m; n)`, and the second with a recursive style, specifying and providing, let's say, the function `rxeuc(m; n)`. Either of these two functions will receive as arguments two integers m and n and provide three values, namely g, u, and v, such that g = mu + nv. The implementation will have the peculiarity of being that contemplated by classical decimal arithmetic. The solution is in the file `Euclidean_Algorithm_Extended.hs`.

2. Implement the Extended Euclidean Algorithm in Haskell by providing a function, let's say `bxeuc(m; n)`, that given two integers m and n, provides three other values, namely g, u, and v, such that g = mu + nv. This second version and implementation of the Extended Euclidean Algorithm—known as Stein's algorithm in certain environments—have the peculiarity of necessarily departing from the classical arithmetic implementation and instead proceeding exclusively based on the binary representation of numbers. The solution is in the file `Euclidean_Algorithm_Extended_Binary.hs`.

4.  Implement the Towers of Hanoi puzzle in Haskell or Prolog, according to your choice. The solution is in the file `Hanoi_Towers.hs`.

5.  Implement a simulator in Haskell for the Hagelin C-52 machine and make your implementation configurable. The solution is in the file `Hagelin_C52.hs`.


## Compile and Run Code
Compile:  
`ghc -o exe "file_name".hs`   
Run:  
`./exe`

