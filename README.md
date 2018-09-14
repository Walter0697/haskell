Some haskell programs for myself to practice, or some problems I saw in lecture 3007 that I would like to implement for fun

### haskellCompile.sh
- a shell script to help myself compiling or cleanning haskell code to executable
- ./haskellCompile.sh compile <file_name>
- ./haskellCompile.sh clean <file_name>
- (type the file name without .hs)
- usually it will be good to use ghci with :load <module_name>

### list.hs
- self-implemented list data structure with different functions 
- based implementation here : http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures

### number_checker.hs
- Apocalyptic Number : if 2^n contains 666, then n is an apocalyptic number
- http://mathworld.wolfram.com/ApocalypticNumber.html
- Automorphic Number : if n^2 contains n at the end, then n is an automorphic number
- https://en.wikipedia.org/wiki/Automorphic_number

### Vampire Number, maybe 