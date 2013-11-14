Project Euler
=============

[Project Euler][1] is, to quote its website:

> *... a series of challenging mathematical/computer programming problems that will require more than just mathematical insights to solve. Although mathematics will help you arrive at elegant and efficient methods, the use of a computer and programming skills will be required to solve most problems.*

There are around [450 problems][2] in the series (Nov 2013), in roughly increasing order of difficulty: problems 1 to 10 can be solved in minutes, while problems 100 and beyond could take hours. The main challenge, especially from problem 50 on, is to come up with solutions that execute in a reasonable amount of time (usually seconds, but sometimes up to a minute).

I started working on the problems after reading part of [Real World Haskell][3], in January 2010. At that point, I needed something to practice on--to solidify my knowledge of Haskell, to learn thinking the functional way. I remembered I had loved solving logical/mathematical puzzles in secondary and high school, so this was a good fit. After a while, it became quite addictive.

It turns out that Haskell is a good language for the task: it is now the fourth most "highly scored" on *Project Euler* (after *PARI/GP*, *Mathematica*, and *Python*) ([source][4]; needs free login).

I solved 107 of the first 125 problems, which puts me in the 98th percentile of users, by number of problems solved.

Some of the more interesting ones:

- [Problem 54][11]: find the winner in every pair of poker hands, for 1000 pairs ([solution][12])
- [Problem 80][5]: calculate irrational square roots up to 100 decimals ([solution][6]); builds on [problem 64][7] ([solution][8])
- [Problem 89][13]: rewrite 1000 roman numerals in minimal form ([solution][14])
- [Problem 96][9]: solve 50 Sudoku grids ([solution][10])

The series contains a lot of [number theory][100] problems, involving prime numbers and [factorization][101] into primes. [Problem 3][15] is a simple factorization, but its code is used by 21 other problems, so eventually I had to optimize its performance ([solution][16]).

There are also a fair number of [combinatorics][102] problems. [Problem 12][17] contains code for generating [combinations][103], used in 6 other problems ([solution][18]).


  [1]: http://projecteuler.net/
  [2]: http://projecteuler.net/problems
  [3]: http://www.amazon.com/Real-World-Haskell-Bryan-OSullivan/dp/0596514980
  [4]: http://projecteuler.net/languages
  
  [100]: http://en.wikipedia.org/wiki/Number_Theory
  [101]: http://en.wikipedia.org/wiki/Integer_factorization
  [102]: http://en.wikipedia.org/wiki/Combinatorics
  [103]: http://en.wikipedia.org/wiki/Combinations
  
  [5]: http://projecteuler.net/problem=80
  [6]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob080.hs
  
  [7]: http://projecteuler.net/problem=64
  [8]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob064.hs
  
  [9]: http://projecteuler.net/problem=96
  [10]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob096.hs
  
  [11]: http://projecteuler.net/problem=54
  [12]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob054.hs
  
  [13]: http://projecteuler.net/problem=89
  [14]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob089.hs
  
  [15]: http://projecteuler.net/problem=3
  [16]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob003.hs
  
  [17]: http://projecteuler.net/problem=12
  [18]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob012.hs
