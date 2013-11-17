Project Euler
=============

[Project Euler][1] is, to quote its website:

> *... a series of challenging mathematical/computer programming problems that will require more than just mathematical insights to solve. Although mathematics will help you arrive at elegant and efficient methods, the use of a computer and programming skills will be required to solve most problems.*

There are around [450 problems][2] in the series (Nov 2013), in roughly increasing order of difficulty: problems 1 to 10 can be solved in minutes, while problems 100 and beyond could take hours. The main challenge, especially from problem 50 on, is to come up with solutions that execute in a reasonable amount of time (usually seconds, but sometimes up to a minute).

This repository contains my solutions to some of the problems, in Haskell. I solved **107 problems** out of the first 125 in the series (which puts me in the 98th percentile of users, by number of problems solved).

I started working on the problems after reading part of [Real World Haskell][3], in January 2010. At that point, I needed something to practice on, to solidify my knowledge of Haskell, to learn to think the functional way. I remembered how much I loved solving logical/mathematical puzzles in secondary and high school, so I thought this would fit the bill. After a while, it became quite addictive, almost like a video game.

Some of the more interesting problems:

- [Problem 54][11]: for 1000 pairs of poker hands, find the winning hand in every pair ([solution][12])
- [Problem 80][5]: calculate irrational square roots up to 100 decimals ([solution][6]); builds on [problem 64][7] ([solution][8])
- [Problem 89][13]: rewrite 1000 Roman numerals in minimal form ([solution][14])
- [Problem 96][9]: solve 50 Sudoku grids ([solution][10])

The series contains a lot of problems from [number theory][100] (prime numbers, [factorization][101] into primes), and [combinatorics][102].


  [1]: http://projecteuler.net/
  [2]: http://projecteuler.net/problems
  [3]: http://www.amazon.com/Real-World-Haskell-Bryan-OSullivan/dp/0596514980
  [4]: http://projecteuler.net/languages
  
  [100]: http://en.wikipedia.org/wiki/Number_Theory
  [101]: http://en.wikipedia.org/wiki/Integer_factorization
  [102]: http://en.wikipedia.org/wiki/Combinatorics
  
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
