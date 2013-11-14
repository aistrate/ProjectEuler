Project Euler
=============

[Project Euler][1] is, to quote its website:

> *... a series of challenging mathematical/computer programming problems that will require more than just mathematical insights to solve. Although mathematics will help you arrive at elegant and efficient methods, the use of a computer and programming skills will be required to solve most problems.*

There are almost [450 problems][2] in the series (Nov 2013), in roughly increasing order of difficulty. Problems 1 to 10 can be solved in minutes, while problems 100 and beyond can take hours or even days. The main challenge, especially from problem 50 on, is coming up with solutions that execute in a reasonable amount of time (usually a few seconds, but sometimes up to a minute).

I started on the problems after reading part of [Real World Haskell][3], in January 2010. At that point, I needed something to practice on, to solidify my knowledge of the Haskell language. Also, I remembered I had loved solving logical/mathematical puzzles in secondary/high school, so this was a good fit. After a while, it became quite addictive.

It turns out that Haskell is a good language for the task, as it is now the fourth most "highly scored" on *Project Euler* (after *PARI/GP*, *Mathematica*, and *Python*) ([source][4]; needs free login).

I solved 107 of the first 125 problems, which puts me in the 98th percentile of users, by number of problems solved.

Here are some of the more interesting problems (note that some of them build on earlier ones):

- [Problem 80][5]: calculate irrational square roots up to 100 decimals ([solution][6]); builds on [problem 64][7] ([solution][8])
- [Problem 96][9]: solve 50 Sudoku grids ([solution][10])


  [1]: http://projecteuler.net/
  [2]: http://projecteuler.net/problems
  [3]: http://www.amazon.com/Real-World-Haskell-Bryan-OSullivan/dp/0596514980
  [4]: http://projecteuler.net/languages
  
  [5]: http://projecteuler.net/problem=80
  [6]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob080.hs
  
  [7]: http://projecteuler.net/problem=64
  [8]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob064.hs
  
  [9]: http://projecteuler.net/problem=96
  [10]: https://github.com/aistrate/ProjectEuler/blob/master/Solutions/Prob096.hs
