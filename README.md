# alg-int-calc-scala

The Algebraic Integer Calculator project, but in Scala, using TDD more 
thoroughly.

The main appeal of Scala for this project is the exploration of the Euclicean 
algorithm for greatest common divisor (GCD). In $\mathbb Z$, the choice of the 
absolute value function is the best and the most obvious. But already in 
quadratic rings we see that different functions are needed.

Although Java has had higher order functions since Java 8, they feel far more 
natural in Scala, which has had them from the beginning.

I started this project on Scala 2.13 under Java 8, with JUnit 4. Even though I 
have upgraded to Java 21, I'm still using Java 8 for this project, and maybe 
that also means this project has to stay on Scala 2 as well. But I've been 
gradually upgrading the tests to JUnit 5.7.
