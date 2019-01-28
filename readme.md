The four seasons of commodity futures
================
Olivier
2019-01-28

Distances
---------

Hi Devraj & Pawel,

It seems the distance functions in the R TDA package calculate distances over a grid of points:

``` r
set.seed(4321L); by <- 0.05

matrix <- matrix(runif(25L, 0L, 1L), nrow = 5L, ncol = 5L)
grid <- expand.grid(seq(0L, 1L, by = by), seq(0L, 1L, by = by), 
                    seq(0L, 1L, by = by), seq(0L, 1L, by = by), 
                    seq(0L, 1L, by = by))

distance <- TDA::distFct(X = matrix, Grid = grid)

head(distance)
```

    #> [1] 1.0856491 1.0510870 1.0178084 0.9859432 0.9556327 0.9270296

This approach is a rather different from the one we take in the paper. The most convenient way to proceed at this stage I guess is to go for the individual .csv files as we discussed; you can find them here:
[correlation matrices](https://www.dropbox.com/s/zrwt773wjn0bukc/matrices.tar.xz?dl=0).

Regarding clustering, are we agnostic on the algorithmic approach or do you have a particular method in mind?

Speak soon,

Olivier.
