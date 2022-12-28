# Exercise 1 --------------------------------------------------------------

library(bench)

# one
one <- function(nr, nc){
  set.seed(1)
  X <- matrix(rnorm(nr * nc, 10, 3), nrow = nr)
  grp <- gl(2, nc/2)
  res <- vector("list", nr)
  for(i in seq_len(nr)){
    res[[i]] <- coef(lm(X[i,] ~ grp))
  }
  do.call("cbind", res)
}

# two
two <- function(nr, nc){
  set.seed(1)
  X <- matrix(rnorm(nr * nc, 10, 3), nrow = nr)
  grp <- gl(2, nc/2)
  res2 <- coef(lm(t(X) ~ grp))
}

nr <- 10
nc <- 50
bm <- bench::mark(one(nr, nc), two(nr, nc))
bm


# Exercise 2 --------------------------------------------------------------

