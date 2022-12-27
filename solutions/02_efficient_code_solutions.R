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

library(efficient)
library(future)
library(future.apply)

plan(sequential)
set.seed(42)
y <- future_sapply(seq_len(N), snakes_ladders, future.seed = TRUE)

RNGkind("L'Ecuyer-CMRG")
set.seed(42)
seed <- .Random.seed
par_seeds <- vector(mode = "list", length = N)
for (i in seq_len(N)) {
  par_seeds[[i]] <- nextRNGSubStream(seed)
  seed <- nextRNGStream(seed)
}

sapply(seq_len(N), function(i) {
  .Random.seed <<- par_seeds[[i]] # set .Random.seed in global workspace
  snakes_ladders()
})


# use parallelly::availableCores() - 1 in general!
clus <- makeCluster(parallel::detectCores() - 1)

N <- 10

one <- function(N){
  set.seed(1)
  sapply(seq_len(N), snakes_ladders)
}

two <- function(N){
  set.seed(1)
  parSapply(clus, seq_len(N), snakes_ladders)
}

bench::mark(one(10), two(10))
