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
plot(bm)


# Exercise 2 --------------------------------------------------------------

library(bench)

one <- function(n){ # n: the number of values to sample
  # Set the random seed
  set.seed(1)        
  
  # Sample the component each value belongs to
  component <- sample(1:3, prob = c(0.3, 0.5, 0.2),
                      size = n, replace = TRUE)
  
  # Sample from the corresponding Normal for each value
  x <- numeric(n)
  for(i in seq_len(n)){
    if (component[i] == 1){
      x[i] <- rnorm(1, 0, 1)
    } else if (component[i] == 2) {
      x[i] <- rnorm(1, 10, 1)
    } else {
      x[i] <- rnorm(1, 3, sqrt(0.1))
    }
  }
  x
}

two <- function(n){ # n: the number of values to sample
  # Set the random seed
  set.seed(1)        
  
  # Sample the component each value belongs to
  component <- sample(1:3, prob = c(0.3, 0.5, 0.2),
                      size = n, replace = TRUE)
  
  # Sample from the corresponding Normal for each value
  mu <- c(0, 10, 3)
  sd <- sqrt(c(1, 1, 0.1))
  x <- rnorm(n, mu[component], sd[component])
  x
}

n <- 10000
bm <- bench::mark(one(n), two(n))
plot(bm)

# Exercise 3 --------------------------------------------------------------

## part i

remotes::install_github("csgillespie/efficient",
                        INSTALL_opts = "--with-keep.source")

library(efficient)
library(bench)
library(profvis)

## part ii
profvis(simulate_monopoly(10000))

## part iii

run <- function(n, fun){
  x <- numeric(n)
  for (i in seq_len(n)) {
    set.seed(i)
    x[i] <- fun(1)
  }
  x
}

source("solutions/move_square2.R")
n <- 1000
bm <- bench::mark(run(n, move_square), run(n, move_square2))
plot(bm)

## too quick: no data shown
profvis(run(10, move_square))

profvis(run(n, move_square))

profvis(run(n, move_square2))
