
### Title: Variable selection by Permutation_correlation ####
### Author: Patron-Rivero, C. ####
### Date: 06/22/2023 ###
### Project: 'Perm_correlation' ###

# ------------------------------------------------------------------------------------------------ #

# Select different variable sets of non-correlated variebles #

# ------------------------------------------------------------------------------------------------ #

variable_permutation <- function(num_variables, data) {

  packages <- c("devtools", "raster", "ntbox")
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }

  perm <- function(v) {
    n <- length(v)
    indices <- 1:n
    permutations <- matrix(NA, nrow = factorial(n), ncol = n)
    count <- 1
    while (count <= factorial(n)) {
      permutations[count, ] <- v
      count <- count + 1
      j <- n - 1
      while (j >= 1 && indices[j] >= indices[j + 1]) {
        j <- j - 1
      }
      if (j < 1) {
        break
      }
      k <- n
      while (indices[j] >= indices[k]) {
        k <- k - 1
      }
      temp <- indices[j]
      indices[j] <- indices[k]
      indices[k] <- temp
      indices[(j + 1):n] <- rev(indices[(j + 1):n])
      v <- v[indices]
    }
    permutations
  }

  P_wo <- perm(1:num_variables)
  sets <- vector("list", nrow(P_wo))
  for (i in 1:nrow(P_wo)) {
    a <- P_wo[i, ]
    data2 <- data[, a]
    env <- ntbox::correlation_finder(cor(data2), threshold = 0.8, verbose = FALSE)
    b <- env$descriptors
    b <- sort(b)
    if (length(b) < num_variables) {
      b <- c(b, rep(NA, num_variables - length(b)))
    }
    sets[[i]] <- b
  }

  df <- data.frame(do.call(rbind, sets))
  final <- unique(df)
  return(final)
}

# ------------------------------------------------------------------------------------------------ #

### EndNotRun
