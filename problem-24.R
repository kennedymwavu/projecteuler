# ----problem----
# A permutation is an ordered arrangement of objects.
# For example, 3124 is one possible permutation of the digits
# 1, 2, 3 and 4. If all of the permutations are listed numerically or
# alphabetically, we call it lexicographic order. The lexicographic
# permutations of 0, 1 and 2 are:
# 012   021   102   120   201   210

# What is the millionth lexicographic permutation of the digits
# 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?


# ----solution----
# refer to https://stackoverflow.com/a/20199902/16246909
# for this fn:
permutations <- function(n) {
  if (n == 1) {
    return(matrix(1))
  } else {
    sp <- permutations(n - 1)
    p <- nrow(sp)
    A <- matrix(nrow = n * p, ncol = n)
    for (i in 1:n) {
      A[(i - 1) * p + 1:p, ] <- cbind(i, sp + (sp >= i))
    }
    return(A)
  }
}

# Testing it:
permutations(3)
permutations(3) - 1
# That last example is what we want

digits <- permutations(10)

(digits - 1)[1e6,]
