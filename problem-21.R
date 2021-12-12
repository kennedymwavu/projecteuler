# ----Problem----
# Let d(n) be defined as the sum of proper divisors of n (numbers less than n
# which divide evenly into n).
# If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
# each of a and b are called amicable numbers.
#
# For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
# and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
# 142; so d(284) = 220.
#
# Evaluate the sum of all the amicable numbers under 10000.

# ----Solution----
library(numbers)
library(purrr)

# Function to calculate the sum of proper divisors:
d_f <- function(x) {
  xx <- divisors(x)
  xx[-length(xx)] |> sum()
}

# vectorise that:
d_f_v <- function(x) {
  x |> map_dbl(.f = d_f)
}

d_f_v(220)
d_f(220)

# all numbers from 1 to 10000:
x <- 1:10000

# Get d(x):
d_x <- d_f_v(x)

# initialize amicables to 0:
amicables <- numeric(0)

for (a in 1:10000) {
  d_a <- d_x[a]
  for (b in 1:10000) {
    if (a != b) {
      d_b <- d_x[b]

      if (b == d_a && a == d_b) {
        n <- length(amicables)
        amicables[(n + 1):(n + 2)] <- c(a, b)
      }
    }
  }
}

sum(amicables) / 2
