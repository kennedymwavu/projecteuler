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
library(tidyverse)
library(numbers)

# Function to calculate the sum of divisors of x:
div_sums <- function(x) {
  # Since numbers::divisors() returns all divisors of x and x itself, we use
  # head(n = -1) to omit the x then sum the result:
  f <- \(x) numbers::divisors(x) |> head(-1) |> sum()

  # map f() over all elements of x:
  x |> purrr::map_dbl(.f = f)
}

# check if that works:
div_sums(1:1000)

# So far so good!

# Create a df with columns:
# a: 1:10000
# b: div_sums(a)
# c: div_sums(b)
DF <- tibble(
  a = 1:10000,
  b = div_sums(a)
) |>
  filter(b > 0) |>  # get non-negative values of b
  mutate(c = div_sums(b))

# We want the rows where a is equal to div_sums of b ie. c but a != b:
DF |> filter(a == c & a != b) |> pull(c) |> sum()
