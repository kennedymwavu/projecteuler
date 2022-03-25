library(RcppAlgos)
library(tidyverse)

# remove the number from its divisors:
divisors <- function(x) {
  divisors_list <- divisorsRcpp(x)

  if (length(x) == 1) {
    return(list(divisors_list[-length(divisors_list)]))
  }

  purrr::map(
    .x = divisors_list,
    .f = ~ .x[-length(.x)]
  )
}
divisors(12)
divisors(c(12, 24))

# check if x is abundant:
is_abundant <- function(x) {
  divisors_list <- divisors(x)

  divisors_sum <- purrr::map_int(
    .x = divisors_list,
    .f = sum
  )

  divisors_sum > x
}

is_abundant(c(12, 24))
is_abundant(1:24)

x <- 1:28123
abundants <- x[is_abundant(x)]

y <- expand.grid(abundants, abundants)
colnames(y) <- c("a", "b")

# sum abundants together:
y$sum_ab <- with(y, a + b)

# filter the sums less than 28123 and get the unique ones:
y <- y |>
  dplyr::filter(sum_ab <= 28123) |>
  dplyr::distinct(sum_ab, .keep_all = TRUE)

# sum of all the positive integers which cannot be written as the sum of two
# abundant numbers:
sum(1:28123) - sum(y$sum_ab)
