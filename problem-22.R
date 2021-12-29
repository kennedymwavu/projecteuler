# ----Problem----
# Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
# containing over five-thousand first names, begin by sorting it into
# alphabetical order. Then working out the alphabetical value for each name,
# multiply this value by its alphabetical position in the list to obtain a
# name score.
#
# For example, when the list is sorted into alphabetical order, COLIN, which is
# worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
# would obtain a score of 938 × 53 = 49714.
#
# What is the total of all the name scores in the file?


# ----Solution----
library(purrr)

# Read in the .txt, sort the names and convert to lower case:
txt_names <- scan(
  file = "p022_names.txt",
  what = character(),
  sep = ",",
  na.strings = ""
) |>
  sort() |>
  tolower()

# I specified na.strings arg coz one of the names is "NA" which
# confuses R. Yes I spent so much time wondering why my answer was wrong!!!
# Decided to leave this comment here to help someone out there.

# look up table for alphabetical number of each letter:
alphabet_numbers <- seq_along(letters) |> setNames(nm = letters)

# fn to return word worth:
word_worth <- function(word) {
  # first split it:
  splt <- strsplit(word, split = "")[[1]]

  # get each letter's value from the look up table and sum the result:
  alphabet_numbers[splt] |> sum()
}

# See if it works:
word_worth("colin")

# map that over all values of txt_names:
txt_worth <- purrr::map_dbl(.x = txt_names, .f = word_worth)

# Since we had already sorted the names, the alphabetical positions of the names
# are their indexes.
# Score of each word:
scores <- seq_along(txt_worth) * txt_worth

# Total scores:
sum(scores)
