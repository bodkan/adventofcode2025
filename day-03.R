source("utils.R")

DAY <- 3

# Read batteries in each bank as a list of integer vectors (each element of
# such vector being a digit of that given battery).
read_banks <- function(kind) {
  readLines(get_path(DAY, kind)) |>
    lapply(function(bank) as.integer(strsplit(bank, "")[[1]]))
}

# Pick n batteries from a given battery bank so that they have the largest
# n possible values (in sequence!). Part 1 effectively reduces to n = 2,
# Part 2 involves n = 12.
pick_n <- function(batteries, n) {
  # get indices of batteries according to their sorted values
  available <- order(batteries, decreasing = TRUE)

  # begin recursive descent through possible orders of batteries to find the
  # indices of the largest total sequence of their values
  picked_indices <- find_largest(length(batteries), n, available)

  # transform the sorted indices back into the respective battery values
  batteries[picked_indices]
}

# Recursively find the index of the i-th largest battery value (i.e., what will
# become the i-th digit of the total joltage of all n picked batteries)
find_largest <- function(n, i, available) {
  # if the last digit is being picked, then the largest battery value is at
  # the first index by definition (because the indices are given already sorted)
  if (i == 1) {
    return(available[1])
  } else {
    # the i-th digit of the final picked battery total value cannot be further
    # "into" the entire bank than at the index (n - i + 1), and because the
    # indices are sorted by the corresponding battery values, the largest
    # such value is again at the first index
    picked <- available[available <= n - i + 1][1]

    # descent recursively to find the following indices
    return(c(picked, find_largest(n, i - 1, available[available > picked])))
  }
}

# Convert values of individual picked batteries and compute their total joltage
compute_total <- function(batteries) {
  battery_values <- sapply(seq_along(batteries), function(i) batteries[i] * 10 ^ (length(batteries) - i))
  sum(battery_values)
}

########################################
# Part 1
########################################

########################################
# example data test

example_banks <- read_banks("example")

example_result1 <- lapply(example_banks, pick_n, n = 2) |> sapply(compute_total) |> sum()

# sanity check for later refactorings
stopifnot(example_result1 == 357)

cat("Part 1, example data:", format(example_result1, scientific = FALSE), "\n")

########################################
# full data run

full_banks <- read_banks("full")
full_result1 <- lapply(full_banks, pick_n, n = 2) |> sapply(compute_total) |> sum()

# sanity check for later refactorings
stopifnot(full_result1 == 17443)

cat("Part 1, full data:", format(full_result1, scientific = FALSE), "\n")

cat("-------------\n")

########################################
# Part 2
########################################

########################################
# example data test

example_banks <- read_banks("example")
example_result2 <- lapply(example_banks, pick_n, n = 12) |> sapply(compute_total) |> sum()

# sanity check for later refactorings
stopifnot(example_result2 == 3121910778619)

cat("Part 2, example data:", format(example_result2, scientific = FALSE), "\n")

########################################
# full data run

full_banks <- read_banks("full")
full_result2 <- lapply(full_banks, pick_n, n = 12) |> sapply(compute_total) |> sum()

# sanity check for later refactorings
stopifnot(full_result2 == 172167155440541)

cat("Part 2, full data:", format(full_result2, scientific = FALSE), "\n")

cat("-------------\n")
