source("utils.R")

DAY <- 2

########################################
# Part 1
########################################

# Read product ID ranges as a list of two-element numerical vectors
# (each element representing the start and end of the given product range)
read_products <- function(kind) {
  lines <- get_path(DAY, kind) |> readLines()
  ranges <- strsplit(lines[[1]], ",")
  lapply(strsplit(ranges[[1]], "-"), as.numeric)
}

# In part 1, product ID being a number with an even number of digits is
# a necessary (but not sufficient) for that ID being potentially invalid.
# First process ranges which do not have an even number of digits by shifting
# them up (start) or down (end) to new boundaries where needed.
process_range <- function(range) {
  # count digits of both current range boundaries
  s_digits <- ceiling(log10(range[1]))
  e_digits <- ceiling(log10(range[2]))

  # find the next valid start and end so that they have an even number of digits
  start <- if (s_digits %% 2 == 0) range[1] else 10^s_digits
  end <- if (e_digits %% 2 == 0) range[2] else 10^(e_digits - 1) - 1

  # if both start and end have an odd number of digits and that number of
  # digits is the same, that range cannot contain an invalid ID
  if (end <= start) return(NULL)

  c(start, end)
}

# Split a given product ID number into two halves
split_halves <- function(x) {
  str <- as.character(x)
  n <- nchar(str)
  halves <- c(substr(str, 1, n / 2), substr(str, n / 2 + 1, n))
  halves
}

# A product ID number is invalid, if its first and second half are the same
invalidity_check1 <- function(x) {
  halves <- split_halves(x)
  halves[1] == halves[2]
}

# Loop over all product IDs within all ranges in the data set and detect those
# which are invalid
find_invalid1 <- function(ranges) {
  parallel::mclapply(ranges, function(range) {
    start <- range[1]
    end <- range[2]
    Filter(invalidity_check1, seq(start, end))
  }, mc.cores = parallel::detectCores()) |> unlist()
}

########################################
# example data test

example_ranges <- read_products("example") |> lapply(process_range) |> (\(x) Filter(Negate(is.null), x))()
example_invalid1 <- find_invalid1(example_ranges)

example_result1 <- sum(example_invalid1)

# sanity check for later refactorings
stopifnot(example_result1 == 1227775554)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_ranges <- read_products("full")
full_invalid1 <- find_invalid1(full_ranges)

full_result1 <- sum(full_invalid1)

# sanity check for later refactorings
stopifnot(full_result1 == 40214376723)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

# Generate all possible factors of a given number L
factorize_number <- function(L) {
  candidates <- 2:L
  candidates[L %% candidates == 0]
}

# Pre-compute lookup map of possible substring coordinates for all possible numbers
# of digits across all IDs in the data set
generate_lookup <- function(id_ranges) {
  # get the longest product ID number in terms of then number of its digits
  # and the range of digit numbers that can be encountered in the given
  # product ID range data
  max_digits <- ceiling(log10(max(unlist(id_ranges))))
  possible_digits <- 2:max_digits

  substring_lookup <- lapply(possible_digits, function(L) {
    # create factors of the ID of the given length (indicating number of possible substrings)
    factors <- factorize_number(L)
    # create coordinates of those substrings for an ID of the given length
    substring_coords <-
      lapply(factors, function(f) seq(1, L + 1, by = L / f)) |>
      lapply(function(coords) {
        lapply(1:(length(coords) - 1), function(i) c(coords[i], coords[i + 1] - 1))
      })
  })
  names(substring_lookup) <- possible_digits

  substring_lookup
}

# Extract all possible substrings given the set of index coordinates
extract_substrings <- function(str, substring_coords) {
  lapply(substring_coords, function(coords) {
    lapply(coords, function(x) substr(str, start = x[1], stop = x[2]))
  })
}

# Split number into all possible substrings of its digits
split_parts <- function(x, substring_lookup) {
  str <- as.character(x)
  ndigits <- nchar(str)

  # get coordinates of all possible substrings of the given digit string (based
  # on the number of its digits) and extract them from the string of digits
  substring_coords <- substring_lookup[[as.character(ndigits)]]
  substrings <- extract_substrings(str, substring_coords)

  substrings
}

# In part 2, a given product ID number is invalid if it can be partitioned
# into any set of substrings of the same length which are all the same
invalidity_check2 <- function(x, substring_lookup) {
  # split the given number into all possible substrings
  partitions <- split_parts(x, substring_lookup)
  # check if any substring partitions are composed of identical substrings
  identical_substrings <- sapply(partitions, function(substrings) length(unique(substrings)) == 1)
  # an ID number is invalid if any substrings within a partitioning are the same
  any(identical_substrings)
}

# Loop over all product IDs within all ranges in the data set and detect those
# which are invalid
find_invalid2 <- function(ranges, substring_lookup) {
  parallel::mclapply(ranges, function(range) {
    sequence <- seq(range[1], range[2])
    Filter(function(id) { invalidity_check2(id, substring_lookup) }, sequence)
  }, mc.cores = parallel::detectCores()) |> unlist()
}

########################################
# example data test

example_ranges <- read_products("example")
example_lookup <- generate_lookup(example_ranges)
example_invalid2 <- find_invalid2(example_ranges, example_lookup)

example_result2 <- sum(example_invalid2)

# sanity check for later refactorings
stopifnot(example_result2 == 4174379265)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_ranges <- read_products("full")
full_lookup2 <- generate_lookup(full_ranges)
full_invalid2 <- find_invalid2(full_ranges, full_lookup2)

full_result2 <- sum(full_invalid2)

# sanity check for later refactorings
stopifnot(full_result2 == 50793864718)

cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
