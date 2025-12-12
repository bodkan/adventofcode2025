source("utils.R")

DAY <- 12

read_presents <- function(kind) {
  lines <- readLines(get_path(DAY, kind))

  lines_presents <- grep("^\\d+x\\d+:", lines, value = TRUE, invert = TRUE)
  lines_regions <- grep("^\\d+x\\d+:", lines, value = TRUE)

  present_indices <- c(grep("^\\d+:", lines_presents), length(lines_presents) + 1)
  presents <- lapply(seq_len(length(present_indices) - 1), \(i) {
    do.call(rbind, sapply(lines_presents[(present_indices[i] + 1) : (present_indices[i + 1] - 2)], \(x) {
      as.integer(strsplit(x, "")[[1]] == "#")
    }, USE.NAMES = FALSE, simplify = FALSE))
  })

  regions <- lapply(lines_regions, \(r) {
    parts <- strsplit(r, ": ")[[1]]
    dimension <- as.numeric(strsplit(parts[1], "x")[[1]])
    counts <- as.numeric(strsplit(parts[2], " ")[[1]])
    list(dimension = dimension, counts = counts)
  })

  list(presents = presents, regions = regions)
}

sum_occupied <- function(region, presents) {
  sum(sapply(seq_along(region$counts), \(i) region$counts[i] * sum(presents[[i]])))
}

########################################
# Part 1
########################################

########################################
# example data test

example_result1 <- ":)"

# sanity check for later refactorings
stopifnot(example_result1 == ":)")

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_data <- read_presents("full")

full_available <- sapply(full_data$regions, \(r) prod(r$dimension))

full_occupied1 <- sapply(full_data$regions, \(r) 9 * sum(r[["counts"]]))
full_occupied2 <- sapply(full_data$regions, sum_occupied, full_data$presents, simplify = TRUE)

full_result1 <- sum(full_occupied1 <= full_available)

# sanity check for later refactorings
stopifnot(full_result1 == 406)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

########################################
# example data test

example_result2 <- "🎄"

stopifnot(example_result2 == "🎄")

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_result2 <- "🎄"

stopifnot(full_result2 == "🎄")

cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
