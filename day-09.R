source("utils.R")

DAY <- 9

read_tiles <- function(kind) {
  coords <- readLines(get_path(DAY, kind)) |> lapply(\(l) as.integer(strsplit(l, ",")[[1]]))
  do.call(rbind, coords)
}

########################################
# Part 1
########################################

# A custom distance function based on an area of a rectangle defined by two
# of its corner points
area_dist <- function(tiles) {
  dx <- outer(X = tiles[, 1], Y = tiles[, 1], FUN = \(x, y) abs(x - y) + 1)
  dy <- outer(X = tiles[, 2], Y = tiles[, 2], FUN = \(x, y) abs(x - y) + 1)
  dx * dy
}

########################################
# example data test

example_tiles <- read_tiles("example")
example_dist <- area_dist(example_tiles)
example_result1 <- max(example_dist)

# sanity check for later refactorings
stopifnot(example_result1 == 50)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_tiles <- read_tiles("full")
full_dist <- area_dist(full_tiles)
full_result1 <- max(full_dist)

# sanity check for later refactorings
# stopifnot(full_result1 == )

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

########################################
# example data test

example_result2 <- NA

# stopifnot(example_result2 == )

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_result2 <- NA

# stopifnot(full_result2 == )

cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
