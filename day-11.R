source("utils.R")

DAY <- 11

read_devices <- function(x, file) {
  if (file == TRUE)
    lines <- readLines(get_path(DAY, x))
  else
    lines <- strsplit(x, "\n")[[1]]

  # get a list of pairs (from, to) representing edges in a graph
  pairs <-
    lines |>
    lapply(\(x) strsplit(x, ": ")[[1]]) |>
    lapply(\(pair) {
      from <- pair[1]
      tos <- strsplit(pair[2], " ")[[1]]
      lapply(tos, \(to) list(from = from, to = to))
    }) |>
    unlist(recursive = FALSE)

  # convert the list into a plain character two-column matrix
  m <- do.call(rbind, pairs)
  mode(m) <- "character"

  m
}

########################################
# Part 1
########################################

# Find all paths between start and end nodes in a graph represented by
# a given matrix of all possible edges
count_paths <- function(start, end, pairs) {
  # cache for paths following already explored nodes (environments in R
  # are the only object type which is modified in place!)
  cache <- new.env()
  cache$nodes <- list()

  recursive_search(start, end, pairs, cache)
}

# Recursively find a path from start to end
recursive_search <- function(start, end, pairs, cache) {
  if (start == end) {
    # return from the final destination if it's been reached
    return(1)
  } else if (!is.null(cache$nodes[[start]])) {
    # if we visited this node before, no need to recurse from it further,
    # just get the count of paths from it discovered previously
    return(cache$nodes[[start]])
  } else {
    # sum up possible paths from the descendants of the current node
    count <- 0
    neighbors <- as.vector(pairs[pairs[, "from"] == start, "to"])
    for (n in neighbors)
      count <- count + recursive_search(n, end, pairs, cache)
    # add the count to the cache in case we reach it again
    cache$nodes[[start]] <- count
    return(count)
  }
}

########################################
# example data test

example_pairs <- read_devices("example", file = TRUE)
example_result1 <- count_paths(example_pairs, start = "you", end = "out")

# sanity check for later refactorings
stopifnot(example_result1 == 5)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_pairs <- read_devices("full", file = TRUE)
full_result1  <- count_paths(full_pairs, start = "you", end = "out")

# sanity check for later refactorings
stopifnot(full_result1 == 733)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

# Reuse the straightforward DFS solution for Part 1 to solve Part 2
# by decomposing the problem into a sequence of possible paths, either
# through fft -> dac, or through dac -> fft
count_linked <- function(pairs) {
  (
    count_paths(start = "svr", end = "fft", pairs) *
    count_paths(start = "fft", end = "dac", pairs) *
    count_paths(start = "dac", end = "out", pairs)
  ) +
  (
    count_paths(start = "svr", end = "dac", pairs) *
    count_paths(start = "dac", end = "fft", pairs) *
    count_paths(start = "fft", end = "out", pairs)
  )
}

########################################
# example data test

example_pairs2 <- read_devices("svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out", file = FALSE)

example_result2 <- count_linked(example_pairs2)

stopifnot(example_result2 == 2)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

# plot_graph(full_pairs, 10)
full_result2 <- count_linked(full_pairs)

stopifnot(full_result2 == 290219757077250)

options(scipen = 999)
cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
