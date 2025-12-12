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
# a given matrix of all posible edges
find_paths <- function(start, end, pairs) {
  # cache for paths following already explored nodes
  cache <- new.env()
  cache$cache <- list()

  paths <- recursive_search(start, end, pairs, cache)
  result <- flatten_paths(c(), paths)

  result
}

# Recursively find a path from start to end
recursive_search <- function(start, end, pairs, cache) {
  if (start == end) {
    # cat("Found path!\n")
    return(start)
  }
  else if (!is.null(cache$cache[[start]])) {
    # cat("Getting node", node, "from the cache\n")
    return(cache$cache[[start]])
  }
  else{
    results <- list()
    neighbors <- as.vector(pairs[pairs[, "from"] == start, "to"])
    for (n in neighbors) {
      results[[length(results) + 1]] <- c(start, recursive_search(n, end, pairs, cache))
    }
    cache$cache[[start]] <- results
    return(results)
  }
}

# Because the discovered paths are represented by nested lists due to the
# recursive nature of the search algorithm, flatten them at the end
flatten_paths <- function(path, nested) {
  if (length(nested) == 0) {
    return(list(path))
  }
  results <- list()
  for (subpath in nested) {
    results <- append(results, flatten_paths(c(path, subpath[[1]]), subpath[-1]))
  }
  return(results)
}

########################################
# example data test

example_pairs <- read_devices("example", file = TRUE)
example_paths <- find_paths(example_pairs, start = "you", end = "out")
example_result1 <- length(example_paths)

# sanity check for later refactorings
stopifnot(example_result1 == 5)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_pairs <- read_devices("full", file = TRUE)
full_paths <- find_paths(full_pairs, start = "you", end = "out")
full_result1  <- length(full_paths)

# sanity check for later refactorings
stopifnot(full_result1 == 733)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

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

# plot_graph(example_pairs2)

example_paths2 <- find_paths(start = "svr", end = "out", example_pairs2)
example_result2 <- length(Filter(\(path) "dac" %in% path && "fft" %in% path, example_paths2))

stopifnot(example_result2 == 2)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

# plot_graph(full_pairs, 10)
# full_paths <- collect_paths(start = "svr", end = "out", full_pairs)
# full_result2 <- length(Filter(\(path) "dac" %in% path && "fft" %in% path, full_paths))

# # stopifnot(full_result2 == )
#
# cat("Part 2, full data:", full_result2, "\n")
#
# cat("-------------\n")
