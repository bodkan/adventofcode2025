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

find_paths <- function(pairs, start, end) {
  solutions <- list()

  queue <- list(c(start))

  while (length(queue) > 0) {
    cat("Queue length:", length(queue), "\r")
    # take the next path out of the queue
    path <- queue[[1]]; queue[[1]] <- NULL

    # get the last node of that path (i.e., the current node)
    node <- path[[length(path)]]

    # get all neighbors of the current node
    neighbors <- unlist(pairs[pairs[, "from"] == node, "to", drop = FALSE])

    for (n in neighbors) {
      # add the neighbor to the path
      p <- append(path, n)

      # if we reached the final destination, add the path to the solutions
      if (n == end) {
        solutions <- append(solutions, list(p))
        next
      } else {
        # otherwise keep exploring (but only if we wouldn't visit an
        # already visited node, i.e. forming a loop)
        if (!n %in% path)
          queue <- append(queue, list(p))
      }
    }
  }

  solutions
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
