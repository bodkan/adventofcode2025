source("utils.R")

DAY <- 8

read_boxes <- function(kind) {
  coords <- readLines(get_path(DAY, kind)) |> lapply(\(l) as.integer(strsplit(l, ",")[[1]]))
  m <- do.call(rbind, coords)
  rownames(m) <- 1:nrow(m)
  m
}

########################################
# Part 1
########################################

# Compute pairwise distances as an upper-triangular matrix
compute_distances <- function(boxes) {
  d <- as.matrix(dist(boxes))
  d[lower.tri(d, diag = TRUE)] <- Inf
  d
}

# Find indices of the two closest junction boxes
find_closest <- function(d) {
  which(d == min(d), arr.ind = TRUE)
}

# Initialize clusters representing circuits of junction boxes to each
# box being its own circuits at the beginning -- clusters are represented
# by a hash map indexed by a string identifier of that circuit
init_clusters <- function(boxes) {
  clusters <- as.list(seq_len(nrow(boxes)))
  names(clusters) <- seq_along(clusters)
  clusters
}

# Join a given pair of circuits into a single circuit
join_pair <- function(pair, clusters) {
  # extract hash keys of both elements of the pair
  key1 <- Filter(\(x) length(intersect(clusters[[x]], pair[1])), names(clusters))
  key2 <- Filter(\(x) length(intersect(clusters[[x]], pair[2])), names(clusters))

  if (key1 != key2) {
    # extract the box numbers from the hash map, removing both entries
    elems1 <- clusters[[key1]]; clusters[[key1]] <- NULL
    elems2 <- clusters[[key2]]; clusters[[key2]] <- NULL

    # add a new hash map element with the union of both (the key being composed
    # by numbers of all boxes joined by '_')
    elems <- sort(c(elems1, elems2))
    key <- paste(elems, collapse = "_")
    clusters[[key]] <- elems
  }

  clusters
}

# Perform n joining operations
connect_n <- function(boxes, n) {
  distances <- compute_distances(boxes)
  clusters <- init_clusters(boxes)

  for (i in seq_len(n)) {
    cat(i, "\r")

    pair <- find_closest(distances)

    # remove the pair from further processing
    distances[pair] <- Inf

    clusters <- join_pair(pair, clusters)
  }

  clusters
}

# Compute Part 1 result
three_largest <- function(clusters) {
  prod(sort(sapply(clusters, length), decreasing = TRUE)[1:3])
}

########################################
# example data test

example_boxes <- read_boxes("example")
example_clusters1 <- connect_n(example_boxes, n = 10)
example_result1 <- three_largest(example_clusters1)

# sanity check for later refactorings
stopifnot(example_result1 == 40)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_boxes <- read_boxes("full")
full_clusters1 <- connect_n(full_boxes, n = 1000)
full_result1 <- three_largest(full_clusters1)

# sanity check for later refactorings
stopifnot(full_result1 == 81536)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

# Join all circuits into a single circuit, returning the coordinates of
# the last to junction boxes joined
connect_all <- function(boxes) {
  distances <- compute_distances(boxes)
  clusters <- init_clusters(boxes)

  i <- 1
  repeat {
    cat(i, "\r")

    pair <- find_closest(distances)

    # remove the pair from further processing
    distances[pair] <- Inf

    clusters <- join_pair(pair, clusters)

    if (length(clusters) == 1)
      break

    i <- i + 1
  }

  boxes[pair, ]
}

########################################
# example data test

example_last <- connect_all(example_boxes)
example_result2 <- prod(example_last[, 1])

# sanity check for later refactorings
stopifnot(example_result2 == 25272)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_last <- connect_all(full_boxes)
full_result2 <- prod(full_last[, 1])

# sanity check for later refactorings
stopifnot(full_result2 == 7017750530)

cat("Part 2, full data:", full_result2, "\n")
