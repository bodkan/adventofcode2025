source("utils.R")

DAY <- 7

# Read the manifold configuration as a logical vector indicating the initial
# position of the beam and the logical vector "map" of the position of each
# beam splitter
read_manifold <- function(kind) {
  lines <- lapply(readLines(get_path(DAY, kind)), \(l) strsplit(l, "")[[1]])

  list(beams = lines[[1]] == "S", splitters = lapply(lines[-1], \(l) l == "^"))
}

options(scipen = 999)

########################################
# Part 1
########################################

########################################
# example data test

# Count the number of beam splits by tracking them along each layer of the
# splitter manifold, returning the total split count together with the presence
# (or absence) of all beams and splitters during each time step
track_beams <- function(beams, splitters) {
  # if there are no more splitters, we've reached the recursion bottom
  if (!length(splitters))
    return(list(count = 0, beams = list(beams), hits = list(rep(FALSE, length(beams)))))

  # get positions of each splitter hit by a beam
  hits <- beams & splitters[[1]]
  # once a beam hits a splitter, it stops existing on its own
  beams[hits] <- FALSE

  # each hit splitter creates two beams, one on its left, another on its right
  if (any(hits)) {
    splits <- unique(c(which(hits) - 1, which(hits) + 1))
    splits <- splits[splits >= 1 & splits <= length(beams) & !splits %in% which(beams)]
    beams[splits] <- TRUE
  }

  # track beams recursively through the next layer of splitters
  res <- track_beams(beams, splitters[-1])

  return(list(
    count = sum(hits) + res$count,
    beams = append(res$beams, list(beams)),
    hits = append(res$hits, list(hits))
  ))
}

example_manifold <- read_manifold("example")
example_result1 <- track_beams(example_manifold$beams, example_manifold$splitters)

# sanity check for later refactorings
stopifnot(example_result1$count == 21)

cat("Part 1, example data:", example_result1$count, "\n")

########################################
# full data run

full_manifold <- read_manifold("full")
full_result1 <- track_beams(full_manifold$beams, full_manifold$splitters)

# sanity check for later refactorings
stopifnot(full_result1$count == 1581)

cat("Part 1, full data:", full_result1$count, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

# Similarly to delving "forward" through time in Part 1, now retrace the beams
# back through each splitter, counting the timelines experienced by each beam
# "backwards" through time using dynamic programming
count_timelines <- function(beams, hits, counts) {
  # the last layer of splitter hits (looking backwards through time) is the
  # very first single splitter -- its assigned count is the number of all
  # possible futures
  if (length(hits) == 1)
    return(counts[which(beams[[1]])])

  # go through each splitter hit in this layer and sum up the count of futures
  # encountered by the beams on its left and right
  counts <- sapply(seq_along(hits[[1]]), \(i) {
    if (hits[[1]][i]) {
      futures <- c(i - 1, i + 1)
      futures <- futures[futures >= 1 & futures <= length(hits[[1]])]
      sum(counts[futures])
    } else {
      # if a beam didn't hit a splitter, its status looking backwards in time
      # is the same as of the beam in the layer above
      counts[i] * beams[[2]][i]
    }
  })

  count_timelines(beams[-1], hits[-1], counts)
}

########################################
# example data test

beams <- example_result1$beams
hits <- example_result1$hits
counts <- as.integer(beams[[1]])
example_result2 <- count_timelines(beams, hits, counts)

# sanity check for later refactorings
stopifnot(example_result2 == 40)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

beams <- full_result1$beams
hits <- full_result1$hits
counts <- as.integer(beams[[1]])
full_result2 <- count_timelines(beams, hits, counts)

# sanity check for later refactorings
stopifnot(full_result2 == 73007003089792)

cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
