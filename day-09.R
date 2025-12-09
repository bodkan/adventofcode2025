source("utils.R")

DAY <- 9

# Read coordinates of individual red tiles as a matrix
read_tiles <- function(kind) {
  coords <- readLines(get_path(DAY, kind)) |> lapply(\(l) as.integer(strsplit(l, ",")[[1]]))
  do.call(rbind, coords)
}

# Helper functions for filtering matrices of loop edges
filter_horizontal <- function(outline) outline[outline[, 2] == outline[, 4], ]
filter_vertical <- function(outline) outline[outline[, 1] == outline[, 3], ]

# Given the matrix of coordinates of individual red tiles, create a matrix
# of edges of the outline formed by connected red tiles
compose_outline <- function(tiles) {
  # create a matrix of the edges forming the overall outline
  outline <- do.call(rbind, lapply(1:nrow(tiles), function(i) {
    x <- tiles[i, ]
    if (i == nrow(tiles)) i <- 0
    y <- tiles[i + 1, ]
    c(x, y)
  }))

  # sort edge lines so that they all point left -> right (horizontal edges)
  # and top -> bottom (vertical edges) -- this is mostly for easier debugging
  # and eyeballing what's going on
  outline_h <- filter_horizontal(outline) |> apply(1, FUN = \(x) if(x[1] > x[3]) x[c(3, 4, 1, 2)] else x, simplify = FALSE)
  outline_v <- filter_vertical(outline) |> apply(1, FUN = \(x) if(x[2] > x[4]) x[c(3, 4, 1, 2)] else x, simplify = FALSE)

  # merge the horizontal and vertical edge matrices back together
  outline <- rbind(do.call(rbind, outline_h), do.call(rbind, outline_v))

  outline
}

########################################
# Part 1
########################################

# A custom distance function based on an area of a rectangle defined by two
# of its corner points
compute_area <- function(tiles) {
  dx <- outer(X = tiles[, 1], Y = tiles[, 1], FUN = \(x, y) abs(x - y) + 1)
  dy <- outer(X = tiles[, 2], Y = tiles[, 2], FUN = \(x, y) abs(x - y) + 1)
  dx * dy
}

########################################
# example data test

example_tiles <- read_tiles("example")
example_area <- compute_area(example_tiles)
example_result1 <- max(example_area)

# sanity check for later refactorings
stopifnot(example_result1 == 50)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_tiles <- read_tiles("full")
full_area <- compute_area(full_tiles)
full_result1 <- max(full_area)

# sanity check for later refactorings
stopifnot(full_result1 == 4750297200)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

# Given the coordinates of two opposite corners (formed by two red tiles,
# here noted A and C), compute the remaining two corners (noted B and D) whose
# coordinates are implied by A and C
compose_rectangle <- function(x) {
  A <- as.vector(x[1:2])
  C <- as.vector(x[3:4])

  # orientation #0
  # -- a single tile high/wide rectangle has B and D simply equal to C and A
  if (A[1] == C[1] || A[2] == C[2]) {
    B <- C
    D <- A
  }

  # depending on how a rectangle is orientated (which is given by the position
  # of A opposite from C), B and D have to be oriented accordingly

  # orientation #1 -- A is in the top left corner
  else if (A[1] <= C[1] && A[2] <= C[2]) {
    B <- c(C[1], A[2])
    D <- c(A[1], C[2])
  }

  # orientation #2 -- A is in the top right corner
  else if (A[1] >= C[1] && A[2] <= C[2]) {
    B <- c(A[1], C[2])
    D <- c(C[1], A[2])
  }

  # orientation #3 -- A is in the bottom right corner
  else if (A[1] >= C[1] && A[2] >= C[2]) {
    B <- c(C[1], A[2])
    D <- c(A[1], C[2])
  }

  # orientation #4 -- A is in the bottom left corner
  else if (A[1] <= C[1] && A[2] >= C[2]) {
    B <- c(A[1], C[2])
    D <- c(C[1], A[2])
  }

  c(A, C, B, D)
}

# Take the coordinates of all red tiles, generate all possible pairs of
# opposite corners (denoted A and C), and create a matrix of coordinates of
# the four corners (A, B, C, D) defining all possible rectangles
make_rectangles <- function(tiles) {
  # get numerical identifiers of each potential diagonal corner tile pairs
  ids <- t(combn(1:nrow(tiles), 2))

  # get coordinates of those tile pairs (named A and C)
  corners <- t(apply(ids, 1, \(id) c(tiles[id[1], ], tiles[id[2], ])))

  # compute the remaining two corner coordinates (B and D) of rectangles given by
  # the pair A and C, producing a matrix with coordinate columns for A, B, C, D
  recs <- t(apply(corners, 1, FUN = compose_rectangle))

  # add a column with areas of each rectangle, and sort rectangles based on this
  area <- compute_area(tiles)[ids]
  recs <- cbind(recs, area)[order(-area), ]

  # name each column and sort rectangles by area
  colnames(recs) <- c("A1", "A2", "C1", "C2", "B1", "B2", "D1", "D2", "area")

  recs
}

# Given a matrix of coordinates of a rectangle ([A1, A2] for the corner A,
# [B1, B2] for the corner B, etc.), compute the coordinates of its two
# horizontal and two vertical edges
get_sides <- function(r) {
  (sides_1 <- r[c("A1", "A2", "B1", "B2", "D1", "D2", "C1", "C2")])
  (sides_2 <- r[c("A1", "A2", "D1", "D2", "B1", "B2", "C1", "C2")])

  # decide which sides are horizontal vs vertical
  if (sides_1[2] == sides_1[4]) {
    sides_h <- sides_1
    sides_v <- sides_2
  } else {
    sides_h <- sides_2
    sides_v <- sides_1
  }

  sides_h <- matrix(sides_h, nrow = 2, byrow = TRUE)
  sides_v <- matrix(sides_v, nrow = 2, byrow = TRUE)

  # flip right -> left and bottom -> up sides
  if (sides_h[1, 1] > sides_h[1, 3]) {
    sides_h <- sides_h[, c(3, 4, 1, 2)]
  }
  if (sides_v[1, 2] > sides_v[1, 4]) {
    sides_v <- sides_v[, c(3, 4, 1, 2)]
  }

  list(h = sides_h[order(sides_h[, 2]), ], v = sides_v[order(sides_v[, 1]), ])
}

# Check that a given rectangle is valid by testing that no edge of the overall
# outline of the loop of red tiles doesn't breach inside that rectangle
is_valid <- function(r, outline) {
  # get coordinates of horizontal and vertical sides of the rectangle
  sides <- get_sides(r)

  # extract the coordinates of only horizontal or only vertical edges of
  # the overall outline loop
  outline_h <- filter_horizontal(outline)
  outline_v <- filter_vertical(outline)

  # get horizontal/vertical edges which could be potentially breaching the rectangle boundaries
  inside_h <- outline_h[sides$h[1, 2] < outline_h[, 2] & outline_h[, 2] < sides$h[2, 2], , drop = FALSE]
  inside_v <- outline_v[sides$v[1, 1] < outline_v[, 1] & outline_v[, 1] < sides$v[2, 1], , drop = FALSE]

  # check if those edges do breach the rectangle boundaries
  valid_h <- all(inside_h[, 3] <= sides$v[1, 1] | inside_h[, 1] >= sides$v[2, 1])
  valid_v <- all(inside_v[, 4] <= sides$h[1, 2] | inside_v[, 2] >= sides$h[2, 2])

  # rectangle is valid if no outline edges breach its boundaries
  valid_h && valid_v
}

# Find the largest rectangle which fits within the given outline
find_largest <- function(recs, outline) {
  i <- 1
  repeat {
    if (is_valid(recs[i, ], outline)) break
    i <- i + 1
  }
  recs[i, "area"]
}

########################################
# example data test

example_recs <- make_rectangles(example_tiles)
example_outline <- compose_outline(example_tiles)
example_result2 <- find_largest(example_recs, example_outline)

stopifnot(example_result2 == 24)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_recs <- make_rectangles(full_tiles)
full_outline <- compose_outline(full_tiles)
full_result2 <- find_largest(full_recs, full_outline)

stopifnot(full_result2 == 1578115935)

cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
