source("utils.R")

DAY <- 1

DIAL_SIZE <- 100
START_STATE <- 50

# Read rotation instructions as a single integer vector:
#
#   - Left rotations: negative integers
#   - Right rotations: positive integers
#
# As an example, a series of rotation instructions such as [L30, R20, L42, R1]
# would be parsed as a numerical vector [-30, 20, -42, 1].
#
# This representation encodes the full information about transitions
# that happen in the modulo 100 "state space".
read_rotations <- function(kind) {
  rotations <-
    readLines(get_path(DAY, kind)) |>
    sapply(function(line) {
      direction <- gsub("^(.).*$", "\\1", line)
      clicks <- as.integer(gsub("^.(\\d+)$", "\\1", line))
      if (direction == "L") clicks <- -clicks
      clicks
    })

  rotations
}

########################################
# Part 1
########################################

# From a given state of the dial, we get to the next state by turning it
# by a given number of clicks (sign indicating direction) modulo 100
rotate_one <- function(state, clicks) {
  (state + clicks) %% DIAL_SIZE
}

# A series of states after each rotation instruction starting from some state
# is then a simple reduce operation, starting from that given state
rotate_all <- function(start, rotations) {
  Reduce(f = rotate_one, x = rotations, init = start, accumulate = TRUE)
}

########################################
# example data test

example_rotations <- read_rotations("example")
example_states <- rotate_all(start = START_STATE, rotations = example_rotations)

example_result1 <- sum(example_states == 0)

# sanity check for later refactorings
stopifnot(example_result1 == 3)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_rotations <- read_rotations("full")
full_states <- rotate_all(start = START_STATE, rotations = full_rotations)

full_result1 <- sum(full_states == 0)

# sanity check for later refactorings
stopifnot(full_result1 == 1102)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

# Instead of just detecting arrivals to 0 state after an entire rotation
# as in Part 1, track how many times each rotation passes over 0 (either
# passing it on the way elsewhere, or ending a rotation at 0).
rotate_one <- function(state, clicks) {
  # get the current dial state
  dial <- state$dial

  # convert left rotation into a right rotation by virtue of symmetry
  # in the addition modulo group by "projecting" the dial state to its
  # symmetrical counterpart, i.e. the following two equations are effectively
  # the same:
  #     1. a       - b mod n
  #     2. (n - a) + b mod n
  if (clicks < 0) {
    dial <- (DIAL_SIZE - dial) %% DIAL_SIZE
  }

  # having converted left rotations into right rotations, we can count
  # the distance traveled by the clicks of the current rotation
  new_distance <- dial + abs(clicks)
  # get the new dial state by taking the modulo (in case we "passed through 0")
  new_dial <- new_distance %% DIAL_SIZE

  # again, in case we converted left rotation into right rotation, get the
  # state in the original symmetry so that the rotation that comes after
  # this one starts from the correct state
  if (clicks < 0) {
    new_dial <- (DIAL_SIZE - new_dial) %% DIAL_SIZE
  }

  # next rotation will start from the new dial state, distances are being
  # accumulated for computing the final number of passes through 0 at the end
  list(dial = new_dial, distance = new_distance)
}

# Execute successive rotation operations from a starting state, but this time
# accumulate the distance travelled by each rotation given by the number of
# clicks. Any time this distance is greater than the span of the dial means
# that a 0 must have been crossed (potentially multiple times).
rotate_all <- function(start, rotations) {
  Reduce(f = rotate_one, x = rotations, init = list(dial = start, distance = 0), accumulate = TRUE)
}

########################################
# example data test

example_states <- rotate_all(start = START_STATE, rotations = example_rotations)
example_crosses <- sapply(example_states, `[[`, "distance") %/% DIAL_SIZE

example_result2 <- sum(example_crosses)

# sanity check for later refactorings
stopifnot(example_result2 == 6)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_states <- rotate_all(start = START_STATE, rotations = full_rotations)
full_crosses <- sapply(full_states, `[[`, "distance") %/% DIAL_SIZE

full_result2 <- sum(full_crosses)

# sanity check for later refactorings
stopifnot(full_result2 == 6175)

cat("Part 2, full data:", full_result2, "\n")
