source("utils.R")

DAY <- 10

read_machines <- function(kind) {
  machines <-
    readLines(get_path(DAY, kind)) |>
    lapply(\(x) strsplit(x, " ")[[1]]) |>
    lapply(\(x) {
      final_lights <- strsplit(gsub("\\[|\\]", "", x[1]), "")[[1]] == "#"
      init_lights <- rep(FALSE, length(final_lights))

      final_joltage <- as.integer(strsplit(gsub("\\{|\\}", "", grep("\\{", x, value = TRUE)), ",")[[1]])
      init_joltage <- rep(0, length(final_lights))

      buttons <-
        strsplit(gsub("\\(|\\)", "", grep("\\(", x, value = TRUE)), ",") |>
        lapply(\(i) { buttons <- init_lights; buttons[as.integer(i) + 1] <- TRUE; buttons } )

      list(
        init_lights = init_lights,
        final_lights = final_lights,
        init_joltage = init_joltage,
        final_joltage = final_joltage,
        buttons = buttons
      )
    })
  machines
}

key <- function(lights) paste(as.integer(lights), collapse = "")

########################################
# Part 1
########################################

switch_lights <- function(lights, buttons) {
  xor(lights, buttons)
}

search_lights <- function(machine) {
  init <- machine$init_lights
  final <- machine$final_lights
  buttons <- machine$buttons

  # initialize queue with the starting lights configuration of the current machine
  queue <- list(list(n = 0, lights = init))
  # tracker of visited configurations
  visited <- list()

  repeat {
    # extract the first item of the queue
    item <- queue[[1]]; queue[[1]] <- NULL
    n <- item$n
    lights <- item$lights

    for (b in buttons) {
      # create a new lights configuration
      next_lights <- switch_lights(lights, b)

      # return the count of presses needed to reach the final configuration
      if (all(next_lights == final)) {
        return(n = n + 1)
      }

      # if the new configuration of lights hasn't been explored yet, add it
      # to the queue for further exploration
      if (!key(next_lights) %in% visited) {
        visited <- append(visited, key(next_lights))
        queue <- append(queue, list(list(n = n + 1, lights = next_lights)))
      }
    }
  }
}

########################################
# example data test

example_machines <- read_machines("example")
example_result1 <- sum(sapply(example_machines, search_lights))

stopifnot(example_result1 == 7)

cat("Part 1, example data:", example_result1, "\n")

########################################
# full data run

full_machines <- read_machines("full")

full_result1 <- sum(sapply(full_machines, search_lights))

# sanity check for later refactorings
stopifnot(full_result1 == 432)

cat("Part 1, full data:", full_result1, "\n")

cat("-------------\n")

########################################
# Part 2
########################################

library(lpSolveAPI)

# Extract button joltage increases and final joltage levels as an integer matrix
# and vector, respectively, then turn them into a linear programming optimization problem
# Reference: https://civil.colorado.edu/~balajir/CVEN5393/R-sessions/sess1/lpSolveAPI-vignettes.pdf
build_linprog <- function(m) {
  # extract matrix of constraints
  A <- t(do.call(rbind, m$buttons))
  mode(A) <- "integer"
  # extract
  b <- m$final_joltage

  n <- ncol(A)
  m <- nrow(A)

  # create a linear programming model object with m constraints (rows of the
  # A matrix, i.e. m constraint equations) and n decision variables (columns
  # of the A matrix, i.e. number of parameters to estimate)
  lmod <- make.lp(m, n)
  # lp.control(lmod, sense = "min")

  # force the model to be an integer linear program
  set.type(lmod, columns = 1:n, type = "integer")

  # set the right hand side to be the final voltage values
  set.rhs(lmod, b)
  # the target constraint is to reach the final voltage levels exactly
  set.constr.type(lmod, rep("=", m))


  # add constraints as columns of the matrix A (each column representing
  # coefficients of )
  for (i in 1:ncol(A)) {
    set.column(lmod, i, A[, i])
  }

  # set the coefficients of the objective function to be unit values
  set.objfn(lmod, rep(1, n))

  lmod
}

# Solve the linear programming model, returning the minimum number of
# button presses needed to reach the final joltage levels
solve_linprog <- function(model) {
  solve(model)
  button_presses <- get.objective(model)
  rm(model)
  button_presses
}

########################################
# example data test

example_result2 <- sum(sapply(example_machines, \(m) build_linprog(m) |> solve_linprog()))

stopifnot(example_result2 == 33)

cat("Part 2, example data:", example_result2, "\n")

########################################
# full data run

full_result2 <- sum(sapply(full_machines, \(m) build_linprog(m) |> solve_linprog()))

stopifnot(full_result2 == 18011)

cat("Part 2, full data:", full_result2, "\n")

cat("-------------\n")
