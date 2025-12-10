source("utils.R")

DAY <- 10

read_machines <- function(kind) {
  machines <-
    readLines(get_path(DAY, kind)) |>
    lapply(\(x) strsplit(x, " ")[[1]]) |>
    lapply(\(x) {
      final <- strsplit(gsub("\\[|\\]", "", x[1]), "")[[1]] == "#"
      init <- rep(FALSE, length(final))
      buttons <-
        strsplit(gsub("\\(|\\)", "", grep("\\(", x, value = TRUE)), ",") |>
        lapply(\(i) { buttons <- init; buttons[as.integer(i) + 1] <- TRUE; buttons } )
      joltage <-
        strsplit(gsub("\\{|\\}", "", grep("\\{", x, value = TRUE)), ",") |>
        lapply(as.integer)
      list(init = init, final = final, buttons = buttons, joltage = joltage)
    })
  machines
}

########################################
# Part 1
########################################

switch_lights <- function(lights, buttons) {
  xor(lights, buttons)
}

key <- function(lights) paste(as.integer(lights), collapse = "")

search_lights <- function(machine) {
  init <- machine$init
  final <- machine$final
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
