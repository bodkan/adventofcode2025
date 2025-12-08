############################################################
# !!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!
#
# I wrote this on a tiny phone screen using a barebones
# web-based R REPL during a family trip. Apologies to
# the poor souls reading this code.
#
# !!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!
############################################################

map = do.call(rbind, lapply(readLines("data/full-04.txt"), function(x) strsplit(x, "")[[1]])) == "@"

find_neighbors = function(ij, map) {
  n = nrow(map); m = ncol(map)
  i = ij[1]; j = ij[2]

  res = list()

  if (i > 1 && map[i - 1, j]) res[[length(res) + 1]] = c(i - 1, j)
  if (i < n && map[i + 1, j]) res[[length(res) + 1]] = c(i + 1, j)
  if (j > 1 && map[i, j - 1]) res[[length(res) + 1]] = c(i, j - 1)
  if (j < m && map[i, j + 1]) res[[length(res) + 1]] = c(i, j + 1)

  if (i > 1 && j > 1 && map[i - 1, j - 1]) res[[length(res) + 1]] = c(i - 1, j - 1)
  if (i < n && j < m && map[i + 1, j + 1]) res[[length(res) + 1]] = c(i + 1, j + 1)
  if (i < n && j > 1 && map[i + 1, j - 1]) res[[length(res) + 1]] = c(i + 1, j - 1)
  if (i > 1 && j < m && map[i - 1, j + 1]) res[[length(res) + 1]] = c(i - 1, j + 1)

  res
}

############################
# Part 1

rolls = which(map, arr.ind = T)
neighbors = apply(rolls, 1, find_neighbors, map)
cat("Part 1, full data:", sum(sapply(neighbors, length) < 4), "\n")

############################
# Part 2

remove <- function(map) {
  rolls = which(map, arr.ind = T)
  accessible = sapply(apply(rolls, 1, find_neighbors, map), length) < 4
  to_remove = sum(accessible)

  if (!to_remove) {
    return(0)
  } else {
    map[rolls[accessible, , drop = F]] = F
    return(to_remove + remove(map))
  }
}

cat("Part 2, full data:", remove(map), "\n")
