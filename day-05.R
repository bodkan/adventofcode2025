############################################################
# !!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!
#
# I wrote this on a tiny phone screen using a barebones
# web-based R REPL during a family trip. Apologies to
# the poor souls reading this code.
#
# !!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!
############################################################

lines = readLines("data/full-05.txt")

br = which(lines == "")

ranges = lapply(lines[1:(br-1)], function(r) strsplit(r, "-")[[1]])
items = as.numeric(lines[(br+1):length(lines)])

s = as.numeric(sapply(ranges, `[`, 1))
e = as.numeric(sapply(ranges, `[`, 2))

############################
# Part 1

result = sum(sapply(items, function(i) any(s <= i & i <= e)))
cat("Part 1, full data:", result, "\n")

############################
# Part 2

m = cbind(s, e)[order(s), ]

l = list(m[1, ])
m = m[-1, , drop = F]

repeat {
  if (m[1, 1] <= l[[length(l)]][2] && m[1, 2] >= l[[length(l)]][2]) {
    l[[length(l)]][2] = m[1, 2]
  } else if (m[1, 1] > l[[length(l)]][2]) {
    l[[length(l) + 1]] = m[1, ]
  }
  m = m[-1, , drop = F]

  if (!nrow(m)) break
}

result = sum(sapply(l, function(i) i[2] - i[1] + 1))
options(scipen = 999)
cat("Part 2, full data:", result, "\n")
