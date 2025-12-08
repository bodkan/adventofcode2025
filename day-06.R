############################################################
# !!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!
#
# I wrote this on a tiny phone screen using a barebones
# web-based R REPL during a family trip. Apologies to
# the poor souls reading this code.
#
# !!!!!!!!!!!!!!!!!!!!!!! WARNING !!!!!!!!!!!!!!!!!!!!!!!
############################################################

options(scipen = 999)

############################
# Part 1

m = do.call(rbind, lapply(readLines("data/example-06.txt"), \(line) { x = strsplit(line, " ")[[1]]; x[x != ""]} ))
result = sum(apply(m, 2, \(col) { op = col[length(col)]; Reduce(op, as.numeric(col[-length(col)])) }))
cat("Part 1, full data:", result, "\n")

############################
# Part 2

lines = readLines("data/full-06.txt")
funs = strsplit(lines[length(lines)], " +")[[1]]
digits = t(do.call(rbind, lapply(lines[-length(lines)], \(l) strsplit(l, "")[[1]])))
nums = as.numeric(apply(digits, 1, \(row) gsub(" +", "", paste(row, collapse = ""))))
delims = c(0, which(is.na(nums)), length(nums) + 1)
blocks = lapply(seq_len(length(delims) - 1), \(i) nums[(delims[i] + 1):(delims[i + 1] - 1)])
result = sum(sapply(seq_along(blocks), function(i) Reduce(funs[i], blocks[[i]])))
cat("Part 2, full data:", result, "\n")
