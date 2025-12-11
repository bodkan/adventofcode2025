# Read path to a given input file for a given day
get_path <- function(day, kind = c("full", "example")) {
  kind <- match.arg(kind)
  file.path("data", paste0(kind, "-", sprintf("%02d", day), ".txt"))
}

# A couple of debugging helpers for Day 9 ---------------------------------

draw_outline <- function(outline, clear = TRUE) {
  if (clear)
    plot(1, type = "n", xlab = "", ylab = "", xlim=c(0, max(tiles[, 1] + 1)), ylim=c(max(tiles[, 2]) + 1, 0))

  for (i in 1:nrow(outline)) {
    segments(outline[i, 1], outline[i, 2], outline[i, 3], outline[i, 4], col = "black")
  }
}

draw_rectangle <- function(r, outline) {
  plot(1, type = "n", xlab = "", ylab = "", xlim=c(0, max(tiles[, 1] + 1)), ylim=c(max(tiles[, 2]) + 1, 0))

  if (r["A2"] == r["C2"]) {
    rect(xleft = r["A1"], ybottom = r["A2"] - 0.2, xright = r["C1"], ytop = r["C2"] + 0.2, col = "orange", border = FALSE)
  } else if (r["A1"] == r["C1"]) {
    rect(xleft = r["A1"] - 0.1, ybottom = r["A2"], xright = r["C1"] + 0.1, ytop = r["C2"], col = "orange", border = FALSE)
  } else {
    rect(xleft = r["A1"], ybottom = r["A2"], xright = r["C1"], ytop = r["C2"], col = "orange", border = FALSE)
  }

  points(r["A1"], r["A2"], col = "black", pch = 20)

  draw_outline(outline, clear = FALSE)
}

# A couple of debugging helpers for Day 9 ---------------------------------

suppressPackageStartupMessages({
  library(igraph)
  library(ggraph)
  library(tidygraph)
})

plot_graph <- function(pairs, degrees = 0) {
  g <- graph_from_edgelist(pairs, directed = TRUE)
  key_nodes <- c("svr", "out", "fft", "dac", "you")
  as_tbl_graph(g) |>
    mutate(name = ifelse(degree(g) > degrees | name %in% key_nodes, name, NA),
           color = name %in% key_nodes) |>
    ggraph(layout = "kk") +
    geom_edge_fan(alpha = 0.3, color = "gray") +
    geom_node_text(aes(label = name, color = color), repel = TRUE) +
    scale_color_manual(values = c("red", "darkblue")) +
    theme_void() +
    theme(legend.position = "none")
}
