get_path <- function(day, kind = c("full", "example")) {
  kind <- match.arg(kind)
  file.path("data", paste0(kind, "-", sprintf("%02d", day), ".txt"))
}
