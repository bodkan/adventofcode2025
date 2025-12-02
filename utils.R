get_path <- function(day, kind) {
  file.path("data", paste0(kind, "-", sprintf("%02d", day), ".txt"))
}
