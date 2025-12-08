for (script in list.files(".", pattern = "^day-\\d+.R")) {
  day <- gsub("^day-(\\d+).R$", "\\1", script)
  cat("============\n")
  cat("=  Day", day, " =\n")
  cat("============\n")
  source(script, local = TRUE)
}
