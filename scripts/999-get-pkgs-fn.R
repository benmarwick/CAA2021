
# from rrtools, thanks Clemens Schmid!

get_pkgs <- function(r_character_vector){


  current_file <- strsplit(as.character(r_character_vector)," ")[[1]] # readLines(y, warn = FALSE)
  library_lines <- gsub("library\\(\\)", "", current_file)
  library_lines <- grep("library\\(", library_lines, value = TRUE)
  l_libs <- unlist(strsplit(library_lines, split = "library\\("))
  l_libs <- grep("[A-Za-z0-9\\.]*\\)", l_libs, value = TRUE)
  l_libs <- unlist(lapply(l_libs, function(x) {
    gsub("(.+?)(\\).*)", "\\1", x)
  }))
  require_lines <- gsub("require\\(\\)", "", current_file)
  require_lines <- grep("require\\(", require_lines, value = TRUE)
  r_libs <- unlist(strsplit(require_lines, split = "require\\("))
  r_libs <- grep("[A-Za-z0-9\\.]*\\)", r_libs, value = TRUE)
  r_libs <- unlist(lapply(r_libs, function(x) {
    gsub("(.+?)(\\).*)", "\\1", x)
  }))
  points_lines <- grep(pattern = "[a-zA-Z]:{2,3}[a-zA-Z]", x = current_file,
                       value = TRUE)
  p2_libs <- unlist(regmatches(points_lines, gregexpr("(?<=^|[^a-zA-Z0-9])[a-zA-Z0-9]*?(?=::)",
                                                      points_lines, perl = TRUE)))
  p3_libs <- unlist(regmatches(points_lines, gregexpr("(?<=^|[^a-zA-Z0-9])[a-zA-Z0-9]*?(?=:::)",
                                                      points_lines, perl = TRUE)))
  importFrom_lines <- grep(pattern = "@importFrom", x = current_file,
                           value = TRUE)
  iF_libs <- gsub("#'\\s*@importFrom\\s([^ ]*).*$", "\\1",
                  importFrom_lines)
  res <- c(l_libs, r_libs, p2_libs, p3_libs, iF_libs)
  return(unique(res))
}


