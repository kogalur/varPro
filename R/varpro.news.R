varpro.news <- function(...) {
  newsfile <- file.path(system.file(package="varPro"), "NEWS")
  file.show(newsfile)
}
