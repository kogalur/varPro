.onAttach <- function(libname, pkgname) {
  varpro.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                            fields="Version")
  packageStartupMessage(paste("\n",
                              pkgname,
                              varpro.version,
                              "\n",
                              "\n",
                              "Type varpro.news() to see new features, changes, and bug fixes.",
                              "\n",
                              "\n"))
}
