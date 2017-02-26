.onAttach <- function(libname, pkgname) {
    version <- read.dcf(file = system.file("DESCRIPTION", package = pkgname), fields = "Version")
    packageStartupMessage(" ")
    packageStartupMessage("#################################################################")
    packageStartupMessage("This is ", paste(pkgname, version))
    packageStartupMessage(pkgname, " is BETA software! Please report any bugs.")
    packageStartupMessage(pkgname, " was first developed at the University of Kansas Center for")
    packageStartupMessage("Research Methods and Data Analysis, under NSF Grant 1053160.")
    packageStartupMessage("#################################################################")
	options('simsem.multicore' = FALSE)
}

