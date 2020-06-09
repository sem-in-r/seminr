# Seminr info
seminr_info <- function() {
  list(
    pkgname = "seminr",
    version = read.dcf(
      file=system.file("DESCRIPTION", package="seminr"), fields="Version")
  )
}

print_pkginfo <- function(msg, pkg) {
  cat(msg, sprintf(" package %s (%s)\n", pkg$pkgname, pkg$version))
}
