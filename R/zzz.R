.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("metagear ", utils::packageVersion("metagear")))

  packageStartupMessage("** For information on installing/troubleshooting metagear, see:")
  packageStartupMessage("**   http://lajeunesse.myweb.usf.edu/metagear/metagear_basic_vignette.html")
  packageStartupMessage("")

  if (!requireNamespace("EBImage", quietly = TRUE)) {
      packageStartupMessage("** metagear needs the EBImage package from Bioconductor to be installed...")
      eval(parse(text = "install.packages(\"BiocManager\"); BiocManager::install(\"EBImage\");"))
  }

  if ((.Platform$OS.type == "windows" || .Platform$GUI == "AQUA" ) &&
      (capabilities("tcltk") || capabilities("X11") || suppressWarnings(tcltk::.TkUp))) {
    packageStartupMessage("** metagear system check: current setup supports GUIs [ TRUE ]")
  } else {
    packageStartupMessage("** metagear system check: current setup supports GUIs [ FALSE ]")
    packageStartupMessage("")
    packageStartupMessage("**   Fix for windows users:")
    packageStartupMessage("**      Update R (tcltk is now part of all new R builds).")
    packageStartupMessage("**   Fix for Mac users:")
    packageStartupMessage("**      Install xQuartz (X11) from:")
    packageStartupMessage("**         https://support.apple.com/en-us/HT201341")
  }

}

