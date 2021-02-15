.onAttach <- function(libname, pkgname) {

  packageStartupMessage(paste0("** metagear ", utils::packageVersion("metagear"), ", for installing/troubleshooting help see:"))
  packageStartupMessage("**     http://lajeunesse.myweb.usf.edu/metagear/metagear_basic_vignette.html")
  packageStartupMessage("***** External dependencies check:")

   if ((.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") &&
      (capabilities("tcltk") || capabilities("X11") || suppressWarnings(tcltk::.TkUp))) {
    packageStartupMessage("***** setup supports GUIs [ TRUE ]")
   } else {
		packageStartupMessage("***** setup supports GUIs [ FALSE ]")
		packageStartupMessage("*****    NOTE: Your configuration may still support GUIs,")
		packageStartupMessage("*****          use the fixes below only after you try")
		packageStartupMessage("*****          running metagear's abstract_screener().")
		packageStartupMessage("**")
		packageStartupMessage("**   Fix for Windows users:")
		packageStartupMessage("**      Update R (tcltk is now part of all new R builds).")
		packageStartupMessage("**   Fix for Mac users:")
		packageStartupMessage("**      Install xQuartz (X11) from https://www.xquartz.org/")
	}

  if (!requireNamespace("EBImage", quietly = TRUE)) {
      packageStartupMessage("***** setup supports data extraction from plots/figures [ FALSE ]")
	    packageStartupMessage("*****       NOTE: EBImage package (Bioconductor) will be installed only")
		packageStartupMessage("*****             when a figure_* function is used.")
  } else {
      packageStartupMessage("***** setup supports data extraction from plots/figures [ TRUE ]")
  }

}
