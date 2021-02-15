Lajeunesse, M.J. (2016) Facilitating systematic reviews, data extraction and meta-analysis with the metagear package for R. Methods in Ecology and Evolution 7, 323−330.

# metagear 0.7 (2/12/21)

* Removed some testThat tests that use random numbers: randomly got errors and CRAN will not like it (since they won't rerun tests)
* Moved many dependencies from Imports to Suggest -- in particular focusing on minimizing EBImage package footprint and limiting installation from Bioconductor until a figure extraction function is called
* Added external dependency checks when package is loaded
* Converted image data examples from .rda to orginal jpg formats (keeps package size small) [also modified vignette to reflect new way of loading packaged images]
* Updated some of the PDF_download() ad-hoc searches for PDFs.
* typo fixes


# metagear 0.6 (4/11/20)

* removed version requirements for EBImage package (CRAN binary compilers may not always have the latest version from Bioconductor)
* typo fixes


# metagear 0.5 (3/20/20)

* Added zzz.R file with major dependency error catching during first installation: message to install EBImage from bioconductor and message to check if Tcl/Tk GUI toolkit is installed
* Completely migrated non-R GUI dependencies of abstract_screener(); it now uses the base package tcltk rather than gWidgets and gWidgetsRGtk2 to build the GUI. The later packages required users to install non-R dependency GTK+; it also always lead to failed CRAN checks for MacOS (resulting in no CRAN OS X binary builds for more than a year) since the gWidgetsRGtk2 package has not been updated since 2014 and consequently no longer helps mac users to install GTK+. I think Tcl/Tk is included by default in windows and linux (but mac users still need to get XQuartz X11), so this should really improve installation and dependency rot since tcltk is part of base R. TESTED on 1/10/19 in Windows 7 & 10, and Mac OS Mojave. NOTE: since this change, gWidgetsRGtk2 has been updated and old version now works for Mac Os, but will keep new one since it uses base...   
* Updated abstract_screener() to dynamically generate coding buttons as needed. Thanks to Matt Jones for this suggestion. Now you can remove or add screening buttons beyond the typical YES, NO, MAYBE for coding studies
* Updated abstract_screener() to have flexible keyboard key-bindings to coding buttons
* Updated abstract_screener() to include menu dropdown options for reversing coding decisions, backtrack previous coding decisions, refresh abstract/title text (if text was accidentally deleted; now replaces previous protect option), and peek into what previous coding decisions were made. These are found under the new dropdown menu called "Issue fixes"
* Updated abstract_screener() to include highlighted keywords 
* Fixed bug in effort_summary() that did not properly sort screening outcomes among the paired Reviewers from dual screening efforts
* Added Crossref DOI searches to browse_DOI()
* Updated replicate_phyloMeta1.3() to include tree shape statistics and original VCV calculation as in the C++ (2011) version
* Fixed PDF download templates for Elsiver which broke vignette


# metagear 0.4 (3/14/17)

* As of Jan/2017, Elsevier added numerous redirects and cookies for their PDF downloads; consequently, metagear is (sometimes) no longer able to extract PDFs from this publisher; this also broke an entire vignette section :(
* Updated vignette to reflect low download success of Elsevier journal articles
* Fixed effort_redistribute() bug that incorrectly calculated effort % for distribution of references (thanks to Melanie Hartley for making me aware of this issue)
* Added new color and style themes for PRISMA flow charts via the plot_PRISMA(); this includes several flat schemes
* Fixed PDF_extractImages() bug preventing some of the extracted images from being recognized as image files (was an SOI file signature problem for some jpg formats)
* Fixed bug in abstract_screener() that continuously prompted a validation check when screening abstracts
* Added figure_split() to automatically split-up a figure image with multiple inlaid plots


# metagear 0.3 (01/03/17)

* Updated abstract_screener() to include options for changing the default settings of GUI window size, font size, button sizes, url of search engine, or text protection (thanks to Maria Gatta for this suggestion). A menu bar was also added to help quickly fix screening errors.
* Total reworking of plot_PRISMA() since previous version could not plot phase grobs when loaded from within the package (although works fine when loaded separately from metagear). Thanks to Hautahi Kingi for making me aware of this problem.  Unfortunately, this new version is not as flexible since it no longer makes use of 'drawDetails' functionality of grid objects that allow for rescaling of plots/objects as the window size gets adjusted by the user.  This mostly results in the spacing among the phase labels to change with window size.  In case high quality plots are needed, the previous plot_PRISMA() function with the nice scaling abilities was added as a supplementary to metagear's vignette.  TO DO: reintroduce the 'drawDetails' function approach and get it to load within the package!
* Fixed plot_PRISMA() not properly connecting phases when only a single START_PHASE is included in the phase list  
* Added poorman's bibliographic scraper using Web of Science's OpenUrl
* Added extra examples in vignette (PRISMA plot, acknowledgments, installation tips, WOS citations scrape)
* Updated all figure_ functions that used depreciated R calls from EBImage bioconductor package
* Added Chachi tribute
* Added installation guide to R manual that includes troubleshooting links
* Added unit tests for: datasets, effort_initialize, effort_summary, effort_distribute, effort_merge, effort_redistribute, random_d, random_missingness, random_N, random_OR, random_pairedN, random_r, random_RR
* Fixed scrape_bibliography to extract multiline abstracts from WOS (still not perfect)
* Fixed PDFs_collect() directory name issue and now defaults to working directory
* Added to PDFs_collect() an option to randomize the download order of PDFs. Also added a random time-delay between downloads to decrease the likelihood of being blocked by website hosts.   
* Updated some of the PDF_download() ad-hoc searches for PDFs.
* Fixed "In file(con, "r") : InternetOpenUrl failed: 'The operation timed out'" error when trying to download > 40 PDFs with PDFs_collect(). Establishing connections via wininet largely fixes this issue on Windows computers. 
* Added PDF figure/image extractor
* Updated replicate_metawin2.0: fixed issues with resampling analyses, and confidence interval estimation


# metagear 0.2 (8/18/15)

* Added vignette (v. 0.2, version 0.1 not included in previous metagear versions)
* Added figure_add() for manual extractions of data points on a figure image
* Added figure_barPlot() for extracting data from vertical or horizontal bar-plots
* Added figure_display() for visualizing the loaded figure image
* Added example image data for a bar plot: Kortum_and_Acymyan_2013_Fig4
* Fixed summary bug for non-dual screening outcomes in effort_summary()
* Corrected typos in descriptions for: abstract_screener, effort_initialize, figure_removeOutlyingPoints, isPDF


# metagear 0.1 (5/20/15)

* Released package
