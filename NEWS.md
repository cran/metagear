Lajeunesse, M.J. (2016) Facilitating systematic reviews, data extraction and meta-analysis with the metagear package for R. Methods in Ecology and Evolution 7, 323−330.


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
* Fixed plot_PRISMA() not properly connecting phases when only a single START_PHASE is included in the phase list.  
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
