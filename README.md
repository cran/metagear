## metagear package

### purpose of the metagear package
**metagear** is a comprehensive, multifunctional toolbox with capabilities aimed to cover much of the research synthesis taxonomy: from applying a systematic review approach to objectively assemble and screen the literature, to extracting data from studies, and to finally summarize and analyze these data with the statistics of meta-analysis. More information about metagear can be found at http://lajeunesse.myweb.usf.edu.

### installation instructions and dependencies
**metagear** has two external dependencies that need to be installed and loaded prior to use in R. The first is the **EBImage** R package (Pau et al. 2010) available only from the Bioconductor repository: https://www.bioconductor.org. 
To properly install **metagear**, start with the following R script that loads the Bioconductor resources needed to install the **EBImage** (also accept all of its dependencies):

``` r
source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(metagear)
``` 

The second is associated with **metagear**’s abstract_screener function that generates a GUI to help quickly sift bibliographic data from multiple studies.
The first loading of **metagear** with library(metagear) will trigger the download of the **gWidgets** package and associated toolkits needed to build GUI interfaces. A small window will also prompt you to download GTK+ asking "Need GTK+ ?". From the listed options answer: "Install GTK+" and click "OK". Once installed, these will not be downloaded again. 

Finally, sometimes the installation will freeze; however, re-starting the R session can fix this issue.

### How to cite?
Lajeunesse, M.J. (2016) Facilitating systematic reviews, data extraction and meta-analysis with the metagear package for R. Methods in Ecology and Evolution 7, 323−330.