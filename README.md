## metagear package

### purpose of the metagear package
**metagear** is a comprehensive, multifunctional toolbox with capabilities aimed to cover much of the research synthesis taxonomy: from applying a systematic review approach to objectively assemble and screen the literature, to extracting data from studies, and to finally summarize and analyze these data with the statistics of meta-analysis. More information about metagear can be found at [http://lajeunesse.myweb.usf.edu](http://lajeunesse.myweb.usf.edu).

### installation instructions and dependencies
**metagear** has an external dependency that needs to be installed and loaded prior to use in R. This is the **EBImage** R package (Pau et al. 2010) available only from the Bioconductor repository: https://www.bioconductor.org. 
To properly install **metagear**, start with the following R script that loads the Bioconductor resources needed to install the **EBImage** (also accept all dependencies):

``` r
install.packages("BiocManager"); 
BiocManager::install("EBImage")
library(metagear)
``` 

### How to cite?
Lajeunesse, M.J. (2016) Facilitating systematic reviews, data extraction and meta-analysis with the metagear package for R. *Methods in Ecology and Evolution* 7, 323−330. Download PDF here: [http://lajeunesse.myweb.usf.edu](http://lajeunesse.myweb.usf.edu/papers/Lajeunesse_2016_Methods_in_Ecology_and_Evolution.pdf).