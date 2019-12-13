## UofR STT 218: Introduction to Categorical Data Analysis
The package takes all of the functions that Dr. Ciminelli has created for the purposes of STT 218, and concatenates them into a single package. Rather than download and source in all of the files separately, they are now all included in a single, easy to install, package. Included herein are the following (subject to increase as new functions are introduced in class): Linear trend, relative risk, odds ratios, and local odds ratios.  
Additionally, all required packages for the course are installed and loaded along with URStat218. The included packages are: `stats`, `reshape`, `ggplot2`, `pROC`, `vcdExtra`, `nnet`, `MASS`, `ResourceSelection`, `BradleyTerry2`, and `irr`. Though all of these packages are loaded, some may go unused in either large portions, or their entirety. 

### Installation
You can install URStat218 from GitHub with the following code in R or RStudio:
```r
install.packages("devtools")
devtools::install.github("ESunRoc/URStat218")
```