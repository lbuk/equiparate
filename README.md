# equiparate

### Overview
equiparate is an R package for creating comparative visualisations of urban density.  

### Installation
```
# Install devtools
install.packages("devtools")
library(devtools)

# Install equiparate from Github
devtools::install_github("lbuk/equiparate")
library(equiparate)
```

### Usage
The functions of equiparate include: 
* equal_dwellings
* equal_far
* equal_gsi
* max_storeys
* max_far
* min_max_storeys
* min_max_far

To find out how to use a function, type ?equal_dwellings, for example, into the console after loading the package.  

```
# Compare sites with an equal number of dwellings
# Specify the number of columns, rows and dwellings and the filename. It prints the number of combinations and exports a PDF. Note: the rendering of the charts and the creation of the PDFs will take longer for calculations that generate more combinations.
equiparate::equal_dwellings(nrow = 3, ncol = 2, dwellings = 4, filename = "equal_dwellings_nrow3_ncol2_dwellings4_chart.pdf")
```
![](https://github.com/lbuk/equiparate/blob/master/img/equal_dwellings_nrow3_ncol2_dwellings4_chart.png)

```
# Chart all combinations between a specified minimum and maximum storey count
# Specify the number of columns, rows and minumum storey, maximum storey and the filename. It prints the number of combinations and exports a PDF.
# Note: the rendering of the charts and the creation of the PDFs will take longer for calculations that generate more combinations.
equiparate::min_max_storeys(nrow = 1, ncol = 3, min_storey = 3, max_storey = 6, filename = "min_max_storeys_nrow1_ncol3_min3_max6_chart.pdf")
```
![](https://github.com/lbuk/equiparate/blob/master/img/min_max_storeys_nrow1_ncol3_min3_max6_chart.png)

```
# Chart all combinations up to a specified maximum Floor Area Ratio (FAR)
# Specify the number of columns, rows and maximum FAR and the filename. It prints the number of combinations and exports a PDF. Note: the rendering of the charts and the creation of the PDFs will take longer for calculations that generate more combinations.
equiparate::max_far(nrow = 3, ncol = 1, max_far = 4/3, filename = "max_far_nrow3_ncol1_maxfar4div3_chart.pdf")
```
![](https://github.com/lbuk/equiparate/blob/master/img/github_max_far_nrow3_ncol1_maxfar4div3_chart.png)

