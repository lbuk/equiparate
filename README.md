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

To find out how to use a function, type '?equal_dwellings', for example, into the console after loading the package.  

```
# Compare sites with an equal number of dwellings
# Specify the number of columns, rows and dwellings and the filename. It prints the number of combinations and exports a PDF. Note: the rendering of the charts and the creation of the PDFs will take longer for calculations that generate more combinations.
equal_dwellings(nrow = 3, ncol = 2, dwellings = 4, filename = "equal_dwellings_nrow3_ncol2_dwellings4_chart.pdf")
```
![](https://github.com/lbuk/equiparate/blob/master/img/equal_dwellings_nrow3_ncol2_dwellings4_charts.png)

```
# Chart all combinations between a specified minimum and maximum storey count
# Specify the number of columns, rows and minumum storey, maximum storey and the filename. It prints the number of combinations and exports a PDF. Note: the rendering of the charts and the creation of the PDFs will take longer for calculations that generate more combinations.
min_max_storeys(nrow = 1, ncol = 3, min_storey = 3, max_storey = 6, filename = "min_max_storeys_nrow1_ncol3_min3_max6_chart.pdf")
```
![](https://github.com/lbuk/equiparate/blob/master/img/min_max_storeys_nrow1_ncol3_min3_max6_charts.png)

```
# Compare sites with an equal Ground Space Index (GSI)
# Specify the number of columns, rows and GSI and the filename. It prints the number of combinations and exports a PDF. Note: the rendering of the charts and the creation of the PDFs will take longer for calculations that generate more combinations.
equal_gsi(nrow = 4, ncol = 3, gsi = 5/6, filename = "equal_gsi_nrow4_ncol3_gsi5div6.pdf")
```
![](https://github.com/lbuk/equiparate/blob/master/img/equal_gsi_nrow4_ncol3_gsi5div6_charts.png)
