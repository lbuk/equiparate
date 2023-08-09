# equiparate

### Overview

equiparate is an R package for comparing the density and adaptability of buildings and sites using permutations and heatmaps. The technique abstracts space, building on the seminal work of Leslie Martin and Lionel March on density.

### Install
    library(devtools)
    install_github("lbuk/equiparate")

### Use

    library(equiparate)

    # Compare the adaptability of floorplans with an equal number of units
    equal_units(nrow = 4, ncol = 3, units = 4)

![](https://github.com/lbuk/equiparate/blob/master/img/equal_units_nrow4_ncol3_units4_charts.png)

    # Compare the density of sites with an equal number of dwellings
    equal_dwellings(nrow = 3, ncol = 2, dwellings = 4)

![](https://github.com/lbuk/equiparate/blob/master/img/equal_dwellings_nrow3_ncol2_dwellings4_charts.png)