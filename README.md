
<img src="vignettes/images/antares_simulator.png" align="right" width=250 />
<br/>

# Watervalues

The R package watervalues allows to :
Prepare the Antares study by creating the fictive area
Launch the simulations by updating the coupling constraints
Read the results and calculate the water values

## Install package


You can install the package from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("dhia-gharsallaoui/watervalues", build_vignettes = TRUE)
```

To install all the package dependencies you can run the script `inst/dependencies.R`

## Load the package
```{r Load the package, eval=TRUE}
library(watervalues)
```


Now we are ready to use our package.

```r
shiny_water_values()
```

 ![tutorial](https://github.com/dhia-gharsallaoui/watervalues/blob/main/vignettes/images/calculate%20water%20values.gif?raw=true)
