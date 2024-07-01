
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ServicePlanningFunctions

<!-- badges: start -->
<!-- badges: end -->

The goal of ServicePlanningFunctions is to standardize functions
repeatedly used in service planning analyses at King County Metro. These
are the functions that have been developed over multiple analyses and
projects, so they are not all necessarily related to each other. Read
through the function descriptions below to learn about available
functions

## Installation

You can install the development version of ServicePlanningFunctions from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KC-Metro-Transit/ServicePlanningFunctions")
```

## save_objects() Example

save_objects() is designed to work with the purrr package to iterate
over all objects in your environment and save a copy of them to disk.
This is useful if you are trying to save your work or if you are getting
data ready to import into a Shiny app.

``` r
library(ServicePlanningFunctions)
## basic example code
object_1 <- cars
object_2 <- iris
object_3 <- gapminder

object_list <- ls() #list all object names you want to export


object_list <- object_list[! object_list %in% c("object_list")] #remove the list of objects from the list of names of objects to export

purrr::map(object_list, file_location ="your/file/location",  save_objects) #choose where the files should write to disk
```

## remove_water() Example

``` r
 kc_tracts <- sf::read_sf(fs::path_package( "extdata", "2020_Census_Tracts_for_King_County___tracts20_area.shp", package = "ServicePlanningFunctions"))

kc_tracts_no_water <- remove_water(polygon = kc_tracts, state_code = "WA", county_code = "King", crs = 2926)

mapview::mapview(kc_tracts_no_water)
```
