
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndangeRed

<!-- badges: start -->
<!-- badges: end -->

The goal of EndangeRed is to …

## Installation

You can install the development version of EndangeRed like so:

``` r
remotes::install_github("https://github.com/CENTRO-INTERNACIONAL-DE-LA-PAPA/EndangeRed")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(EndangeRed)

data(varieties_data)

ocf_data <- OCF(
    dfr = varieties_data,
    vname = "variety_name",
    hh = "household_code",
    community = "community",
    location = "location"
)

rcf_data <- RCF(
    dfr = varieties_data,
    vname = "variety_name",
    hh = "household_code",
    nsvarie = "number_of_tubers",
    community =
        "community",
    location = "location"
)

Plot_Variable_Red_Listing(ocf_data, rcf_data,type = "log")
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

combined_scales <- Get_Red_Listing(ocf_data, rcf_data)

print(combined_scales)
#> # A tibble: 14 × 3
#>    OCF_scale           RCF_scale   varieties 
#>    <fct>               <fct>       <list>    
#>  1 very few households very scarse <chr [23]>
#>  2 very few households scarse      <chr [7]> 
#>  3 few households      very scarse <chr [33]>
#>  4 few households      scarse      <chr [24]>
#>  5 few households      common      <chr [1]> 
#>  6 few households      abundant    <chr [2]> 
#>  7 many households     very scarse <chr [9]> 
#>  8 many households     scarse      <chr [14]>
#>  9 many households     common      <chr [10]>
#> 10 many households     abundant    <chr [4]> 
#> 11 most households     very scarse <chr [1]> 
#> 12 most households     scarse      <chr [6]> 
#> 13 most households     common      <chr [9]> 
#> 14 most households     abundant    <chr [10]>

red_listing_varieties <- combined_scales$varieties[[1]]

print(red_listing_varieties)
#>  [1] "variety_223" "variety_252" "variety_208" "variety_205" "variety_118"
#>  [6] "variety_123" "variety_52"  "variety_117" "variety_250" "variety_211"
#> [11] "variety_34"  "variety_215" "variety_221" "variety_242" "variety_57" 
#> [16] "variety_41"  "variety_238" "variety_220" "variety_256" "variety_137"
#> [21] "variety_94"  "variety_29"  "variety_106"
```
