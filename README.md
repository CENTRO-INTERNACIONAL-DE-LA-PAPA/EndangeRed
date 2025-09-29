
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EndangeRed

<!-- badges: start -->

<!-- badges: end -->

The goal of EndangeRed is to provide a tool to calculate the Red Listing
of varieties based on the Overall Cultivar Frequency (OCF) and Relative
Cultivar Frequency (RCF) values.

## Installation

You can install the development version of EndangeRed like this:

``` r
remotes::install_github("https://github.com/CENTRO-INTERNACIONAL-DE-LA-PAPA/EndangeRed")
```

## Example

This is a basic example which shows you how to get the Red Listing of
varieties based on the Overall Cultivar Frequency (OCF) and Relative
Cultivar Frequency (RCF) values.

Here we have two years of data, 2013 and 2017 on the same location.

``` r

library(EndangeRed)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
data("Huancavelica_2013")

Huancavelica_2013 %>% 
    head(5)
#> # A tibble: 5 × 44
#>      id unique_id_hjuarez source         year   id1 id_ppgis join  region codigo
#>   <dbl>             <dbl> <chr>         <dbl> <dbl> <lgl>    <chr> <chr>  <lgl> 
#> 1 14257             26959 https://doi.…  2013  3187 NA       CA-0… Huanc… NA    
#> 2 14258             26960 https://doi.…  2013  3188 NA       CA-0… Huanc… NA    
#> 3 14259             26961 https://doi.…  2013  3189 NA       CA-0… Huanc… NA    
#> 4 14260             26962 https://doi.…  2013  3190 NA       CA-0… Huanc… NA    
#> 5 14261             26963 https://doi.…  2013  3191 NA       CA-0… Huanc… NA    
#> # ℹ 35 more variables: potato_landraces_data_set_cusco_code_id <chr>,
#> #   comunidad <chr>, familia <chr>, household <chr>, parcela <dbl>,
#> #   synonyms <chr>, cantidad <dbl>, categoria <chr>, sub_parcela <chr>,
#> #   area_m2 <dbl>, area_ha <dbl>, altitud <dbl>, r_alt_200 <chr>,
#> #   r_alt_100m <chr>, categoria_final <chr>, sum_ctdad <dbl>, a_var_m2 <dbl>,
#> #   a_var_ha <dbl>, cod_fam <chr>, sum_var_xfaml <dbl>, n_cmdes <dbl>,
#> #   n_famls <dbl>, fam_total_region <dbl>, final_variety_name <chr>, …
```

First let’s select a year. For this example we will use Huancavelica
data from 2013.

``` r

ocf_data <- OCF(
    dfr = Huancavelica_2013,
    vname = "final_variety_name",
    hh = "household",
    community = "comunidad",
    location = "region"
)

print(head(ocf_data))
#> # A tibble: 6 × 7
#>   community      nhh variety_name  nhhxvarie   ccf sumccf    OCF
#>   <chr>        <int> <chr>             <int> <dbl>  <dbl>  <dbl>
#> 1 Castillapata    70 Achanqayra            5  7.14   7.14  2.38 
#> 2 Castillapata    70 Allqa Suytu           4  5.71   9.35  3.12 
#> 3 Castillapata    70 Allqa frescos         3  4.29   4.29  1.43 
#> 4 Castillapata    70 Allqa palta           9 12.9   96.9  32.3  
#> 5 Castillapata    70 Allqa suytu           1  1.43   1.43  0.476
#> 6 Castillapata    70 Allqay Walash        11 15.7   73.7  24.6

rcf_data <- RCF(
    dfr = Huancavelica_2013,
    vname = "final_variety_name",
    hh = "household",
    nsvarie = "cantidad",
    community = "comunidad",
    location = "region"
)

print(head(rcf_data))
#> # A tibble: 6 × 47
#>      id unique_id_hjuarez source       year   id1 id_ppgis join  location codigo
#>   <dbl>             <dbl> <chr>       <dbl> <dbl> <lgl>    <chr> <chr>    <lgl> 
#> 1 14257             26959 https://do…  2013  3187 NA       CA-0… Huancav… NA    
#> 2 14258             26960 https://do…  2013  3188 NA       CA-0… Huancav… NA    
#> 3 14259             26961 https://do…  2013  3189 NA       CA-0… Huancav… NA    
#> 4 14260             26962 https://do…  2013  3190 NA       CA-0… Huancav… NA    
#> 5 14261             26963 https://do…  2013  3191 NA       CA-0… Huancav… NA    
#> 6 14262             26964 https://do…  2013  3192 NA       CA-0… Huancav… NA    
#> # ℹ 38 more variables: potato_landraces_data_set_cusco_code_id <chr>,
#> #   community <chr>, familia <chr>, hh <chr>, parcela <dbl>, synonyms <chr>,
#> #   nsvarie <dbl>, categoria <chr>, sub_parcela <chr>, area_m2 <dbl>,
#> #   area_ha <dbl>, altitud <dbl>, r_alt_200 <chr>, r_alt_100m <chr>,
#> #   categoria_final <chr>, sum_ctdad <dbl>, a_var_m2 <dbl>, a_var_ha <dbl>,
#> #   cod_fam <chr>, sum_var_xfaml <dbl>, n_cmdes <dbl>, n_famls <dbl>,
#> #   fam_total_region <dbl>, variety_name <chr>, plots_cusc <lgl>, …
```

If we need to get the endangered varieties we can do it by using calling
the function `get_red_listing`.

``` r

endangered_varieties <- get_red_listing(Huancavelica_2013)

endangered_varieties %>% 
    pull(risk_category) %>% table()
#> .
#>                At Risk     Critically At Risk Potentially Vulnerable 
#>                    115                     83                   1296 
#>                 Secure    Stable, Low Concern 
#>                    287                   1848
```

Let’s see which varieties are the ones **At Risk**

``` r

endangered_varieties %>% 
    dplyr::filter(risk_category == "At Risk") %>% 
    pull(final_variety_name)
#>   [1] "Culebras"              "Culebras"              "Culebras"             
#>   [4] "Yuraq Ipillu"          "Yuraq Ipillu"          "Yuraq Ipillu"         
#>   [7] "Qillu Ipillu"          "Qillu Ipillu"          "Qillu Ipillu"         
#>  [10] "Qillu Ipillu"          "Qillu Ipillu"          "Kichka matanka"       
#>  [13] "Kichka matanka"        "Kichka matanka"        "Kichka matanka"       
#>  [16] "Yana llumchuy waqachi" "Yana llumchuy waqachi" "Yana llumchuy waqachi"
#>  [19] "Yana llumchuy waqachi" "Allqa frescos"         "Allqa frescos"        
#>  [22] "Allqa frescos"         "Llamapa Sullun"        "Llamapa Sullun"       
#>  [25] "Yana Poncho"           "Yana Poncho"           "Uqi paya"             
#>  [28] "Uqi paya"              "Uqi paya"              "Uqi paya"             
#>  [31] "Uqi paya"              "Uqi paya"              "Uqi paya"             
#>  [34] "Uqi paya"              "Uqi paya"              "Uqi paya"             
#>  [37] "Uqi paya"              "Uqi paya"              "Uqi paya"             
#>  [40] "Uqi paya"              "Qolqi tupu"            "Qolqi tupu"           
#>  [43] "Qolqi tupu"            "Qolqi tupu"            "Qolqi tupu"           
#>  [46] "Qolqi tupu"            "Azul Waña"             "Azul Waña"            
#>  [49] "Azul Waña"             "Azul Waña"             "Azul Waña"            
#>  [52] "Azul Waña"             "Azul Waña"             "Azul Waña"            
#>  [55] "Azul Waña"             "Cucharcas"             "Cucharcas"            
#>  [58] "Cucharcas"             "Cucharcas"             "Amarilis"             
#>  [61] "Amarilis"              "Yuraq manua"           "Yuraq manua"          
#>  [64] "Yuraq manua"           "Leona"                 "Leona"                
#>  [67] "Leona"                 "Leona"                 "Leona"                
#>  [70] "Leona"                 "Leona"                 "Leona"                
#>  [73] "Leona"                 "Cuchi Pelo"            "Cuchi Pelo"           
#>  [76] "Cuchi Pelo"            "Cuchi Pelo"            "Cuchi Pelo"           
#>  [79] "Cuchi Pelo"            "Yana pumapa makin"     "Yana pumapa makin"    
#>  [82] "Yana pumapa makin"     "Yana pumapa makin"     "Yana pumapa makin"    
#>  [85] "Yana pumapa makin"     "Wamanpa Qallun"        "Wamanpa Qallun"       
#>  [88] "Wamanpa Qallun"        "Wamanpa Qallun"        "Wamanpa Qallun"       
#>  [91] "Wamanpa Qallun"        "Wamanpa Qallun"        "Wamanpa Qallun"       
#>  [94] "Yuraq tuqu"            "Yuraq tuqu"            "Yuraq tuqu"           
#>  [97] "Puka Puqya"            "Puka Puqya"            "Chaulina"             
#> [100] "Chaulina"              "Chaulina"              "Chaulina"             
#> [103] "Chaulina"              "Chaulina"              "Chaulina"             
#> [106] "Chaulina"              "Chaulina"              "Chaulina"             
#> [109] "Chaulina"              "Amillica"              "Amillica"             
#> [112] "Misipa Makin"          "Misipa Makin"          "Misipa Makin"         
#> [115] "Misipa Makin"
```
