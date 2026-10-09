# Recoding character data

Recoding character data

## Usage

``` r
recode(data, old, new, oldvar = NULL, newvar = oldvar)
```

## Arguments

- data:

  data frame, list or vector

- old:

  vector of old values

- new:

  vector of new values

- oldvar:

  Variable in data to be recoded. When NULL, vector input assumed.

- newvar:

  A possible new variable for recoded version of oldvar

## Value

Recoded version of data

## Examples

``` r
z <- SSBtools::SSBtoolsData("sprt_emp_withEU")
recode(z, c("Y15-29", "Y30-64"), c("young", "old"), oldvar = "age", newvar = "newage")
#>       age      geo year ths_per    eu newage
#> 1  Y15-29    Spain 2014    66.9    EU  young
#> 2  Y15-29  Iceland 2014     1.8 nonEU  young
#> 3  Y15-29 Portugal 2014    11.6    EU  young
#> 4  Y30-64    Spain 2014   120.3    EU    old
#> 5  Y30-64  Iceland 2014     1.5 nonEU    old
#> 6  Y30-64 Portugal 2014    20.2    EU    old
#> 7  Y15-29    Spain 2015    63.4    EU  young
#> 8  Y15-29  Iceland 2015     1.9 nonEU  young
#> 9  Y15-29 Portugal 2015    14.2    EU  young
#> 10 Y30-64    Spain 2015   119.6    EU    old
#> 11 Y30-64  Iceland 2015     1.6 nonEU    old
#> 12 Y30-64 Portugal 2015    24.3    EU    old
#> 13 Y15-29    Spain 2016    69.1    EU  young
#> 14 Y15-29  Iceland 2016     1.9 nonEU  young
#> 15 Y15-29 Portugal 2016    12.7    EU  young
#> 16 Y30-64    Spain 2016   122.1    EU    old
#> 17 Y30-64  Iceland 2016     1.9 nonEU    old
#> 18 Y30-64 Portugal 2016    25.8    EU    old
recode(z$year, 2014:2016, 14:16)
#>  [1] "14" "14" "14" "14" "14" "14" "15" "15" "15" "15" "15" "15" "16" "16" "16"
#> [16] "16" "16" "16"
recode(z, c("2014", "2016"), c("14", "16"), oldvar = "year")
#>       age      geo year ths_per    eu
#> 1  Y15-29    Spain   14    66.9    EU
#> 2  Y15-29  Iceland   14     1.8 nonEU
#> 3  Y15-29 Portugal   14    11.6    EU
#> 4  Y30-64    Spain   14   120.3    EU
#> 5  Y30-64  Iceland   14     1.5 nonEU
#> 6  Y30-64 Portugal   14    20.2    EU
#> 7  Y15-29    Spain 2015    63.4    EU
#> 8  Y15-29  Iceland 2015     1.9 nonEU
#> 9  Y15-29 Portugal 2015    14.2    EU
#> 10 Y30-64    Spain 2015   119.6    EU
#> 11 Y30-64  Iceland 2015     1.6 nonEU
#> 12 Y30-64 Portugal 2015    24.3    EU
#> 13 Y15-29    Spain   16    69.1    EU
#> 14 Y15-29  Iceland   16     1.9 nonEU
#> 15 Y15-29 Portugal   16    12.7    EU
#> 16 Y30-64    Spain   16   122.1    EU
#> 17 Y30-64  Iceland   16     1.9 nonEU
#> 18 Y30-64 Portugal   16    25.8    EU

SSBtools::SSBtoolsData("sprt_emp_withEU") |>
  recode(c("Y15-29", "Y30-64"), c("young", "old"), oldvar = "age", newvar = "newage") |>
  recode(2014:2016, 14:16, oldvar = "year") 
#>       age      geo year ths_per    eu newage
#> 1  Y15-29    Spain   14    66.9    EU  young
#> 2  Y15-29  Iceland   14     1.8 nonEU  young
#> 3  Y15-29 Portugal   14    11.6    EU  young
#> 4  Y30-64    Spain   14   120.3    EU    old
#> 5  Y30-64  Iceland   14     1.5 nonEU    old
#> 6  Y30-64 Portugal   14    20.2    EU    old
#> 7  Y15-29    Spain   15    63.4    EU  young
#> 8  Y15-29  Iceland   15     1.9 nonEU  young
#> 9  Y15-29 Portugal   15    14.2    EU  young
#> 10 Y30-64    Spain   15   119.6    EU    old
#> 11 Y30-64  Iceland   15     1.6 nonEU    old
#> 12 Y30-64 Portugal   15    24.3    EU    old
#> 13 Y15-29    Spain   16    69.1    EU  young
#> 14 Y15-29  Iceland   16     1.9 nonEU  young
#> 15 Y15-29 Portugal   16    12.7    EU  young
#> 16 Y30-64    Spain   16   122.1    EU    old
#> 17 Y30-64  Iceland   16     1.9 nonEU    old
#> 18 Y30-64 Portugal   16    25.8    EU    old
```
