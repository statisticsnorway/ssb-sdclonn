# tot_code_match

Limits match to rows with total code in non-common dim columns

## Usage

``` r
tot_code_match(
  x,
  y,
  x_tot_code = {
k <- as.list(rep("Total", ncol(x)))
names(k) <- names(x)
    
    k
 },
  y_tot_code = {
k <- as.list(rep("Total", ncol(y)))
names(k) <- names(y)
    
    k
 },
  both = FALSE,
  complete_match = FALSE
)
```

## Arguments

- x:

  x

- y:

  y

- x_tot_code:

  x_tot_code

- y_tot_code:

  y_tot_code

- both:

  both

- complete_match:

  Kun for testing. TRUE skal gi samme svar, men ikke like rask kode.

## Value

integer vector or list

## Examples

``` r
z <- SSBtools::SSBtoolsData("sprt_emp_withEU")
z$age[z$age == "Y15-29"] <- "young"
z$age[z$age == "Y30-64"] <- "old"
geoDimList <- SSBtools::FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]

mm1 <- SSBtools::ModelMatrix(z, list(age = "All", year = "AllYears", geo = "Europe"), 
                             crossTable = TRUE)
mm2 <- SSBtools::ModelMatrix(z, formula = ~age * year * eu + geo * age, crossTable = TRUE)
mm3 <- SSBtools::ModelMatrix(z, dimVar = 1:2, crossTable = TRUE)


tot_code1 <- find_tot_code(mm1$modelMatrix, mm1$crossTable)
tot_code2 <- find_tot_code(mm2$modelMatrix, mm2$crossTable)
tot_code3 <- find_tot_code(mm3$modelMatrix, mm3$crossTable)

mm1$crossTable[tot_code_selection(mm1$crossTable, tot_code1), ]
#>   age     year    geo
#> 1 All AllYears Europe
mm1$crossTable[tot_code_selection(mm1$crossTable, tot_code1[c(1, 3)]), ]
#>    age     year    geo
#> 1  All AllYears Europe
#> 5  All     2014 Europe
#> 9  All     2015 Europe
#> 13 All     2016 Europe
mm2$crossTable[tot_code_selection(mm2$crossTable, tot_code2[c(1, 2)]), ]
#>      age  year      geo
#> 1  Total Total    Total
#> 7  Total Total       EU
#> 8  Total Total    nonEU
#> 9  Total Total  Iceland
#> 10 Total Total Portugal
#> 11 Total Total    Spain
mm3$crossTable[tot_code_selection(mm3$crossTable, tot_code3[2]), ]
#>     age   geo
#> 1 Total Total
#> 5   old Total
#> 9 young Total

d <- mm1$crossTable
d$value <- 10 * 1:(nrow(d))

cbind(d, tot_code_replace(d, dim_var = c("age", "year", "geo"), tot_code1[c(2, 3)]))
#>      age     year      geo value value istot
#> 1    All AllYears   Europe    10    10  TRUE
#> 2    All AllYears  Iceland    20    10 FALSE
#> 3    All AllYears Portugal    30    10 FALSE
#> 4    All AllYears    Spain    40    10 FALSE
#> 5    All     2014   Europe    50    10 FALSE
#> 6    All     2014  Iceland    60    10 FALSE
#> 7    All     2014 Portugal    70    10 FALSE
#> 8    All     2014    Spain    80    10 FALSE
#> 9    All     2015   Europe    90    10 FALSE
#> 10   All     2015  Iceland   100    10 FALSE
#> 11   All     2015 Portugal   110    10 FALSE
#> 12   All     2015    Spain   120    10 FALSE
#> 13   All     2016   Europe   130    10 FALSE
#> 14   All     2016  Iceland   140    10 FALSE
#> 15   All     2016 Portugal   150    10 FALSE
#> 16   All     2016    Spain   160    10 FALSE
#> 17   old AllYears   Europe   170   170  TRUE
#> 18   old AllYears  Iceland   180   170 FALSE
#> 19   old AllYears Portugal   190   170 FALSE
#> 20   old AllYears    Spain   200   170 FALSE
#> 21   old     2014   Europe   210   170 FALSE
#> 22   old     2014  Iceland   220   170 FALSE
#> 23   old     2014 Portugal   230   170 FALSE
#> 24   old     2014    Spain   240   170 FALSE
#> 25   old     2015   Europe   250   170 FALSE
#> 26   old     2015  Iceland   260   170 FALSE
#> 27   old     2015 Portugal   270   170 FALSE
#> 28   old     2015    Spain   280   170 FALSE
#> 29   old     2016   Europe   290   170 FALSE
#> 30   old     2016  Iceland   300   170 FALSE
#> 31   old     2016 Portugal   310   170 FALSE
#> 32   old     2016    Spain   320   170 FALSE
#> 33 young AllYears   Europe   330   330  TRUE
#> 34 young AllYears  Iceland   340   330 FALSE
#> 35 young AllYears Portugal   350   330 FALSE
#> 36 young AllYears    Spain   360   330 FALSE
#> 37 young     2014   Europe   370   330 FALSE
#> 38 young     2014  Iceland   380   330 FALSE
#> 39 young     2014 Portugal   390   330 FALSE
#> 40 young     2014    Spain   400   330 FALSE
#> 41 young     2015   Europe   410   330 FALSE
#> 42 young     2015  Iceland   420   330 FALSE
#> 43 young     2015 Portugal   430   330 FALSE
#> 44 young     2015    Spain   440   330 FALSE
#> 45 young     2016   Europe   450   330 FALSE
#> 46 young     2016  Iceland   460   330 FALSE
#> 47 young     2016 Portugal   470   330 FALSE
#> 48 young     2016    Spain   480   330 FALSE
cbind(d, tot_code_replace(d, dim_var = c("age", "year", "geo"), tot_code1[1]))
#>      age     year      geo value value istot
#> 1    All AllYears   Europe    10    10  TRUE
#> 2    All AllYears  Iceland    20    20  TRUE
#> 3    All AllYears Portugal    30    30  TRUE
#> 4    All AllYears    Spain    40    40  TRUE
#> 5    All     2014   Europe    50    50  TRUE
#> 6    All     2014  Iceland    60    60  TRUE
#> 7    All     2014 Portugal    70    70  TRUE
#> 8    All     2014    Spain    80    80  TRUE
#> 9    All     2015   Europe    90    90  TRUE
#> 10   All     2015  Iceland   100   100  TRUE
#> 11   All     2015 Portugal   110   110  TRUE
#> 12   All     2015    Spain   120   120  TRUE
#> 13   All     2016   Europe   130   130  TRUE
#> 14   All     2016  Iceland   140   140  TRUE
#> 15   All     2016 Portugal   150   150  TRUE
#> 16   All     2016    Spain   160   160  TRUE
#> 17   old AllYears   Europe   170    10 FALSE
#> 18   old AllYears  Iceland   180    20 FALSE
#> 19   old AllYears Portugal   190    30 FALSE
#> 20   old AllYears    Spain   200    40 FALSE
#> 21   old     2014   Europe   210    50 FALSE
#> 22   old     2014  Iceland   220    60 FALSE
#> 23   old     2014 Portugal   230    70 FALSE
#> 24   old     2014    Spain   240    80 FALSE
#> 25   old     2015   Europe   250    90 FALSE
#> 26   old     2015  Iceland   260   100 FALSE
#> 27   old     2015 Portugal   270   110 FALSE
#> 28   old     2015    Spain   280   120 FALSE
#> 29   old     2016   Europe   290   130 FALSE
#> 30   old     2016  Iceland   300   140 FALSE
#> 31   old     2016 Portugal   310   150 FALSE
#> 32   old     2016    Spain   320   160 FALSE
#> 33 young AllYears   Europe   330    10 FALSE
#> 34 young AllYears  Iceland   340    20 FALSE
#> 35 young AllYears Portugal   350    30 FALSE
#> 36 young AllYears    Spain   360    40 FALSE
#> 37 young     2014   Europe   370    50 FALSE
#> 38 young     2014  Iceland   380    60 FALSE
#> 39 young     2014 Portugal   390    70 FALSE
#> 40 young     2014    Spain   400    80 FALSE
#> 41 young     2015   Europe   410    90 FALSE
#> 42 young     2015  Iceland   420   100 FALSE
#> 43 young     2015 Portugal   430   110 FALSE
#> 44 young     2015    Spain   440   120 FALSE
#> 45 young     2016   Europe   450   130 FALSE
#> 46 young     2016  Iceland   460   140 FALSE
#> 47 young     2016 Portugal   470   150 FALSE
#> 48 young     2016    Spain   480   160 FALSE


ma <- tot_code_match(mm3$crossTable, mm1$crossTable, tot_code3, tot_code1)

# Like koder kreves for match. Må altså ha samme total-koder. Det er det ikke her.
cbind(mm3$crossTable[!is.na(ma), ], mm1$crossTable[ma[!is.na(ma)], ]) 
#>      age      geo   age     year      geo
#> 6    old  Iceland   old AllYears  Iceland
#> 7    old Portugal   old AllYears Portugal
#> 8    old    Spain   old AllYears    Spain
#> 10 young  Iceland young AllYears  Iceland
#> 11 young Portugal young AllYears Portugal
#> 12 young    Spain young AllYears    Spain
```
