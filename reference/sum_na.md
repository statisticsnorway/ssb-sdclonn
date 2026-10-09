# sum_na

sum_na

## Usage

``` r
sum_na(
  x,
  code = "prikket_",
  is_na = TRUE,
  fix_names = TRUE,
  cross_prod = FALSE
)

sum2(x)

s(...)
```

## Arguments

- x:

  data frame

- code:

  code

- is_na:

  Teller missing når TRUE

- fix_names:

  Tar bort ´code´ fra navn

- cross_prod:

  Kryssmatrise ved TRUE

## Value

named integer vector eller matrise

## Details

`sum2` gir resultater fra både `prikket_` og `krav_`

`s` definert som `sum_na(sdc_lonn(...))`.
