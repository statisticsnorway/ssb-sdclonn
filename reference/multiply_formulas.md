# Multiply formulas

Combine formulas by `*` or another operator

## Usage

``` r
multiply_formulas(formula1, formula2, operator = "*")
```

## Arguments

- formula1:

  formula1

- formula2:

  formula2

- operator:

  `*`, `:` or another operator

## Value

A formula

## Examples

``` r
multiply_formulas(~x, ~y)
#> ~(x) * (y)
#> <environment: 0x5fefa8ae2ec0>
multiply_formulas(~x+y, ~a*b + d:e)
#> ~(x + y) * (a * b + d:e)
#> <environment: 0x5fefa8ae2ec0>
multiply_formulas(~x+y, ~a*b + d:e, ":")
#> ~(x + y):(a * b + d:e)
#> <environment: 0x5fefa8ae2ec0>
```
