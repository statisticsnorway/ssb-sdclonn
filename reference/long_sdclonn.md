# Output fra `sdc_lonn` omgjort til langformat

Eksempler inkluderer omkoding med [`recode`](recode.md).

## Usage

``` r
long_sdclonn(
  data,
  prikket = TRUE,
  long = TRUE,
  statvar = c("manedslonn", "avtalt", "bonus", "uregtil", "overtid")
)
```

## Arguments

- data:

  output fra [`sdc_lonn`](sdc_lonn.md)

- prikket:

  Prikkede tall i output ved `TRUE`. Prikkede tall er missing (`NA`).

- long:

  langformat ved `TRUE`. Ved `FALSE` returneres en versjon forberedt
  dataene.

- statvar:

  Kategorinavn som er del av variabelnavn i input og som vil finnes i
  variabelen `statvar` i output.

## Value

data i langformat

## Details

Funksjonen `long_sdclonn` er ment for å lage data på format som trengs i
statistikkbanken. Men den lager ikke de endelige variabelnavn og koder
som trengs.

Ytterligere omkoding kan gjøres på flere måter. I eksemplene her vises
det hvordan det kan gjøres med funksjonen [`recode`](recode.md) som nå
er innebygget i denne pakken. Det kan uansett være problem med de norske
bokstavene. Dette avhenger kanskje av plattformen dette kjøres på.
Norske bokstaver unngås i eksemplene.

Ved kjøring av `long_sdclonn`-funksjonen så vil det bli vil det bli
mange "kopi-rader" med antall arbeidsforhold og antall
heltidsekvivalenter. Dette fordi disse variablene blir satt sammen med
de ulike statistikkvariablene. Dersom det av en eller annen grunn skjer
at andre variabler feilkodes kan flere variabler bli «kopiert» til mange
rader.

## Examples

``` r
a <- sdclonn_data("syntetisk_5000")
out <- sdc_lonn(a,  between = c("yrke1", "yrke2", "sektor3"),  
                within = c("pers_kjoenn", "arb_heldeltid"), 
                k1 = 80, k2 = 85)
#> [extend0 5000*39->5006*40] [pre_aggregate 5006*41->160*15] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................

# Lager data i langformat  
long1 <- long_sdclonn(out)

# Legger inn nye koder for kategorier i to trinn 
long2 <- recode(long1, 
                old = c("manedslonn", "avtalt", "bonus", "uregtil", "overtid"),  
                new = c("Manedslonn", "Avtalt manedslonn", "Bonus", "Uregelmessige tillegg", 
                        "Overtidsgodtgjorelse"), 
                oldvar = "statvar", 
                newvar = "statistikkvariabel")
long3 <- recode(long2, 
                old = c("gjennomsnitt", "median", "nedre_kvartil", "Ovre_kvartil", 
                        "antall_arbeidsforhold", "antall_heltidsekvivalenter"),  
                new = c("Gjennomsnitt", "Median", "Nedre kvartil", "Ovre kvartil", 
                        "Antall arbeidsforhold med lonn", "Antall heltidsekvivalenter"),
                oldvar = "statmal", 
                newvar = "statistikkmal")
head(long3)
#>   yrke2 sektor3 pers_kjoenn arb_heldeltid    statvar      statmal    value
#> 1 Total   Total       Total         Total manedslonn gjennomsnitt 43767.48
#> 2 Total   Total       Total             1 manedslonn gjennomsnitt 43920.05
#> 3 Total   Total       Total             2 manedslonn gjennomsnitt 43212.23
#> 4 Total   Total           1         Total manedslonn gjennomsnitt 43333.07
#> 5 Total   Total           1             1 manedslonn gjennomsnitt 43582.89
#> 6 Total   Total           1             2 manedslonn gjennomsnitt 42506.49
#>   statistikkvariabel statistikkmal
#> 1         Manedslonn  Gjennomsnitt
#> 2         Manedslonn  Gjennomsnitt
#> 3         Manedslonn  Gjennomsnitt
#> 4         Manedslonn  Gjennomsnitt
#> 5         Manedslonn  Gjennomsnitt
#> 6         Manedslonn  Gjennomsnitt


# Viser alternativ metode der long blir det samme som long3.
# Her brukes pipeoperatoren, |> , som tilsvarer %>% i tidyverse.
# |> er relativt ny i base R. Brukes denne istedenfor %>% behoves ikke ekstra pakker. 

statvar_koder <- c("manedslonn", "avtalt", "bonus", "uregtil", "overtid")
statistikkvariabel_koder <- c("Manedslonn", "Avtalt manedslonn", "Bonus", 
                              "Uregelmessige tillegg", "Overtidsgodtgjorelse")
statmal_koder <- c("gjennomsnitt", "median", "nedre_kvartil", "Ovre_kvartil", 
                   "antall_arbeidsforhold", "antall_heltidsekvivalenter")
statistikkmal_koder <- c("Gjennomsnitt", "Median", "Nedre kvartil", "Ovre kvartil", 
                         "Antall arbeidsforhold med lonn", "Antall heltidsekvivalenter")

long <- sdclonn_data("syntetisk_5000") |>
sdc_lonn(between = c("yrke1", "yrke2", "sektor3"), 
         within = c("pers_kjoenn", "arb_heldeltid"), k1 = 80, k2 = 85) |>
  long_sdclonn() |>
  recode(old = statvar_koder, new = statistikkvariabel_koder, 
         oldvar = "statvar", newvar = "statistikkvariabel") |>
  recode(old = statmal_koder, new = statistikkmal_koder, 
         oldvar = "statmal", newvar = "statistikkmal")
#> [extend0 5000*39->5006*40] [pre_aggregate 5006*41->160*15] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................
head(long) 
#>   yrke2 sektor3 pers_kjoenn arb_heldeltid    statvar      statmal    value
#> 1 Total   Total       Total         Total manedslonn gjennomsnitt 43767.48
#> 2 Total   Total       Total             1 manedslonn gjennomsnitt 43920.05
#> 3 Total   Total       Total             2 manedslonn gjennomsnitt 43212.23
#> 4 Total   Total           1         Total manedslonn gjennomsnitt 43333.07
#> 5 Total   Total           1             1 manedslonn gjennomsnitt 43582.89
#> 6 Total   Total           1             2 manedslonn gjennomsnitt 42506.49
#>   statistikkvariabel statistikkmal
#> 1         Manedslonn  Gjennomsnitt
#> 2         Manedslonn  Gjennomsnitt
#> 3         Manedslonn  Gjennomsnitt
#> 4         Manedslonn  Gjennomsnitt
#> 5         Manedslonn  Gjennomsnitt
#> 6         Manedslonn  Gjennomsnitt
```
