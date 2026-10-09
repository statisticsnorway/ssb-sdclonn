# Undertrykking i lønnsstatistikk

Input er mikrodata og output er (samordnede) tabell(er) med alle ønskede
aggregeringsnivåer.

## Usage

``` r
sdc_lonn(
  data,
  between = NULL,
  within = NULL,
  var_vektet = c(manedslonn = "lonn_ekv_ialt_pub", avtalt = "lonn_ekv_fmlonn_pub", bonus
    = "lonn_ekv_bonus_pub", uregtil = "lonn_ekv_uregtil_pub", overtid =
    "lonn_overtid_pub"),
  var_uvektet = NULL,
  privat = "privat",
  var_med_privat = NULL,
  var_frtk_id = "frtk_id_ssb",
  var_virk_id = "virk_id_ssb",
  var_ekv_vekt = "lonn_ekv_vekt",
  var_dominans = var_vektet[1],
  var_sum = NULL,
  freq_within = 2,
  between_regel = TRUE,
  k1,
  k2,
  secondary2 = TRUE,
  grenseverdi = 100,
  grenseverdi2 = 4,
  prosentiler = c(nedre_kvartil = 25, median = 50, ovre_kvartil = 75),
  extend0 = TRUE,
  removeEmpty = FALSE,
  formula = NULL,
  output = NULL,
  suppressed_data = NULL,
  dim_var_extra = NULL,
  avoidHierarchical = FALSE,
  structuralEmpty = FALSE,
  run_gauss = TRUE,
  roundBase = 0,
  roundMultiple = 1,
  verbose = TRUE,
  double_vars = NULL,
  double_priority = 0,
  use_diff_groups = FALSE,
  remove_diff_vars = TRUE
)
```

## Arguments

- data:

  input data eller resultat fra `output = "aggregated"`

- between:

  Variabler som grupperer foretak eller en formel for disse. Se også
  parameteren `formula` som er et alternativ.

- within:

  Variabler innen foretak eller en formel for disse.

- var_vektet:

  Lønnsvariabler navngitt ved navn som skal brukes i output. Ved
  manglende navn brukes variabelnavnet direkte.

- var_uvektet:

  Som `var_vektet` over, men disse variablene beregnes uvektet. Default
  (`NULL`) er at ingen slike variabler er med.

- privat:

  Privatkoden som brukes. Default er `"privat"`.

- var_med_privat:

  Variabel (sektor) med privatkoden. Default (`NULL`) betyr at det letes
  automatisk gjennom tabellvariabler (between) for å finne privat-koden.

- var_frtk_id:

  Variabelnavn for id for foretak. Default er `"frtk_id_ssb"`.

- var_virk_id:

  Variabelnavn for id for virksomhet.Default er `"virk_id_ssb"`.

- var_ekv_vekt:

  Variabelnavn for vekt for heltidsekvivalenter. Default er
  `"lonn_ekv_vekt",`.

- var_dominans:

  Variabelnavn for variabel som er utganspunkt for dominansberegning.
  Default er første `var_vektet`-variabel. Dominansberegningen vekter
  uansett denne variabelen.

- var_sum:

  Ekstra numeriske variabler Som bare skal summeres, navngitt ved navn
  som skal brukes i output. Tilsvarende som `var_vektet` over.

- freq_within:

  max-verdi for primærprikking av Antall arbeidsforhold "within". Se
  tekst (n_within) i
  [`sdc_lonn_extra_details`](sdc_lonn_extra_details.md).

- between_regel:

  Ved TRUE er "between" utgangspunkt for dominansregler

- k1:

  Parameter i dominansprikking. Gjelder for dominansprikkingen der man
  ser på dominansen av ett foretak. F.eks. betyr verdien `k1 = 80` her
  at hvis ett foretak, der `krav_ikke_offentlig = 0`, står for mer enn
  80% av alle arbeidsforholdene og/eller mer enn 80% av summen av lønn
  når det fordeles etter `between`-variabelene, så slår
  dominansprikkingen ut.

- k2:

  Parameter i dominansprikking. Gjelder for dominansprikkingen der man
  ser på dominansen av to foretak. F.eks. betyr verdien `k2 = 90` her at
  hvis to foretak, der `krav_ikke_offentlig = 0`, står for mer enn 90%
  av alle arbeidsforholdene og/eller mer enn 90% av summen av lønn når
  det fordeles etter `between`-variabelene, så slår dominansprikkingen
  ut.

- secondary2:

  **Når `secondary2` er `TRUE`** sekundærprikkes det i to omganger.

  - I første omgang sekundærprikkes det ikke med tanke på å hindre
    tilbakeregning av between-totaler av Antall arbeidsforhold ut fra
    dominansregler (`k1` og `k2`), men kun med tanke på within (f.eks.
    kjønn).

  - I andre omgang sekundærprikkes det også med tanke between-totaler
    som beskrevet over. Det er antall heltidsekvivalenter som blir
    sekundærprikket ut fra dette.

  Poenget med to runder er publisering av gjennomsnittsverdier og antall
  heltidsekvivalenter som må beskyttes av dominansregler.
  Gjennomsnittsverdier kan tilbakeregnes via summer (gjennomsnitt \*
  antall heltidsekvivalenter) dersom både gjennomsnitt og antall
  heltidsekvivalenter er tilgjengelig. Dette hindres altså av
  sekundærprikkingen av antall heltidsekvivalenter.

  **Når `secondary2` er `FALSE`** fortas sekundærprikking bare en gang
  uten tanke på at antall arbeidsforhold for between-totaler ikke må
  beskyttes.

- grenseverdi:

  Minste antall ok arbeidsforhold for publisering av median, kvartiler
  og gjennomsnittslønn, samt andre variabler som antall arbeidsforhold
  og antall heltidsekvivalenter. (ingen sekundærprikking). Se
  [`sdc_lonn_extra_details`](sdc_lonn_extra_details.md).

- grenseverdi2:

  Minste antall ok arbeidsforhold som ikke primærprikkes (`primary2`)
  forut for andre runde sekundærprikking. Se
  [`sdc_lonn_extra_details`](sdc_lonn_extra_details.md).

- prosentiler:

  Prosentiler med navn som skal brukes i output. Parameteren kan f.eks.
  settes til:

  - `c(p10 = 10, nedre_kvartil = 25, median = 50, ovre_kvartil = 75, p90 = 90)`

  - `c(10, nedre_kvartil = 25, median = 50, ovre_kvartil = 75, 90)`

  Disse to eksemplene gir samme resultat fordi navn settes til
  prosentverdien med `p` foran dersom navn mangler.

- extend0:

  Ved noe annet enn `extend0 = TRUE` kjøres `Extend0` i SSBtools. Trengs
  for beskyttelse av "within"-0-ere.

  - **`extend0 = TRUE`** (default): Innen hver between-gruppen så
    sjekkes det om alle kombinasjoner av within-variablene som fra før
    finnes i dataene også fins i den bestemte between-gruppen. Hvis ikke
    legges det til observasjoner med 0. `FALSE` og også `TRUE` kan gi
    warning som kan slås av med parameteren `structuralEmpty` (se
    nedenfor).

  - **`extend0 = "full"`**: I motsetning til `TRUE` så legges det også
    til kombinasjoner som ikke fins i dataene fra før. Ved
    `avoidHierarchical = FALSE` (default) respekteres hierarkiske
    sammenhenger slik at ikke "umulige" kombinasjoner legges til. Ved
    `avoidHierarchical = TRUE` legges det til alle mulige kombinasjoner.

  - **`extend0` som liste**: Avansert spesifisering av hvordan
    within-variablene kombineres. Eks:
    `extend0 <- list("A", "B", c("C", "D"))`. Det legges til alle mulige
    kombinasjoner av `A`, `B` og `(C`-`D`-kombinasjoner som fins i
    dataene fra før`)`.

- removeEmpty:

  Ved TRUE fjernes between-kombinasjoner der det ikke finnes data. Dette
  er en parameter til ModelMatrix i SSBtools.

- formula:

  En formel som inkluderer både between- og within-variablene.
  Parameteren `within` brukes da i tillegg, mens `between`-parameteren
  ikke er i bruk (ignoreres).

- output:

  Mulighet for annen output.

- suppressed_data:

  Outputdata fra en annen kjøring av `sdc_lonn`. Prikkingen skal
  samordnes slik at kombinasjoner som fins i `suppressed_data` prikkes
  på samme måte. Det blir nye variabler i output, `krav_inputsupp` og
  `krav_inputsupp2`. Disse forteller om prikking som er samordnet.
  Verdiene ved samordning er 0 (ikke prikket) eller 1 (prikket) og
  ellers missing. Overlapp mellom prikkingene i de to rundene er ikke
  fjernet. For at dette skal virke må man passe på at like variabler får
  samme navn. Se `dim_var_extra` nedenfor. Ved bruk av `list` kan flere
  datasett være input. F.eks.:
  `suppressed_data = list(out6, out7, out8)`

- dim_var_extra:

  Ekstra dimensjonsvariabler (som i `between`, `within`, `formula`) som
  skal tas hensyn til ved navngiving av outputvariabler. Dette for å
  sikre like navn ved bruk av `suppressed_data`. Det er ok med variabler
  som ikke får betydning (fins fra før i `between`, `within`, `formula`
  eller ikke relatert til disse).

- avoidHierarchical:

  Ved TRUE og ved formelinput kombineres ikke hierarkiske variabler i
  output. Dette er en parameter til ModelMatrix i SSBtools. Om dette
  brukes blir `dim_var_extra` ignorert siden dette da ikke gir mening.
  Se også parameteren `extend0`.

- structuralEmpty:

  When `TRUE`, output cells with no contributing inner cells (only zeros
  in column of `x`) are forced to be not primary suppressed. Thus, these
  cells are considered as structural zeros. When `structuralEmpty` is
  `TRUE`, the following error message is avoided: `Suppressed` `cells`
  `with` `empty` `input` `will` `not` `be` `protected.` `Extend` `input`
  `data` `with` `zeros?`. When `removeEmpty` is `TRUE`,
  `structuralEmpty` is superfluous. Parameteren og beskrivelsen er
  kopiert fra
  [`GaussSuppression::GaussSuppressionFromData`](https://statisticsnorway.github.io/ssb-gausssuppression/reference/GaussSuppressionFromData.html).
  Bruk av `suppressed_data` kan føre til at celler allikevel blir
  primærprikket.

- run_gauss:

  Ingen sekundærprikking dersom denne settes til `FALSE`, Default er
  `TRUE`.

- roundBase:

  Ved andre verdier enn 0 foretas avrunding av antall arbeidsforhold.
  Funksjonen
  [`PLSroundingPublish`](https://statisticsnorway.github.io/ssb-smallcountrounding/reference/PLSrounding.html)
  bruker da denne `roundBase`-parameteren. Ny ekstra variabel helt til
  slutt i output er `rounded_antall_arbeidsforhold`. Variabelen
  `prikket_antall_arbeidsforhold` blir endret til å inneholde de samme
  avrundede verdiene, bortsett fra at noen av dem er prikket. Denne
  prikkingen endres også slik at dette kun handler om dominansregel.
  Teknisk har da `n_within_krav` blitt flyttet til andre runde prikking
  (fra `primary` til `primary2`). Det sørges også for ingen
  primærprikking i første runde ved `rounded_antall_arbeidsforhold` `<=`
  `roundBase`. Litt modifisert blir også `krav_grenseverdi` ved at det
  også kreves `rounded_antall_arbeidsforhold` `<` `grenseverdi` i
  tillegg til at, som vanlig, `regel_within_okantall` `<` `grenseverdi`.

- roundMultiple:

  Når dette heltalltallet er større enn 1, vil avrunding også sørge for
  at alle tall \< `roundBase` `*` `roundMultiple` er
  `roundBase`-multipler. F.eks. ved `roundBase`=3 og `roundMultiple`=2
  så vil 1,2,4 og 5 ikke forekomme i avrundet output, mens 0, 3 og 6
  forekommer. Det sørges også for ingen primærprikking i første runde
  ved `rounded_antall_arbeidsforhold` `<=` `roundBase` `*`
  `roundMultiple`.

- verbose:

  Whether to print information during calculations.

- double_vars:

  Spesifisering av doble nace-koder, f.eks:
  `list(nar8_begge = c("nar8", "nar8_ny"), nar17_begge = c("nar17", "nar17_ny"))`.
  Da brukes `nar8_begge` og `nar17_begge` i `between`-formel, mens de
  andre kodene er variablene i `data` for gammel og ny standard.

- double_priority:

  Spesifisering av prioritet tilknyttet doble nace-koder, samt
  spesifisering av spesielle kjøringer. Mulige valg er

  - **`0`:** Kun differanseceller nedprioriteres.

  - **`1` eller `2`:** Første eller andre nace-kode prioriteres.

  - **`-1` eller `-2`:** Kjøring med kun første eller andre nace-kode.

  - **`-9`:** Kjøring ute bruk av prioritering (for testing).

- use_diff_groups:

  Hvorvidt differanseceller skal hensyntas. Mulige valg er `FALSE`
  (nei), `TRUE` (kun enkle differanser) eller `"extra"` (kompliserte
  differanseceller inkluderes).

- remove_diff_vars:

  Ved `TRUE` (default) fjernes differanseceller fra vanlig
  data.frame-output. Merk at ved `FALSE` kan navn på differanseceller
  noen ganger se ulogiske ut. Dette skyldes at ulike variabelnavn for
  variabler som i praksis er like, unngås.

## Value

data frame eller liste

## Details

Det er cellene med laveste antall arbeidsforhold som i utgangspunktet
velges ut til sekundærprikkingen. Teknisk sett er det `n_arbeidsforhold`
som styrer prioriteringsrekkefølgen. Grunnen til at primary2 og
secondary2 ikke nødvendigvis summeres til suppressed2 er at det i
primary2 kan inngå celler som er med i suppressed. Disse er fjernet fra
suppressed2.

|  |  |
|----|----|
| **Når regel_within\_ er starten på** | **variabelnavnet** |
| `n_unique` | Antall unike foretak |
| `n_privat` | Antall unike private foretak |
| `n_offentlig` | Antall unike private offentlige foretak |
| `max_n` | Antall arbeidsforhold til foretaket med flest. |
|  | Dette foretaket er `max_frtk_n`. |
| `second_max_n` | Antall arbeidsforhold til foretaket med nest flest. |
|  | Dette foretaket er `second_max_frtk_n`. |
| `max_privat_n` | Antall arbeidsforhold til foretaket med flest blant de private. |
|  | Dette foretaket er `max_frtk_privat_n`. |
| `second_max_privat_n` | Antall arbeidsforhold til foretaket med nest flest blant de private. |
|  | Dette foretaket er `second_max_frtk_privat_n`. |
| `max_frtk_n` | Se over |
| `second_max_frtk_n` | Se over |
| `max_frtk_privat_n` | Se over |
| `second_max_frtk_privat_n` | Se over |
| `max_is_privat_n` | Hvorvidt `max_frtk_n` er et privat foretak. |
| `second_max_is_privat_n` | Hvorvidt `second_max_frtk_n` er et privat foretak. |
| `sum_n` | Totalt antall arbeidsforhold. |
| `priv_n` | Antall arbeidsforhold hos private foretak. |
| `max_l` | Sum lonn til foretaket med hoyest sum. Dette foretaket er `max_frtk_l`. |
| `second_max_l` | osv. ... |
| `max_privat_l` | ... |
| `second_max_privat_l` | ... |
| `max_frtk_l` | ... |
| `second_max_frtk_l` | ... |
| `max_frtk_privat_l` | ... |
| `second_max_frtk_privat_l` | ... |
| `max_is_privat_l` | ... |
| `second_max_is_privat_l` | ... |
| `sum_l` | Sum lonn total |
| `priv_l` | Sum lonn hos private foretak. |
| `n_arbeidsforhold` | Totalt antall arbeidsforhold. (`= sum_n`, men beregnet separat fra dominansrutinen) |
| `okantall` | Nøyaktig samme variabel som `"okantall_arbeidsforhold".` |

**Variablene som starter med regel\_** er avledet fra
`regel_within`\_-variablene på følgende måte:

- `regel_within_` ser på hver kombinasjon av within-variabler (som kjønn
  og alder). I `regel_` er det bare totalverdiene for within-variablene
  som gjelder. F.eks. antall heltidsekvivalenter er da ikke for
  spesifikk kombinasjon av kjønn og alder men total-verdi for
  between-variablene man ser på. F.eks en kombinasjon av
  arb_yrke_styrk08 og sektor3. I `regel_within_` er dermed mange rader
  like.

- Nye variabler er lagt til

  - `regel_istot`: Denne er 1 nå det er en snakk om en rad der det er
    totaler for within-variablene. For disse radene er
    `regel_`-variablene og `regel_within_`-variablene like.

  - `regel_dominant1_n`: Beregnet fra `regel_`-variablene som
    `max_n/sum_n`.

  - `regel_dominant2_n`: Beregnet fra `regel_`-variablene som
    `(max_n + second_max_n)/sum_n`.

  - `regel_dominant1_l`: Beregnet fra `regel_`-variablene som
    `max_l/sum_l`.

  - `regel_dominant2_l`: Beregnet fra `regel_`-variablene som
    `(max_l + second_max_l)/sum_l`.

|  |  |
|----|----|
| **krav\_ i starten på** | **variabelnavnet** forteller om spesifikke regler for prikking er oppfylt |
| `ikke_offentlig` | = `max_is_privat` OR (`n_offentlig == 1` AND `n_privat == 1`) |
| `dominant1_n` | = `regel_dominant1_n > k1` AND `ikke_offentlig` der `k1` er parameter i dominansregel |
| `dominant2_n` | = `regel_dominant2_n > k2` AND `ikke_offentlig` der `k2` er parameter i dominansregel |
| `dominant1_l` | = `regel_dominant1_l > k1` AND `ikke_offentlig` der `k1` er parameter i dominansregel |
| `dominant2_l` | = `regel_dominant2_l > k2` AND `ikke_offentlig` der `k2` er parameter i dominansregel |
| `dominant` | = `dominant1_n` OR `dominant2_n` OR `dominant1_l` OR `dominant2_l` |
| `n_within` | = `!regel_istot` AND `regel_within_n_arbeidsforhold <= freq_within`, |
|  | der `freq_within` er input-parameter (2 er default). |
|  | Når `roundBase` ikke brukes, er dette er krav til primærprikking av |
|  | antall arbeidsforhold "within" basert på frekvensregel. |
|  | Når `roundBase` brukes, inngår kravet kun i andre runde primærprikking. |
| `istot` | = `regel_istot` |
| `grenseverdi` | = `regel_within_okantall < grenseverdi` (altså krav_grenseverdi = ...) |
|  | `grenseverdi` er input-parameter, 100 er default. (altså ikke krav_grenseverdi) |
| `grenseverdi2` | = `regel_within$n_arbeidsforhold > 0 & regel_within_okantall < grenseverdi2` |
| `primary` | = (`!istot` AND `dominant`) OR `n_within.` |
|  | Dette er primærprikking til grunn for føreste runde med sekundærprikking. |
|  | Dette modifiseres når `roundBase` brukes (se parameterbeskrivelse). |
| `primary2` | = (`istot` AND `dominant`) OR `grenseverdi2_krav` |
|  | Dette er primærprikking til grunn for andre runde med sekundærprikking. |
|  | Dette modifiseres når `roundBase` brukes (se parameterbeskrivelse). |
| `secondary` | Resultat fra første runde med sekundærprikking. |
| `secondary2` | Resultat fra andre runde med sekundærprikking. |
| `suppressed` | All prikking i første runde. |
| `suppressed2` | All prikking fra andre runde bortsett fra at mulig overlapp med suppressed er fjernet. |
| `lonn` | = `grenseverdi` OR `dominant` |
| `arbeidsforhold` | = `suppressed` OR `lonn` |
| `ekvivalent` | = `suppressed` OR `suppressed2` OR `lonn` |

- Disse krav-variablene er egentlig logiske (TRUE/FALSE), men i output
  blir dette 0/1.

- `krav_arbeidsforhold` og `krav_ekvivalent` samsvarer nøyaktig med
  endelig prikking av tilsvarende variabler.

- `krav_lonn` samsvarer med endelig prikking av lønnsvariablene (median,
  snitt og kvartiler).

- Her er det skrevet OR og AND istedenfor vanlige r-kode operatorer som
  er `|` og `&`. (Det var ikke mulig med tegnet `|` i tabellen over
  p.g.a kombinasjon markdown/roxygen/latex.)

- Operatoren `!` betyr "ikke".

## See also

Se ekstra detaljer:
[`sdc_lonn_extra_details`](sdc_lonn_extra_details.md).

## Examples

``` r

# Siden default er fjernet i ny versjon 
between <- c("yrke1", "yrke2", "yrke3", "arb_yrke_styrk08", "sektor3")
within <- c("pers_kjoenn", "arb_heldeltid")

a <- sdclonn_data("syntetisk_5000")
out <- sdc_lonn(a, between = between, within = within, k1 = 85, k2 = 95)
#> [extend0 5000*39->5014*40] [pre_aggregate 5014*41->272*17] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................
out2 <- sdc_lonn(a, between = ~yrke3 + (yrke2 + yrke1) * sektor3,  
                 within = ~arb_heldeltid, k1 = 85, k2 = 95)
#> [extend0 5000*39->5001*40] [pre_aggregate 5001*41->112*15] [ModelMatrix] 
#> [dummy_aggregate..........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: ..........................

# Med avrunding. Altså bruk av roundBase-parameteren
outR <- sdc_lonn(a, between = between, within = within, roundBase = 3,
                 k1 = 85, k2 = 95)
#> [extend0 5000*39->5014*40] [pre_aggregate 5014*41->272*18] [ModelMatrix] [crossprod] 
#> [dummy_aggregate.........................] 
#> SmallCountRounding: [-**.......=]
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................

out2A <- sdc_lonn(a, between = ~yrke3 + (yrke2 + yrke1) * sektor3, within = ~arb_heldeltid, 
                  output = "aggregated")
#> [extend0 5000*39->5001*40] [pre_aggregate 5001*41->112*15] [ModelMatrix] 
#> [dummy_aggregate..........................] 
out2B <- sdc_lonn(out2A, k1 = 85, k2 = 95)
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: ..........................
identical(out2, out2B)
#> [1] TRUE
out2C <- sdc_lonn(out2A, k1 = 80, k2 = 90)
#> GaussSuppression_anySum_numttH: ...........................
#> GaussSuppression_anySum_numttH: .........................
identical(out2, out2C)
#> [1] FALSE

out3 <- sdc_lonn(a, grenseverdi = 20, between = between, within = within,
                 var_uvektet = c(stillingspst = "arb_stillingspst", alder = "pers_alder"),
                 k1 = 80, k2 = 90)
#> [extend0 5000*39->5014*40] [pre_aggregate 5014*41->272*20] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................

out4 <- sdc_lonn(a, between = 
  ~(yrke3 + (yrke2 + yrke1) * sektor3) + pers_kommnr * (yrke2 + yrke1) + arb_fylke * yrke1, 
  within = ~arb_heldeltid * pers_kjoenn, output = "aggregated")
#> [extend0 5000*39->5589*40] [pre_aggregate 5589*41->1764*18] [ModelMatrix] 
#> [dummy_aggregate.........................] 
  
out4A <- sdc_lonn(out4, k1 = 90, k2 = 95)
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................
out4B <- sdc_lonn(out4, k1 = 80, k2 = 90)
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................
out4C <- sdc_lonn(out4, k1 = 90, k2 = 95, grenseverdi = 20)
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................

### Skriver ut antall prikker eller antall der krav-variabel er TRUE.
sum2(out)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                               1910                               1917 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                               1897                               1897 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                               1897                               1897 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                               1897                               1897 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                               1897                               1897 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                               1897                               1897 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                               1897                               1897 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                               1897                               1897 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                               1897                               1897 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                               1897                               1897 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                               1897                               1897 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                                747                                 90 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                135                                 90 
#>                   krav_dominant2_l                      krav_dominant 
#>                                135                                135 
#>                      krav_n_within                         krav_istot 
#>                                138                                264 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                               1897                                138 
#>                       krav_primary                      krav_primary2 
#>                                213                                150 
#>                    krav_suppressed                   krav_suppressed2 
#>                                761                                182 
#>                     krav_secondary                    krav_secondary2 
#>                                548                                165 
#>                          krav_lonn                krav_arbeidsforhold 
#>                               1897                               1910 
#>                    krav_ekvivalent 
#>                               1917 
sum2(out3)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                               1156                               1250 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                                885                                885 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                                885                                885 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                                885                                885 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                                885                                885 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                                885                                885 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                                885                                885 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                                885                                885 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                                885                                885 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                                885                                885 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                                885                                885 
#>  prikket_stillingspst_gjennomsnitt prikket_stillingspst_nedre_kvartil 
#>                                885                                885 
#>        prikket_stillingspst_median  prikket_stillingspst_ovre_kvartil 
#>                                885                                885 
#>         prikket_alder_gjennomsnitt        prikket_alder_nedre_kvartil 
#>                                885                                885 
#>               prikket_alder_median         prikket_alder_ovre_kvartil 
#>                                885                                885 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                                747                                126 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                153                                126 
#>                   krav_dominant2_l                      krav_dominant 
#>                                162                                171 
#>                      krav_n_within                         krav_istot 
#>                                138                                264 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                                859                                138 
#>                       krav_primary                      krav_primary2 
#>                                245                                154 
#>                    krav_suppressed                   krav_suppressed2 
#>                                788                                170 
#>                     krav_secondary                    krav_secondary2 
#>                                543                                149 
#>                          krav_lonn                krav_arbeidsforhold 
#>                                885                               1156 
#>                    krav_ekvivalent 
#>                               1250 
sum2(out4A)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                               2970                               2970 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                               2965                               2965 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                               2965                               2965 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                               2965                               2965 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                               2965                               2965 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                               2965                               2965 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                               2965                               2965 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                               2965                               2965 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                               2965                               2965 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                               2965                               2965 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                               2965                               2965 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                               1080                                 27 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                 63                                 27 
#>                   krav_dominant2_l                      krav_dominant 
#>                                 72                                 72 
#>                      krav_n_within                         krav_istot 
#>                                674                                391 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                               2965                                538 
#>                       krav_primary                      krav_primary2 
#>                                703                                543 
#>                    krav_suppressed                   krav_suppressed2 
#>                               1245                                155 
#>                     krav_secondary                    krav_secondary2 
#>                                542                                 84 
#>                          krav_lonn                krav_arbeidsforhold 
#>                               2965                               2970 
#>                    krav_ekvivalent 
#>                               2970 
sum2(out4B)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                               2976                               2979 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                               2965                               2965 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                               2965                               2965 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                               2965                               2965 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                               2965                               2965 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                               2965                               2965 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                               2965                               2965 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                               2965                               2965 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                               2965                               2965 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                               2965                               2965 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                               2965                               2965 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                               1080                                 54 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                 72                                 45 
#>                   krav_dominant2_l                      krav_dominant 
#>                                 90                                 90 
#>                      krav_n_within                         krav_istot 
#>                                674                                391 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                               2965                                538 
#>                       krav_primary                      krav_primary2 
#>                                714                                545 
#>                    krav_suppressed                   krav_suppressed2 
#>                               1277                                159 
#>                     krav_secondary                    krav_secondary2 
#>                                563                                 86 
#>                          krav_lonn                krav_arbeidsforhold 
#>                               2965                               2976 
#>                    krav_ekvivalent 
#>                               2979 
sum2(out4C)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                               2133                               2153 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                               2020                               2020 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                               2020                               2020 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                               2020                               2020 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                               2020                               2020 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                               2020                               2020 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                               2020                               2020 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                               2020                               2020 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                               2020                               2020 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                               2020                               2020 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                               2020                               2020 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                               1080                                 27 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                 63                                 27 
#>                   krav_dominant2_l                      krav_dominant 
#>                                 72                                 72 
#>                      krav_n_within                         krav_istot 
#>                                674                                391 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                               2018                                538 
#>                       krav_primary                      krav_primary2 
#>                                703                                543 
#>                    krav_suppressed                   krav_suppressed2 
#>                               1245                                155 
#>                     krav_secondary                    krav_secondary2 
#>                                542                                 84 
#>                          krav_lonn                krav_arbeidsforhold 
#>                               2020                               2133 
#>                    krav_ekvivalent 
#>                               2153 
sum2(out2)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                                151                                151 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                                151                                151 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                                151                                151 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                                151                                151 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                                151                                151 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                                151                                151 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                                151                                151 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                                151                                151 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                                151                                151 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                                151                                151 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                                151                                151 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                                117                                  6 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                  9                                  6 
#>                   krav_dominant2_l                      krav_dominant 
#>                                  9                                  9 
#>                      krav_n_within                         krav_istot 
#>                                  1                                104 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                                151                                  2 
#>                       krav_primary                      krav_primary2 
#>                                  6                                  4 
#>                    krav_suppressed                   krav_suppressed2 
#>                                 22                                  6 
#>                     krav_secondary                    krav_secondary2 
#>                                 16                                  5 
#>                          krav_lonn                krav_arbeidsforhold 
#>                                151                                151 
#>                    krav_ekvivalent 
#>                                151 
sum2(out2C)
#>      prikket_antall_arbeidsforhold prikket_antall_heltidsekvivalenter 
#>                                153                                156 
#>    prikket_manedslonn_gjennomsnitt   prikket_manedslonn_nedre_kvartil 
#>                                151                                151 
#>          prikket_manedslonn_median    prikket_manedslonn_ovre_kvartil 
#>                                151                                151 
#>        prikket_avtalt_gjennomsnitt       prikket_avtalt_nedre_kvartil 
#>                                151                                151 
#>              prikket_avtalt_median        prikket_avtalt_ovre_kvartil 
#>                                151                                151 
#>         prikket_bonus_gjennomsnitt        prikket_bonus_nedre_kvartil 
#>                                151                                151 
#>               prikket_bonus_median         prikket_bonus_ovre_kvartil 
#>                                151                                151 
#>       prikket_uregtil_gjennomsnitt      prikket_uregtil_nedre_kvartil 
#>                                151                                151 
#>             prikket_uregtil_median       prikket_uregtil_ovre_kvartil 
#>                                151                                151 
#>       prikket_overtid_gjennomsnitt      prikket_overtid_nedre_kvartil 
#>                                151                                151 
#>             prikket_overtid_median       prikket_overtid_ovre_kvartil 
#>                                151                                151 
#>                krav_ikke_offentlig                   krav_dominant1_n 
#>                                117                                 12 
#>                   krav_dominant2_n                   krav_dominant1_l 
#>                                 12                                 12 
#>                   krav_dominant2_l                      krav_dominant 
#>                                 12                                 12 
#>                      krav_n_within                         krav_istot 
#>                                  1                                104 
#>                   krav_grenseverdi                  krav_grenseverdi2 
#>                                151                                  2 
#>                       krav_primary                      krav_primary2 
#>                                  8                                  5 
#>                    krav_suppressed                   krav_suppressed2 
#>                                 30                                 10 
#>                     krav_secondary                    krav_secondary2 
#>                                 22                                  8 
#>                          krav_lonn                krav_arbeidsforhold 
#>                                151                                153 
#>                    krav_ekvivalent 
#>                                156 


####  Bruk av formula_selection + long_sdclonn  ####

s2a <- formula_selection(out2, ~yrke1 * sektor3 * arb_heldeltid)
s2b <- formula_selection(out2, ~yrke3 * arb_heldeltid)
s2a_long <- long_sdclonn(s2a)
s2b_long <- long_sdclonn(s2b)


####  Bruk av formula + formula_selection + long_sdclonn  ####

out5 <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn 
                     + (arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
                 within = c("pers_kjoenn", "arb_heldeltid"),
                 var_med_privat = "sektor3", 
                 k1 = 80, k2 = 90)
#> [extend0 5000*39->5217*40] [pre_aggregate 5217*41->800*16] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................

s5a <- formula_selection(out5, ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn)
s5b <- formula_selection(out5, ~(arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn)
s5a_long <- long_sdclonn(s5a)
s5b_long <- long_sdclonn(s5b)


####  Bruk av suppressed_data.   #### 
#   out7 og out8 er som out5, uten alt samtidig 

out6 <- sdc_lonn(a, formula = ~arb_fylke * yrke1 * arb_heldeltid * pers_kjoenn 
                     + arb_fylke * yrke1 * pers_kjoenn, # Overflødig linje, men skader ikke 
                 within = c("pers_kjoenn", "arb_heldeltid"), 
                 var_med_privat = "sektor3", 
                 k1 = 80, k2 = 90,
                 dim_var_extra = c("yrke2", "pers_kommnr"),)
#> [extend0 5000*39->5045*40] [pre_aggregate 5045*41->232*14] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: ..........................

out7 <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn, 
                 within = c("pers_kjoenn", "arb_heldeltid"), 
                 var_med_privat = "sektor3", 
                 dim_var_extra = c("yrke2", "pers_kommnr"),
                 k1 = 80, k2 = 90, 
                 suppressed_data = out6)
#> [extend0 5000*39->5124*40] [pre_aggregate 5124*41->488*15] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................

out8 <- sdc_lonn(a, formula = ~(arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
                 within = "pers_kjoenn", 
                 var_med_privat = "sektor3", 
                 dim_var_extra = c("yrke2", "pers_kommnr"),
                 k1 = 80, k2 = 90, 
                 suppressed_data = out6)
#> [extend0 5000*39->5018*40] [pre_aggregate 5018*41->186*14] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .............
#> Warning: 2 (0 ordinary, 0 extra, 2 singleton) unsafe primary cells due to forced cells
#> ............
#> GaussSuppression_anySum_numttH: ..........................
     
# out5 på nytt med tre datasett som suppressed_data                   
out5b <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn 
                     + (arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
                 within = c("pers_kjoenn", "arb_heldeltid"),
                 var_med_privat = "sektor3",
                 k1 = 80, k2 = 90,
                 suppressed_data = list(out6, out7, out8))                
#> [extend0 5000*39->5217*40] [pre_aggregate 5217*41->800*16] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................
                 

###############################################################
#  Bruk av double_vars, use_diff_groups og double_priority
###############################################################

# Genererer først noen doble yrke-koder 
# som kan tenkes på som nace-koder

a <- sdclonn_data("syntetisk_5000")
a$yrke11 <- a$yrke1
a$yrke22 <- a$yrke2
a$yrke33 <- a$yrke3
ind = 1:22
a$yrke11[ind] <-paste0("x", a$yrke1[rev(ind)])
a$yrke22[ind] <-paste0("x", a$yrke2[rev(ind)])
a$yrke33[ind] <-paste0("x", a$yrke3[rev(ind)])


out9a  <- sdc_lonn(a, between = ~yrke333 + (yrke222 + yrke111) * sektor3, 
                   within = ~arb_heldeltid, k1 = 85, k2 = 95, 
                   double_vars = list(
                   yrke111 = c("yrke1", "yrke11"),
                   yrke222 = c("yrke2", "yrke22"), 
                   yrke333 = c("yrke3", "yrke33")), 
                   use_diff_groups = TRUE, 
                   double_priority = 1)
#> [extend0 5000*46->5023*47] [pre_aggregate 5023*48->156*21] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: .........................
 
out9b  <- sdc_lonn(a, between = ~yrke333 + (yrke222 + yrke111) * sektor3, 
                   within = ~arb_heldeltid, k1 = 85, k2 = 95, 
                   double_vars = list(
                   yrke111 = c("yrke1", "yrke11"),
                   yrke222 = c("yrke2", "yrke22"), 
                   yrke333 = c("yrke3", "yrke33")), 
                   use_diff_groups = "extra", 
                   double_priority = 2)                    
#> [extend0 5000*68->5023*69] [pre_aggregate 5023*70->156*43] [ModelMatrix] 
#> [dummy_aggregate.........................] 
#> GaussSuppression_anySum_numttH: .........................
#> GaussSuppression_anySum_numttH: ..........................
                     
SSBtools::FormulaSelection(out9a, ~yrke1 * sektor3)[c(1:5, 28)]
#>     yrke3 yrke33 sektor3 arb_heldeltid antall_arbeidsforhold
#> 1   Total  Total   Total         Total                  5000
#> 90      1  Total   Total         Total                   165
#> 91      2  Total   Total         Total                  1757
#> 92      3  Total   Total         Total                   941
#> 93      5  Total   Total         Total                  1585
#> 94      7  Total   Total         Total                   174
#> 95      9  Total   Total         Total                   378
#> 107 Total  Total kommune         Total                  2668
#> 108 Total  Total  privat         Total                  1427
#> 109 Total  Total    stat         Total                   905
#> 223     1  Total kommune         Total                   134
#> 224     1  Total  privat         Total                     0
#> 225     1  Total    stat         Total                    31
#> 226     2  Total kommune         Total                   943
#> 227     2  Total  privat         Total                   420
#> 228     2  Total    stat         Total                   394
#> 229     3  Total kommune         Total                   419
#> 230     3  Total  privat         Total                   451
#> 231     3  Total    stat         Total                    71
#> 232     5  Total kommune         Total                   819
#> 233     5  Total  privat         Total                   484
#> 234     5  Total    stat         Total                   282
#> 235     7  Total kommune         Total                   118
#> 236     7  Total  privat         Total                     0
#> 237     7  Total    stat         Total                    56
#> 238     9  Total kommune         Total                   235
#> 239     9  Total  privat         Total                    72
#> 240     9  Total    stat         Total                    71
#>     prikket_antall_arbeidsforhold
#> 1                            5000
#> 90                            165
#> 91                           1757
#> 92                            941
#> 93                           1585
#> 94                            174
#> 95                            378
#> 107                          2668
#> 108                          1427
#> 109                           905
#> 223                           134
#> 224                            NA
#> 225                            NA
#> 226                           943
#> 227                           420
#> 228                           394
#> 229                           419
#> 230                           451
#> 231                            NA
#> 232                           819
#> 233                           484
#> 234                           282
#> 235                           118
#> 236                            NA
#> 237                            NA
#> 238                           235
#> 239                            NA
#> 240                            NA
SSBtools::FormulaSelection(out9b, ~yrke1 * sektor3)[c(1:5, 28)]
#>     yrke3 yrke33 sektor3 arb_heldeltid antall_arbeidsforhold
#> 1   Total  Total   Total         Total                  5000
#> 90      1  Total   Total         Total                   165
#> 91      2  Total   Total         Total                  1757
#> 92      3  Total   Total         Total                   941
#> 93      5  Total   Total         Total                  1585
#> 94      7  Total   Total         Total                   174
#> 95      9  Total   Total         Total                   378
#> 107 Total  Total kommune         Total                  2668
#> 108 Total  Total  privat         Total                  1427
#> 109 Total  Total    stat         Total                   905
#> 223     1  Total kommune         Total                   134
#> 224     1  Total  privat         Total                     0
#> 225     1  Total    stat         Total                    31
#> 226     2  Total kommune         Total                   943
#> 227     2  Total  privat         Total                   420
#> 228     2  Total    stat         Total                   394
#> 229     3  Total kommune         Total                   419
#> 230     3  Total  privat         Total                   451
#> 231     3  Total    stat         Total                    71
#> 232     5  Total kommune         Total                   819
#> 233     5  Total  privat         Total                   484
#> 234     5  Total    stat         Total                   282
#> 235     7  Total kommune         Total                   118
#> 236     7  Total  privat         Total                     0
#> 237     7  Total    stat         Total                    56
#> 238     9  Total kommune         Total                   235
#> 239     9  Total  privat         Total                    72
#> 240     9  Total    stat         Total                    71
#>     prikket_antall_arbeidsforhold
#> 1                            5000
#> 90                            165
#> 91                           1757
#> 92                            941
#> 93                           1585
#> 94                             NA
#> 95                             NA
#> 107                          2668
#> 108                          1427
#> 109                           905
#> 223                           134
#> 224                            NA
#> 225                            NA
#> 226                           943
#> 227                           420
#> 228                           394
#> 229                            NA
#> 230                            NA
#> 231                            NA
#> 232                           819
#> 233                           484
#> 234                           282
#> 235                            NA
#> 236                            NA
#> 237                            NA
#> 238                           235
#> 239                            NA
#> 240                            NA
SSBtools::FormulaSelection(out9b, ~yrke11 * sektor3)[c(1:5, 28)]
#>     yrke3 yrke33 sektor3 arb_heldeltid antall_arbeidsforhold
#> 1   Total  Total   Total         Total                  5000
#> 96  Total      1   Total         Total                   165
#> 97  Total      2   Total         Total                  1750
#> 98  Total      3   Total         Total                   936
#> 99  Total      5   Total         Total                  1580
#> 100 Total      7   Total         Total                   171
#> 101 Total      9   Total         Total                   376
#> 102 Total     x2   Total         Total                     7
#> 103 Total     x3   Total         Total                     5
#> 104 Total     x5   Total         Total                     5
#> 105 Total     x7   Total         Total                     3
#> 106 Total     x9   Total         Total                     2
#> 107 Total  Total kommune         Total                  2668
#> 108 Total  Total  privat         Total                  1427
#> 109 Total  Total    stat         Total                   905
#> 241 Total      1 kommune         Total                   134
#> 242 Total      1  privat         Total                     0
#> 243 Total      1    stat         Total                    31
#> 244 Total      2 kommune         Total                   941
#> 245 Total      2  privat         Total                   417
#> 246 Total      2    stat         Total                   392
#> 247 Total      3 kommune         Total                   417
#> 248 Total      3  privat         Total                   448
#> 249 Total      3    stat         Total                    71
#> 250 Total      5 kommune         Total                   816
#> 251 Total      5  privat         Total                   482
#> 252 Total      5    stat         Total                   282
#> 253 Total      7 kommune         Total                   115
#> 254 Total      7  privat         Total                     0
#> 255 Total      7    stat         Total                    56
#> 256 Total      9 kommune         Total                   234
#> 257 Total      9  privat         Total                    71
#> 258 Total      9    stat         Total                    71
#> 259 Total     x2 kommune         Total                     5
#> 260 Total     x2  privat         Total                     2
#> 261 Total     x2    stat         Total                     0
#> 262 Total     x3 kommune         Total                     3
#> 263 Total     x3  privat         Total                     1
#> 264 Total     x3    stat         Total                     1
#> 265 Total     x5 kommune         Total                     2
#> 266 Total     x5  privat         Total                     2
#> 267 Total     x5    stat         Total                     1
#> 268 Total     x7 kommune         Total                     1
#> 269 Total     x7  privat         Total                     2
#> 270 Total     x7    stat         Total                     0
#> 271 Total     x9 kommune         Total                     0
#> 272 Total     x9  privat         Total                     2
#> 273 Total     x9    stat         Total                     0
#>     prikket_antall_arbeidsforhold
#> 1                            5000
#> 96                            165
#> 97                           1750
#> 98                            936
#> 99                           1580
#> 100                           171
#> 101                           376
#> 102                            NA
#> 103                            NA
#> 104                            NA
#> 105                            NA
#> 106                            NA
#> 107                          2668
#> 108                          1427
#> 109                           905
#> 241                           134
#> 242                            NA
#> 243                            NA
#> 244                           941
#> 245                           417
#> 246                           392
#> 247                           417
#> 248                           448
#> 249                            NA
#> 250                           816
#> 251                           482
#> 252                           282
#> 253                           115
#> 254                            NA
#> 255                            NA
#> 256                           234
#> 257                            NA
#> 258                            NA
#> 259                            NA
#> 260                            NA
#> 261                            NA
#> 262                            NA
#> 263                            NA
#> 264                            NA
#> 265                            NA
#> 266                            NA
#> 267                            NA
#> 268                            NA
#> 269                            NA
#> 270                            NA
#> 271                            NA
#> 272                            NA
#> 273                            NA
```
