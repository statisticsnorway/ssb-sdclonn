
#' Undertrykking i lønnsstatistikk
#' 
#' Input er mikrodata og output er (samordnede) tabell(er) med alle ønskede aggregeringsnivåer. 
#' 
#' Grunnen til at primary2 og secondary2 ikke nødvendigvis summeres til suppressed2 er 
#' at det i primary2 kan inngå celler som er med i suppressed. Disse er fjernet fra suppressed2.
#' 
#' 
#' |  **Når regel_within_ er starten på** | **variabelnavnet**                                                                                                                    |
#' |--------------------------:|:--------------------------------------------------------------------------------------------------------------------|
#' |                 `n_unique`|Antall unike foretak                                                                                                 |
#' |                 `n_privat`|Antall unike private foretak                                                                                         |
#' |              `n_offentlig`|Antall unike private offentlige foretak                                                                              |
#' |                    `max_n`|Antall arbeidsforhold til foretaket med flest.                                    |
#' |                           |Dette foretaket er `max_frtk_n`.                                     |
#' |             `second_max_n`|Antall arbeidsforhold til foretaket med nest flest.                         |
#' |                           |Dette foretaket er `second_max_frtk_n`.                         |
#' |             `max_privat_n`|Antall arbeidsforhold til foretaket med flest blant de private.             |
#' |                           |Dette foretaket er `max_frtk_privat_n`.             |
#' |      `second_max_privat_n`|Antall arbeidsforhold til foretaket med nest flest blant de private. |
#' |                           |Dette foretaket er `second_max_frtk_privat_n`. |
#' |               `max_frtk_n`|Se over                                                                                                              |
#' |        `second_max_frtk_n`|Se over                                                                                                              |
#' |        `max_frtk_privat_n`|Se over                                                                                                              |
#' | `second_max_frtk_privat_n`|Se over                                                                                                              |
#' |          `max_is_privat_n`|Hvorvidt `max_frtk_n` er et privat foretak.                                     |
#' |   `second_max_is_privat_n`|Hvorvidt `second_max_frtk_n` er et privat foretak.                              |
#' |                    `sum_n`|Totalt antall arbeidsforhold.                                                                                     |
#' |                   `priv_n`|Antall arbeidsforhold hos private foretak.                                                                        |
#' |                    `max_l`|Sum lonn til foretaket med hoyest sum. Dette foretaket er `max_frtk_l`.                                           |
#' |             `second_max_l`|osv. ...                                                                                                          |
#' |             `max_privat_l`|...                                                                                                               |
#' |      `second_max_privat_l`|...                                                                                                               |
#' |               `max_frtk_l`|...                                                                                                               |
#' |        `second_max_frtk_l`|...                                                                                                               |
#' |        `max_frtk_privat_l`|...                                                                                                               |
#' | `second_max_frtk_privat_l`|...                                                                                                               |
#' |          `max_is_privat_l`|...                                                                                                               |
#' |   `second_max_is_privat_l`|...                                                                                                               |
#' |                    `sum_l`|Sum lonn total                                                                                                    |
#' |                   `priv_l`|Sum lonn hos private foretak.                                                                                     |
#' |         `n_arbeidsforhold`|Totalt antall arbeidsforhold.  (`= sum_n`, men beregnet separat fra dominansrutinen)                                                                                        |
#' |                 `okantall`|Nøyaktig samme variabel som `"okantall_arbeidsforhold".`                                |
#'
#'
#' **Variablene som starter med  regel_**  er avledet fra `regel_within`_-variablene på følgende måte:
#' *	`regel_within_` ser på hver kombinasjon av within-variabler (som kjønn og alder). 
#'    I `regel_` er det bare totalverdiene for within-variablene som gjelder. 
#'    F.eks. antall heltidsekvivalenter er da ikke for spesifikk kombinasjon av kjønn og alder men total-verdi for between-variablene man ser på. 
#'    F.eks  en kombinasjon av arb_yrke_styrk08 og sektor3. I `regel_within_` er dermed mange rader like.  
#' *	Nye variabler er lagt til 
#'     *	`regel_istot`: Denne er 1 nå det er en snakk om en rad der det er totaler for within-variablene. 
#'                       For disse radene er `regel_`-variablene og `regel_within_`-variablene like. 
#'     *	`regel_dominant1_n`: Beregnet fra `regel_`-variablene som `max_n/sum_n`.
#'     *	`regel_dominant2_n`: Beregnet fra `regel_`-variablene som `(max_n + second_max_n)/sum_n`.
#'     *	`regel_dominant1_l`: Beregnet fra `regel_`-variablene som `max_l/sum_l`.
#'     *	`regel_dominant2_l`: Beregnet fra `regel_`-variablene som `(max_l + second_max_l)/sum_l`.
#'
#' 
#' |**krav_ i starten på**| **variabelnavnet** forteller om spesifikke regler for prikking er oppfylt |
#' |--------------:|:--------------------------------------------------------------------------------------|
#' |`ikke_offentlig`|= `max_is_privat` OR (`n_offentlig == 1` AND `n_privat == 1`)                            |
#' |    `dominant1_n`|= `regel_dominant1_n > k1` AND `ikke_offentlig` der `k1` er parameter i dominansregel      |
#' |    `dominant2_n`|= `regel_dominant2_n > k2` AND `ikke_offentlig` der `k2` er parameter i dominansregel      |
#' |    `dominant1_l`|= `regel_dominant1_l > k1` AND `ikke_offentlig` der `k1` er parameter i dominansregel      |
#' |    `dominant2_l`|= `regel_dominant2_l > k2` AND `ikke_offentlig` der `k2` er parameter i dominansregel      |
#' |       `dominant`|= `dominant1_n` OR `dominant2_n` OR `dominant1_l` OR `dominant2_l`                     |
#' |     `n_within`|= `!regel_istot` AND `regel_within_n_arbeidsforhold <= freq_within`                        |
#' |               | Dette er krav til primærprikking av Antall arbeidsforhold "within" basert på frekvensregel. |
#' |               | `freq_within` er input-parameter, 2 er default.                        |
#' |        `istot`|= `regel_istot`                                                                        |
#' |  `grenseverdi`|= `regel_within_okantall < grenseverdi`   (altså krav_grenseverdi = ...)               |
#' |               | `grenseverdi` er input-parameter, 100 er default. (altså ikke krav_grenseverdi)               |
#' | `grenseverdi2`|= `regel_within$n_arbeidsforhold > 0 & regel_within_okantall < grenseverdi2`           |
#' |      `primary`|= (`!istot` AND `dominant`) OR `n_within.`                                               |
#' |               | Dette er primærprikking til grunn for føreste runde med sekundærprikking. |
#' |     `primary2`|= (`istot` AND `dominant`)  OR  `grenseverdi2_krav`                                                               |
#' |               | Dette er primærprikking til grunn for andre runde med sekundærprikking.    |
#' |    `secondary`|Resultat fra første runde med sekundærprikking.                                        |
#' |   `secondary2`|Resultat fra andre runde med sekundærprikking.                                         |
#' |   `suppressed`|All prikking i første runde.                                                           |
#' |  `suppressed2`|All prikking fra andre runde bortsett fra at mulig overlapp med suppressed er fjernet. |
#' |         `lonn`|= `grenseverdi` OR `dominant`                                        |
#' |`arbeidsforhold`|= `suppressed` OR `lonn`                                                        |
#' |   `ekvivalent`|= `suppressed` OR `suppressed2` OR `lonn`          
#' 
#' * Disse krav-variablene er egentlig logiske (TRUE/FALSE), men i output blir dette 0/1.
#' * `krav_arbeidsforhold` og `krav_ekvivalent` samsvarer nøyaktig med endelig prikking av tilsvarende variabler.  
#' * `krav_lonn` samsvarer med endelig prikking av lønnsvariablene (median, snitt og kvartiler).  
#' * Her er det skrevet OR og AND istedenfor vanlige r-kode operatorer som er `|` og `&`.
#'   (Det var ikke mulig med tegnet  `|` i tabellen over p.g.a kombinasjon markdown/roxygen/latex.) 
#' * Operatoren `!` betyr "ikke".
#'
#' @param data input data eller resultat fra `output = "aggregated"`
#' @param between Variabler som grupperer foretak eller en formel for disse. 
#'                Se også parameteren `formula` som er et alternativ. 
#' @param within Variabler innen foretak eller en formel for disse.  
#' @param var_vektet Lønnsvariabler navngitt ved navn som skal brukes i output. Ved manglende navn brukes variabelnavnet direkte. 
#' @param var_uvektet Som `var_vektet` over, men disse variablene beregnes uvektet. 
#'                    Default (`NULL`) er at ingen slike variabler er med. 
#' @param privat           Privatkoden som brukes. Default er `"privat"`.        
#' @param var_med_privat  Variabel (sektor) med privatkoden. 
#'                        Default (`NULL`) betyr at det letes automatisk gjennom tabellvariabler (between) for å finne privat-koden.  
#' @param var_frtk_id Variabelnavn for id for foretak. Default er `"frtk_id_ssb"`.
#' @param var_virk_id Variabelnavn for id for virksomhet.Default er `"virk_id_ssb"`.
#' @param var_ekv_vekt Variabelnavn for vekt for heltidsekvivalenter. Default er `"lonn_ekv_vekt",`.
#' @param var_dominans Variabelnavn for variabel som er utganspunkt for dominansberegning. 
#'                     Default er første `var_vektet`-variabel.
#'                     Dominansberegningen vekter uansett denne variabelen.  
#' @param var_sum   Ekstra numeriske variabler Som bare skal summeres, 
#'                  navngitt ved navn som skal brukes i output. 
#'                  Tilsvarende som `var_vektet` over.
#' @param freq_within    max-verdi for primærprikking av Antall arbeidsforhold "within". 
#'                       Se tekst (n_within) i  \code{\link{sdc_lonn_extra_details}}.
#' @param between_regel  Ved TRUE er "between" utgangspunkt for dominansregler 
#' @param k1 Parameter i dominansregler (en prosentverdi). TRUE betyr vanlig verdi.    
#' @param k2 Parameter i dominansregler (en prosentverdi). TRUE betyr vanlig verdi. 
#' @param secondary2 **Når `secondary2` er `TRUE`** sekundærprikkes det i to omganger.
#'   * I første omgang sekundærprikkes det ikke med tanke 
#'          på å hindre tilbakeregning  av between-totaler av Antall arbeidsforhold 
#'          ut fra dominansregler (`k1` og `k2`), men kun med tanke på within (f.eks. kjønn). 
#'          
#'   * I andre omgang sekundærprikkes det også med tanke between-totaler som beskrevet over.
#'          Det er antall heltidsekvivalenter som blir sekundærprikket ut fra dette.
#'   
#'   Poenget med to runder er publisering av gjennomsnittsverdier og antall heltidsekvivalenter som må beskyttes av dominansregler. 
#'   Gjennomsnittsverdier kan tilbakeregnes  via summer (gjennomsnitt * antall heltidsekvivalenter) 
#'   dersom både gjennomsnitt og antall heltidsekvivalenter er tilgjengelig. 
#'   Dette hindres altså av sekundærprikkingen av antall heltidsekvivalenter.
#'   
#'   **Når `secondary2` er `FALSE`** fortas sekundærprikking bare en gang uten tanke på at antall 
#'          arbeidsforhold for between-totaler ikke må beskyttes.  
#'          
#' @param grenseverdi Minste antall ok arbeidsforhold for publisering av 
#'                    median, kvartiler og gjennomsnittslønn, 
#'                    samt andre variabler som antall arbeidsforhold og antall heltidsekvivalenter.
#'                    (ingen sekundærprikking).
#'                     Se \code{\link{sdc_lonn_extra_details}}.
#' @param grenseverdi2 Minste antall ok arbeidsforhold som ikke primærprikkes (`primary2`) forut for andre runde sekundærprikking.
#'                     Se \code{\link{sdc_lonn_extra_details}}.
#' @param extend0 Ved noe annet enn `extend0 = TRUE`  kjøres `Extend0` i SSBtools. Trengs for beskyttelse av "within"-0-ere. 
#'   * **`extend0 = TRUE`** (default): Innen hver between-gruppen så sjekkes det om alle kombinasjoner av within-variablene som fra før 
#'                          finnes i dataene også fins i den bestemte between-gruppen. Hvis ikke legges det til observasjoner med 0.
#'                          `FALSE` og også `TRUE` kan gi warning som kan slås av med parameteren `structuralEmpty` (se nedenfor).
#'   * **`extend0 = "full"`**: I motsetning til `TRUE` så legges det også til kombinasjoner som ikke fins i dataene fra før. 
#'                             Ved `avoidHierarchical = FALSE` (default) respekteres hierarkiske sammenhenger slik at ikke 
#'                             "umulige" kombinasjoner legges til. Ved `avoidHierarchical = TRUE` legges det til alle mulige kombinasjoner. 
#'   * **`extend0` som liste**: Avansert spesifisering av hvordan within-variablene kombineres. 
#'                              Eks: `extend0 <- list("A", "B", c("C", "D"))`. 
#'                              Det legges til alle mulige kombinasjoner av `A`, `B` og `(C`-`D`-kombinasjoner som fins i dataene fra før`)`.  
#'                
#' @param removeEmpty Ved TRUE fjernes between-kombinasjoner der det ikke finnes data. Dette er en parameter til ModelMatrix i SSBtools. 
#' @param formula En formel som inkluderer både between- og within-variablene. 
#'                Parameteren `within` brukes da i tillegg, mens `between`-parameteren ikke er i bruk (ignoreres). 
#' @param output Mulighet for annen output. 
#' @param suppressed_data Outputdata fra en annen kjøring av `sdc_lonn`. Prikkingen skal samordnes slik at 
#'                        kombinasjoner som fins i `suppressed_data` prikkes på samme måte. 
#'                        Det blir nye variabler i output, `krav_inputsupp` og `krav_inputsupp2`.  
#'                        Disse forteller om prikking som er samordnet. Verdiene ved samordning 
#'                        er 0 (ikke prikket) eller 1 (prikket) og ellers missing. 
#'                        Overlapp mellom prikkingene i de to rundene er ikke fjernet.  
#'                        For at dette skal virke må man passe på at like variabler får samme navn. 
#'                        Se `dim_var_extra` nedenfor.  
#'                        Ved bruk av `list` kan flere datasett være input. F.eks.:  
#'                        `suppressed_data = list(out6, out7, out8)` 
#'                        
#' @param dim_var_extra Ekstra dimensjonsvariabler (som i `between`, `within`, `formula`) som skal tas hensyn til ved 
#'                      navngiving av outputvariabler. Dette for å sikre like navn ved bruk av `suppressed_data`. 
#'                      Det er ok med variabler som ikke får betydning (fins fra før i `between`, `within`, `formula` 
#'                      eller ikke relatert til disse). 
#' @param avoidHierarchical Ved TRUE og ved formelinput kombineres ikke hierarkiske variabler i output. 
#'                          Dette er en parameter til ModelMatrix i SSBtools. 
#'                          Om dette brukes blir `dim_var_extra` ignorert siden dette da ikke gir mening.   
#'                          Se også parameteren `extend0`.
#' @param structuralEmpty  When `TRUE`, output cells with no contributing inner cells (only zeros in column of `x`) 
#'                         are forced to be not primary suppressed. 
#'                         Thus, these cells are considered as structural zeros. 
#'                         When `structuralEmpty` is `TRUE`, the following error message is avoided: 
#'                         `Suppressed` `cells` `with` `empty` `input` `will` `not` `be` `protected.` 
#'                         `Extend` `input` `data` `with` `zeros?`.    
#'                         When `removeEmpty` is `TRUE`, `structuralEmpty` is superfluous.
#'                         Parameteren og beskrivelsen er kopiert fra `GaussSuppression::GaussSuppressionFromData`.
#'                         Bruk av `suppressed_data` kan føre til at celler allikevel blir primærprikket.  
#' @param run_gauss  Ingen sekundærprikking dersom denne settes til `FALSE`, Default er `TRUE`. 
#' @param roundBase Ved andre verdier enn 0 foretas avrunding av antall arbeidsforhold. 
#'                  Funksjonen  \code{\link{PLSroundingPublish}} bruker da denne `roundBase`-parameteren.
#'                  Ny ekstra variabel helt til slutt i output er `rounded_antall_arbeidsforhold`.
#'                  Variabelen `prikket_antall_arbeidsforhold` blir endret til å inneholde de samme avrundede verdiene, 
#'                  bortsett fra at noen av dem er prikket.     
#'                  Denne prikkingen endres også slik at dette kun handler om dominansregel. 
#'                  Teknisk har da `n_within_krav` blitt flyttet til andre runde prikking (fra  `primary` til `primary2`).
#'                  Det sørges også for ingen primærprikking i første runde ved 
#'                  `rounded_antall_arbeidsforhold` `<=` `roundBase`.
#'                  Litt modifisert blir også `krav_grenseverdi` ved at det også kreves 
#'                  `rounded_antall_arbeidsforhold` `<` `grenseverdi`
#'                   i tillegg til at, som vanlig, `regel_within_okantall` `<` `grenseverdi`.
#' @param roundMultiple Når dette heltalltallet er større enn 1, vil avrunding også sørge for at 
#'                  alle tall < `roundBase` `*` `roundMultiple` er `roundBase`-multipler. 
#'                  F.eks. ved `roundBase`=3 og `roundMultiple`=2 så vil 1,2,4 og 5 
#'                  ikke forekomme i avrundet output, mens 0, 3 og 6 forekommer.  
#'                   Det sørges også for ingen primærprikking i første runde ved 
#'                  `rounded_antall_arbeidsforhold` `<=` `roundBase` `*` `roundMultiple`.            
#'                   
#' @param verbose Whether to print information during calculations.
#' 
#' @seealso Se ekstra detaljer: \code{\link{sdc_lonn_extra_details}}.
#'
#' @return  data frame eller liste 
#' @export
#' @importFrom SSBtools Extend0 model_aggregate GaussSuppression HierarchicalGroups2 quantile_weighted
#' @importFrom GaussSuppression CandidatesDefault PrimaryFromSuppressedData ForcedFromSuppressedData
#' @importFrom SmallCountRounding PLSroundingPublish 
#' @importFrom stats aggregate weighted.mean quantile setNames
#' @importFrom Matrix rowSums colSums
#' @importFrom utils flush.console
#'
#' @examples
#'
#' # Siden default er fjernet i ny versjon 
#' between <- c("yrke1", "yrke2", "yrke3", "arb_yrke_styrk08", "sektor3")
#' within <- c("pers_kjoenn", "arb_heldeltid")
#'
#' a <- sdclonn_data("syntetisk_5000")
#' out <- sdc_lonn(a, between = between, within = within, k1 = 85, k2 = 95)
#' out2 <- sdc_lonn(a, between = ~yrke3 + (yrke2 + yrke1) * sektor3,  
#'                  within = ~arb_heldeltid, k1 = 85, k2 = 95)
#' 
#' # Med avrunding. Altså bruk av roundBase-parameteren
#' outR <- sdc_lonn(a, between = between, within = within, roundBase = 3,
#'                  k1 = 85, k2 = 95)
#' 
#' out2A <- sdc_lonn(a, between = ~yrke3 + (yrke2 + yrke1) * sektor3, within = ~arb_heldeltid, 
#'                   output = "aggregated")
#' out2B <- sdc_lonn(out2A, k1 = 85, k2 = 95)
#' identical(out2, out2B)
#' out2C <- sdc_lonn(out2A, k1 = 80, k2 = 90)
#' identical(out2, out2C)
#' 
#' out3 <- sdc_lonn(a, grenseverdi = 20, between = between, within = within,
#'                  var_uvektet = c(stillingspst = "arb_stillingspst", alder = "pers_alder"),
#'                  k1 = 80, k2 = 90)
#' 
#' out4 <- sdc_lonn(a, between = 
#'   ~(yrke3 + (yrke2 + yrke1) * sektor3) + pers_kommnr * (yrke2 + yrke1) + arb_fylke * yrke1, 
#'   within = ~arb_heldeltid * pers_kjoenn, output = "aggregated")
#'   
#' out4A <- sdc_lonn(out4, k1 = 90, k2 = 95)
#' out4B <- sdc_lonn(out4, k1 = 80, k2 = 90)
#' out4C <- sdc_lonn(out4, k1 = 90, k2 = 95, grenseverdi = 20)
#' 
#' ### Skriver ut antall prikker eller antall der krav-variabel er TRUE.
#' sum2(out)
#' sum2(out3)
#' sum2(out4A)
#' sum2(out4B)
#' sum2(out4C)
#' sum2(out2)
#' sum2(out2C)
#' 
#' 
#' ####  Bruk av formula_selection + long_sdclonn  ####
#' 
#' s2a <- formula_selection(out2, ~yrke1 * sektor3 * arb_heldeltid)
#' s2b <- formula_selection(out2, ~yrke3 * arb_heldeltid)
#' s2a_long <- long_sdclonn(s2a)
#' s2b_long <- long_sdclonn(s2b)
#' 
#' 
#' ####  Bruk av formula + formula_selection + long_sdclonn  ####
#' 
#' out5 <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn 
#'                      + (arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
#'                  within = c("pers_kjoenn", "arb_heldeltid"),
#'                  var_med_privat = "sektor3", 
#'                  k1 = 80, k2 = 90)
#' 
#' s5a <- formula_selection(out5, ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn)
#' s5b <- formula_selection(out5, ~(arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn)
#' s5a_long <- long_sdclonn(s5a)
#' s5b_long <- long_sdclonn(s5b)
#' 
#' 
#' ####  Bruk av suppressed_data.   #### 
#' #   out7 og out8 er som out5, uten alt samtidig 
#' 
#' out6 <- sdc_lonn(a, formula = ~arb_fylke * yrke1 * arb_heldeltid * pers_kjoenn 
#'                      + arb_fylke * yrke1 * pers_kjoenn, # Overflødig linje, men skader ikke 
#'                  within = c("pers_kjoenn", "arb_heldeltid"), 
#'                  var_med_privat = "sektor3", 
#'                  k1 = 80, k2 = 90,
#'                  dim_var_extra = c("yrke2", "pers_kommnr"),)
#' 
#' out7 <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn, 
#'                  within = c("pers_kjoenn", "arb_heldeltid"), 
#'                  var_med_privat = "sektor3", 
#'                  dim_var_extra = c("yrke2", "pers_kommnr"),
#'                  k1 = 80, k2 = 90, 
#'                  suppressed_data = out6)
#' 
#' out8 <- sdc_lonn(a, formula = ~(arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
#'                  within = "pers_kjoenn", 
#'                  var_med_privat = "sektor3", 
#'                  dim_var_extra = c("yrke2", "pers_kommnr"),
#'                  k1 = 80, k2 = 90, 
#'                  suppressed_data = out6)
#'      
#' # out5 på nytt med tre datasett som suppressed_data                   
#' out5b <- sdc_lonn(a, formula = ~arb_fylke * (yrke1 + yrke2) * arb_heldeltid * pers_kjoenn 
#'                      + (arb_fylke + pers_kommnr) * yrke1 * pers_kjoenn, 
#'                  within = c("pers_kjoenn", "arb_heldeltid"),
#'                  var_med_privat = "sektor3",
#'                  k1 = 80, k2 = 90,
#'                  suppressed_data = list(out6, out7, out8))                
#'                  
#' 
sdc_lonn <- function(data, 
                     between = NULL,
                     within = NULL, 
                     var_vektet = c(manedslonn = "lonn_ekv_ialt_pub", avtalt = "lonn_ekv_fmlonn_pub",
                                  bonus = "lonn_ekv_bonus_pub", uregtil = "lonn_ekv_uregtil_pub",
                                  overtid = "lonn_overtid_pub"), 
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
                     verbose = TRUE){
  
  if (!is.null(dim_var_extra) & avoidHierarchical) {
    dim_var_extra <- NULL
    warning("dim_var_extra ignored when avoidHierarchical")
  }
  
  var_vektet <- fix_char_names(var_vektet)
  noen_fun_vars <- NULL
  for (i in seq_along(var_vektet)) {
    list_i <- list(list(snitt = c(as.vector(var_vektet)[i], var_ekv_vekt)), 
                   list(q = c(as.vector(var_vektet)[i], var_ekv_vekt)))
    
    names(list_i) <- c(paste0(names(var_vektet)[i], "_gjennomsnitt"), 
      paste(paste0(names(var_vektet)[i], 
        c("_nedre_kvartil", "_median", "_ovre_kvartil")), 
        collapse = ","))
    
    noen_fun_vars <- c(noen_fun_vars, list_i)
  }
  
  if (length(var_uvektet)) {
    var_uvektet <- fix_char_names(var_uvektet)
  } 
  for (i in seq_along(var_uvektet)) {
    list_i <- list(list(snitt = c(as.vector(var_uvektet)[i], "is_nonempty")), 
                   list(q = c(as.vector(var_uvektet)[i], "is_nonempty")))
    
    names(list_i) <- c(paste0(names(var_uvektet)[i], "_gjennomsnitt"), 
                       paste(paste0(names(var_uvektet)[i], 
                                    c("_nedre_kvartil", "_median", "_ovre_kvartil")), 
                             collapse = ","))
    
    noen_fun_vars <- c(noen_fun_vars, list_i)
  }
  

  secondary2_input <- secondary2 
  
  if(is.null(output)){
    output = ""
  }
  
  rename_extra <- NULL
  
  ##############################################################################
  ##############################################################################
  if(!is.data.frame(data) & all(c("cross_table", "fun_data", "x") %in% names(data))){
    out <- data
    list2env(out$annet, envir = environment())
  } else {  ########### START - Vanlig input-data. Altså ikke "aggregated" i input.  
  
    
    if (!is.null(formula)) {
      formula <- fix_formula(formula)
      dim_var <- all.vars(formula)
      within_ <- within
      if (!all(within_ %in% dim_var)) {
        stop("All within variables must be included in formula")
      }
      between_ <- dim_var[!(dim_var %in% within_)]
    } else {
      is_formula <- c(inherits(between, "formula"), inherits(within, "formula"))
      if (any(is_formula)) {
        if (any(!is_formula)) {
          stop("Enten må både between og within være formel eller ingen av dem.")
        }
        formula <- multiply_formulas(between, within)
        dim_var <- NULL
        between_ <- all.vars(between)
        within_ <- all.vars(within)
      } else {
        formula <- NULL
        dim_var <- c(between, within)
        between_ <- between
        within_ <- within
      }
    }
    
    if (!is.null(dim_var_extra)) {
      hg <- HierarchicalGroups2(data[dim_var])
      hg_extra <- HierarchicalGroups2(data[unique(c(dim_var, dim_var_extra))])
      rename_extra <- setNames(rep(names(hg_extra), lengths(hg_extra)), unlist(hg_extra, use.names = FALSE))
      if (anyDuplicated(rename_extra[names(hg)])) {
        stop("Spesiell dim_var_extra. Mer programmering trengs.")
      }
    } 
    
    
    if(!is.null(var_med_privat)){
      is_privat <- data[[var_med_privat]] == privat
    } else {
      is_privat <- NULL
      for (var in between_) {
        if (any(privat %in% data[[var]])) {
          is_privat_ <- data[[var]] == privat
          if (!is.null(is_privat)) {
            if (!identical(is_privat_, is_privat)) {
              stop("ikke entydig is_privat")
            }
          }
          is_privat <- is_privat_
        }
      }
      if (is.null(is_privat)) {
        warning("Ingen privat")
        is_privat <- rep(FALSE, nrow(data))
      }
    }
    data$is_privat <- is_privat 
    
    data$unik_frtk <- data[[var_frtk_id]]
    data$unik_frtk[is.na(data$unik_frtk)] <- ""
    data$unik_frtk[data$unik_frtk == ""] <- data[[var_virk_id]][data$unik_frtk == ""]
    
    
    freqName <- rev(make.unique(c(names(data), "freq")))[1] # Dvs "freq" dersom freq ikke fins fra før
    
    if (is.character(extend0)) {
      if (extend0 == "full") {
        if (!avoidHierarchical) {
          extend0 <- HierarchicalGroups2(data[within_])
        } else {
          extend0 <- as.list(within_)
        }
      } else {
        stop("extend0 must be \"full\" when supplied as character")
      }
    }
    if (is.list(extend0)) {
      within__ <- extend0
      if (!identical(as.vector(sort(unlist(within__))), as.vector(sort(within_)))) {
        stop("Alle within-variabler må med i avansert extend0-input")
      }
      extend0 <- TRUE
    } else {
      within__ <- list(within_)
    }
      
    if (extend0) {
      if (verbose) {
        cat("[extend0 ", dim(data)[1], "*", dim(data)[2], "->", sep = "")
        flush.console()
      }
      if (length(within_)) {
        if (length(between_)) {
          data <- Extend0(data, varGroups = c(within__, list(between_)), freqName = freqName)
        } else {
          data <- Extend0(data, varGroups = within__, freqName = freqName)
        }
      } else {
        data <- Extend0(data, varGroups = list(between_), freqName = freqName)
      }
      if (verbose) {
        cat(dim(data)[1], "*", dim(data)[2], "] ", sep = "")
        flush.console()
      }
    } else {
      data[[freqName]] <- 1L
    }
    
    
    # Omkoder til integer. Skal omkode output tilbake seinere 
    unik_frtk_c2i <- character2integer(data$unik_frtk)
    data$unik_frtk <- unik_frtk_c2i$x
    
    # is_nonempty skal brukes som vekt for det som er uvektet 
    data$is_nonempty <- !is.na(data$is_privat)
    
    # Missing etter Extend0 -> FALSE
    data$is_privat[is.na(data$is_privat)] <- FALSE
    
    
    if(get0("force_KuN_lONN_EKV_VEKT_doMiNans", ifnotfound = FALSE)){ # hack for å få gamle svar med LONN_EKV_VEKT
      maxw_n <-  list(max_w2 = c("unik_frtk", var_ekv_vekt, "is_privat"))    # Denne var brukt tidligere 
      maxw_l <-  list(max_w3 = c("unik_frtk", var_ekv_vekt, "is_privat"))    # Dette blir det samme siden vekt ikke sendes inn 
    } else {   
      maxw_n <-  list(max_w2 = c("unik_frtk", freqName, "is_privat"))     
      maxw_l <-  list(max_w3 = c("unik_frtk", var_dominans, "is_privat", var_ekv_vekt))
    }
    
    if (roundBase) {
      sum_vars <- c(freqName, var_sum)
    } else {
      sum_vars <- var_sum
    }
    
    out <- model_aggregate(data, 
                           dim_var = dim_var,
                           formula = formula, 
                           mm_args = list(removeEmpty = removeEmpty, avoidHierarchical = avoidHierarchical), 
                           sum_vars = sum_vars,  
                           fun_vars = c(list(antall_arbeidsforhold      = list(sum = freqName),
                                             okantall_arbeidsforhold      = list(sum = "okantall"),
                                           antall_heltidsekvivalenter = list(sum = var_ekv_vekt)),  # Private_heltidsekvivalenter = list(wsum = c(var_ekv_vekt, "is_privat")),
                                        noen_fun_vars, list(   
                                           n_unique                   = list(n_unique = "unik_frtk"),
                                           n_privat                  = list(n_privat = c("unik_frtk", "is_privat")),
                                           n_offentlig = list(n_offentlig = c("unik_frtk", "is_privat")),
                                           maxw_n = maxw_n,   
                                           maxw_l = maxw_l)),
                           fun = c(q = quantile_w, snitt = weighted.mean, sum = sum, # wsum = weighted_sum,
                                   n_unique = n_unique, n_privat = n_privat, n_offentlig = n_offentlig,  max_w2 =  max_w2, max_w3 =  max_w3), 
                           list_return = TRUE, pre_return = TRUE,
                           inc_progress = verbose, verbose = verbose)
    
    if (output == "model_aggregated") {
      return(out)
    }
    
    # Tilbakeomkoding fra integer/numeric
    for (i in grep("max_frtk", names(out$fun_data))) {
      out$fun_data[[i]] <- integer2character(out$fun_data[[i]], unik_frtk_c2i$x_levels)
    }
    # Tilbakekoder til logical
    for (i in grep("is_privat", names(out$fun_data))) {
      out$fun_data[[i]] <- as.logical(out$fun_data[[i]])
    }
    # Fjerner alt av missing
    out$fun_data <- na_remove(out$fun_data)
    
    # Flytter variabler til regel_within (antall_arbeidsforhold kopieres)
    grepl_maxw_n <- grepl("maxw_n.", names(out$fun_data))
    grepl_maxw_l <- grepl("maxw_l.", names(out$fun_data))
    regel_var <- grepl_maxw_n | grepl_maxw_l | (names(out$fun_data) %in% c("n_unique", "n_privat", "n_offentlig"))
    names(out$fun_data) <- sub("maxw_n.", "", names(out$fun_data))
    names(out$fun_data) <- sub("maxw_l.", "", names(out$fun_data))
    names(out$fun_data)[grepl_maxw_n] <- paste0(names(out$fun_data)[grepl_maxw_n], "_n") 
    names(out$fun_data)[grepl_maxw_l] <- paste0(names(out$fun_data)[grepl_maxw_l], "_l") 
    out$regel_within <- out$fun_data[regel_var]
    out$regel_within$n_arbeidsforhold <- out$fun_data$antall_arbeidsforhold
    out$regel_within$okantall <- out$fun_data$okantall_arbeidsforhold
    
    # Fjerner variabler som er flyttet til regel_within
    out$fun_data <- out$fun_data[!regel_var]
    
    if (output == "aggregated") {
      out$annet <- list(between  = between,  within  = within, 
                        between_ = between_, within_ = within_, 
                        formula = formula)
      return(out)
    }
  } ########### END - Vanlig input-data. Altså ikke "aggregated" i input.
  ##############################################################################
  ##############################################################################
  
  tot_code <- find_tot_code(out$x, out$cross_table)
  
  # TotCodeReplace dersom 'between' utgangspunkt for dominansregler
  if (between_regel) {
    out$regel <- tot_code_replace(cbind(out$cross_table, out$regel_within), 
                                  dim_var = names(out$cross_table), 
                                  tot_code = tot_code[names(tot_code) %in% within_])
  } else {
    out$regel <- out$regel_within
  }
  
  # Dominansregler gjelder bare ikke-offentlig som definert her
  ikke_offentlig_krav_n <- with(out$regel, NA2FALSE(max_is_privat_n) | (n_offentlig == 1 & n_privat == 1))
  ikke_offentlig_krav_l <- with(out$regel, NA2FALSE(max_is_privat_l) | (n_offentlig == 1 & n_privat == 1))
  
  
  if (missing(k1) | missing(k2)) {
    stop("k1 og k2 må spesifiseres")
  }
  
  if (k1 > 0 & k1 <= 1 | k2 > 0 & k2 <= 1) {
    stop("k1 og k2 skal være en prosentverdier, f.eks k1 = 75 og k2 = 85.")
  }
  
  out$regel$dominant1_n <- with(out$regel, 100 * max_n/sum_n)
  out$regel$dominant2_n <- with(out$regel, 100 * (max_n + second_max_n)/sum_n)
  out$regel$dominant1_n[is.na(out$regel$dominant1_n)] <- 0
  out$regel$dominant2_n[is.na(out$regel$dominant2_n)] <- 0
  
  # Dominansregler
  dominant1_n_krav <- with(out$regel, dominant1_n > k1 & ikke_offentlig_krav_n)
  dominant2_n_krav <- with(out$regel, dominant2_n > k2 & ikke_offentlig_krav_n)
  
  out$regel$dominant1_l <- with(out$regel, 100 * max_l/sum_l)
  out$regel$dominant2_l <- with(out$regel, 100 * (max_l + second_max_l)/sum_l)
  out$regel$dominant1_l[is.na(out$regel$dominant1_l)] <- 0
  out$regel$dominant2_l[is.na(out$regel$dominant2_l)] <- 0
  
  # Dominansregler
  dominant1_l_krav <- with(out$regel, dominant1_l > k1 & ikke_offentlig_krav_l)
  dominant2_l_krav <- with(out$regel, dominant2_l > k2 & ikke_offentlig_krav_l)
  
  
  dominant_krav <- dominant1_n_krav | dominant2_n_krav | dominant1_l_krav | dominant2_l_krav 
  

  # primærprikking av Antall arbeidsforhold 'within' Handler om beskyttelse av personer. Within-0-ere prikkes.
  if (between_regel) {
    n_within_krav <- !out$regel$istot & out$regel$n_arbeidsforhold > 0 & out$regel_within$n_arbeidsforhold <= freq_within
  } else {
    warning("Bruk av freq_within sammen med ikke-between_regel primærprikker for mange 0-ere")
    n_within_krav <- !out$regel$istot & out$regel_within$n_arbeidsforhold <= freq_within
  }
  
  grenseverdi2_krav <- out$regel_within$n_arbeidsforhold > 0 &  out$regel_within$okantall < grenseverdi2
  
  if (roundBase) {
    if (verbose) {
      cat("SmallCountRounding: ")
    }
    out$rounded <- PLSroundingPublish(out$pre_sum, 
                                      freqVar = freqName, 
                                      roundBase = roundBase, 
                                      maxRound = roundBase*roundMultiple - 1,
                                      hierarchies = NULL, 
                                      formula = NULL, 
                                      x = out$x, 
                                      crossTable = FALSE,
                                      printInc = verbose)
    
    grenseverdi_krav <- out$regel_within$okantall < grenseverdi | out$rounded[["rounded"]] < grenseverdi
  } else {
    grenseverdi_krav <- out$regel_within$okantall < grenseverdi
  }
  
  
  if ( secondary2_input) {
    if (roundBase) {
      primary  <-  (!(out$regel$istot | out$rounded$rounded <= (roundBase*roundMultiple) ) & dominant_krav) 
      primary2 <-  ( (out$regel$istot | out$rounded$rounded <= (roundBase*roundMultiple) ) & dominant_krav) |  grenseverdi2_krav |  n_within_krav
    } else {
      primary  <-  (!out$regel$istot & dominant_krav) |  n_within_krav
      primary2 <-  ( out$regel$istot & dominant_krav) |  grenseverdi2_krav 
    }
  } else {
    primary <-  dominant_krav |  n_within_krav      |  grenseverdi2_krav
    primary2 <- rep(FALSE, length(primary))
  }
  
  
  # "+ colSums(x)" er triks som er viktig når det ikke er freq-within-prikking.   #### Her endres til bruk av sign() slik at antall_arbeidsforhold er viktigst 
  #Da sørges det for at "secondaryZeros = FALSE" bare setter empty 0-ere først i rekkefølgen, 
  # men 0-ere laget ved extend0 foretrekkes til prikking på vanlig måte. 
  # Denne warningen unngås også: 
  #    "Cells with empty input will never be secondary suppressed. Extend input data with zeros?"
  candidates <- CandidatesDefault(freq = out$fun_data$antall_arbeidsforhold + sign(colSums(out$x)), x = out$x, secondaryZeros = FALSE, weight = NULL)
  
  # Må ha to typer singleton
  # Må gjøre om til integer
  singleton <- rep(NA, nrow(out$pre_data))
  for (i in seq_len(nrow(out$pre_data))) {
    singleton[i] <- length(unique(out$pre_data$unik_frtk[[i]][out$pre_data$is_privat[[i]]])) == 1 & length(unique(out$pre_data$unik_frtk[[i]][!out$pre_data$is_privat[[i]]])) == 0
  }
  
  singleton_freq_candidates <- which(primary & out$fun_data$antall_arbeidsforhold == 0)
  singleton_freq_candidates <- singleton_freq_candidates[colSums(out$x[, singleton_freq_candidates, drop = FALSE]) == 1]
  singleton_freq <- rowSums(out$x[, singleton_freq_candidates, drop = FALSE]) > 0
  
  if (!is.null(rename_extra)) {
    names(out$cross_table) <- rename_extra[names(out$cross_table)]
  }
  
  if (structuralEmpty) {
    colSums_x_0 <- colSums(out$x) == 0
    primary[colSums_x_0] <- FALSE
    primary2[colSums_x_0] <- FALSE
  }
  
  prim1 <- primary
  if (!is.null(suppressed_data)) {
    if (!is.null(dim(suppressed_data))) {   # not list of several suppressed_data
      suppressed_data <- list(suppressed_data)
    }
    forced1 <- rep(FALSE, length(primary))
    forced2 <- rep(FALSE, length(primary))
    inputsupp <- rep(FALSE, length(primary))
    inputsupp2 <- rep(FALSE, length(primary))
    for (i in seq_along(suppressed_data)) {
      totCode <- attr(suppressed_data[[i]], "totCode")
      if (is.null(totCode)) {
        stop("suppressed_data mangler totCode-attributt")
      }
      suppressed_data[[i]] <- suppressed_data[[i]][c(names(totCode), "krav_suppressed", "krav_suppressed2")]
      attr(suppressed_data[[i]], "totCode") <- totCode
      names(suppressed_data[[i]])[names(suppressed_data[[i]]) == "krav_suppressed"] <- "suppressed"
      suppressed_data[[i]]$suppressed <- as.logical(suppressed_data[[i]]$suppressed)
      inputsuppi <- GaussSuppression::PrimaryFromSuppressedData(out$x, out$cross_table, suppressed_data[[i]])
      inputsupp[inputsuppi] <- TRUE
      forced1i <- GaussSuppression::ForcedFromSuppressedData(out$x, out$cross_table, suppressed_data[[i]])
      forced1[forced1i] <- TRUE
      suppressed_data[[i]]$suppressed[as.logical(suppressed_data[[i]]$krav_suppressed2)] <- TRUE
      inputsupp2i <- GaussSuppression::PrimaryFromSuppressedData(out$x, out$cross_table, suppressed_data[[i]])
      inputsupp2[inputsupp2i] <- TRUE
      forced2i <- GaussSuppression::ForcedFromSuppressedData(out$x, out$cross_table, suppressed_data[[i]])
      forced2[forced2i] <- TRUE
    }
    if(any(inputsupp & forced1)){
      any1 <- inputsupp & forced1
      inputsupp[any1] <- FALSE
      forced1[any1] <- FALSE
      warning("Suppression pattern in suppressed_data is not unique")  
    }
    if(any(inputsupp2 & forced2)){
      any2 <- inputsupp2 & forced2
      inputsupp2[any2] <- FALSE
      forced2[any2] <- FALSE
      warning("Suppression pattern in suppressed_data is not unique")  
    }
    prim1[inputsupp] <- TRUE
  } else {
    forced1 <- NULL
    forced2 <- NULL
    inputsupp2 <- logical(0)
  }
  
  if (run_gauss) {
    sec <- GaussSuppression(x = out$x, candidates = candidates, primary = prim1, forced = forced1, hidden = NULL, 
                            singleton = list(freq = singleton_freq, num = singleton), 
                            singletonMethod = c(freq = "anySum", num = "numttH"), printInc = verbose) 
  } else {
    sec  <- integer(0)
  }
  
  if(run_gauss & any(primary2)) {
    prim2a <- unique(c(which(primary), sec)) 
    prim2 <- unique(c(which(prim1), sec, which(primary2), which(inputsupp2)))
    sec2 <- GaussSuppression(x = out$x, candidates = candidates, primary = prim2, forced = forced2, hidden = NULL, 
                            singleton = list(freq = singleton_freq, num = singleton), 
                            singletonMethod = c(freq = "anySum", num = "numttH"), printInc = verbose) 
  } else {
    sec2 <- integer(0)
  }
  
  
  rep_FALSE <- rep(FALSE, length(primary))
  
  suppressed <- prim1 # primary
  suppressed[sec] <- TRUE
  
  # Merk: primary og suppressed ikke med 
  suppressed2 <- rep_FALSE
  suppressed2[primary2] <- TRUE
  suppressed2[inputsupp2] <- TRUE
  suppressed2[sec2] <- TRUE
  suppressed2[suppressed] <- FALSE # Eventuell overlapp tas bort
  
  secondary <- rep_FALSE
  secondary[sec] <- TRUE
  
  secondary2 <- rep_FALSE
  secondary2[sec2] <- TRUE
  
  lonn_krav <- grenseverdi_krav | dominant_krav 
  
  arbeidsforhold_krav <- suppressed  | lonn_krav
  ekvivalent_krav <- suppressed | suppressed2 | lonn_krav
  
  out$krav <- data.frame(ikke_offentlig = ikke_offentlig_krav_n,
                         dominant1_n = dominant1_n_krav, 
                         dominant2_n = dominant2_n_krav, 
                         dominant1_l = dominant1_l_krav, 
                         dominant2_l = dominant2_l_krav, 
                         dominant = dominant_krav, 
                         n_within = n_within_krav, 
                         istot = out$regel$istot,
                         grenseverdi = grenseverdi_krav,
                         grenseverdi2 = grenseverdi2_krav,
                         primary = primary, 
                         primary2 = primary2, 
                         suppressed = suppressed,
                         suppressed2 = suppressed2,
                         secondary = secondary,
                         secondary2 = secondary2,
                         lonn = lonn_krav, 
                         arbeidsforhold = arbeidsforhold_krav,
                         ekvivalent = ekvivalent_krav) 
  
  if (!is.null(suppressed_data)) {
    inputsupp[!inputsupp] <- NA
    inputsupp2[!inputsupp2] <- NA
    inputsupp[forced1] <- FALSE
    inputsupp2[forced2] <- FALSE
    out$krav <- cbind(out$krav, inputsupp = inputsupp, inputsupp2 = inputsupp2)
    rm(suppressed_data)
  }
  
  logical2integer = TRUE
  
  # unngår funksjon for å spare minne 
  if (logical2integer) {
    for (j in seq_along(out)) {
      if (is.data.frame(out[[j]])) {
        is_logical <- which(sapply(out[[j]], is.logical))
        for (i in is_logical) {
          out[[j]][[i]] <- as.integer(out[[j]][[i]])
        }
      }
    }
  }
  
  # Unngår problem når denne er etter logical2integer
  if (!roundBase) {
    out$rounded <- as.data.frame(matrix(0, ncol(out$x), 0))
  }
  
  if (length(var_sum)) {
    out$sum_data <- out$sum_data[var_sum]
    var_sum <- fix_char_names(var_sum)
    names(out$sum_data) <- names(var_sum)
  } else {
    out$sum_data <- as.data.frame(matrix(0, ncol(out$x), 0))
  }
  rownames(out$sum_data) <- NULL
  
  out$prikket <- out$fun_data[names(out$fun_data) != "okantall_arbeidsforhold"]
  if (roundBase) {
    out$prikket[["antall_arbeidsforhold"]] <- out$rounded[["rounded"]]
  }  
  out$prikket[["antall_arbeidsforhold"]][arbeidsforhold_krav] <- NA
  out$prikket[["antall_heltidsekvivalenter"]][ekvivalent_krav] <- NA
  
  lonn_var_names <- unlist(strsplit(names(noen_fun_vars), ",", fixed = TRUE))
  out$prikket[lonn_krav, lonn_var_names] <- NA
  
  if (output == "list") {
    return(out)
  }
  
  if (roundBase) {
    out$rounded = out$rounded["rounded"]
    names(out$rounded) = "rounded_antall_arbeidsforhold"
  }
  rownames(out$rounded) <- NULL
  
  names(out$krav) <- paste0("krav_", names(out$krav))
  names(out$prikket) <- paste0("prikket_", names(out$prikket))
  names(out$regel_within) <- paste0("regel_within_", names(out$regel_within))
  names(out$regel) <- paste0("regel_", names(out$regel))
  
  if (output == "nlist") {
    return(out)
  }
  
  totCode <- attr(out, "totCode") <- GaussSuppression:::FindTotCode2(out$x, out$cross_table) # bør eksporteres 
  startCol <- attr(out$x, "startCol", exact = TRUE)
  
  out <- cbind(out$cross_table, out$fun_data, out$prikket, out$krav, out$regel, out$regel_within, out$rounded, out$sum_data)
  
  
  if (!is.null(startCol)) {
    attr(out, "startRow") <- startCol
  }
  attr(out, "totCode") <- totCode
  
  out
}
                     

dataframe_logical2integer = function(a){
  if(is.data.frame(a)){
    is_logical <- which(sapply(a, is.logical))
    for(i in is_logical) a[[i]] = as.integer(a[[i]])
    return(a)
  } 
  if(is.list(a)){
    for(i in seq_along(a)) a[[i]] = dataframe_logical2integer(a[[i]])
    return(a)
  }
  return(a)
}


fix_formula <- function(formula) {
  if (is.character(formula)) {
    if (length(formula) != 1) {
      stop("formula as string must be of length 1.")
    }
    if (!grepl("~", formula)) {
      formula <- paste0("~", formula)
    }
  }
  as.formula(formula)
}




