#' Ekstra sdclonn-detaljer
#' 
#' Ekstra \code{\link{sdc_lonn}}-detaljer. Teksten kopiert fra annen sammenheng. 
#'
#' @details
#' **n_within:** Utgangspunktet, uten avrunding, for parameteren `freq_within` 
#' som fører til `n_within_krav` er at dette er en frekvenstabell i tillegg til 
#' en volumtabell med lønnsdata.
#'
#' I en frekvenstabell er poenget å beskytte mot avsløring av kategoriinformasjon slik som 
#' kjønn, alder og heltid/deltid. Det kan selvfølgelig diskuteres om dette skal beskyttes.
#'
#' Men dersom det skal beskyttes er et hovedpoeng å primærprikke 0-ere. I tillegg til 0-ere 
#' kan 1-ere og 2-ere også primærprikkes for å gjøre det tryggere.
#'
#' Illustrerer dette ved disse dataene:
#'
#' \preformatted{
#'  kjonn  heldeltid  antall_heltidsekvivalenter  antall_arbeidsforhold
#'  Total      Total                          13                     20
#'  Total     deltid                           7                     14
#'  Total     heltid                           6                      6
#' kvinne      Total                           4                      8
#' kvinne     deltid                           4                      8
#' kvinne     heltid                           0                      0
#'   mann      Total                           9                     12
#'   mann     deltid                           3                      6
#'   mann     heltid                           6                      6
#' }
#'
#' Her kan man altså avsløre at alle kvinner jobber deltid. Alternativt kan man avsløre at 
#' kjønnet er mann dersom man vet at det er heltid.
#'
#' Dette er en ekstra krevende del av sdclonn siden dataene må utvides siden de 
#' nødvendige/riktige 0-ere kommer med i dataene.
#'
#' Når avrunding gjøres kan man ikke lenger stole på at 0-ere man ser i `antall_arbeidsforhold` 
#' er korrekte 0-er, og man trenger ikke gjøre slik prikking. **Men** man kan fremdeles se ekte 0-er 
#' ved å se på `antall_heltidsekvivalenter`. Løsningen ble da å fortsette med `n_within`-prikking 
#' når det gjelder prikking av `antall_heltidsekvivalenter`. Bruken av `n_within` ble dermed 
#' flyttet til andre runde prikking.
#'
#' **grenseverdi2:** Utgangspunktet for denne er å beskytte mot at det skal gå an å regne seg 
#'                   tilbake til små verdier av antall heltidsekvivalenter, som igjen gjør at 
#'                   man kan beregne lønn via gjennomsnittslønn.
#' Etter at avrunding er tatt inn er det i praksis to parametere som styrer samme type primærprikking. 
#' Det prikkes når enten `n_arbeidsforhold <= freq_within` eller `okantall < grenseverdi2`. 
#' En forskjell er at `freq_within` kun brukes «within» (ikke når det er total på alle 
#' within-variabler) mens `grenseverdi2` brukes på alle tallene. I praksis tror jeg ikke `freq_within` 
#' har noen betydning så lenge `grenseverdi2` er et mye større tall. I begge tilfeller vil 0-ere bli 
#' prikket slik det er programmert nå, men egentlig er det logikken bak `freq_within` som skal 
#' medføre prikking av 0.
#'
#' **grenseverdi:** Denne parameteren, som nå har 100 som standardverdi, brukes til prikking av 
#'                  lønnsvariablene samt antall arbeidsforhold og antall heltidsekvivalenter i output. 
#'                  **Men** dette fører ikke til sekundærprikking, og det 
#'                  beskyttes altså ikke mot mulig tilbakeregning av tallene for antall 
#'                  heltidsekvivalenter. Parameteren har ingenting å gjøre med den krevende 
#'                  sekundærprikkealgoritmen i programmet. Altså kun noe som gjøres på output.
#'
#' **Hvorfor er sdclonn tregere enn SdcForetakPerson?**
#'
#'   * Det er altså prikkingen som tar tid. I SdcForetakPerson handler all prikking om between, mens problemstillingene within er løst ved avrunding. Prikkingen kan derfor løses ved et datasett som er aggregert kun til between-variablene.
#'
#'   * I sdclonn er det dominansprikking som kun er between. Men det er allikevel gjenværende problemstillinger within, slik som parameteren `grenseverdi2`. Prikkingen kan derfor ikke løses ved et datasett som er aggregert kun til between-variablene. Dessuten er det problemstillingen med 0-ere som faktisk gjør at dataene utvides med rader som representerer nødvendige 0-ere.
#'
#' @name sdc_lonn_extra_details
NULL
