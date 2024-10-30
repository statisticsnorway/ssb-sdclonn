
#' Output fra `sdc_lonn` omgjort til langformat
#' 
#' Eksempler inkluderer omkoding med \code{\link{recode}}.
#' 
#' Funksjonen \code{\link{long_sdclonn}} er ment for å lage data på format som trengs i statistikkbanken. 
#' Men den lager ikke de endelige variabelnavn og koder som trengs.
#' 
#' Ytterligere omkoding kan gjøres på flere måter. 
#' I eksemplene her vises det hvordan det kan gjøres med funksjonen \code{\link{recode}} som nå er innebygget i denne pakken. 
#' Det kan uansett være problem med de norske bokstavene. 
#' Dette avhenger kanskje av plattformen dette kjøres på. 
#' Norske bokstaver unngås i eksemplene. 
#' 
#' Ved kjøring av \code{\link{long_sdclonn}}-funksjonen så vil det bli vil det bli mange "kopi-rader" med antall 
#' arbeidsforhold og antall heltidsekvivalenter. 
#' Dette fordi disse variablene blir satt sammen med de ulike statistikkvariablene. 
#' Dersom det av en eller annen grunn skjer at andre variabler feilkodes kan flere variabler bli «kopiert» til mange rader.
#' 
#'
#' @param data  output fra \code{\link{sdc_lonn}}
#' @param prikket Prikkede tall i output ved `TRUE`. Prikkede tall er missing (`NA`).  
#' @param long langformat ved `TRUE`. Ved `FALSE` returneres en versjon forberedt dataene.
#' @param statvar  Kategorinavn som er del av variabelnavn i input og som vil finnes i variabelen `statvar` i output. 
#'
#' @return data i langformat
#' @export
#' @importFrom SSBtools Stack
#'
#' @examples
#' a <- sdclonn_data("syntetisk_5000")
#' out <- sdc_lonn(a,  between = c("yrke1", "yrke2", "sektor3"),  
#'                 within = c("pers_kjoenn", "arb_heldeltid"), 
#'                 k1 = 80, k2 = 85)
#' 
#' # Lager data i langformat  
#' long1 <- long_sdclonn(out)
#' 
#' # Legger inn nye koder for kategorier i to trinn 
#' long2 <- recode(long1, 
#'                 old = c("manedslonn", "avtalt", "bonus", "uregtil", "overtid"),  
#'                 new = c("Manedslonn", "Avtalt manedslonn", "Bonus", "Uregelmessige tillegg", 
#'                         "Overtidsgodtgjorelse"), 
#'                 oldvar = "statvar", 
#'                 newvar = "statistikkvariabel")
#' long3 <- recode(long2, 
#'                 old = c("gjennomsnitt", "median", "nedre_kvartil", "Ovre_kvartil", 
#'                         "antall_arbeidsforhold", "antall_heltidsekvivalenter"),  
#'                 new = c("Gjennomsnitt", "Median", "Nedre kvartil", "Ovre kvartil", 
#'                         "Antall arbeidsforhold med lonn", "Antall heltidsekvivalenter"),
#'                 oldvar = "statmal", 
#'                 newvar = "statistikkmal")
#' head(long3)
#' 
#' 
#' # Viser alternativ metode der long blir det samme som long3.
#' # Her brukes pipeoperatoren, |> , som tilsvarer %>% i tidyverse.
#' # |> er relativt ny i base R. Brukes denne istedenfor %>% behoves ikke ekstra pakker. 
#' 
#' statvar_koder <- c("manedslonn", "avtalt", "bonus", "uregtil", "overtid")
#' statistikkvariabel_koder <- c("Manedslonn", "Avtalt manedslonn", "Bonus", 
#'                               "Uregelmessige tillegg", "Overtidsgodtgjorelse")
#' statmal_koder <- c("gjennomsnitt", "median", "nedre_kvartil", "Ovre_kvartil", 
#'                    "antall_arbeidsforhold", "antall_heltidsekvivalenter")
#' statistikkmal_koder <- c("Gjennomsnitt", "Median", "Nedre kvartil", "Ovre kvartil", 
#'                          "Antall arbeidsforhold med lonn", "Antall heltidsekvivalenter")
#' 
#' long <- sdclonn_data("syntetisk_5000") |>
#' sdc_lonn(between = c("yrke1", "yrke2", "sektor3"), 
#'          within = c("pers_kjoenn", "arb_heldeltid"), k1 = 80, k2 = 85) |>
#'   long_sdclonn() |>
#'   recode(old = statvar_koder, new = statistikkvariabel_koder, 
#'          oldvar = "statvar", newvar = "statistikkvariabel") |>
#'   recode(old = statmal_koder, new = statistikkmal_koder, 
#'          oldvar = "statmal", newvar = "statistikkmal")
#' head(long) 
#' 
long_sdclonn <- function(data, prikket = TRUE, long = TRUE, 
    statvar = c("manedslonn", "avtalt", "bonus", "uregtil", "overtid")){
  
  prikket_ <- "prikket_"
  
  cnames <- colnames(data)
  pnames <- cnames[grep(prikket_, cnames)]
  vnames <- gsub(prikket_, "", pnames)
  
  n_dim_var <- min(match(vnames, cnames)) - 1
  
  dim_var <- cnames[seq_len(n_dim_var)]
  
  if (prikket) {
    lonn_var <- pnames
  } else {
    lonn_var <- vnames
  }
  
  data <- data[c(dim_var, lonn_var)]
  
  names(data) <- c(dim_var, vnames)
  
  
  m_grep <- mgrep(statvar, vnames)
  
  if (length(unique(m_grep$n_pattern[m_grep$n_pattern > 0])) != 1) {
    stop("Statistikkvariabel-koding passer ikke")
  }
  
  statvar <- statvar[order(m_grep$first_pattern, na.last = NA)]
  
  
  vnames_true <- vnames[m_grep$x_pattern]
  vnames_false <- vnames[!m_grep$x_pattern]
  
  
  data <- data[c(dim_var, vnames_true, rep(vnames_false, each = length(statvar)))]
  
  
  lonn_var_new <- c(vnames_true, paste(rep(statvar, length(vnames_false)), rep(vnames_false, each = length(statvar)), sep = "_"))
  
  names(data) <- c(dim_var, lonn_var_new)
  
  data <- data[c(dim_var, sort(lonn_var_new))]
  
  if (!long) {
    return(data)
  }
  
  row_data <- pattern_split(lonn_var_new, statvar)
  
  names(row_data) <- c("statvar", "statmal")
   
  Stack(data, stackVar = lonn_var_new, blockVar = dim_var, rowData = row_data, indName = NULL, valueName = "value")
}


mgrep <- function(pattern, x, fixed = TRUE) {
  a <- rep(FALSE, length(x))
  n_pattern <- integer(0)
  first_pattern <- integer(0)
  for (i in seq_along(pattern)) {
    grep_i <- grep(pattern[i], x, fixed = fixed)
    n_pattern <- c(n_pattern, length(grep_i))
    first_pattern <- c(first_pattern, grep_i[1])
    a[grep_i] <- TRUE
  }
  list(x_pattern = a, n_pattern = n_pattern, first_pattern = first_pattern)
}

pattern_split <- function(x, pattern, fixed = TRUE) {
  a <- rep("", length(x))
  b <- a
  for (p in pattern) {
    r <- grepl(p, x, fixed = fixed)
    a[r] <- p
    s <- strsplit(x[r], paste0(p, "_"), fixed = fixed)
    b[r] <- sapply(s, function(x) x[2])
  }
  data.frame(a = a, b = b)
}



