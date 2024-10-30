
#' Lager ekstra variabler i data 
#' 
#' Gammel funksjon som ikke brukes lenger
#'
#' @param data data
#'
#' @return data frame
#'
#' @examples
#' a <- sdclonn_data("synthetic_5000")
#' # b <- lag_variabler(a) # kommentert bort siden ikke eksportert lenger
#' 
lag_variabler <- function(data) {
  
  if (any(names(data) %in% c("freq", "sektor3", "is_privat", "privat_EKV_VEKT", "unik_frtk"))) {
    stop("Variabelnavn finnes allerede")
  }
  data$freq <- 1
  
  data$sektor3 <- "privat"
  data$sektor3[data$frtk_SEKTOR_2014 == "6100"] <- "stat"
  data$sektor3[data$frtk_SEKTOR_2014 == "6500"] <- "kommune"
  
  data$is_privat <- data[["sektor3"]] == "privat"
  data$privat_EKV_VEKT <- data[["LONN_EKV_VEKT"]] * data$is_privat
  
  data$unik_frtk <- data[["frtk_ID_SSB"]]
  data$unik_frtk[data$unik_frtk == ""] <- data[["VIRK_ID_SSB"]][data$unik_frtk == ""]
  
  data
}
