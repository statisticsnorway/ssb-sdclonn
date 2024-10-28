#' Function that returns a dataset
#' 
#' @param dataset Name of data set within the sdclonn package
#'
#' @return data frame
#' 
#' @export
#' @importFrom utils data
#'
#' @examples
#' a <- sdclonn_data("synthetic_5000")
#' 
#' # Alle variabler med små bosktaver + variabel sektor3
#' b <- sdclonn_data("syntetisk_5000")
#' 
sdclonn_data <- function(dataset) {
  if(dataset == "syntetisk_5000"){
    data <- sdclonn_data("synthetic_5000")
    data$sektor3 <- "privat"
    data$sektor3[data$FRTK_SEKTOR_2014 == "6100"] <- "stat"
    data$sektor3[data$FRTK_SEKTOR_2014 == "6500"] <- "kommune"
    names(data) <- tolower(names(data))
    return(data)
  }
  return(sdclonn_data_(dataset))
}


# stackoverflow questions 30357330
pkgenv_sdclonn <- new.env(parent=emptyenv())


sdclonn_data_ <- function(dataset) {
  if (!exists(dataset, pkgenv_sdclonn))
    data(list = dataset, package = "sdclonn", envir = pkgenv_sdclonn)
  return(pkgenv_sdclonn[[dataset]])
  return(NULL)
}


#' Fictitious datasets returned by sdclonn_data()
#'
#' @docType data
#' @keywords datasets internal
#' @name synthetic_5000
NULL
