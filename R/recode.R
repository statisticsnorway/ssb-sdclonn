
#' Recoding character data
#'
#' @param data data frame, list or vector 
#' @param old vector of old values 
#' @param new vector of new values
#' @param oldvar Variable in data to be recoded. When NULL, vector input assumed. 
#' @param newvar A possible new variable for recoded version of oldvar 
#'
#' @return Recoded version of data 
#' @export
#' 
#'
#' @examples
#' z <- SSBtools::SSBtoolsData("sprt_emp_withEU")
#' recode(z, c("Y15-29", "Y30-64"), c("young", "old"), oldvar = "age", newvar = "newage")
#' recode(z$year, 2014:2016, 14:16)
#' recode(z, c("2014", "2016"), c("14", "16"), oldvar = "year")
#' 
#' SSBtools::SSBtoolsData("sprt_emp_withEU") |>
#'   recode(c("Y15-29", "Y30-64"), c("young", "old"), oldvar = "age", newvar = "newage") |>
#'   recode(2014:2016, 14:16, oldvar = "year") 
recode <- function(data, old, new, oldvar = NULL, newvar = oldvar){
  if(is.null(oldvar)){
    return(CharacterReCode(data, old, new))
  }
  data[[newvar]] <- CharacterReCode(data[[oldvar]], old, new)
  data
} 


# Koipert fra Kostra::CharacterReCode
CharacterReCode <- function(x,oldLevels, newLevels){
  x = as.factor(x)
  le = levels(x)
  ma = match(le,oldLevels)
  isMatch = !is.na(ma)
  le[isMatch] = newLevels[ma[isMatch]]
  levels(x) = le
  as.character(x)
}
