#' sum_na
#'
#' @param x data frame
#' @param code code
#' @param is_na Teller missing når TRUE
#' @param fix_names Tar bort ´code´ fra navn
#' @param cross_prod Kryssmatrise ved TRUE
#'
#' @return named integer vector eller matrise
#' @export
#' @keywords internal
#'
sum_na <- function(x, code = "prikket_", is_na = TRUE, fix_names = TRUE, cross_prod = FALSE) {
  x_na <- x[, grepl(code, names(x)), drop = FALSE]
  if(fix_names){
    names(x_na) <- gsub(code, "", names(x_na), fixed = TRUE)
  }
  if(is_na){
    x_na <- is.na(x_na)
  }
  if(cross_prod){
    return(crossprod(x_na))
  }
  colSums(x_na)
}

#' @rdname sum_na
#' @export
sum2 <- function(x) {
  s1 <- sum_na(x, code = "prikket_", is_na = TRUE, fix_names = FALSE)
  s2 <- sum_na(x, code = "krav_", is_na = FALSE, fix_names = FALSE)
  c(s1, s2)
}

#' @rdname sum_na
#' @details 
#' 
#'  `sum2` gir resultater fra både  `prikket_` og `krav_`  
#' 
#' `s` definert som `sum_na(sdc_lonn(...))`.
#' 
#' @export
s <- function(...){
  sum_na(sdc_lonn(...))
}


