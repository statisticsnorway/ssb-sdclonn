
#' Multiply formulas
#' 
#' Combine formulas by `*` or another operator
#'
#' @param formula1 formula1
#' @param formula2 formula2 
#' @param operator `*`, `:` or another operator
#' @return A formula
#' @export
#' @importFrom stats as.formula
#'
#' @examples
#' multiply_formulas(~x, ~y)
#' multiply_formulas(~x+y, ~a*b + d:e)
#' multiply_formulas(~x+y, ~a*b + d:e, ":")
multiply_formulas <- function(formula1, formula2, operator = "*") {
  f1 <- as.character(formula1)
  f2 <- as.character(formula2)
  f1 <- f1[length(formula1)]  # any left side ignored
  f2 <- f2[length(formula2)]
  f1_f2 <- paste("~(", f1, ")", operator, "(", f2, ")")
  as.formula(f1_f2, env = attr(formula1, ".Environment"))
}

