
#' find_tot_code
#' 
#' Based on GaussSuppression:::FindTotCode2
#'
#' @param x  x
#' @param cross_table cross_table 
#'
#' @return list
#' @keywords internal
#' @export
#' @importFrom Matrix colSums
#' 
#' 
#' 
#'
find_tot_code <- function(x, cross_table) {   # other function SSBtools:::FindTotCode exist
  tot_col <- find_tot_col(x)
  if (!length(tot_col)) {
    warning("No tot_col found")
  }
  lapply(as.list(cross_table[tot_col, , drop = FALSE]), unique)
}

find_tot_col <- function(x) {  # based on SSBtools:::FindTotRow
  nr <- nrow(x)
  w1 <- which(colSums(x) == nr)
  if (length(w1)) {
    z <- x[, w1, drop = FALSE]^2
    w2 <- which(colSums(z) == nr)
    if (length(w2)) {
      return(w1[w2])
    }
  }
  w1
}


#' tot_code_selection
#'
#' @param x x
#' @param x_tot_code x_tot_code 
#'
#' @return logical vector 
#' @keywords internal
#' @export
#'
tot_code_selection <- function(x,  
                         x_tot_code = {k <- as.list(rep("Total", ncol(x))); names(k) <- names(x); k}) {
  
  if(any(duplicated(names(x))) | is.null(names(x))){
    stop("x: non-duplicate names are needed")
  }
  if(any(duplicated(names(x_tot_code))) | is.null(names(x_tot_code))){
    stop("x_tot_code: non-duplicate names are needed")
  }
  names_in <- names(x_tot_code) %in% names(x)
  if (any(!names_in)) {
    warning("x_tot_code elements without matching names in x ignorded")
    x_tot_code <- x_tot_code[names_in]
  }
  x_names <- names(x)[names(x) %in% names(x_tot_code)]
  x_rows <- rep(TRUE, nrow(x))
  if (length(x_names)) {
    for (i in seq_along(x_names)) {
      x_rows <- x_rows & (x[[x_names[i]]] %in% x_tot_code[[x_names[i]]])
    }
  }
  x_rows
}


#' tot_code_replace
#'
#' @param data data 
#' @param dim_var dim_var
#' @param tot_code tot_code
#'
#' @return data frame
#' @keywords internal
#' @export
#' @importFrom SSBtools Match
#'
tot_code_replace <- function(data, dim_var, tot_code, replaced_only =TRUE, include_istot = TRUE){
  dim_var = dim_var[!(dim_var %in% names(tot_code))]
  selection = which(tot_code_selection(data, tot_code))
  if(length(dim_var)){
    ma = Match(data[dim_var], data[selection, dim_var, drop=FALSE])
  } else {
    ma = rep(1L, nrow(data))
  }
  
  replace_col = !(names(data) %in% unique(c(dim_var,names(tot_code))))
  
  if(replaced_only){
    data = data[replace_col]
    replace_col = rep(TRUE, ncol(data))
  }
  
  
  data[!is.na(ma), replace_col] = data[selection[ma[!is.na(ma)]], replace_col, drop = FALSE]
  
  if(any(is.na(ma))){
    warning("Missing replacement in case of non-tot_code match")
    data[is.na(ma), replace_col] = NA
  }
  if(include_istot){
    data$istot = FALSE
    data$istot[selection] = TRUE
  }
  data
}



#' tot_code_match
#' 
#' Limits match to rows with total code in non-common dim columns
#'
#' @param x x
#' @param y y
#' @param x_tot_code x_tot_code 
#' @param y_tot_code y_tot_code
#' @param both 
#' @param complete_match Kun for testing. TRUE skal gi samme svar, men ikke like rask kode. 
#'
#' @return integer vector or list 
#' @keywords internal
#' @export
#'
#' @examples
#' z <- SSBtools::SSBtoolsData("sprt_emp_withEU")
#' z$age[z$age == "Y15-29"] <- "young"
#' z$age[z$age == "Y30-64"] <- "old"
#' geoDimList <- SSBtools::FindDimLists(z[, c("geo", "eu")], total = "Europe")[[1]]
#' 
#' mm1 <- SSBtools::ModelMatrix(z, list(age = "All", year = "AllYears", geo = "Europe"), 
#'                              crossTable = TRUE)
#' mm2 <- SSBtools::ModelMatrix(z, formula = ~age * year * eu + geo * age, crossTable = TRUE)
#' mm3 <- SSBtools::ModelMatrix(z, dimVar = 1:2, crossTable = TRUE)
#' 
#' 

#' tot_code1 <- find_tot_code(mm1$modelMatrix, mm1$crossTable)
#' tot_code2 <- find_tot_code(mm2$modelMatrix, mm2$crossTable)
#' tot_code3 <- find_tot_code(mm3$modelMatrix, mm3$crossTable)
#' 
#' mm1$crossTable[tot_code_selection(mm1$crossTable, tot_code1), ]
#' mm1$crossTable[tot_code_selection(mm1$crossTable, tot_code1[c(1, 3)]), ]
#' mm2$crossTable[tot_code_selection(mm2$crossTable, tot_code2[c(1, 2)]), ]
#' mm3$crossTable[tot_code_selection(mm3$crossTable, tot_code3[2]), ]
#' 
#' d <- mm1$crossTable
#' d$value <- 10 * 1:(nrow(d))
#' 
#' cbind(d, tot_code_replace(d, dim_var = c("age", "year", "geo"), tot_code1[c(2, 3)]))
#' cbind(d, tot_code_replace(d, dim_var = c("age", "year", "geo"), tot_code1[1]))
#' 
#' 
#' ma <- tot_code_match(mm3$crossTable, mm1$crossTable, tot_code3, tot_code1)
#' 
#' # Like koder kreves for match. Må altså ha samme total-koder. Det er det ikke her.
#' cbind(mm3$crossTable[!is.na(ma), ], mm1$crossTable[ma[!is.na(ma)], ]) 
tot_code_match <- function(x, y, 
                         x_tot_code = {k <- as.list(rep("Total", ncol(x))); names(k) <- names(x); k},
                         y_tot_code = {k <- as.list(rep("Total", ncol(y))); names(k) <- names(y); k},
                         both = FALSE, complete_match = FALSE) {
  
  
  if(any(duplicated(names(x))) | is.null(names(x))){
    stop("x: non-duplicate names are needed")
  }
  if(any(duplicated(names(y))) | is.null(names(y))){
    stop("y: non-duplicate names are needed")
  }
  if(any(duplicated(names(x_tot_code))) | is.null(names(x_tot_code))){
    stop("x_tot_code: non-duplicate names are needed")
  }
  if(any(duplicated(names(y_tot_code))) | is.null(names(y_tot_code))){
    stop("y_tot_code: non-duplicate names are needed")
  }
  
  names_x <- names(x)
  names_y <- names(y)
  
  names_in <- names(x_tot_code) %in% names_x
  if (any(!names_in)) {
    warning("x_tot_code elements without matching names in x ignorded")
    x_tot_code <- x_tot_code[names_in]
  }
  names_in <- names(y_tot_code) %in% names_y
  if (any(!names_in)) {
    warning("y_tot_code elements without matching names in y ignorded")
    y_tot_code <- y_tot_code[names_in]
  }
  names_in <- names_x %in% names(x_tot_code)
  if (any(!names_in)) {
    warning("x columns without matching names in x_tot_code ignorded")
    names_x <- names_x[names_in]
  }
  names_in <- names_y %in% names(y_tot_code)
  if (any(!names_in)) {
    warning("y columns without matching names in y_tot_code ignorded")
    names_y <- names_y[names_in]
  }
  
  
  x_names_only =  names_x[!(names_x %in% names_y)]
  y_names_only =  names_y[!(names_y %in% names_x)]
  
  x_rows <- rep(TRUE, nrow(x))
  if (length(x_names_only)) {
    for (i in seq_along(x_names_only)) {
      x_rows <- x_rows & (x[[x_names_only[i]]] %in% x_tot_code[[x_names_only[i]]])
    }
  }
  y_rows <- rep(TRUE, nrow(y))
  if (length(y_names_only)) {
    for (i in seq_along(y_names_only)) {
      y_rows <- y_rows & (y[[y_names_only[i]]] %in% y_tot_code[[y_names_only[i]]])
    }
  }
  
  
  if(complete_match){
    x = x[names_x[names_x %in% names_y]]
    y = y[names_y[names_y %in% names_x]]
    
    x$ok = 0L
    y$ok = 0L
    x$ok[!x_rows] = 1L
    y$ok[!y_rows] = 2L
    
    if(is.na(both)){
      return(Match(y,x))
    }
    if(both){
      return(list(forward = Match(x,y), reverse = Match(y,x)))
    }
    return(Match(x,y))  
  }
  
  
  if (length(x_names_only)) {
    x = x[x_rows, names_x[names_x %in% names_y], drop = FALSE]
  }
  if (length(y_names_only)) {
    y = y[y_rows, names_y[names_y %in% names_x], drop = FALSE]
  }
  if(!isFALSE(both)){
    ma = Match(y,x)
    ma[!is.na(ma)] = which(x_rows)[ma[!is.na(ma)]]
    out_r = rep(NA_integer_, length(y_rows))
    out_r[y_rows] = ma
  }
  if(is.na(both)){
    return(out_r)
  }
  ma = Match(x,y)
  ma[!is.na(ma)] = which(y_rows)[ma[!is.na(ma)]]
  out = rep(NA_integer_, length(x_rows))
  out[x_rows] = ma
  if(both){
    return(list(forward = out, reverse = out_r))
  }
  return(out)
}
