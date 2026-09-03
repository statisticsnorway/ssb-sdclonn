FormulaSelection_NEW <- function(x, formula, intercept = NA, logical = FALSE) {
  
  if (is.character(formula)) {
    if (!grepl("~", formula[1])) {
      formula <- trimws(formula)
      formula[formula == "(Intercept)"] <- "1"
      if (is.na(intercept)) {
        intercept <- "1" %in% formula
      }
      formula <- paste0("~", paste(formula, collapse = " + "))
    }
    formula <- as.formula(formula)
  }
  if (!inherits(formula, "formula")) {
    stop("parameter formula must be a single formula")
  }
  
  if (is.na(intercept)) {
    intercept <- TRUE
  }
  
  
  startInd <- c(attr(x, "startCol"), ncol(x) + 1)
  isCol <- length(startInd) > 1
  if (isCol) {
    n <- ncol(x)
  } else {
    startInd <- c(attr(x, "startRow"), nrow(x) + 1)
    n <- nrow(x)
  }
  if (length(startInd) <= 1) {
    stop("startCol or startRow attribute not found")
  }
  terms <- attr(terms(formula), "term.labels")
  if (intercept) {
    if (attr(terms(formula), "intercept")) {
      terms <- c("(Intercept)", terms)
    }
  }
  selection <- rep(FALSE, n)
  for (i in seq_along(terms)) {
    ma <- match(SSBtools:::OrderedVarNames(terms[i]), SSBtools:::OrderedVarNames(names(startInd)))
    selection[SeqInc(startInd[ma], (startInd[ma + 1] - 1))] <- TRUE
  }
  if (logical) {
    return(selection)
  }
  
  new_index <- rep(0L, n)
  new_index[selection] <- seq_len(sum(selection))
  
  if (isCol) {
    out <- x[, selection, drop = FALSE]
    s <- attr(out, "startCol")
    s <- s[s %in% which(selection)]
    s <- new_index[s]
    attr(out, "startCol") <- s
  } else {
    out <- x[selection, , drop = FALSE]
    rownames(out) <- NULL
    s <- attr(out, "startRow")
    s <- s[s %in% which(selection)]
    s[] <- new_index[s]
    attr(out, "startRow") <- s
  out
  }
}