
quantile_w <- function(x, w) {
  quantile_weighted(x = x, probs = (1:3)/4, weights = w)
}

quantile_u <- function(x) {
  quantile(x = x, probs = (1:3)/4, type = 2)
}


unique_narm <- function(x, na.rm = TRUE) {
  if (na.rm) {
    if (anyNA(x)) {
      x <- x[!is.na(x)]
    }
  }
  unique(x)
}

n_unique <- function(a, selection = NULL) {
  if (is.null(selection)) {
    return(length(unique_narm(a)))
  }
  length(unique_narm(a[selection]))
}

n_privat <- function(a, is_privat) {
  n_unique(a, is_privat)
}


n_offentlig <- function(a, is_privat) {
  n_unique(a, !is_privat)
}


weighted_sum <- function(x, w) {
  sum(x * w)
}




#  Koder frem og tilbake til integer/numeric
#  p.g.a bruk i model_aggregate
character2integer <- function(x) {
  x <- as.factor(x)
  list(x = as.integer(x), x_levels = levels(x))
}
integer2character <- function(x, x_levels) {
  x <- as.integer(x)
  attr(x, "levels") <- x_levels
  class(x) <- "factor"
  as.character(x)
}

# max og which.max som en funksjon Dessuten NA ved -Inf
max_which <- function(x) {
  wm <- which.max(x)
  m <- x[wm]
  if (!length(m)) {
    m <- -Inf
  }
  if (m == -Inf) {
    return(list(max = NA_real_, which = NA_integer_))
  }
  list(max = m, which = wm)
}


NA2FALSE <- function(x) {
  x[is.na(x)] <- FALSE
  x
}

# Denne brukes ikke
max_second_max <- function(x) {
  wm1 <- which.max(x)
  m1 <- x[wm1]
  x[wm1] <- -Inf
  wm2 <- which.max(x)
  m2 <- x[wm2]
  c(max = m1, second_max = m2, which_max = wm1, which_second_max = wm2)
}


# Mye beregnes i denne funksjonen 
# SUm og  Priv bare kopi av beregninger utenfor denne funskjonen (som sjekk)
max_second_max_info <- function(x, unik_frtk, is_privat,  na.rm = TRUE){
  
  if (na.rm) {
    if (anyNA(unik_frtk)) {
      velg <- !is.na(unik_frtk)
      x <- x[velg]
      unik_frtk <- unik_frtk[velg]
      is_privat <- is_privat[velg]
    }
  }
  
  Sum <- sum(x)
  Priv <- sum(x * is_privat)
  if (!length(x)) {
    x <- -Inf
    unik_frtk <- NA
    is_privat <- NA
  }
  max_w <- max_which(x)
  m1 <- max_w[[1]]
  wm1 <- max_w[[2]]
  
  x[wm1] <- -Inf
  max_w <- max_which(x)
  m2 <- max_w[[1]]
  wm2 <- max_w[[2]]
  
  x[wm1] <- m1
  x[!is_privat] <- -Inf
  
  max_w <- max_which(x)
  m1p <- max_w[[1]]
  wm1p <- max_w[[2]]
  
  x[wm1p] <- -Inf
  max_w <- max_which(x)
  m2p <- max_w[[1]]
  wm2p <- max_w[[2]]
  
  c(max             =   m1, second_max             = m2, 
    max_privat      =  m1p, second_max_privat      = m2p,
    max_frtk        =  unik_frtk[wm1], second_max_frtk        = unik_frtk[wm2],
    max_frtk_privat = unik_frtk[wm1p], second_max_frtk_privat = unik_frtk[wm2p],
    max_is_privat = is_privat[wm1], second_max_is_privat = is_privat[wm2],
    sum  = Sum, 
    priv = Priv)
}


# Denne brukes ikke 
max_w <- function(a, w) {
  if(!length(w)){
    return(c(max =  0, second_max = 0, which_max = 0, which_second_max = 0))
  }
  max_second_max(aggregate(w,list(a), sum)[[2]])
}


#
# Foreløpig ikke enhetlig variabelnavn og rekkefølge for max_w2 og max_second_max_info
#
max_w2 <- function(a, w, is_privat, na.rm = TRUE) {
  if (na.rm) {
    if (anyNA(a)) {
      velg <- !is.na(a)
      a <- a[velg]
      w <- w[velg]
      is_privat <- is_privat[velg]
    }
  }
  if (!length(w)) {
    return(max_second_max_info(w, a, is_privat))
  }
  if (anyNA(is_privat)) {
    is_privat[is.na(is_privat)] <- as.logical(w[is.na(is_privat)])
  }
  agg <- aggregate(w, list(a, is_privat), sum)
  max_second_max_info(agg[[3]], agg[[1]], agg[[2]])
}

#
#  Som max_w2, men vektet  
#
max_w3 <- function(unik_frtk, hoved_var, is_privat, vekt_var = 1) {
  max_w2(unik_frtk, hoved_var * vekt_var, is_privat)
}


# Alt som er NA blir til 0 eller ""
na_remove <- function(x) {
  is_character_x <- sapply(x, is.character)
  for (i in seq_along(x)) {
    is_na <- is.na(x[[i]])
    if (any(is_na)) {
      if (is_character_x[i]) {
        x[is_na, i] <- ""
      } else {
        x[is_na, i] <- FALSE  # -> 0 when numeric
      }
    }
  }
  x
}


fix_char_names <- function(x) {
  if (is.null(names(x))) {
    names(x) <- NA
  }
  names(x)[is.na(names(x))] <- ""
  names(x)[names(x) == ""] <- x[names(x) == ""]
  x
}

