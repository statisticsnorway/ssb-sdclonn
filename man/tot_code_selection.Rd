% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tot_code_match.R
\name{tot_code_selection}
\alias{tot_code_selection}
\title{tot_code_selection}
\usage{
tot_code_selection(
  x,
  x_tot_code = {
     k <- as.list(rep("Total", ncol(x)))
     names(k) <- names(x)
    
    k
 }
)
}
\arguments{
\item{x}{x}

\item{x_tot_code}{x_tot_code}
}
\value{
logical vector
}
\description{
tot_code_selection
}
\keyword{internal}
