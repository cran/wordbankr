% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wordbankr.R
\name{get_instrument_table}
\alias{get_instrument_table}
\title{Connect to an instrument's Wordbank table}
\usage{
get_instrument_table(src, language, form)
}
\arguments{
\item{src}{A connection to the Wordbank database.}

\item{language}{A string of the instrument's language (insensitive to case
and whitespace).}

\item{form}{A string of the instrument's form (insensitive to case and
whitespace).}
}
\value{
A \code{tbl} object containing the instrument's data.
}
\description{
Connect to an instrument's Wordbank table
}
\keyword{internal}
