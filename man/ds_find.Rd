% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds_find.R
\name{ds_find}
\alias{ds_find}
\title{data set find}
\usage{
ds_find(proj = "somWASH", ds = "2205_h2r_settlement")
}
\arguments{
\item{proj}{project (default = "somWASH") - might be useful for expanding to related projects later}

\item{ds}{dataset name. This is an easy to remember short name that's been given (default = 2205_h2r_settlement)}
}
\value{
file path to data set
}
\description{
data set find
}
\examples{
\dontrun{
library(somaliaWASH)
ds_find(ds= "202205_h2r_settlement")
}

}
