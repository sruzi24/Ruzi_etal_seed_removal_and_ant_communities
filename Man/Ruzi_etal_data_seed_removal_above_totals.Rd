\name{Ruzi_etal_data_seed_removal_above_totals}
\alias{Ruzi_etal_data_seed_removal_above_totals}
\docType{data}
\title{
Ruzi_etal_data_seed_removal_above_totals.csv
}
\description{
Seed removal from Neotropical pioneer tree species placed in petri dish caches at five sites on Barro Colorado Island, Panama during the dry and wet seasons of 2013.
}
\usage{data("Ruzi_etal_data_seed_removal_above_totals")}
\format{
  A data frame with 250 observations on the following 10 variables.
  \describe{
    \item{\code{Species}}{a character vector - the Neotropical pioneer tree species}
    \item{\code{Dispersion}}{a character vector - primary dispersal mode}
    \item{\code{Season}}{a character vector - the season tested in}
    \item{\code{Plot}}{a character vector - the prexisting site in the forest where caches were placed}
    \item{\code{Replicate}}{a numeric vector - the trial number of the caches placed at a plot}
    \item{\code{Total.Seeds.Remaining}}{a numeric vector - the total number of seeds remaining in the petri dish cache after the trial was over}
    \item{\code{Total.Seeds.Removed}}{a numeric vector - the total number of seeds removed at the end of the trial}
    \item{\code{Position}}{a numeric vector - the randomized position along the transect given to the cache}
    \item{\code{Start.Date}}{a character vector - the date of Day 1 of the trial}
    \item{\code{Dormancy}}{a character vector - the type of seed dormancy that the seeds of Neotropical pioneer tree species have}
  }
}
\details{
Used in partial least squares regression analyses.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Data also used in:
Ruzi, S.A., D.P. Roche, P-C. Zalamea, A.C. Robison*, and J.W. Dalling. 2017. Species identity influences secondary removal of seeds of Neotropical pioneer tree species. Plant Ecology 218(8): 983-995. doi:10.1007/s11258-017-0745-7
}
\examples{
data(Ruzi_etal_data_seed_removal_above_totals)
str(Ruzi_etal_data_seed_removal_above_totals)
}
\keyword{datasets}
