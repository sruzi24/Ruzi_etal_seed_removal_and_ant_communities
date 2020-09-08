\name{Ruzi_etal_data_seed_removal_below_totals}
\alias{Ruzi_etal_data_seed_removal_below_totals}
\docType{data}
\title{
Ruzi_etal_data_seed_removal_below_totals.csv
}
\description{
Seed removal from Neotropical pioneer tree species placed approximatly two centimeters within the topsoil at five sites on Barro Colorado Island, Panama during the wet season of 2013.}
\usage{data("Ruzi_etal_data_seed_removal_below_totals")}
\format{
  A data frame with 140 observations on the following 12 variables.
  \describe{
    \item{\code{Species}}{a character vector - the Neotropical pioneer tree species}
    \item{\code{Season}}{a character vector - the season tested in}
    \item{\code{Dispersion}}{a character vector - primary dispersal mode}
    \item{\code{Num.weeks}}{a numeric vector - the number of weeks the seed cache was in the environment for}
    \item{\code{Plot}}{a character vector - the prexisting site in the forest where caches were placed}
    \item{\code{Direction}}{a character vector - the randomized side of the plot where the caches were placed}
    \item{\code{Replicate}}{a numeric vector - the trial number of the caches placed at a plot}
    \item{\code{Total.Recovered}}{a numeric vector - the total number of seeds recovered when the seed cache was dug back up}
    \item{\code{Total.Removed}}{a numeric vector - the total number of seeds removed. Calculated as the number of seeds placed (10) minus the \code{Total.Recovered}}
    \item{\code{Partial.Recovered}}{a numeric vector - the number of seeds that were not intact when recovered}
    \item{\code{Vials.Present}}{a character vector - whether there were subterranean traps present}
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
data(Ruzi_etal_data_seed_removal_below_totals)
str(Ruzi_etal_data_seed_removal_below_totals)
}
\keyword{datasets}
