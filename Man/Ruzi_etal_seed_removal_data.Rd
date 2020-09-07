\name{Ruzi_etal_seed_removal_data}
\alias{Ruzi_etal_seed_removal_data}
\docType{data}
\title{
Ruzi_etal_seed_removal_data.csv
}
\description{
Data of foragers of different ant species observed in seed removal dishes, whether the ants were observed removing seeds, and whether there was a change in seed count.
}
\usage{data("Ruzi_etal_seed_removal_data")}
\format{
  A data frame with 3750 observations on the following 36 variables.
  \describe{
    \item{\code{Plot}}{a character vector - the site in Barro Colorado Island, Panama where the data was collected.}
    \item{\code{Replicate}}{a numeric vector - the trial number at that particular site (\code{Plot}).}
    \item{\code{Species}}{a character vector - the Neotropical pioneer plant species that the seed cache was for.}
    \item{\code{Day}}{a numeric vector - the day the sampling point was from. Days 1 and 2 were from 10am to 4pm while Day 3 only had a 9am point.}
    \item{\code{Time.Point}}{a numeric vector - the hour at which the data point was collected. Days 1 and 2 range from 10-16 (10am to 4pm) and Day 3 is only 9.}
    \item{\code{Observed.ant.present}}{a character vector - whether an ant was observed in the seed cache. \code{0} = no ant was observed. \code{1} or \code{SAR}### indicates that there was at least one ant observed in the dish. The \code{SAR}### refers to a collection code from Selina A. Ruzi.}
    \item{\code{observed.ant.removing}}{a character vector - whether an ant was observed removing at least one seed from the seed cache. \code{0} = ants were not observed removing seeds. \code{1} or \code{SAR}### indicates that there was at least one ant observed removing a seed from the dish. The \code{SAR}### refers to a collection code from Selina A. Ruzi.}
    \item{\code{sample.of.ant.observed.removing}}{a character vector - whether a sample of the ant forager(s) removing seed(s) from the seed cache was collected. \code{0} = no ant sample was collected. \code{1} or \code{SAR}### indicates that there was a sample collected. The \code{SAR}### refers to a collection code from Selina A. Ruzi.}
    \item{\code{ant.sample}}{a character vector - If an ant sample was collected then \code{SAR}### indicates the collection code from Selina A. Ruzi.}
    \item{\code{ant.identification}}{a character vector - the ant species that were identified from the \code{ant.sample}}
    \item{\code{PheSus}}{a numeric vector - ant species: Pheidole susannae}
    \item{\code{EctRui}}{a numeric vector - ant species: Ectatomma ruidum}
    \item{\code{SerAma}}{a numeric vector - ant species: Sericomyrmex amabilis}
    \item{\code{PheMul}}{a numeric vector - ant species: Pheidole multispina}
    \item{\code{WasAur}}{a numeric vector - ant species: Wasmannia auropunctata}
    \item{\code{CypRim}}{a numeric vector - ant species: Cyphomyrmex rimosus}
    \item{\code{ParCor}}{a numeric vector - ant species: Paratrachymyrmex cornetzi}
    \item{\code{MycIst}}{a numeric vector - ant species: Mycetomoellerius isthmicus}
    \item{\code{MycZet}}{a numeric vector - ant species: Mycetomoellerius zeteki}
    \item{\code{PheSim}}{a numeric vector - ant species: Pheidole simonsi}
    \item{\code{PheRug}}{a numeric vector - ant species: Pheidole rugiceps}
    \item{\code{LabPra}}{a numeric vector - ant species: Labidus praedator}
    \item{\code{Phe025}}{a numeric vector - ant species: Pheidole sp. 025}
    \item{\code{PhePug}}{a numeric vector - ant species: Pheidole pugnax}
    \item{\code{AphAra}}{a numeric vector - ant species: Aphaenogaster araneoides}
    \item{\code{Phe091927}}{a numeric vector - ant species: Pheidole sp. 009_019_027}
    \item{\code{CypCos}}{a numeric vector - ant species: Cyphomyrmex costatus}
    \item{\code{ParBug}}{a numeric vector - ant species: Paratrachymyrmex bugnioni}
    \item{\code{Phe015}}{a numeric vector - ant species: Pheidole sp. 015}
    \item{\code{side}}{a character vector - the side of the plot in which the cache was placed}
    \item{\samp{position in line}}{a numeric vector - the randomized position that the cache was placed at}
    \item{\code{season}}{a character vector - the season the sample was tested in}
    \item{\code{zero.seeds.remaining}}{a character vector - \code{yes} if there were no longer any seeds remaining, \code{no} if there were still seeds remaining}
    \item{\code{diff.between.this.timeperiod.and.next}}{a numeric vector - the number of seeds lost or removed between time periods}
    \item{\code{day.time}}{a numeric vector - combination of \code{Day} and \code{Time.Point} together separated by a \code{.}}
    \item{\code{notes}}{a character vector - notes about the sampling}
  }
}
\details{
Used to determine: (i) number and percent of time periods where there was an hourly change in seed count out of all daytime intervals with seeds remaining, (ii) number and percent of time periods where ants were present, (iii) number and percent of time periods where ants were observed removing seeds, (iv) the total number of time periods ants coult have been observed removing seeds per plant species, (v) the plant species with that have more than three ant samples collected, (vi) list of ant species that had foragers observed removing seeds, and (vii) to find which ant species had foragers collected in the most number of time periods.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_etal_seed_removal_data)
(Ruzi_etal_seed_removal_data)
}
\keyword{datasets}
