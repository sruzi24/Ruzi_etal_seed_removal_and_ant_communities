\name{Ruzi_etal_data_ant_morphological_measurements}
\alias{Ruzi_etal_data_ant_morphological_measurements}
\docType{data}
\title{
Ruzi_etal_data_ant_morphological_measurements.csv
}
\description{
Measurements of the most common ant species (found in at least 10 percent of the pitfall traps at a single site when the seasons were pooled) and those observed removing seeds.}
\usage{data("Ruzi_etal_data_ant_morphological_measurements")}
\format{
  A data frame with 22 observations on the following 27 variables.
  \describe{
    \item{\code{Species}}{a character vector - ant species identification}
    \item{\code{Sp_ID}}{a character vector - the shortened abbreviation for the ant species identification}
    \item{\code{Pitfalls_highest_cutoff_met}}{a character vector - cutoffs for the percentage of traps collected in at a single site both seasons pooled}
    \item{\code{Above_seed_removal}}{a character vector - whether foragers of this ant species were collected during timeperiods where ants were observed removing seed(s)}
    \item{\code{Experiment}}{a character vector - the category that the ant species fall into (ie., common in pitfall traps, observed removing seeds, or both)}
    \item{\code{Specimen_ID}}{a character vector - in this dataset all \code{pooled} as it is the aggregated measurments across all the individuals measured}
    \item{\code{Num_Individuals}}{a numeric vector - the number of individuals measured for this species}
    \item{\code{HW_avg}}{a numeric vector - average of head width measurement}
    \item{\code{HL_avg}}{a numeric vector - average of head length measurement}
    \item{\code{OW_avg}}{a numeric vector - average of intraocular width measurement}
    \item{\code{ML_avg}}{a numeric vector - average of mandible length measurement}
    \item{\code{SL_avg}}{a numeric vector - average of scape length measurement}
    \item{\code{EW_avg}}{a numeric vector - average of eye width measurement}
    \item{\code{EL_avg}}{a numeric vector - average of eye length measurement}
    \item{\code{HF_avg}}{a numeric vector - average of hind femur length measurement}
    \item{\code{WL_avg}}{a numeric vector - average of Weber's length measurement}
    \item{\code{EP_avg}}{a numeric vector - average of eye position. Eye position is calculated as head width minus intraocular width.}
    \item{\code{HW_SD}}{a numeric vector - standard deviation of head width measurement}
    \item{\code{HL_SD}}{a numeric vector - standard deviation of head length measurement}
    \item{\code{OW_SD}}{a numeric vector - standard deviation of intraocular width measurement}
    \item{\code{ML_SD}}{a numeric vector - standard deviation of mandible length measurement}
    \item{\code{SL_SD}}{a numeric vector - standard deviation of scape length measurement}
    \item{\code{EW_SD}}{a numeric vector - standard deviation of eye width measurement}
    \item{\code{EL_SD}}{a numeric vector - standard deviation of eye length measurement}
    \item{\code{HF_SD}}{a numeric vector - standard deviation of hind femur length measurement}
    \item{\code{WL_SD}}{a numeric vector - standard deviation of Weber's length measurement}
    \item{\code{EP_SD}}{a numeric vector - standard deviation of eye position}
  }
}
\details{
Used to calculate PCA and ANOVAs on the loadings.}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_etal_data_ant_morphological_measurements)
str(Ruzi_etal_data_ant_morphological_measurements)
}
\keyword{datasets}
