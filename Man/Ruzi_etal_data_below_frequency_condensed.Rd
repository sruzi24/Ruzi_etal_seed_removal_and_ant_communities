\name{Ruzi_etal_data_below_frequency_condensed}
\alias{Ruzi_etal_data_below_frequency_condensed}
\docType{data}
\title{
Ruzi_etal_data_below_frequency_condensed.csv
}
\description{
Frequency data of ant species captured in different samples collected from subterranean traps placed at different sites at Barro Colorado Island, Panama and associated either with seed caches from Neotropical pioneer tree species or control caches or passive traps. Only ant species that were captured are included in this file.}
\usage{data("Ruzi_etal_data_below_frequency_condensed")}
\format{
  A data frame with 36 observations on the following 26 variables.
  \describe{
    \item{\code{ID}}{a character vector - a combination of \code{Plot_abbre} and \code{Species}}
    \item{\code{CarUri}}{a numeric vector - ant species: Carebara urichi}
    \item{\code{EctRui}}{a numeric vector - ant species: Ectatomma ruidum}
    \item{\code{LabCoe}}{a numeric vector - ant species: Labidus coecus}
    \item{\code{NeiIri}}{a numeric vector - ant species: Neivamyrmex iridescens}
    \item{\code{NeiMac}}{a numeric vector - ant species: Neivamyrmex macrodentatus}
    \item{\code{Nyl001}}{a numeric vector - ant species: Nylanderia sp. 001}
    \item{\code{PacHar}}{a numeric vector - ant species: Pachycondyla harpax}
    \item{\code{Phe011423}}{a numeric vector - ant species: Pheidole sp. 001_014_023}
    \item{\code{Phe0216}}{a numeric vector - ant species: Pheidole sp. 002_016}
    \item{\code{PheCol}}{a numeric vector - ant species: Pheidole colobopsis}
    \item{\code{PheGlo}}{a numeric vector - ant species: Pheidole glomericeps}
    \item{\code{PheMul}}{a numeric vector - ant species: Pheidole multispina}
    \item{\code{PhePug}}{a numeric vector - ant species: Pheidole pugnax}
    \item{\code{RogFor}}{a numeric vector - ant species: Rogeria foreli}
    \item{\code{SolCfBic}}{a numeric vector - ant species: Solenopsis cf. bicolor}
    \item{\code{SolCfCas}}{a numeric vector - ant species: Solenopsis cf. castor}
    \item{\code{SolCfPol}}{a numeric vector - ant species: Solenopsis cf. pollux}
    \item{\code{SolCfVin}}{a numeric vector - ant species: Solenopsis cf. vinsoni}
    \item{\code{TranGil}}{a numeric vector - ant species: Tranopelta gilva}
    \item{\code{WasAur}}{a numeric vector - ant species: Wasmannia auropunctata}
    \item{\code{Sample_num}}{a numeric vector - total number of traps placed at this plot and cache type during the wet season. It is always 4 for this dataset.}
    \item{\code{Plot_abbre}}{a character vector - An abbreviation of the plot (refered to as site in the associated manuscript). \code{25} = 25Ha or site 1, \code{A} = AVA or site 2, \code{D} = Dryaton or site 3, \code{P} = Pearson or site 4, \code{Z} = Zetek or site 5.}
    \item{\code{Season}}{a character vector - the season in which the sample was taken. For this dataset it can only be during the wet season.}
    \item{\code{Species}}{a character vector - the Neotropical plant species associated with the sample.}
    \item{\code{Experiment}}{a character vector - the community that was sampled. For this dataset, all are \code{below}.}
  }
}
\details{
Used to determine which ant species are captured in different trap types.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_etal_data_below_frequency_condensed)
## maybe str(Ruzi_etal_data_below_frequency_condensed) ; plot(Ruzi_etal_data_below_frequency_condensed) ...
}
\keyword{datasets}
