\name{Ruzi_etal_data_removal_pitfalls_met_criteria}
\alias{Ruzi_etal_data_removal_pitfalls_met_criteria}
\docType{data}
\title{
Ruzi_etal_data_removal_pitfalls_met_criteria.csv
}
\description{
Frequency data and seed removal data of the three most commonly removed tree species (ie. there were at least three timeperiods in which there were ant samples collected) and the ant communities captured by pitfall traps at those same sites.
}
\usage{data("Ruzi_etal_data_removal_pitfalls_met_criteria")}
\format{
  A data frame with 16 observations on the following 63 variables.
  \describe{
    \item{\code{ID}}{a character vector - combination of \code{Plot_abbre} and \code{Season} and includes the tree species if sample came from observing the seed removal caches}
    \item{\code{AphAra}}{a numeric vector - ant species: Aphaenogaster araneoides}
    \item{\code{CamJTL004}}{a numeric vector - ant species: Camponotus cf. JTL-004 sp. 001}
    \item{\code{CarBre}}{a numeric vector - ant species: Carebara brevipilosa}
    \item{\code{CypDix}}{a numeric vector - ant species: Cyphomyrmex cf. dixus}
    \item{\code{CypMin}}{a numeric vector - ant species: Cyphomyrmex minutus}
    \item{\code{CypRim}}{a numeric vector - ant species: Cyphomyrmex rimosus}
    \item{\code{DolBis}}{a numeric vector - ant species: Dolichoderus bispinosus}
    \item{\code{EctRui}}{a numeric vector - ant species: Ectatomma ruidum}
    \item{\code{GnaCon}}{a numeric vector - ant species: Gnamptogenys continua}
    \item{\code{GnaReg}}{a numeric vector - ant species: Gnamptogenys regularis}
    \item{\code{HylDen}}{a numeric vector - ant species: Hylomyrma dentiloba}
    \item{\code{HypOpa}}{a numeric vector - ant species: Hypoponera opacior}
    \item{\code{LabPra}}{a numeric vector - ant species: Labidus praedator}
    \item{\code{LepJTL002}}{a numeric vector - ant species: Leptogenys JTL-002}
    \item{\code{LepPun}}{a numeric vector - ant species: Leptogenys punctaticeps}
    \item{\code{MycIst}}{a numeric vector - ant species: Mycetomoellerius isthmicus}
    \item{\code{MycTar}}{a numeric vector - ant species: Mycocepurus tardus}
    \item{\code{Myr001}}{a numeric vector - ant species: Myrmicocrypta sp. 001}
    \item{\code{Myr002}}{a numeric vector - ant species: Myrmicocrypta sp. 002}
    \item{\code{NeiHum}}{a numeric vector - ant species: Neivamyrmex humilis}
    \item{\code{NeoVer}}{a numeric vector - ant species: Neoponera verenae}
    \item{\code{NylGua}}{a numeric vector - ant species: Nylanderia cf. guatemalensis}
    \item{\code{OdoBau}}{a numeric vector - ant species: Odontomachus bauri}
    \item{\code{OdoMei}}{a numeric vector - ant species: Odontomachus meinerti}
    \item{\code{PacHar}}{a numeric vector - ant species: Pachycondyla harpax}
    \item{\code{PacImp}}{a numeric vector - ant species: Pachycondyla "impressa" - like}
    \item{\code{ParBug}}{a numeric vector - ant species: Paratrachymyrmex bugnioni}
    \item{\code{ParCor}}{a numeric vector - ant species: Paratrachymyrmex cornetzi}
    \item{\code{Phe011423}}{a numeric vector - ant species: Pheidole sp. 001_014_023}
    \item{\code{Phe015}}{a numeric vector - ant species: Pheidole sp. 015}
    \item{\code{Phe0216}}{a numeric vector - ant species: Pheidole sp. 002_016}
    \item{\code{Phe025}}{a numeric vector - ant species: Pheidole sp. 025}
    \item{\code{Phe091927}}{a numeric vector - ant species: Pheidole sp. 009_019_027}
    \item{\code{PheCfAvi}}{a numeric vector - ant species: Pheidole cf. avia}
    \item{\code{PheCfNit}}{a numeric vector - ant species: Pheidole cf. nitidicollis}
    \item{\code{PheCol}}{a numeric vector - ant species: Pheidole colobopsis}
    \item{\code{PheDas}}{a numeric vector - ant species: Pheidole dasypyx}
    \item{\code{PheGlo}}{a numeric vector - ant species: Pheidole glomericeps}
    \item{\code{PheMid}}{a numeric vector - ant species: Pheidole midas}
    \item{\code{PheMul}}{a numeric vector - ant species: Pheidole multispina}
    \item{\code{PhePug}}{a numeric vector - ant species: Pheidole pugnax}
    \item{\code{PheRug}}{a numeric vector - ant species: Pheidole rugiceps}
    \item{\code{PheSen}}{a numeric vector - ant species: Pheidole sensitiva}
    \item{\code{PheSim}}{a numeric vector - ant species: Pheidole simonsi}
    \item{\code{PheSus}}{a numeric vector - ant species: Pheidole susannae}
    \item{\code{SerAma}}{a numeric vector - ant species: Sericomyrmex amabilis}
    \item{\code{SolCfBic}}{a numeric vector - ant species: Solenopsis cf. bicolor}
    \item{\code{SolCfBre}}{a numeric vector - ant species: Solenopsis cf. brevicornis}
    \item{\code{SolCfCas}}{a numeric vector - ant species: Solenopsis cf. castor}
    \item{\code{SolCfPol}}{a numeric vector - ant species: Solenopsis cf. pollux}
    \item{\code{SolCfVin}}{a numeric vector - ant species: Solenopsis cf. vinsoni}
    \item{\code{SolCfZet}}{a numeric vector - ant species: Solenopsis cf. zeteki}
    \item{\code{SolSp007}}{a numeric vector - ant species: Solenopsis sp. 007}
    \item{\code{StrMar}}{a numeric vector - ant species: Strumigenys marginiventris}
    \item{\code{WasAur}}{a numeric vector - ant species: Wasmannia auropunctata}
    \item{\code{WasRoc}}{a numeric vector - ant species: Wasmannia rochai}
    \item{\code{Sample_num}}{a numeric vector - sample size. Either the total number of traps placed for pitfall traps or the total number of time periods where there could have been an ant observed removing seeds (different for different tree species as it is based on the number of daytime time periods where there was an hourly change in seed count and there were seeds remaining to be removed).}
    \item{\code{Plot_abbre}}{a character vector - An abbreviation of the plot (refered to as site in the associated manuscript). \code{25} = 25Ha or site 1, \code{A} = AVA or site 2, \code{D} = Dryaton or site 3, \code{P} = Pearson or site 4, \code{Z} = Zetek or site 5.}
    \item{\code{Season}}{a character vector - the season in which the sample was taken.}
    \item{\code{Species}}{a character vector - the Neotropical plant species associated with the sample. \code{P} = passive to indicate they are pitfall traps which are not associated with specific plant species.}
    \item{\code{Experiment}}{a character vector - the experiment in which the ants were captured. If pitfall then it includes the season. If \code{removal} then indicates that the sample was observed removing seeds from the seed cache.}
    \item{\code{Plot_season}}{a character vector - combination of the \code{Plot_abbre} and \code{Season}}
  }
}
\details{
Used to determine if ant communities observed removing seeds of the three most commonly observed tree species being removed by ants are distinct from the ant communities captured at the same sites by pitfall traps. 
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_etal_data_removal_pitfalls_met_criteria)
str(Ruzi_etal_data_removal_pitfalls_met_criteria)
}
\keyword{datasets}
