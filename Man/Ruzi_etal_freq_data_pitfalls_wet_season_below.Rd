\name{Ruzi_etal_freq_data_pitfalls_wet_season_below}
\alias{Ruzi_etal_freq_data_pitfalls_wet_season_below}
\docType{data}
\title{
Ruzi_etal_freq_data_pitfalls_wet_season_below.csv
}
\description{
Frequency data of ant species captured at different sites collected using pitfall or subterranean traps placed in the wet season. Data was collected from five sites at Barro Colorado Island, Panama.
}
\usage{data("Ruzi_etal_freq_data_pitfalls_wet_season")}
\format{
  A data frame with 41 observations on the following 54 variables.
  \describe{
    \item{\code{ID}}{a character vector - combination of \code{Plot_abbre} and \code{Season}}
    \item{\code{AphAra}}{a numeric vector - ant species: Aphaenogaster araneoides}
    \item{\code{AztXan001}}{a numeric vector - ant species: Azteca cf. xanthochroa sp. 001}
    \item{\code{CarBre}}{a numeric vector - ant species: Carebara brevipilosa}
    \item{\code{CarUri}}{a numeric vector - ant species: Carebara urichi}
    \item{\code{CypMin}}{a numeric vector - ant species: Cyphomyrmex minutus}
    \item{\code{EctRui}}{a numeric vector - ant species: Ectatomma ruidum}
    \item{\code{GnaCon}}{a numeric vector - ant species: Gnampotgenys continua}
    \item{\code{GnaReg}}{a numeric vector - ant species: Gnamptogenys regularis}
    \item{\code{HypOpa}}{a numeric vector - ant species: Hypoponera opacior}
    \item{\code{LabCoe}}{a numeric vector - ant species: Labidus coecus}
    \item{\code{LepPun}}{a numeric vector - ant species: Leptogenys punctaticeps}
    \item{\code{MayCon}}{a numeric vector - ant species: Mayaponera constricta}
    \item{\code{MegSp001}}{a numeric vector - ant species: Megalomyrmex sp. 001}
    \item{\code{MycTar}}{a numeric vector - ant species: Mycocepurus tardus}
    \item{\code{NeiHum}}{a numeric vector - ant species: Neivamyrmex humilis}
    \item{\code{NeiIri}}{a numeric vector - ant species: Neivamyrmex cf. iridescens}
    \item{\code{NeiMac}}{a numeric vector - ant species: Neivamyrmex macrodentatus}
    \item{\code{NeoVer}}{a numeric vector - ant species: Neoponera verenae}
    \item{\code{Nyl001}}{a numeric vector - ant species: Nylanderia sp. 001}
    \item{\code{OdoBau}}{a numeric vector - ant species: Odotomachus bauri}
    \item{\code{OdoMei}}{a numeric vector - ant species: Odontomachus meinerti}
    \item{\code{PacHar}}{a numeric vector - ant species: Pachycondyla harpax}
    \item{\code{Phe011423}}{a numeric vector - ant species: Pheidole sp. 001014023}
    \item{\code{Phe015}}{a numeric vector - ant species: Pheidole sp. 015}
    \item{\code{Phe0216}}{a numeric vector - ant species: Pheidole sp. 002_016}
    \item{\code{Phe025}}{a numeric vector - ant species: Pheidole sp. 025}
    \item{\code{Phe031}}{a numeric vector - ant species: Pheidole sp. 031}
    \item{\code{Phe091927}}{a numeric vector - ant species: Pheidole sp. 009_019_027}
    \item{\code{PheCfAvi}}{a numeric vector - ant species: Pheidole cf. avia}
    \item{\code{PheCol}}{a numeric vector - ant species: Pheidole colobopsis}
    \item{\code{PheDas}}{a numeric vector - ant species: Pheidole dasypyx}
    \item{\code{PheGlo}}{a numeric vector - ant species: Pheidole glomericeps}
    \item{\code{PheMul}}{a numeric vector - ant species: Pheidole multispina}
    \item{\code{PhePug}}{a numeric vector - ant species: Pheidole pugnax}
    \item{\code{PheRug}}{a numeric vector - ant species: Pheidole rugiceps}
    \item{\code{PheSen}}{a numeric vector - ant species: Pheidole sensitiva}
    \item{\code{PheSim}}{a numeric vector - ant species: Pheidole simonsi}
    \item{\code{RogFor}}{a numeric vector - ant species: Rogeria foreli}
    \item{\code{SerAma}}{a numeric vector - ant species: Sericomyrmex amabilis}
    \item{\code{SolCfBic}}{a numeric vector - ant species: Solenopsis cf. bicolor}
    \item{\code{SolCfBre}}{a numeric vector - ant species: Solenopsis cf. brevicornis}
    \item{\code{SolCfCas}}{a numeric vector - ant species: Solenopsis cf. castor}
    \item{\code{SolCfPol}}{a numeric vector - ant species: Solenopsis cf. pollux}
    \item{\code{SolCfVin}}{a numeric vector - ant species: Solenopsis cf. vinsoni}
    \item{\code{SolSp008}}{a numeric vector - ant species: Solenopsis sp. 008}
    \item{\code{StrMar}}{a numeric vector - ant species: Strumigenys marginiventris}
    \item{\code{TranGil}}{a numeric vector - ant species: Tranopelta gilva}
    \item{\code{WasAur}}{a numeric vector - ant species: Wasmannia auropunctata}
    \item{\code{Sample_num}}{a numeric vector - total number of traps placed at this plot or combination of plot and cache type during the wet season}
    \item{\code{Plot_abbre}}{a character vector - An abbreviation of the plot (refered to as site in the associated manuscript). \code{25} = 25Ha or site 1, \code{A} = AVA or site 2, \code{D} = Dryaton or site 3, \code{P} = Pearson or site 4, \code{Z} = Zetek or site 5.}
    \item{\code{Season}}{a character vector - the season in which the sample was taken. For this dataset it can only be during the wet season.}
    \item{\code{Species}}{a character vector - the Neotropical plant species associated with the sample.}
    \item{\code{Experiment}}{a character vector - combination of the trap used and the season placed. Can either be \code{pitfall_wet}  or \code{below} for this dataset.}
  }
}
\details{
This file is used to compare ant communities captured by pitfall traps to the subterranean traps.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_etal_freq_data_pitfalls_wet_season)
## maybe str(Ruzi_etal_freq_data_pitfalls_wet_season) ; plot(Ruzi_etal_freq_data_pitfalls_wet_season) ...
}
\keyword{datasets}
