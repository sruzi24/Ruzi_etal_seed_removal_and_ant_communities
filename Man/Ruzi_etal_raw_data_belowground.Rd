\name{Ruzi_etal_raw_data_belowground}
\alias{Ruzi_etal_raw_data_belowground}
\docType{data}
\title{
Ruzi_etal_raw_data_belowground.csv
}
\description{
Raw incidence data of ant species captured in different samples collected from subterranean traps placed at different sites at Barro Colorado Island, Panama and associated either with seed caches from Neotropical pioneer tree species or control caches or passive traps. 
}
\usage{data("Ruzi_etal_raw_data_belowground")}
\format{
  A data frame with 160 observations on the following 113 variables.
  \describe{
    \item{\code{Sample}}{a character vector - The unique collection code associated with the trap. If the trap had no specimens in it during specimen process then it was given empty sample.}
    \item{\code{Plot}}{a character vector - One of five pre-established sites on Barro Colorado Island, Panama where sampling took place.}
    \item{\code{Experiment}}{a character vector - Defaults to \code{below} for this dataset as all traps were subterranean traps.}
    \item{\code{Replicate}}{a character vector - Which side of the pre-established plat traps were placed if known.}
    \item{\code{Year}}{a numeric vector - Sample collection year, YYYY. All was \code{2013} for this dataset.}
    \item{\code{Season}}{a character vector - Season in which the samples were collected. All in the \code{wet} season for this dataset.}
    \item{\code{Position}}{a character vector - Position the trap was placed with respect to the buried seed or bead cache if known.}
    \item{\code{Species}}{a character vector - Neotropical pioneer tree species. \code{OcPy} = Ochroma pyramidale, \code{ZaEk} = Zanthoxylum ekmanii, \code{CeLo} = Cecropia longipes, \code{glass} = silica bead control, \code{TrBl} = Trema micrantha black seed morph, \code{ApMe} = Apeiba membranaceae, \code{JaCo} = Jacaranda copaia, \code{passive} = passive control}
    \item{\code{Sample_num}}{a numeric vector - The number of samples this row represents.}
    \item{\code{Data_type}}{a character vector - The type of data that is represented here. Defaults to \code{binary} as this is incidence data.}
    \item{\code{NeiHum}}{a numeric vector - ant species: Neivamyrmex humilis}
    \item{\code{LabCoe}}{a numeric vector - ant species: Labidus coecus}
    \item{\code{LabPra}}{a numeric vector - ant species: Labidus praedator}
    \item{\code{EciBur}}{a numeric vector - ant species: Eciton burchellii foreli}
    \item{\code{NeiMac}}{a numeric vector - ant species: Neivamyrmex macrodentatus}
    \item{\code{OdoMei}}{a numeric vector - ant species: Odontomachus meinerti}
    \item{\code{OdoBau}}{a numeric vector - ant species: Odontomachus bauri}
    \item{\code{Nyl001}}{a numeric vector - ant species: Nylanderia sp 1}
    \item{\code{NylGua}}{a numeric vector - ant species: Nylanderia cf guatemalensis}
    \item{\code{OdoChe}}{a numeric vector - ant species: Odontomachus chelifer}
    \item{\code{CamSer}}{a numeric vector - ant species: Camponotus sericeiventris}
    \item{\code{TecFul}}{a numeric vector - ant species: Technomyrmex fulvus}
    \item{\code{Pse001}}{a numeric vector - ant species: Pseudomyrmex sp. 1}
    \item{\code{TapMel}}{a numeric vector - ant species: Tapinoma melanocephalum}
    \item{\code{DolBis}}{a numeric vector - ant species: Dolichoderus bispinosus}
    \item{\code{CamJTL004}}{a numeric vector - ant species: Camponotus cf. JTL-004 sp 1.}
    \item{\code{DolDeb}}{a numeric vector - ant species: Dolichoderus debilis}
    \item{\code{NeoVer}}{a numeric vector - ant species: Neoponera verenae}
    \item{\code{PacImp}}{a numeric vector - ant species: Pachycondyla "impressa" -like}
    \item{\code{MayCon}}{a numeric vector - ant species: Mayaponera constricta}
    \item{\code{PacHar}}{a numeric vector - ant species: Pachycondyla harpax}
    \item{\code{AztXan001}}{a numeric vector - ant species: Azteca cf. xanthochroa sp. 1}
    \item{\code{AztNig002}}{a numeric vector - ant species: Azteca "cf. nigro" sp. 2}
    \item{\code{NeoApi}}{a numeric vector - ant species: Neoponera apicalis}
    \item{\code{NeoLae}}{a numeric vector - ant species: Neoponera laevigata}
    \item{\code{PacJTL013}}{a numeric vector - ant species: Pachycondyla JTL-013 Longino ms}
    \item{\code{LepJTL002}}{a numeric vector - ant species: Leptogenys JTL-002}
    \item{\code{GnaCon}}{a numeric vector - ant species: Gnamptogenys continua
    \item{\code{EctTur}}{a numeric vector - ant species: Ectatomma tuberculatum}
    \item{\code{HypOpa}}{a numeric vector - ant species: Hypoponera opacior}
    \item{\code{GnaReg}}{a numeric vector - ant species: Gnamptogenys regularis}
    \item{\code{LepPun}}{a numeric vector - ant species: Leptogenys punctaticeps}
    \item{\code{WasAur}}{a numeric vector - ant species: Wasmannia auropunctata}
    \item{\code{EctRui}}{a numeric vector - ant species: Ectatomma ruidum}
    \item{\code{HylDen}}{a numeric vector - ant species: Hylomyrma dentiloba}
    \item{\code{CepUmb}}{a numeric vector - ant species: Cephalotes umbraculatus}
    \item{\code{SerAma}}{a numeric vector - ant species: Sericomyrmex amabilis}
    \item{\code{Myr002}}{a numeric vector - ant species: Myrmicocrypta sp. 2}
    \item{\code{MycTar}}{a numeric vector - ant species: Mycocepurus tardus}
    \item{\code{Myr001}}{a numeric vector - ant species: Myrmecocrypta sp. 1}
    \item{\code{CarJTL002}}{a numeric vector - ant species: Carebara cf. JTL-002 eyclemm?}
    \item{\code{CarBre}}{a numeric vector - ant species: Carebara brevipilosa}
    \item{\code{AphAra}}{a numeric vector - ant species: Aphaenogaster araenoides}
    \item{\code{TranGil}}{a numeric vector - ant species: Tranopelta gilva}
    \item{\code{MycZet}}{a numeric vector - ant species: Mycetomoellerius zeteki}
    \item{\code{ParBug}}{a numeric vector - ant species: Paratrachymyrmex bugnioni}
    \item{\code{RogFor}}{a numeric vector - ant species: Rogeria foreli}
    \item{\code{ParCor}}{a numeric vector - ant species: Paratrachymyrmex cornetzi}
    \item{\code{CypRim}}{a numeric vector - ant species: Cyphomyrmex rimosus}
    \item{\code{CypDix}}{a numeric vector - ant species: Cyphomyrmex cf. dixus}
    \item{\code{CypMin}}{a numeric vector - ant species: Cyphomyrmex minutus}
    \item{\code{CypSal}}{a numeric vector - ant species: Cyphomyrmex salvini}
    \item{\code{CypCos}}{a numeric vector - ant species: Cyphomyrmex costatus}
    \item{\code{MycCur}}{a numeric vector - ant species: Mycocepurus curvispinosus}
    \item{\code{MycIst}}{a numeric vector - ant species: Mycetomoellerius isthmicus}
    \item{\code{AttCol}}{a numeric vector - ant species: Atta colombica}
    \item{\code{StrElo}}{a numeric vector - ant species: Strumigenys elongata}
    \item{\code{StrZet}}{a numeric vector - ant species: Strumigenys zeteki}
    \item{\code{StrCar}}{a numeric vector - ant species: Strumigenys carinithorax}
    \item{\code{StrMar}}{a numeric vector - ant species: Strumigenys marginiventris}
    \item{\code{CreFla}}{a numeric vector - ant species: Crematogaster flavosensitiva}
    \item{\code{NeiIri}}{a numeric vector - ant species: Neivamyrmex cf. iridescens}
    \item{\code{EciHam}}{a numeric vector - ant species: Eciton hamatum}
    \item{\code{CepBas}}{a numeric vector - ant species: Cephalotes basalis}
    \item{\code{CepSet}}{a numeric vector - ant species: Cephalotes setulifer}
    \item{\code{RasArh}}{a numeric vector - ant species: Rasopone arhuaca}
    \item{\code{MegSp001}}{a numeric vector - ant species: Megalomyrmex sp. 1}
    \item{\code{CarUri}}{a numeric vector - ant species: Carebara urichi}
    \item{\code{WasRoc}}{a numeric vector - ant species: Wasmannia rochai}
    \item{\code{SolCfPic}}{a numeric vector - ant species: Solenopsis cf. picea}
    \item{\code{SolSp008}}{a numeric vector - ant species: Solenopsis sp. 8}
    \item{\code{SolSp007}}{a numeric vector - ant species: Solenopsis sp. 7}
    \item{\code{SolMolCom}}{a numeric vector - ant species: Solenopsis molesta complex}
    \item{\code{SolCfPyg}}{a numeric vector - ant species: Solenopsis cf. pygmaea}
    \item{\code{SolCfBic}}{a numeric vector - ant species: Solenopsis cf. bicolor}
    \item{\code{SolCfBre}}{a numeric vector - ant species: Solenopsis cf. brevicornis}
    \item{\code{SolCfPol}}{a numeric vector - ant species: Solenopsis cf. pollux}
    \item{\code{SolCfCas}}{a numeric vector - ant species: Solenopsis cf. castor}
    \item{\code{SolCfZet}}{a numeric vector - ant species: Solenopsis cf. zeteki}
    \item{\code{SolCfVin}}{a numeric vector - ant species: Solenopsis cf. vinsoni}
    \item{\code{PheSim}}{a numeric vector - ant species: Pheidole simonsi (morphospecies 7, 11)}
    \item{\code{PheDas}}{a numeric vector - ant species: Pheidole dasypyx (sp. 8)}
    \item{\code{Phe091927}}{a numeric vector - ant species: Pheidole sp. 091927)}
    \item{\code{PhePug}}{a numeric vector - ant species: Pheidole pugnax}
    \item{\code{PheGlo}}{a numeric vector - ant species: Pheidole glomericeps}
    \item{\code{PheCol}}{a numeric vector - ant species: Pheidole colobopsis}
    \item{\code{PheMul}}{a numeric vector - ant species: Pheidole multispina}
    \item{\code{Phe011423}}{a numeric vector - ant species: Pheidole sp. 011423}
    \item{\code{Phe0216}}{a numeric vector - ant species: Pheidole sp. 0216}
    \item{\code{Phe031}}{a numeric vector - ant species: Pheidole sp. 31}
    \item{\code{PheMid}}{a numeric vector - ant species: Pheidole midas}
    \item{\code{PheSen}}{a numeric vector - ant species: Pheidole sensitiva}
    \item{\code{PheCfAvi}}{a numeric vector - ant species: Pheidole cf. avia}
    \item{\code{Phe015}}{a numeric vector - ant species: Pheidole sp. 15}
    \item{\code{PheRug}}{a numeric vector - ant species: Pheidole rugiceps}
    \item{\code{SolTer}}{a numeric vector - ant species: Solenopsis terricola}
    \item{\code{PheZet}}{a numeric vector - ant species: Pheidole zeteki}
    \item{\code{PheSus}}{a numeric vector - ant species: Pheidole susannae}
    \item{\code{Phe025}}{a numeric vector - ant species: Pheidole sp. 025}
    \item{\code{PheCfNit}}{a numeric vector - ant species: Pheidole cf. nitidicollis}
    \item{\code{PheSub}}{a numeric vector - ant species: Pheidole subarmata}
    \item{\code{Phe028}}{a numeric vector - ant species: Pheidole sp. 028}
    \item{\code{PheFim}}{a numeric vector - ant species: Pheidole fimbriata}
  }
}
\details{
This file is used to generate belowground species accumulation curves, belowground sampling coverage, and species estimates between traps associated with control caches (bead caches and passive samples) and traps associated with seed caches.
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ruzi_etal_raw_data_belowground)
## maybe str(Ruzi_etal_raw_data_belowground) ; plot(Ruzi_etal_raw_data_belowground) ...
}
\keyword{datasets}
