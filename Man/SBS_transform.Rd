 \name{SBS_transform}
\alias{SBS_transform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for Converting Raw Incidence Data (samples by species) to Incidence Data (species by samples)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function transforms a tibble of raw incidence data in the form of samples (rows) by species (columns) to raw incidence data in the form of species (rows) by samples (columns). It can also convert raw incidence data to frequencies.
}
\usage{
SBS_transform(x, row_ID = TRUE, trap_ID, spp_ID, rem_spp = TRUE, freq = FALSE, warn = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Is the datafile. It will need to be a class \code{tibble}
}
  \item{row_ID}{
Defaults to \code{TRUE}. When \code{TRUE} it will convert row numbers to a column and will include this in the columns that it will unite together to form a unique trap information column.
}
  \item{trap_ID}{
Is a vector of characters of which columns to unite to form trap information.
}
  \item{spp_ID}{
Is a vector of characters of which columns are the different species.
}
  \item{rem_spp}{
Defaults to \code{TRUE}. When \code{TRUE} will remove rows where the species was never found in any of the traps 
}
  \item{freq}{
Defaults to \code{FALSE}. When \code{FALSE}, raw incidence tibble will be the output. If set to \code{TRUE}, the output will be as frequencies, with the number of traps being the first number and the rest being the number of traps that the species were found in.
}
  \item{warn}{
Defaults to \code{TRUE}. When \code{TRUE}, will warn the user that x is not a tibble.
}
}
\details{
The output of this function should be assigned to an object and that object used in the analyses for the associated manuscript that this data package is a part of.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Selina A. Ruzi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
%%
