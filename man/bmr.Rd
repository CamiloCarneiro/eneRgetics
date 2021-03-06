% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmr.R
\name{bmr}
\alias{bmr}
\title{Calculates the Basal Metabolic Rate of shorebirds}
\usage{
bmr(body_mass, region)
}
\arguments{
\item{body_mass}{body mass in grams.}

\item{region}{"temp" or "trop", for temperate or tropical, respectively.}
}
\value{
Returns the basal metabolic rate in W.
}
\description{
Calculates the Basal Metabolic Rate of shorebirds in temperate or tropical
sites, following Kersten & Piersma (1987) and Kersten et al. (1998).
}
\details{
Kersten, M., Bruinzeel, L., Wiersma, P. & Piersma, T.
1998. Reduced basal metabolic rate of migratory waders wintering in coastal
Africa. - Ardea 86: 71-80.\cr
Kersten, M. &  Piersma, T. 1987. High Levels of Energy Expenditure in
Shorebirds; Metabolic Adaptations to an Energetically Expensive Way of Life.
- Ardea 75: 175-187.
}
\examples{
bmr(0.46, "trop")
}
