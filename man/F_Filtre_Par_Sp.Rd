% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions.R
\name{F_Filtre_Par_Sp}
\alias{F_Filtre_Par_Sp}
\title{Filtre par le nom vernaculaire de l'espece}
\usage{
F_Filtre_Par_Sp(df_Espece, espece)
}
\arguments{
\item{df_Espece}{\code{data.frame} avec une colonne \code{Nom vernaculaire}}
}
\value{
\code{data.frame} \code{df_Especes} filtré selon l'espèce
}
\description{
Permet de filtrer le \code{data.frame} \code{df_Especes} selon l'espèce d'après le nom vernaculaire
}
\examples{
library(dplyr)
Especes \%>\%
 F_Filtre_Par_Genre(genre = "Anas") \%>\%
 F_Filtre_Par_Sp(espece = "Canard colvert")

}
