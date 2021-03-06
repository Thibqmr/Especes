% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions.R
\name{F_Filtre_Par_Sexe}
\alias{F_Filtre_Par_Sexe}
\title{Filtre par sexe}
\usage{
F_Filtre_Par_Sexe(df_Espece, sexe)
}
\arguments{
\item{df_Espece}{\code{data.frame} avec une colonne \code{Sexe}}
}
\value{
\code{data.frame} \code{df_Especes} filtré selon le sexe souhaité
}
\description{
Permet de filtrer le \code{data.frame} \code{df_Especes} selon le sexe
}
\examples{
library(dplyr)
Especes \%>\%
 F_Filtre_Par_Genre(genre = "Eristalis") \%>\%
 F_Filtre_Par_Sexe(sexe = "Femelle")

}
