% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonctions.R
\name{F_Random_SousOrdre}
\alias{F_Random_SousOrdre}
\title{Tirage aleatoire d'un sous-ordre}
\usage{
F_Random_SousOrdre(df_Especes, ordre = NULL)
}
\arguments{
\item{ordre}{Ordre dans lequel nous voulons tirer aléatoirement.
Argument facultatif (par défaut \code{ordre = NULL})}

\item{df_Espece}{\code{data.frame} avec une colonne \code{Sous-ordre}}
}
\value{
\code{character} avec le sous-ordre tiré aléatoirement
}
\description{
Permet de tirer aléatoirement un sous-ordre du \code{data.frame} \code{df_Especes}
}
\examples{
F_Random_SousOrdre(Especes)

F_Random_SousOrdre(Especes, ordre = "Odonates")

}
