
#' Classification phylogenetique
#'
#' Classification phylogénétique, descriptions et lien vers les photos des espèces d'insectes et d'oiseaux
#'
#' @format Un \code{data.frame} avec 11 colonnes :
#' \describe{
#'   \item{Classe}{Classe de l'espèce}
#'   \item{Ordre}{Ordre de l'espèce}
#'   \item{Sous-ordre}{Sous-ordre de l'espèce}
#'   \item{Famille}{Famille de l'espèce}
#'   \item{Genre}{Genre de l'espèce}
#'   \item{Nom vernaculaire}{Nom vernaculaire de l'espèce}
#'   \item{Nom scientifique}{Nom scientifique de l'espèce}
#'   \item{Taille}{Taille de l'espèce}
#'   \item{Description}{Description de l'espèce}
#'   \item{Sexe}{Sexe de l'espèce pour les cas de dimorphisme sexuel}
#'   \item{Img}{Nom de l'image de l'espèce}
#' }
#'
"Especes"



#' Liste des classes
#'
#' Permet d'obtenir la liste des classes du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Classe}
#'
#' @return Un vecteur avec la liste des classes du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Classe(Especes)
#'
#' @export
F_Liste_Classe <- function(df_Espece)
{
  Liste_Classe = sort(unique(df_Espece$Classe))
  return(Liste_Classe)
}


#' Filtre par classe
#'
#' Permet de filtrer le \code{data.frame} \code{df_Especes} selon la classe
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Classe}
#'
#' @return \code{data.frame} \code{df_Especes} filtré selon la classe souhaitée
#'
#' @examples
#' F_Filtre_Par_Classe(Especes, classe = "Aves")
#' F_Filtre_Par_Classe(Especes, classe = "Insectes")
#'
#' @export
F_Filtre_Par_Classe <- function(df_Espece, classe)
{
  if(classe != "")
  {
    tab_Classe = df_Espece %>%
      filter(Classe == classe)
  }else
  {
    tab_Classe = df_Espece
  }
  return(tab_Classe)
}



#' Liste des ordres
#'
#' Permet d'obtenir la liste des ordres du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Ordre}
#'
#' @return Un vecteur avec la liste des ordres du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Ordre(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Classe(classe = "Insectes") %>%
#'  F_Liste_Ordre()
#'
#' @export
F_Liste_Ordre <- function(df_Espece)
{
  Liste_Ordre = sort(unique(df_Espece$Ordre))
  return(Liste_Ordre)
}


#' Filtre par ordre
#'
#' Permet de filtrer le \code{data.frame} \code{df_Especes} selon l'ordre
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Ordre}
#'
#' @return \code{data.frame} \code{df_Especes} filtré selon l'ordre souhaité
#'
#' @examples
#' F_Filtre_Par_Ordre(Especes, ordre = "Odonates")
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Classe(classe = "Insectes") %>%
#'  F_Filtre_Par_Ordre(ordre = "Odonates")
#'
#' @export
F_Filtre_Par_Ordre <- function(df_Espece, ordre)
{
  if(ordre != "")
  {
    tab_Ordre = df_Espece %>%
      filter(Ordre == ordre)
  }else
  {
    tab_Ordre = df_Espece
  }
  return(tab_Ordre)
}


#' Liste des sous-ordres
#'
#' Permet d'obtenir la liste des sous-ordres du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Sous-ordre}
#'
#' @return Un vecteur avec la liste des sous-ordres du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_SousOrdre(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Ordre(ordre = "Odonates") %>%
#'  F_Liste_SousOrdre()
#'
#' @export
F_Liste_SousOrdre <- function(df_Espece)
{
  Liste_SousOrdre = sort(c(na.omit(unique(df_Espece$`Sous-ordre`))))
  return(Liste_SousOrdre)
}


#' Filtre par sous-ordre
#'
#' Permet de filtrer le \code{data.frame} \code{df_Especes} selon le sous-ordre
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Sous-ordre}
#'
#' @return \code{data.frame} \code{df_Especes} filtré selon le sous-ordre souhaité
#'
#' @examples
#' library(dplyr)
#' Vec_SousOrdre = Especes %>%
#'  F_Filtre_Par_Ordre(ordre = "Odonates") %>%
#'  F_Liste_SousOrdre()
#'
#' Especes %>%
#'  F_Filtre_Par_SousOrdre(sousordre = Vec_SousOrdre[1])
#'
#' @export
F_Filtre_Par_SousOrdre <- function(df_Espece, sousordre)
{
  if(sousordre != "")
  {
    tab_SousOrdre = df_Espece %>%
      filter(`Sous-ordre` == sousordre)
  }else
  {
    tab_SousOrdre = df_Espece
  }
  return(tab_SousOrdre)
}




#' Liste des familles
#'
#' Permet d'obtenir la liste des familles du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Famille}
#'
#' @return Un vecteur avec la liste des familles du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Famille(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Ordre(ordre = "Odonates") %>%
#'  F_Liste_Famille()
#'
#' @export
F_Liste_Famille <- function(df_Espece)
{
  Liste_Famille = sort(unique(df_Espece$Famille))
  return(Liste_Famille)
}


#' Filtre par famille
#'
#' Permet de filtrer le \code{data.frame} \code{df_Especes} selon la famille
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Famille}
#'
#' @return \code{data.frame} \code{df_Especes} filtré selon la famille souhaitée
#'
#' @examples
#' F_Filtre_Par_Famille(Especes, famille = "Pieridae")
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Ordre(ordre = "Odonates") %>%
#'  F_Filtre_Par_Famille(famille = "Coenagrionidae")
#'
#' @export
F_Filtre_Par_Famille <- function(df_Espece, famille)
{
  if(famille != "")
  {
    tab_Famille = df_Espece %>%
      filter(Famille == famille)
  }else
  {
    tab_Famille = df_Espece
  }
  return(tab_Famille)
}



#' Liste des genres
#'
#' Permet d'obtenir la liste des genres du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Genre}
#'
#' @return Un vecteur avec la liste des genre du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Genre(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Famille(famille = "Coenagrionidae") %>%
#'  F_Liste_Genre()
#'
#' @export
F_Liste_Genre <- function(df_Espece)
{
  Liste_Genre = sort(unique(df_Espece$Genre))
  return(Liste_Genre)
}


#' Filtre par genre
#'
#' Permet de filtrer le \code{data.frame} \code{df_Especes} selon le genre
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Genre}
#'
#' @return \code{data.frame} \code{df_Especes} filtré selon le genre souhaité
#'
#' @examples
#' F_Filtre_Par_Genre(Especes, genre = "Polyommatus")
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Famille(famille = "Coenagrionidae") %>%
#'  F_Filtre_Par_Genre(genre = "Pyrrhosoma")
#'
#'
#' @export
F_Filtre_Par_Genre <- function(df_Espece, genre)
{
  if(genre != "")
  {
    tab_Genre = df_Espece %>%
      filter(Genre == genre)
  }else
  {
    tab_Genre = df_Espece
  }
  return(tab_Genre)
}



#' Liste des sexes
#'
#' Permet d'obtenir la liste des sexes du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Sexe}
#'
#' @return Un vecteur avec la liste des sexes du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Sexe(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Genre(genre = "Eristalis") %>%
#'  F_Liste_Sexe()
#'
#' @export
F_Liste_Sexe <- function(df_Espece)
{
  Liste_SousOrdre = sort(c(na.omit(unique(df_Espece$Sexe))))
  return(Liste_SousOrdre)
}


#' Filtre par sexe
#'
#' Permet de filtrer le \code{data.frame} \code{df_Especes} selon le sexe
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Sexe}
#'
#' @return \code{data.frame} \code{df_Especes} filtré selon le sexe souhaité
#'
#' @examples
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Genre(genre = "Eristalis") %>%
#'  F_Filtre_Par_Sexe(sexe = "Femelle")
#'
#' @export
F_Filtre_Par_Sexe <- function(df_Espece, sexe)
{
  if(sexe != "")
  {
    tab_Sexe = df_Espece %>%
      filter(Sexe == sexe)
  }else
  {
    tab_Sexe = df_Espece
  }
  return(tab_Sexe)
}


#' Liste des especes
#'
#' Permet d'obtenir la liste des noms vernaculaires des espèces du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Nom vernaculaire}
#'
#' @return Un vecteur avec la liste des noms vernaculaires des espèces du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Sp(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Famille(famille = "Coenagrionidae") %>%
#'  F_Liste_Sp()
#'
#' @export
F_Liste_Sp <- function(df_Espece)
{
  Liste_Sp = sort(c(na.omit(unique(df_Espece$`Nom vernaculaire`))))
  return(Liste_Sp)
}


#' Liste des noms scientifiques
#'
#' Permet d'obtenir la liste des noms scientifiques des espèces du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Nom scientifique}
#'
#' @return Un vecteur avec la liste des noms scientifique des espèces du \code{data.frame} \code{df_Especes}
#'
#' @examples
#' F_Liste_Sp_sci(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Famille(famille = "Coenagrionidae") %>%
#'  F_Liste_Sp_sci()
#'
#' @export
F_Liste_Sp_sci <- function(df_Espece)
{
  Liste_Sp_sci = sort(c(na.omit(unique(df_Espece$`Nom scientifique`))))
  return(Liste_Sp_sci)
}


#' Tirage aleatoire d'une ligne du tableau espece
#'
#' Permet de tirer aléatoirement une ligne du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec la liste des espèces souhaitées
#'
#' @return \code{data.frame} d'une ligne tirée aléatoirement dans \code{df_Especes}
#'
#' @examples
#' F_Random_Row(Especes)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Genre(genre = "Polyommatus") %>%
#'  F_Random_Row()
#'
#' @export
F_Random_Row <- function(df_Especes)
{
  Random_Sp = df_Especes[sample(1:nrow(df_Especes), 1), ]

  return(Random_Sp)
}


#' Tirage aleatoire d'une classe
#'
#' Permet de tirer aléatoirement une classe du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Classe}
#'
#' @return \code{character} avec la classe tirée aléatoirement
#'
#' @examples
#' F_Random_Classe(Especes)
#'
#' @export
F_Random_Classe <- function(df_Especes)
{
  Liste_Classe = F_Liste_Classe(df_Especes)

  Random_Classe = sample(Liste_Classe, 1)

  return(Random_Classe)
}


#' Tirage aleatoire d'un ordre
#'
#' Permet de tirer aléatoirement un ordre du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Ordre}
#' @param classe Classe dans laquelle nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{classe = NULL})
#'
#' @return \code{character} avec l'ordre tiré aléatoirement
#'
#' @examples
#' F_Random_Ordre(Especes)
#'
#' F_Random_Ordre(Especes, classe = "Insectes")
#'
#' @export
F_Random_Ordre <- function(df_Especes, classe = NULL)
{
  if(!is.null(classe))
  {
    df_Especes2 = F_Filtre_Par_Classe(df_Especes, classe = classe)
  }else
  {
    df_Especes2 = df_Especes
  }

  Liste_Ordre = F_Liste_Ordre(df_Especes2)

  Random_Ordre = sample(Liste_Ordre, 1)

  return(Random_Ordre)
}

#' Tirage aleatoire d'un sous-ordre
#'
#' Permet de tirer aléatoirement un sous-ordre du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Sous-ordre}
#' @param ordre Ordre dans lequel nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{ordre = NULL})
#'
#' @return \code{character} avec le sous-ordre tiré aléatoirement
#'
#' @examples
#' F_Random_SousOrdre(Especes)
#'
#' F_Random_SousOrdre(Especes, ordre = "Odonates")
#'
#' @export
F_Random_SousOrdre <- function(df_Especes, ordre = NULL)
{
  if(!is.null(ordre))
  {
    df_Especes2 = F_Filtre_Par_Ordre(df_Especes, ordre = ordre)
  }else
  {
    df_Especes2 = df_Especes
  }

  Liste_SousOrdre = F_Liste_SousOrdre(df_Especes2)

  Random_SousOrdre = sample(Liste_SousOrdre, 1)

  return(Random_SousOrdre)
}


#' Tirage aleatoire d'une famille
#'
#' Permet de tirer aléatoirement une famille du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Famille}
#' @param ordre Ordre dans lequel nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{ordre = NULL})
#' @param sousordre Sous-ordre dans lequel nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{sousordre = NULL})
#'
#' @return \code{character} avec la famille tirée aléatoirement
#'
#' @examples
#' F_Random_Famille(Especes)
#'
#' F_Random_Famille(Especes, ordre = "Odonates")
#'
#' F_Random_Famille(Especes, sousordre = "Apocrites")
#'
#' @export
F_Random_Famille <- function(df_Especes, ordre = NULL, sousordre = NULL)
{
  if(!is.null(ordre))
  {
    df_Especes2 = F_Filtre_Par_Ordre(df_Especes, ordre = ordre)
  }else
  {
    df_Especes2 = df_Especes
  }

  if(!is.null(sousordre))
  {
    df_Especes3 = F_Filtre_Par_SousOrdre(df_Especes2, sousordre = sousordre)
  }else
  {
    df_Especes3 = df_Especes2
  }

  Liste_Famille = F_Liste_Famille(df_Especes3)

  Random_Famille = sample(Liste_Famille, 1)

  return(Random_Famille)
}

#' Tirage aleatoire d'un genre
#'
#' Permet de tirer aléatoirement un genre du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Genre}
#' @param famille Famille dans laquelle nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{famille = NULL})
#'
#' @return \code{character} avec le genre tiré aléatoirement
#'
#' @examples
#' F_Random_Genre(Especes)
#'
#' F_Random_Genre(Especes, famille = "Mantidae")
#'
#' @export
F_Random_Genre <- function(df_Especes, famille = NULL)
{
  if(!is.null(famille))
  {
    df_Especes2 = F_Filtre_Par_Famille(df_Especes, famille = famille)
  }else
  {
    df_Especes2 = df_Especes
  }

  Liste_Genre = F_Liste_Genre(df_Especes2)

  Random_Genre = sample(Liste_Genre, 1)

  return(Random_Genre)
}


#' Tirage aleatoire d'une espece
#'
#' Permet de tirer aléatoirement le nom vernaculaire d'une espèce du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Nom vernaculaire}
#' @param genre Genre dans lequel nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{genre = NULL})
#'
#' @return \code{character} avec le nom vernaculaire tiré aléatoirement
#'
#' @examples
#' F_Random_Sp(Especes)
#'
#' F_Random_Sp(Especes, genre = "Cerambyx")
#'
#' @export
F_Random_Sp <- function(df_Especes, genre = NULL)
{
  if(!is.null(genre))
  {
    df_Especes2 = F_Filtre_Par_Genre(df_Especes, genre = genre)
  }else
  {
    df_Especes2 = df_Especes
  }

  Liste_Sp = F_Liste_Sp(df_Especes2)

  Random_Sp = sample(Liste_Sp, 1)

  return(Random_Sp)
}


#' Tirage aleatoire du nom scientifique d'une espece
#'
#' Permet de tirer aléatoirement le nom scientifique d'une espèce du \code{data.frame} \code{df_Especes}
#'
#' @param df_Espece \code{data.frame} avec une colonne \code{Nom scientifique}
#' @param genre Genre dans lequel nous voulons tirer aléatoirement.
#' Argument facultatif (par défaut \code{genre = NULL})
#'
#' @return \code{character} avec le nom scientifique tiré aléatoirement
#'
#' @examples
#' F_Random_Sp_sci(Especes)
#'
#' F_Random_Sp_sci(Especes, genre = "Cygnus")
#'
#' @export
F_Random_Sp_sci <- function(df_Especes, genre = NULL)
{
  if(!is.null(genre))
  {
    df_Especes2 = F_Filtre_Par_Genre(df_Especes, genre = genre)
  }else
  {
    df_Especes2 = df_Especes
  }

  Liste_Sp_sci = F_Liste_Sp_sci(df_Especes2)

  Random_Sp_sci = sample(Liste_Sp_sci, 1)

  return(Random_Sp_sci)
}


#' Extraction de l'espece
#'
#' Permet d'extraire le nom vernaculaire d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Nom vernaculaire}
#'
#' @return \code{character} avec le nom vernaculaire extrait
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Sp(RandomRow)
#'
#' @export
F_Extraire_Sp <- function(df_Row)
{
  Sp = df_Row %>%
    pull(`Nom vernaculaire`)
  return(Sp)
}

#' Extraction du nom scientifique
#'
#' Permet d'extraire le nom scientifique d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Nom scientifique}
#'
#' @return \code{character} avec le nom scientifique extrait
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Sp_sci(RandomRow)
#'
#' @export
F_Extraire_Sp_sci <- function(df_Row)
{
  Sp_sci = df_Row %>%
    pull(`Nom scientifique`)
  return(Sp_sci)
}

#' Extraction de la famille
#'
#' Permet d'extraire la famille d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Famille}
#'
#' @return \code{character} avec la famille extraite
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Famille(RandomRow)
#'
#' @export
F_Extraire_Famille <- function(df_Row)
{
  Famille = df_Row %>%
    pull(Famille)
  return(Famille)
}

#' Extraction du sous-ordre
#'
#' Permet d'extraire le sous-ordre d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Sous-ordre}
#'
#' @return \code{character} avec le sous-ordre extrait
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_SousOrdre(RandomRow)
#'
#' @export
F_Extraire_SousOrdre <- function(df_Row)
{
  SousOrdre = df_Row %>%
    pull(`Sous-ordre`)
  return(SousOrdre)
}

#' Extraction de l'ordre
#'
#' Permet d'extraire l'ordre d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Ordre}
#'
#' @return \code{character} avec l'ordre extrait
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Ordre(RandomRow)
#'
#' @export
F_Extraire_Ordre <- function(df_Row)
{
  Ordre = df_Row %>%
    pull(Ordre)
  return(Ordre)
}

#' Extraction du sexe
#'
#' Permet d'extraire le sexe d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Sexe}
#'
#' @return \code{character} avec le sexe extrait
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Sexe(RandomRow)
#'
#' @export
F_Extraire_Sexe <- function(df_Row)
{
  Sexe = df_Row %>%
    pull(Sexe)
  return(Sexe)
}

#' Extraction de la taille
#'
#' Permet d'extraire la taille d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Taille}
#'
#' @return \code{character} avec la taille extraite
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Taille(RandomRow)
#'
#' @export
F_Extraire_Taille <- function(df_Row)
{
  Taille = df_Row %>%
    pull(Taille)
  return(Taille)
}

#' Extraction de la description
#'
#' Permet d'extraire la description d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Description}
#'
#' @return \code{character} avec la description extraite
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Description(RandomRow)
#'
#' @export
F_Extraire_Description <- function(df_Row)
{
  Description = df_Row %>%
    pull(Description)
  return(Description)
}


#' Extraction de l'image
#'
#' Permet d'extraire l'image d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Img}
#'
#' @return \code{character} avec le nom de l'image extraite
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#'
#' F_Extraire_Img(RandomRow)
#'
#' @export
F_Extraire_Img <- function(df_Row)
{
  Img = df_Row %>%
    pull(Img)
  return(Img)
}


#' Photo de l'espece
#'
#' Permet d'afficher la photo d'une espèce du \code{data.frame} \code{df_Row}
#'
#' @param df_Row \code{data.frame} avec une colonne \code{Img}
#'
#' @return Un plot avec la photo de l'espece
#'
#' @examples
#' RandomRow = F_Random_Row(Especes)
#' F_Photo_Espece(RandomRow)
#'
#' library(dplyr)
#' Especes %>%
#'  F_Filtre_Par_Famille("Coenagrionidae") %>%
#'  F_Random_Row() %>%
#'  F_Photo_Espece()
#'
#' @export
F_Photo_Espece <- function(df_Row)
{
  Img = F_Extraire_Img(df_Row)
  plot(load.image(system.file("extdata", paste0("Img/", Img), package = "Especes")), axes=FALSE)
}
