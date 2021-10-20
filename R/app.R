
#' Application Especes
#'
#' Permet de lancer l'application Espèces
#'
#' @return Ouvre l'application
#'
#' @examples
#' App_Especes()
#'
#' @export
App_Especes <- function()
{
  shinyApp(ui = fluidPage(
    theme = shinythemes::shinytheme("journal"),
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          inputId = "Classe",
          label = "Classe",
          choices = c(""),
          options = list(`none-selected-text` = "Toutes les classes")
        ),
        pickerInput(
          inputId = "Ordre",
          label = "Ordre",
          choices = c(""),
          options = list(`none-selected-text` = "Tous les ordres")
        ),
        pickerInput(
          inputId = "SousOrdre",
          label = "Sous-Ordre",
          choices = c(""),
          options = list(`none-selected-text` = "Tous les sous-ordres")
        ),
        pickerInput(
          inputId = "Famille",
          label = "Famille",
          choices = c(""),
          options = list(`none-selected-text` = "Toutes les familles")
        ),
        pickerInput(
          inputId = "Genre",
          label = "Genre",
          choices = c(""),
          options = list(`none-selected-text` = "Tous les genres")
        ),
        pickerInput(
          inputId = "Sexe",
          label = "Sexe",
          choices = c(""),
          options = list(`none-selected-text` = "Sexes confondus")
        ),
        hr(),
        actionButton("action", label = "Nouveau", class = "btn-success"),
        width = 2

      ),

      mainPanel(fluidRow(
        column(
          3,
          htmlOutput("sp"),
          htmlOutput("sp_sci"),
          htmlOutput("famille"),
          htmlOutput("ssordre"),
          htmlOutput("ordre"),
          htmlOutput("sexe"),
          htmlOutput("taille"),
          htmlOutput("description")
        ),
        column(9, imageOutput("myImage"))
      ),
      width = 10)
    )
  ), server = function(input, output, session) {

    tab <- reactive({

      Especes

    })

    Liste_Classe <- reactive({

      Liste_Classe = c("", F_Liste_Classe(df_Espece = tab()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Classe",
                        choices = Liste_Classe()
      )

    })

    tab_Classe <- reactive({

      tab_Classe = F_Filtre_Par_Classe(df_Espece = tab(), classe = input$Classe)

    })

    Liste_Ordre <- reactive({

      Liste_Ordre = c("", F_Liste_Ordre(df_Espece = tab_Classe()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Ordre",
                        choices = Liste_Ordre()
      )

    })

    tab_Ordre <- reactive({

      tab_Ordre = F_Filtre_Par_Ordre(df_Espece = tab_Classe(), ordre = input$Ordre)

    })

    Liste_SsOrdre <- reactive({

      Liste_SsOrdre = c("", F_Liste_SousOrdre(df_Espece = tab_Ordre()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "SousOrdre",
                        choices = Liste_SsOrdre()
      )

    })

    tab_SsOrdre <- reactive({

      tab_SsOrdre = F_Filtre_Par_SousOrdre(df_Espece = tab_Ordre(), sousordre  = input$SousOrdre)

    })

    Liste_Famille <- reactive({

      Liste_SsOrdre = c("", F_Liste_Famille(df_Espece = tab_SsOrdre()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Famille",
                        choices = Liste_Famille()
      )

    })

    tab_Famille <- reactive({

      tab_Famille = F_Filtre_Par_Famille(df_Espece = tab_SsOrdre(), famille  = input$Famille)

    })

    Liste_Genre <- reactive({

      Liste_Genre = c("", F_Liste_Genre(tab_Famille()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Genre",
                        choices = Liste_Genre()
      )

    })

    tab_Genre <- reactive({

      tab_Genre = F_Filtre_Par_Genre(df_Espece = tab_Famille(), genre  = input$Genre)

    })

    Liste_Sexe <- reactive({

      Liste_Sexe = c("", F_Liste_Sexe(tab_Genre()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Sexe",
                        choices = Liste_Sexe()
      )

    })

    tab_Sexe <- reactive({

      tab_Sexe = F_Filtre_Par_Sexe(df_Espece = tab_Genre(), sexe  = input$Sexe)

    })

    Random_Row <- eventReactive(input$action, {

      Random_Row = F_Random_Row(tab_Sexe())

    })

    Random_Sp <- reactive({

      Random_Sp = F_Extraire_Sp(Random_Row())
    })

    Random_Sp_sci <- reactive({

      Random_Sp_sci = F_Extraire_Sp_sci(Random_Row())
    })

    Random_Famille <- reactive({

      Random_Famille = F_Extraire_Famille(Random_Row())
    })

    Random_SsOrdre <- reactive({

      Random_SsOrdre = F_Extraire_SousOrdre(Random_Row())
    })

    Random_Ordre <- reactive({

      Random_Ordre = F_Extraire_Ordre(Random_Row())
    })

    Random_Sexe <- reactive({

      Random_Sexe = F_Extraire_Sexe(Random_Row())
    })

    Random_Taille <- reactive({

      Random_Taille = F_Extraire_Taille(Random_Row())
    })

    Random_Description <- reactive({

      Random_Description = F_Extraire_Description(Random_Row())
    })

    output$sp <- renderText({ifelse(!is.na(Random_Sp()),
                                    paste("<b><u>Nom vernaculaire</u></b><br>", Random_Sp()),
                                    "") })

    output$sp_sci <- renderText({ paste("<b><u>Nom scientifique</u></b><br><i>", Random_Sp_sci(), "</i>") })

    output$famille <- renderText({ paste("<b><u>Famille</u></b><br>", Random_Famille()) })

    output$ssordre <- renderText({ ifelse(!is.na(Random_SsOrdre()),
                                          paste("<b><u>Sous-ordre</u></b><br>", Random_SsOrdre()),
                                          "") })

    output$ordre <- renderText({ paste("<b><u>Ordre</u></b><br>", Random_Ordre()) })

    output$sexe <- renderText({ ifelse(!is.na(Random_Sexe()),
                                       paste("<b><u>Sexe</u></b><br>", Random_Sexe()),
                                       "") })

    output$taille <- renderText({ paste("<b><u>Taille</u></b><br>", Random_Taille()) })

    output$description <- renderText({ paste("<b><u>Description</u></b><br>", Random_Description()) })

    Random_Img <- reactive({

      Random_Img = F_Extraire_Img(Random_Row())
    })

    output$myImage <- renderImage({

      filename <- normalizePath(file.path(system.file("extdata", paste0("Img/", Random_Img()), package = "Especes")))

      list(src = filename)

    }, deleteFile = FALSE)
  })
}


