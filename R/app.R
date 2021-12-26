
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
    theme = shinythemes::shinytheme("cosmo"),
    navbarPage(
      "Espèces",
      tabPanel(
        "Exploration",
        sidebarLayout(
          sidebarPanel(
            pickerInput(
              inputId = "Classe",
              label = "Classe",
              choices = c(""),
              options = list(`none-selected-text` = "Toutes les classes"),
              choicesOpt = list(style = "color:red;")
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
              inputId = "SousFamille",
              label = "Sous-famille",
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
              inputId = "Espece",
              label = "Espèce",
              choices = c(""),
              options = list(`none-selected-text` = "Toutes les espèces")
            ),
            pickerInput(
              inputId = "Sexe",
              label = "Sexe",
              choices = c(""),
              options = list(`none-selected-text` = "Sexes confondus")
            ),
            hr(style = "border-top: none;"),
            actionBttn("action",
                       label = "Nouveau",
                       style = "jelly",
                       color = "success",
                       icon = icon("crow")),
            width = 2

          ),

          mainPanel(fluidRow(
            column(
              3,
              htmlOutput("sp"),
              htmlOutput("sp_sci"),
              htmlOutput("sousfamille"),
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

      ),
      tabPanel(
        "Quiz",
        sidebarLayout(
          sidebarPanel(
            pickerInput(
              inputId = "Classe_Quiz",
              label = "Classe",
              choices = c(""),
              options = list(`none-selected-text` = "Toutes les classes")
            ),
            pickerInput(
              inputId = "Ordre_Quiz",
              label = "Ordre",
              choices = c(""),
              options = list(`none-selected-text` = "Tous les ordres")
            ),
            pickerInput(
              inputId = "SousOrdre_Quiz",
              label = "Sous-Ordre",
              choices = c(""),
              options = list(`none-selected-text` = "Tous les sous-ordres")
            ),
            pickerInput(
              inputId = "Famille_Quiz",
              label = "Famille",
              choices = c(""),
              options = list(`none-selected-text` = "Toutes les familles")
            ),
            radioButtons(inputId = "NomSp",
                         label = "Nom de l'espèce",
                         choices = c("Nom vernaculaire", "Nom scientifique")),
            hr(style = "border-top: none;"),
            actionBttn("action_Quiz",
                       label = "Nouveau",
                       style = "jelly",
                       color = "success",
                       icon = icon("bug")),
            width = 2

          ),

          mainPanel(fluidRow(
            column(2, align = "center",
                   wellPanel(
                     actionBttn(
                       inputId = "EspA",
                       label = "-",
                       style = "stretch",
                       color = "success"
                     ),
                     hr(style = "border-top: none;"),
                     actionBttn(
                       inputId = "EspB",
                       label = "-",
                       style = "stretch",
                       color = "success"
                     ),
                     hr(style = "border-top: none;"),
                     actionBttn(
                       inputId = "EspC",
                       label = "-",
                       style = "stretch",
                       color = "success"
                     ),
                     hr(style = "border-top: none;")
                   )
            ),
            column(9, verbatimTextOutput("summary"),
                   imageOutput("myImage_Quiz"))
          ),
          width = 10)
        )

      )

    )
  ),
  server = function(input, output, session) {

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

    Liste_SsFamille <- reactive({

      Liste_SsFamille = c("", F_Liste_SousFamille(df_Espece = tab_Famille()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "SousFamille",
                        choices = Liste_SsFamille()
      )

    })

    tab_SousFamille <- reactive({

      tab_SousFamille = F_Filtre_Par_SousFamille(df_Espece = tab_Famille(), sousfamille  = input$SousFamille)

    })

    Liste_Genre <- reactive({

      Liste_Genre = c("", F_Liste_Genre(tab_SousFamille()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Genre",
                        choices = Liste_Genre()
      )

    })

    tab_Genre <- reactive({

      tab_Genre = F_Filtre_Par_Genre(df_Espece = tab_SousFamille(), genre  = input$Genre)

    })

    Liste_Sp <- reactive({

      Liste_Sp = c("", F_Liste_Sp(tab_Genre()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Espece",
                        choices = Liste_Sp()
      )

    })

    tab_Sp <- reactive({

      tab_Sp = F_Filtre_Par_Sp(df_Espece = tab_Genre(), espece  = input$Espece)

    })

    Liste_Sexe <- reactive({

      Liste_Sexe = c("", F_Liste_Sexe(tab_Sp()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Sexe",
                        choices = Liste_Sexe()
      )

    })

    tab_Sexe <- reactive({

      tab_Sexe = F_Filtre_Par_Sexe(df_Espece = tab_Sp(), sexe  = input$Sexe)

    })

    Random_Row <- eventReactive(input$action, {

      Random_Row = F_Random_Row(tab_Sexe())

    })

    Random_Sp <- reactive({

      Random_Sp = Random_Row() %>%
        F_Extraire_Sp()
    })

    Random_Sp_sci <- reactive({

      Random_Sp_sci = Random_Row() %>%
        F_Extraire_Sp_sci()
    })

    Random_SsFamille <- reactive({

      Random_SsFamille = Random_Row() %>%
        pull(`Sous-famille`)
    })

    Random_Famille <- reactive({

      Random_Famille = Random_Row() %>%
        pull(`Famille`)
    })

    Random_SsOrdre <- reactive({

      Random_SsOrdre = Random_Row() %>%
        pull(`Sous-ordre`)
    })

    Random_Ordre <- reactive({

      Random_Ordre = Random_Row() %>%
        pull(`Ordre`)
    })

    Random_Sexe <- reactive({

      Random_Sexe = Random_Row() %>%
        pull(`Sexe`)
    })

    Random_Taille <- reactive({

      Random_Taille = Random_Row() %>%
        pull(`Taille`)
    })

    Random_Description <- reactive({

      Random_Famille = Random_Row() %>%
        pull(`Description`)
    })

    output$sp <- renderText({ifelse(!is.na(Random_Sp()),
                                    paste("<b><u>Nom vernaculaire</u></b><br>", Random_Sp()),
                                    "") })

    output$sp_sci <- renderText({ paste("<b><u>Nom scientifique</u></b><br><i>", Random_Sp_sci(), "</i>") })

    output$sousfamille <- renderText({ifelse(!is.na(Random_SsFamille()),
                                             paste("<b><u>Sous-famille</u></b><br>", Random_SsFamille()),
                                             "") })

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

      Random_Img = Random_Row() %>%
        pull(Img)
    })

    output$myImage <- renderImage({
      # When input$n is 3, filename is ./images/image3.jpeg
      filename <- normalizePath(file.path(system.file("extdata", paste0("Img/", Random_Img()), package = "Especes")))

      # Return a list containing the filename and alt text
      list(src = filename)

    }, deleteFile = FALSE)



    observe({

      updatePickerInput(session = session,
                        inputId = "Classe_Quiz",
                        choices = Liste_Classe()
      )

    })

    tab_Classe_Quiz <- reactive({

      tab_Classe_Quiz = F_Filtre_Par_Classe(df_Espece = tab(), classe = input$Classe_Quiz)

    })

    Liste_Ordre_Quiz <- reactive({

      Liste_Ordre_Quiz = c("", F_Liste_Ordre(df_Espece = tab_Classe_Quiz()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Ordre_Quiz",
                        choices = Liste_Ordre_Quiz()
      )

    })

    tab_Ordre_Quiz <- reactive({

      tab_Ordre_Quiz = F_Filtre_Par_Ordre(df_Espece = tab_Classe_Quiz(), ordre = input$Ordre_Quiz)

    })

    Liste_SsOrdre_Quiz <- reactive({

      Liste_SsOrdre_Quiz = c("", F_Liste_SousOrdre(df_Espece = tab_Ordre_Quiz()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "SousOrdre_Quiz",
                        choices = Liste_SsOrdre_Quiz()
      )

    })

    tab_SsOrdre_Quiz <- reactive({

      tab_SsOrdre_Quiz = F_Filtre_Par_SousOrdre(df_Espece = tab_Ordre_Quiz(), sousordre  = input$SousOrdre_Quiz)

    })

    Liste_Famille_Quiz <- reactive({

      Liste_SsOrdre_Quiz = c("", F_Liste_Famille(df_Espece = tab_SsOrdre_Quiz()))

    })

    observe({

      updatePickerInput(session = session,
                        inputId = "Famille_Quiz",
                        choices = Liste_Famille_Quiz()
      )

    })

    Tirage <- eventReactive(input$action_Quiz, {

      Tirage <- F_Tirage_Especes(
        df_Espece = tab(),
        classe = input$Classe_Quiz,
        ordre = input$Ordre_Quiz,
        sousordre = input$SousOrdre_Quiz,
        famille = input$Famille_Quiz,
        Sp_sci = (input$NomSp != "Nom vernaculaire"))

    })

    Reechantillonnage <- reactive({

      Reechantillonnage <- sample(Tirage(), 3)

    })

    Bonne_Espece_Quiz <- reactive({

      Bonne_Espece_Quiz <- Tirage()[1]

    })

    Random_Img_Quiz <- eventReactive(input$action_Quiz, {

      if(input$NomSp == "Nom vernaculaire")
      {
        Random_Img_Quiz <- tab() %>%
          mutate(`Nom vernaculaire` = ifelse(is.na(`Nom vernaculaire`), `Nom scientifique`, `Nom vernaculaire`)) %>%
          F_Filtre_Par_Sp(espece = Bonne_Espece_Quiz()) %>%
          F_Random_Row() %>%
          pull(Img)
      }else
      {
        Random_Img_Quiz <- F_Filtre_Par_Sp_sci(tab(), espece = Bonne_Espece_Quiz()) %>%
          F_Random_Row() %>%
          pull(Img)
      }

    })


    output$myImage_Quiz <- renderImage({
      filename <- normalizePath(file.path(system.file("extdata", paste0("Img/", Random_Img_Quiz()), package = "Especes")))

      list(src = filename)

    }, deleteFile = FALSE)


    observe({

      updateActionButton(session = session,
                         inputId = "EspA",
                         label = Reechantillonnage()[1]
      )

    })

    observe({

      updateActionButton(session = session,
                         inputId = "EspB",
                         label = Reechantillonnage()[2]
      )

    })

    observe({

      updateActionButton(session = session,
                         inputId = "EspC",
                         label = Reechantillonnage()[3]
      )

    })

    observeEvent(input$EspA, {
      sendSweetAlert(
        session = session,
        title = ifelse(Reechantillonnage()[1] == Bonne_Espece_Quiz(),
                       "Bonne réponse !", "Mauvaise réponse !"),
        text = ifelse(Reechantillonnage()[1] == Bonne_Espece_Quiz(),
                      paste("C'est bien l'espèce", tolower(Bonne_Espece_Quiz()), "!"),
                      paste("C'était l'espèce", tolower(Bonne_Espece_Quiz()), "!")),
        type = ifelse(Reechantillonnage()[1] == Bonne_Espece_Quiz(),
                      "success", "error")
      )
    })

    observeEvent(input$EspB, {
      sendSweetAlert(
        session = session,
        title = ifelse(Reechantillonnage()[2] == Bonne_Espece_Quiz(),
                       "Bonne réponse !", "Mauvaise réponse !"),
        text = ifelse(Reechantillonnage()[2] == Bonne_Espece_Quiz(),
                      paste("C'est bien l'espèce", tolower(Bonne_Espece_Quiz()), "!"),
                      paste("C'était l'espèce", tolower(Bonne_Espece_Quiz()), "!")),
        type = ifelse(Reechantillonnage()[2] == Bonne_Espece_Quiz(),
                      "success", "error")
      )
    })

    observeEvent(input$EspC, {
      sendSweetAlert(
        session = session,
        title = ifelse(Reechantillonnage()[3] == Bonne_Espece_Quiz(),
                       "Bonne réponse !", "Mauvaise réponse !"),
        text = ifelse(Reechantillonnage()[3] == Bonne_Espece_Quiz(),
                      paste("C'est bien l'espèce", tolower(Bonne_Espece_Quiz()), "!"),
                      paste("C'était l'espèce", tolower(Bonne_Espece_Quiz()), "!")),
        type = ifelse(Reechantillonnage()[3] == Bonne_Espece_Quiz(),
                      "success", "error")
      )
    })
  })
}


