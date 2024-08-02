library(shiny)
library(dplyr)
library(readxl)
library(readr)
library(stringi)
source("import.R")
source("notifications.R")

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .conteneur{
  display: -webkit-box;
  display: -webkit-flex;
  display: -ms-flexbox;
  display: flex;
}
  #shiny-notification-panel{
  left:calc(50% - 150px);
  width: 300px;
  top:0;
  bottom:auto;
  right:auto;
  cursor: pointer;
}

.shiny-notification-error{
  background-color: #bd362f;
  color:white;
}

.shiny-notification-warning{
  background-color: #f89406;
  color:white;
}"))),
    titlePanel("Remplacement IPP par numéro d'inclusion"),
            div("Ce site permet de fusionner le fichier de correspondance avec celui du DIM, puis ne garder que le numéro d'inclusion et les données du DIM."),
          div("Il nécessite que les deux fichiers aient une colonne nommée IPP en majuscule."),
    div("Les fichiers peuvent être au format excel ou csv."),
    sidebarLayout(
        sidebarPanel(
          fileInput("fichierCorrespondance", label = "Fichier de correspondance",
                    placeholder = "Aucun fichier sélectionné", buttonLabel = "Parcourir..."),
          uiOutput("nincl"),
          uiOutput("dim")
        ),

        mainPanel(
          uiOutput("ippmanquants"),
          uiOutput("result")
        )
    )
)


server <- function(input, output) {
  values <- reactiveValues()
  observe({
    req(input$fichierCorrespondance)
    tab_corresp <- import_file(input$fichierCorrespondance$datapath)
    if (!any(grepl("IPP", names(tab_corresp), ignore.case = FALSE))){
      toast_showNotif("Il faut que le fichier de correspondance ait une colonne nommée IPP (en majuscule)", type = "error",
                      id = "corresp")
      return()
    }
    values$tabCorresp <- tab_corresp
  })

  observe({
    values$ready <- isTRUE(!is.null(input$colNincl) && input$colNincl != "-" && !is.null(values$tabDIM))
  })

  output$dim <- renderUI({
    req(input$colNincl != "-")
    fileInput("fichierDIM", label = "Fichier envoyé par le DIM",
              placeholder = "Aucun fichier sélectionné", buttonLabel = "Parcourir...")
  })

  output$nincl <- renderUI({
    req(values$tabCorresp)
    tagList(
      selectInput("colNincl", "Sélectionnez la colonne correspondant au numéro d'inclusion",
                  choices = c("-",colnames(values$tabCorresp)), selected = NULL)
    )
  })
  observe({
    req(input$fichierDIM)
    tab_dim <- import_file(input$fichierDIM$datapath)
    if (!any(grepl("IPP", names(tab_dim), ignore.case = FALSE))){
      showNotification("Il faut que le fichier du DIM ait une colonne nommée IPP (en majuscule)", type = "error",
                       id = "dim")
      return()
    }
    values$tabDIM <- tab_dim
  })

  observe({
    req(values$ready)
    values$final <- left_join(values$tabCorresp %>% select(all_of(c("IPP", input$colNincl))), values$tabDIM, by = "IPP")
  })

  output$result <- renderUI({
    req(values$final)
    tagList(
      h1("Table fusionnée"),
      DT::dataTableOutput("tableFinale")
    )
  })

  output$tableFinale <- DT::renderDataTable({
    values$final
  }, options = list(searching = FALSE, scrollX = '100%',
                    paging = FALSE, info = FALSE, ordering = FALSE))

  output$ippmanquants <- renderUI({
    req(values$ready)
    ipp_manquants <- anti_join(values$tabCorresp %>% select(all_of(c("IPP", input$colNincl))), values$tabDIM, by = "IPP") %>%
      pull(IPP)
    tagList(
      if (length(ipp_manquants)){
        div(class = "alert alert-danger", paste("Il manque les IPP suivants :", paste(ipp_manquants, collapse = ", ")))
      } else {
       "Aucun IPP ne manque."
      },
      fluidRow(
        column(width = 3, offset = 7,
               downloadButton("download", "Télécharger le fichier anonymisé")
        )
      )
    )
  })

  output$download <- downloadHandler(
    filename = function(){
      paste0("fichier-pmsi-urceco-", format(Sys.time(), "%Y-%m-%dT%Hh%M"), ".csv")
    },
    content = function(file){
      write.csv(values$final %>% select(-IPP), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

# Run the application
shinyApp(ui = ui, server = server)
