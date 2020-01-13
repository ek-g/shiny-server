library(shiny)
library(shinyalert)
library(shinybusy)
library(shinycssloaders)
library(DiagrammeR)
library(PRISMAstatement)

box_width <- 200

ui <- fluidPage(
  titlePanel("PRISMA-Flow Diagram generator"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "select { max-width: 160px; }"),
        tags$style(type="text/css", ".span4 { max-width: 250px; }"),
        tags$style(type="text/css", ".well { max-width: 240px; }"),
        tags$style(HTML("input[type=\"number\"] {width: 70px;}")),
      ),
      fluidRow(
        column(5, numericInput("found", "Databases", 10, width = box_width, min = 0)),
        column(5, numericInput("found_other", "Other", 1, width = box_width, min = 0))
      ),
      numericInput("no_dupes", "Without duplicates", 5, width = box_width, min = 0),
      fluidRow(
        column(5, numericInput("screened", "Screened", 5, width = box_width, min = 0)),
        column(5, numericInput("screen_exclusions", "Exclusions", 1, width = box_width, min = 0))
      ),
      fluidRow(
        column(5, numericInput("full_text", HTML("Full&#8209;texts"), 4, width = box_width, min = 0)),
        column(5, numericInput("full_text_exclusions", "Exclusions", 2, width = box_width, min = 0))
      ),
      numericInput("qualitative", "Qualitative synthesis", 2, width = box_width, min = 0),
      numericInput("quantitative", "Quantitative synthesis", 1, width = box_width, min = 0),
      checkboxInput("extra_dupes_box", "Box for excluded duplicates", value = FALSE),
      numericInput("font_size", "Font size", 15, width = 80, min = 12, max = 20),
      p(actionButton("previewButton", "Preview", icon("refresh"))),
      p(downloadButton("download", "Download PDF")),
      p(HTML("Created by: <br/> Eero Kuusisto-Gussmann - 2019")),
      width = 3),
    mainPanel(
      withSpinner(grVizOutput("preview", width = "80%", height = 500),
                  type = 5, size = 0.4),
      width = 9),
    fluid = FALSE),
  useShinyalert(),
  add_busy_spinner(spin = "fading-circle",
                   position = "bottom-left"))


  
server <- function(input, output) {
    output$preview <- renderGrViz({
      input$previewButton
      isolate({
        warnmess <<- tryCatch(
          graph <<- prisma(found = input$found,
                           found_other = input$found_other,
                           no_dupes = input$no_dupes,
                           screened = input$screened,
                           screen_exclusions = input$screen_exclusions,
                           full_text = input$full_text,
                           full_text_exclusions = input$full_text_exclusions,
                           qualitative = input$qualitative,
                           quantitative = input$quantitative,
                           extra_dupes_box = input$extra_dupes_box,
                           font_size = input$font_size),
          warning = function(w){w})$message
        graph
      })})
    output$download <- downloadHandler(
      filename = "prisma.pdf",
      content = function(file){
        prisma_pdf(graph, file)
        
      }
    )
    observeEvent(input$previewButton, 
                 if(!is.null(warnmess)) shinyalert("Oops!",
                                                   paste(warnmess, "You should check your numbers!"),
                                                   type = "warning"))
  }

shinyApp(ui, server, options = list(width = 400))