library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(googlesheets4)
library(readr)
library(plotly)
library(shinydashboard)
library(DT)

datapath <- "data/risklayer.csv"

risklayer <- "1wg-s4_Lz2Stil6spQEYFdZaBEp8nWW26gVyfHqvcl8s"

tzone <- "Europe/Berlin"

Sys.setenv(TZ = "Europe/Berlin")

today <- format(today(tzone = tzone), "%d.%m.")

today_format <- format(today(tzone = tzone), "%d.%m.%Y")

sheets_deauth()

todaystats <- sheets_read(risklayer, sheet = "Statistics", range = "B1:I17")  %>% 
  rename(Bundesland = 1,
         !!today := 3,
         Bevoelkerung = 6,
         Tote = 8)

heute <- todaystats %>% 
  select(1,3) %>% 
  pivot_longer(-Bundesland, names_to = "Datum", values_to = "Infizierte") %>% 
  mutate(Datum = as_date(Datum, tz = tzone, format = "%d.%m.")) %>%
  arrange(Bundesland, Datum)

bevoelk <- todaystats %>% 
  select(1, 6)

gesamtzahl <- sum(todaystats %>% select(!!today))

kuerzel <- tribble(~Kuerzel, ~Bundesland,
                   "BW", "Baden-Württemberg",
                   "BY", "Bayern",
                   "BE", "Berlin",
                   "BB", "Brandenburg",
                   "HB", "Bremen",
                   "HH", "Hamburg",
                   "HE", "Hessen",
                   "MV", "Mecklenburg-Vorpommern",
                   "NI", "Niedersachsen",
                   "NW", "Nordrhein-Westfalen",
                   "RP", "Rheinland-Pfalz",
                   "SL", "Saarland",
                   "SN", "Sachsen",
                   "ST", "Sachsen-Anhalt",
                   "SH", "Schleswig-Holstein",
                   "TH", "Thüringen")

if(date(file.info(datapath)$mtime) < today(tzone = tzone)){
    
    landdata_raw <- sheets_read(risklayer, sheet = "Curve2", range = cell_rows(31:47)) %>% 
        select_if(~sum(!is.na(.)) > 0)
    
    landdata <- landdata_raw %>%
        pivot_longer(-Bundesland, names_to = "Datum", values_to = "Infizierte") %>% 
        mutate(Datum = as_date(Datum, tz = tzone, format = "%e.%m.")) %>%
        arrange(Bundesland, Datum)
    
    write_csv(landdata, datapath)
    
    }else landdata <- read_csv(datapath)

landdata <- left_join(landdata, kuerzel, "Bundesland")

landdata <- full_join(landdata, heute, by = c("Bundesland", "Datum", "Infizierte")) %>% 
  arrange(Bundesland, Datum)

landdata <- left_join(landdata, bevoelk, "Bundesland")

heute <- left_join(heute, bevoelk) %>% 
  left_join(kuerzel)

todesfaelle <- left_join(todaystats %>% select(Bundesland, Tote, Bevoelkerung), kuerzel, "Bundesland")

fallmax <- max(landdata$Infizierte, na.rm = TRUE)
    
gesamt_bevoelkerung <- landdata %>% 
  distinct(Bundesland, .keep_all = T) %>% 
  summarise(Bevoelkerung = sum(Bevoelkerung)) %>% 
  pull()
    
datum <- format(max(landdata$Datum), "%d.%m.%Y")

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Covid-19"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", icon = icon("table"), tabName = "data")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
    fluidRow(
        column(8,
            tabBox(
                  tabPanel("Infizierte",
                           plotlyOutput("landplot", height = "650px"), width = NULL),
                  tabPanel("Infizierte pro 100 Tsd. Einwohner",
                           plotlyOutput("rellandplot", height = "650px"), width = NULL),
                  width = NULL)
              ),
        column(4,
               fluidRow(
                    column(6,
                        valueBox(paste0(gesamtzahl), paste0("Infizierte (", today_format, ")"),
                                    icon = icon("bug"),
                                    width = NULL,
                                    color = "red")
                            ),
                    column(6,
                        valueBox(format(gesamtzahl / gesamt_bevoelkerung * 100000, digits = 4),
                                 "pro 100 Tsd. Einwohner",
                                 width = NULL,
                                 color = "red")
                            )
                    ),
               fluidRow(
                    column(6, valueBox(sum(todesfaelle$Tote), paste0("Todesfälle (", today_format, ")"),
                        width = NULL,
                        color = "black"),
                          ),
                    column(6, valueBox(format(sum(todesfaelle$Tote) / gesamt_bevoelkerung * 100000, digits = 6),
                                       "pro 100 Tsd. Einwohner",
                                       width = NULL,
                                       color = "black")
                          )
                        ),
               tabBox(
                 #title = "Relative Zahlen",
                 tabPanel("Infizierte pro 100 Tsd.",
                          plotlyOutput("relplot"), width = NULL),
                 tabPanel("Todesfälle pro 100 Tsd.",
                          plotlyOutput("toteplot"), width = NULL),
                 width= NULL)
                ),
               ),
    fluidRow(column(12, 
                box(p("Letzte Aktualisierung am ", format(Sys.time(), tzone = tzone), "- Daten: ", a("Risklayer", href="http://www.risklayer.com/de/")),
                 p("©", a("Eero Kuusisto-Gussmann", href="http://kuusisto.de"), a(icon("github"), href="https://github.com/ek-g/shiny-server/tree/master/covid19de")),
                 width = NULL)
                ))),
      tabItem("data",
              fluidRow(
              column(12, box(DTOutput('datatable'), width = NULL))
              )
              )
      )
  )
)


server <- function(input, output) {

    output$landplot <- renderPlotly({
        
        plot_ly(landdata, x =~Datum, y =~Infizierte,
                color =~Bundesland, type='scatter', mode = 'lines',
                line = list(width = 4),
                text = ~Bundesland,
                hovertemplate = paste(
                    "<b>%{text}</b><br>",
                    "%{yaxis.title.text}: %{y:d}<br>",
                    "%{xaxis.title.text}: %{x}",
                    "<extra></extra>")
                ) %>% 
            layout(
                yaxis = list(
                    range = c(0, fallmax)),
                xaxis = list(
                    type = 'date',
                    tickformat = "%d.%m.%Y"),
                legend = list(
                  x = 0.02,
                  y = 0.98,
                  font = list(size = 8)
                )
                )
    })
    
    output$rellandplot <- renderPlotly({
      
      plot_ly(landdata, x =~Datum, y =~Infizierte / Bevoelkerung * 100000,
              color =~Bundesland, type='scatter', mode = 'lines',
              line = list(width = 4),
              text = ~Bundesland,
              hovertemplate = paste(
                "<b>%{text}</b><br>",
                "%{y:.2f} %{yaxis.title.text}<br>",
                "%{xaxis.title.text}: %{x}",
                "<extra></extra>")
      ) %>% 
        layout(
          yaxis = list(
            range = c(0, max(landdata$Infizierte / landdata$Bevoelkerung * 100000)),
            title = "Infizierte pro 100 Tsd. Einwohner"),
          xaxis = list(
            type = 'date',
            tickformat = "%d.%m.%Y"),
          legend = list(
            x = 0.02,
            y = 0.98,
            font = list(size = 8)
          )
        )
    })
    
    output$relplot <- renderPlotly({
        
        heute %>% 
            mutate(pro1000 = Infizierte / Bevoelkerung * 100000) %>% 
            arrange(pro1000) %>% 
            plot_ly(x = ~pro1000, y = ~Kuerzel,
                    type = 'bar', color = ~pro1000,
                    colors = "YlOrRd", orientation = 'h',
                    text = ~Bundesland,
                    hovertemplate = paste(
                        "<b>%{text}</b><br>",
                        "%{x:.2f} %{xaxis.title.text}",
                        "<extra></extra>")) %>% 
            layout(yaxis = list(categoryorder = "array", categoryarray = ~pro1000,
                                title = "Bundesland"),
                   xaxis = list(title = "Infizierte pro 100 Tsd. Einwohner")) %>%
            hide_colorbar()
    })
    
    output$toteplot <- renderPlotly({
        
        todesfaelle %>% 
            mutate(pro1000 = Tote / Bevoelkerung * 100000) %>% 
            arrange(pro1000) %>% 
            plot_ly(x = ~pro1000, y = ~Kuerzel,
                    type = 'bar', color = ~pro1000,
                    colors = "YlOrRd", orientation = 'h',
                    text = ~Bundesland,
                    hovertemplate = paste(
                        "<b>%{text}</b><br>",
                        "%{x:.2f} Todesfälle<br>",
                        "<extra></extra>")) %>% 
            layout(yaxis = list(categoryorder = "array", categoryarray = ~pro1000,
                                title = "Bundesland"),
                   xaxis = list(title = "Todesfälle pro 100 Tsd. Einwohner")) %>%
            hide_colorbar()
    })
    
    output$datatable = renderDT(
      landdata %>% select(-Bevoelkerung, -Kuerzel), options = list(lengthChange = FALSE, pageLength = 20)
    )
}

shinyApp(ui = ui, server = server)
