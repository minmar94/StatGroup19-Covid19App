# Packages
require(tidyverse)
require(magrittr)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(lubridate)
require(DT)
require(stringi)
require(MASS)
require(rmutil)
require(nplr)
require(tscount)
require(foreach)
require(doSNOW)
require(sf)
require(sp)
require(rmarkdown)
require(ggiraph)
require(ggiraphExtra)
require(plotly)
require(rgeos)
require(lme4)

rm(list=ls())

source("Script/_growthGLM.r")
source("Script/UsefulFuns.R")
#load("Data/ICU/ICUOutput.RData")
load("Data/ICU/PastICUPred.RData")

# Read aggregated Italian data up to today 
dati_Ita <- read_italian(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
today <- max(dati_Ita$data)


# Read regional data up to today
dati_reg <- read_regional(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv")


# Leggo dati provinciali up o today
dati_prov <- read_province(path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")

# Read data for regional map
italy_sf_reg <- read_shape_italy(province = F)

# Read data for province map
italy_sf_prov <- read_shape_italy(province = T)

# Join data regional
joined_reg <- italy_sf_reg %>% sp::merge(dati_reg, by.y = "denominazione_regione", by.x = "NAME")

# Join data province
joined_prov <- italy_sf_prov %>% sp::merge(dati_prov %>% dplyr::select(data, denominazione_provincia, denominazione_regione, totale_casi) %>% rename(`Totale casi` = totale_casi),
                                           by.y = c("denominazione_regione", "denominazione_provincia"), 
                                           by.x = c("NAME", "NAME_2"), all.x = T) %>% 
  gather(Key, Value, `Totale casi`) %>% 
  dplyr::select(NAME_2, NAME, Key, data, Value, geometry)

# Leggo dati residenti
residents <- read.csv("Data/residenti2019.csv",header=TRUE,sep=",")
residents[,1] <- as.character(residents[,1])

letalita_reg <- dati_reg %>% spread(Key, Value) %>% dplyr::select(data, denominazione_regione, Deceduti, `Totale casi`) %>%
  filter(`Totale casi`>0) %>% 
  group_by(denominazione_regione) %>% 
  summarise(Totdec = sum(Deceduti),
            Totcasi = sum(`Totale casi`),
            letalita = Totdec/Totcasi)


# Data preparation for the model
data_formodel_prep <- prepdata_for_model(dftoprep = dati_reg %>% spread(Key, Value), resdata = residents)

mortalita_reg <- data_formodel_prep %>% 
  dplyr::select(ti_orig, region, Deceduti, residents) %>% 
  group_by(region) %>% 
  summarise(Totdec = sum(Deceduti),
            Totres = residents[1],
            mortalita = Totdec/Totres)




datasuTabella <- max(outmod_terapie_tab$DataPred)
outmod_terapie_plot <- outmod_terapie_tab %>% filter(DataPred < datasuTabella)
outmod_terapie_table <- outmod_terapie_tab %>% filter(DataPred == max(DataPred))

# ShinyApp ----------------------------------------------------------------
ui <- navbarPage(theme = shinytheme("united"),
                 title = "Analisi dell'epidemia di CoviD-19 in Italia",
                 
                 # Primo pannello: Overview
                 tabPanel(title = "Panoramica",
                          
                          div(style="margin-top:-3.5em",
                              
                              fluidRow(tags$hr(),
                                       column(width = 8,
                                              htmlOutput(outputId = "headSummary"), htmlOutput(outputId = "CurrentSitua"),
                                              style="padding:20px;"),
                                       column(width = 4, tags$strong("Distribuzione attualmente positivi"),
                                              ggiraphOutput(outputId = "DonughtPlot")
                                       )
                              )
                          ), 
                          
                          tags$hr(),
                          
                          fluidPage(
                            
                            sidebarLayout(
                              sidebarPanel(
                                pickerInput(inputId = "SelIdxforMap", 
                                            label = "Seleziona una variabile", 
                                            choices = list("Totale casi", "Totale positivi", "Nuovi positivi",
                                                           "Totale ricoverati", "Ricoverati con sintomi","Terapia intensiva",
                                                           "Isolamento domiciliare","Dismessi/Guariti", "Deceduti", "Tamponi"),
                                            selected = "Nuovi positivi"),
                                
                                materialSwitch(inputId = "ProvinciaSiNo", 
                                               label = "Mappa a livello provinciale",
                                               value = F,
                                               status = "danger"),
                                
                                helpText("Per i dati provinciali è disponibile solo il numero di casi totali"),
                                materialSwitch(inputId = "IncrPercSiNo", 
                                               label = "Visualizza incrementi assoluti", 
                                               value = F, 
                                               status = "danger"),
                                uiOutput(outputId = "SelDateIncr")
                              ),
                              
                              mainPanel(
                                splitLayout(
                                  plotlyOutput(outputId = "MapIta", height = "450px"), 
                                  plotlyOutput(outputId = "TSIncrPerc", height = "450px")
                                )
                              )
                            ),
                            tags$hr(),
                            
                            fluidRow(
                              column(6, tags$strong("Cos'è la COVID-19?"), textOutput(outputId = "COV19"), style="text-align:justify"),
                              column(6, tags$strong("Vocabulary"), htmlOutput(outputId = "Defins"), style="text-align:justify")
                            ))),
                 tabPanel("Modello", 
                          fluidPage(
                            
                            
                            tags$h2(tags$strong("Modello lineare generalizzato di Richards*")),
                            tags$h5("*per maggiori dettagli sulla metodologia si rimanda al seguente ", 
                                    tags$a(href = "http://afarcome.altervista.org/growthGLM.pdf" , "link")), 
                            sidebarLayout(
                              sidebarPanel(
                                pickerInput(inputId = "VarForModel", 
                                            label = "Seleziona una variabile",
                                            choices = list("Totale casi", "Totale positivi", "Totale ricoverati", "Terapia intensiva",
                                                           "Ricoverati con sintomi", "Dismessi/Guariti",
                                                           "Isolamento domiciliare", "Deceduti"),
                                            selected = "Totale casi"), 
                                materialSwitch(inputId = "ModRegioneSiNo", 
                                               label = "Modello a livello regione", 
                                               value = F, status = "danger"),
                                uiOutput(outputId = "SelRegModel"),
                                actionBttn(inputId = "updatemodel", label = "Run the model", style = "jelly", color = "danger"),
                                helpText("Cliccare per stimare il modello e attendere qualche secondo che raggiunga la convergenza"),
                                
                                helpText(tags$h5("Utilizzare sempre (se si vuole), tranne se la variabile selezionata per il modello è 'Deceduti' o 'Totale casi'")),
                                checkboxInput(inputId = "AddReducedMirrored", label = "Confronta con Mirrored Richards Ridotta", value = F),
                                uiOutput(outputId = "SelDateModel"), 
                                helpText("Scorrere per vedere come cambia la curva di previsione"),
                                tags$br(),
                                htmlOutput("SummaryModel")
                              ),
                              
                              mainPanel(tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                              ), 
                              verticalLayout(
                                fluidRow(
                                  splitLayout(cellWidths = c("49%", "49%"), plotlyOutput("PredCumCases"), plotlyOutput("PredNewCases"))
                                ), 
                                tags$hr(),
                                fluidRow(
                                  DTOutput("DatPred")
                                )
                              )
                              )
                            )
                            
                          )
                 ),
                 tabPanel("Previsione terapie intensive", 
                          fluidPage(
                            tags$h2(tags$strong(paste("Previsione* ricoveri in terapia intensiva per il giorno ", 
                                                      day(datasuTabella), 
                                                      stri_trans_totitle(month(datasuTabella, label = T, abbr = F)), 
                                                      year(datasuTabella)))),
                            tags$h5("*per maggiori dettagli sulla metodologia si rimanda al seguente ", 
                                    tags$a(href = "http://afarcome.altervista.org/ICU.predictions.pdf" , "link")), 
                            fluidRow(DTOutput("ICuTab"), 
                                     helpText("Cliccare su 'Download' per scaricare la tabella con le previsioni per domani"),
                                     downloadButton(outputId = "DownICUTomorrow", label = "Download")),
                            tags$hr(),
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  
                                  helpText(tags$h4(tags$strong("Confronto previsioni con valori reali nella data selezionata*")),
                                           tags$h5("Prima data disponibile: 17 Marzo 2020")),
                                  dateInput(inputId = "ICubarDate", label = "Seleziona un giorno", value = datasuTabella-1,
                                            min = "2020-03-17", max = datasuTabella-1, width = "250px"),
                                  uiOutput("CovICUModel"),
                                  helpText("Cliccare su 'Download' per scaricare la tabella con le previsioni della data selezionata"),
                                  downloadButton(outputId = "DownICUWhatever", label = "Download")
                                  #plotOutput("HistICU")
                                ), 
                              mainPanel(plotlyOutput("BarplotIcu"))))
                          )),
                 tabPanel("Info", div(style="margin-top:-2.5em", includeMarkdown("InfoandCredits.md")))
                 
)

server <- function(input, output){
  
  #
  output$COV19 <- renderText({"La COVID19 (dall'inglese COronaVIrus Disease 2019) o malattia respiratoria acuta da SARS-CoV-2 (dall'inglese Severe acute respiratory syndrome coronavirus 2),
  è una malattia infettiva respiratoria causata dal virus denominato SARS-CoV-2 appartenente alla famiglia dei coronavirus. 
  Una persona infetta può presentare sintomi dopo un periodo di incubazione che può variare tra 2 e 14 giorni circa, durante i quali può comunque essere contagiosa.
  Per limitarne la trasmissione devono essere prese precauzioni, come adottare un'accurata igiene personale, lavarsi frequentemente le mani e indossare mascherine. 
  Coloro che ritengono di essere infetti devono rimanere in quarantena, indossare una mascherina chirurgica e chiamare immediatamente un medico al fine di ricevere appropriate indicazioni."})
  
  #
  output$Defins <- renderUI(HTML("<ul><li><b>Attualmente positivi =</b> ricoverati con sintomi + terapia intensiva + isolamento domiciliare</li>
  <li><b>Nuovi attualmente positivi =</b> totale attualmente positivi del giorno corrente - totale attualmente positivi del giorno precedente</li>
  <li><b>Nuovi positivi =</b> totale casi giorno corrente - totale casi giorno precedente</li>
  <li><b>Totale casi =</b> totale attualmente positivi + totale deceduti + totale guariti</li>
  <li><b>Totale ricoverati =</b> ricoverati con sintomi + terapia intensiva</li>
  <li><b>Tasso di mortalità =</b> deceduti/popolazione</li>
  <li><b>Quoziente di letalità =</b> deceduti/totale casi</li></ul>"))
  
  #
  output$headSummary <- renderUI({HTML(paste0(h2("Situazione italiana relativa all'epidemia CoviD-19 ad oggi*:"),
                                              h5("*I dati sono aggiornati alle ore 18pm del",
                                                 day(today), stri_trans_totitle(month(today, label = T, abbr = F)), year(today))))
  })
  
  #
  output$CurrentSitua <- renderUI({
    
    dat_today <- dati_Ita %>% dplyr::filter(data %in% c(today, today-1))
    
    attualmente_positivi_ultimo <- dat_today$`Totale positivi`[2]
    incr_attualmente_positivi <- paste_signpercent(dat_today$`Totale positivi`[2], dat_today$`Totale positivi`[1])
    
    deceduti_ultimo <- dat_today$Deceduti[2]
    incr_deceduti <- paste_signpercent(dat_today$Deceduti[2], dat_today$Deceduti[1])
    
    dimessi_guariti_ultimo <- dat_today$`Dismessi/Guariti`[2]
    incr_guariti <- paste_signpercent(dat_today$`Dismessi/Guariti`[2], dat_today$`Dismessi/Guariti`[1])
    
    totale_casi_ultimo <- dat_today$`Totale casi`[2]
    incr_totale_casi <- paste_signpercent(dat_today$`Totale casi`[2], dat_today$`Totale casi`[1])
    
    ricov_sintomi_ultimo <- dat_today$`Ricoverati con sintomi`[2]
    incr_ricoverati <- paste_signpercent(dat_today$`Ricoverati con sintomi`[2], dat_today$`Ricoverati con sintomi`[1])
    
    isol_domic_ultimo <- dat_today$`Isolamento domiciliare`[2]
    incr_isolamento <- paste_signpercent(dat_today$`Isolamento domiciliare`[2], dat_today$`Isolamento domiciliare`[1])
    
    terapia_intens_ultimo <- dat_today$`Terapia intensiva`[2]
    incr_terapia <- paste_signpercent(dat_today$`Terapia intensiva`[2], dat_today$`Terapia intensiva`[1])

    totale_ricoverati_ultimo <- dat_today$`Totale ricoverati`[2]
    incr_ricoverati <- paste_signpercent(dat_today$`Totale ricoverati`[2], dat_today$`Totale ricoverati`[1])
    
    
    quoz_let_ita <- round(sum(letalita_reg$Totdec)/sum(letalita_reg$Totcasi),4)*100
    
    tasso_mort_ita <- round(sum(mortalita_reg$Totdec)/sum(mortalita_reg$Totres),4)*100
    
    HTML(paste0("<ul>",
                "<li><b>Totale casi: </b>", totale_casi_ultimo, "(",incr_totale_casi,")","</li>",
                "<li><b>Attualmente positivi: </b>", attualmente_positivi_ultimo,"(",incr_attualmente_positivi,")","</li>",
                "<li><b>Totale ricoverati: </b>", totale_ricoverati_ultimo, "(",incr_ricoverati,")","</li>",
                "<li><b>Terapia intensiva: </b>", terapia_intens_ultimo, "(",incr_terapia,")","</li>",
                "<li><b>Ricoverati con sintomi: </b>", ricov_sintomi_ultimo, "(",incr_ricoverati,")","</li>",
                "<li><b>Isolamento domiciliare: </b>", isol_domic_ultimo, "(",incr_isolamento,")","</li>",
                "<li><b>Dimessi/Guariti: </b>", dimessi_guariti_ultimo,"(",incr_guariti,")","</li>",
                "<li><b>Decessi: </b>", deceduti_ultimo, "(",incr_deceduti,")","</li>",
                "<li><b>Tasso di mortalità: </b>", tasso_mort_ita, "%","</li>",
                "<li><b>Quoziente di letalità: </b>", quoz_let_ita, "%","</li>",
                "</ul>",
                collapse = ""
    ))
  })
  
  # Dynamic inputs
  output$SelDateIncr <- renderUI({
    sliderTextInput(
      inputId = "SelDateIncr",
      label = "Seleziona un giorno",
      grid = FALSE,
      force_edges = TRUE,
      width = 250,
      selected = max(dati_reg$data),
      choices = seq(min(dati_reg$data), max(dati_reg$data),1)
    )
  })
  
  output$SelDateModel <- renderUI({
    sliderInput(
      inputId = "SelDateModel",
      label = "Orizzonte di previsione (giorni):",
      width = 250,
      value = 1,
      min = 1,
      max = 15
    )
  })
  
  
  output$SelRegModel <- renderUI({
    if(input$ModRegioneSiNo){
      pickerInput(inputId = "SelRegModel",
                  label = "Seleziona una regione",
                  choices = list("Northern Italy" = list("Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                                         "Emilia Romagna", "Friuli V. G.", "Veneto", "TrentinoAltoAdige"),
                                 "Central Italy" = list("Lazio", "Marche", "Toscana", "Umbria"),
                                 "Sourhern Italy" = list("Abruzzo", "Basilicata", "Calabria",
                                                         "Campania", "Molise", "Puglia"),
                                 "Insular" = list("Sardegna", "Sicilia")),
                  selected = "Lombardia",
                  options = list(
                    `actions-box` = FALSE,
                    header = "Regions"
                  ))
    }else{
      return(NULL)
    }
    
  })
  
  
  # Donut plot
  output$DonughtPlot <- renderggiraph({
    
    ggDonut(data = dati_Ita %>% 
              gather(Key, Value, c(`Ricoverati con sintomi`, `Terapia intensiva`, `Isolamento domiciliare`)) %>% 
              filter(data == max(data)),
            mapping = aes(donuts = Key, count = Value), 
            palette = "Spectral", labelposition=0, direction = 1, labelsize = 5, alpha = 1, colour = "white", interactive = T) 
    
  })
  
  
  variable <- reactive({
    input$SelIdxforMap
  })
  
  datetoselect <- reactive({
    input$SelDateIncr
  })
  
  # Incrementi Plot
  output$TSIncrPerc <- renderPlotly({
    
    dat_Ita_incrperc <-  dati_Ita %>% 
      # Creo gli incrementi percentuali
      mutate_if(is.numeric, .funs = function(x) (c(0, diff(x)))/x)
    
    dat_Ita_incrperc <- dat_Ita_incrperc[-1,] %>% 
      filter(data <= as.character(datetoselect()))
    
    ylabel <- "Incrementi percentuali"
    
    # Se voglio gli incrementi assoluti
    if(input$IncrPercSiNo){
      
      dat_Ita_incrperc <-  dati_Ita %>% 
        # incrementi assoluti
        mutate_if(is.numeric, .funs = function(x) c(0, diff(x)))
      
      dat_Ita_incrperc <- dat_Ita_incrperc[-1,] %>% 
        filter(data <= as.character(datetoselect()))
      
      ylabel <- "Incrementi assoluti"
    }
    
    # incr_plot <- dat_Ita_incrperc %>% 
    #   gather(Key, Value, -data, -stato) %>% 
    #   filter(Key == "Totale casi") %>% 
    #   ggplot(aes(x=data, y = Value, colour = I("firebrick"))) +
    #   geom_line() + geom_point() + 
    #   labs(x="", y = as.character(variable()), colour = "", caption = paste(ylabel,"su scala nazionale")) +
    #   scale_x_date(date_labels = "%d/%m", date_breaks = "7 days", expand = c(0,0)) +
    #   theme_bw() +
    #   theme(text = element_text(size = 14))
    
    plot_ly(data = dat_Ita_incrperc %>% 
              gather(Key, Value, -data, -stato) %>% 
              filter(Key == as.character(variable())), 
            x = ~data, y = ~Value, type = 'scatter', mode = 'lines+markers', color = I("firebrick"),
            showlegend = F) %>% 
      layout(xaxis = list(title = ""), yaxis = list(title = as.character(variable())),
             annotations = list(x = 1, y = -0.1, text = paste(ylabel,"su scala nazionale"), 
                                showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                font=list(size=15, color="black")))
      
    
  })
  
  
  output$MapIta <- renderPlotly({
  
    titleannots <- ifelse(input$ProvinciaSiNo, "Mappa provinciale ", "Mappa regionale ")
    annots <- list(x = 1, y = -0.1, text = paste(titleannots, "in data ", as.character(datetoselect())), 
                       showarrow = F, xref='paper', yref='paper', 
                       xanchor='right', yanchor='auto', xshift=0, yshift=0,
                       font=list(size=15, color="black"))
    p <- plot_ly(data = joined_reg %>% 
                   filter(Key == as.character(variable()), data == as.character(datetoselect())), 
                 stroke = I("black"),
                 split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                 text = ~paste0(NAME, "\n", Value, " people"),
                 hoveron = "fills",
                 hoverinfo = "text", 
                 showlegend = F) %>%
      layout(legend = list(orientation = "h"), annotations = annots) %>% 
      colorbar( title = list(text = paste0("<b>", as.character(variable()), "</b>")), len = 0.6)
    
    if(input$ProvinciaSiNo){
      p <- plot_ly(data = joined_prov %>% filter(Key == "Totale casi", data == as.character(datetoselect())), 
                   stroke = I("black"),
                   split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME_2, " (", NAME, ")", "\n", Value, " persone"),
                   hoveron = "fills",
                   hoverinfo = "text", 
                   showlegend = F) %>%
        layout(legend = list(orientation = "h"), annotations = annots) %>% 
        colorbar(title = list(text = "<b>Totale casi</b>"), len = 0.6)
      
    }
    
    p
  })
  
  
  # Modello per Italia
  outputModello <- eventReactive( input$updatemodel, {
    
    if(input$ModRegioneSiNo){
      return(
        tryCatch(
          suppressWarnings(run_growth_model(da = data_formodel_prep, reg = input$SelRegModel, wh = input$VarForModel, horizon = 30, fam = "Poisson")),
          error = function(e){
            return("IL MODELLO NON HA RAGGIUNTO LA CONVERGENZA!")
            
          } 
        )
      )
    }else{
      return(
        tryCatch(
          suppressWarnings(run_growth_model(da = data_formodel_prep, reg = NULL, wh = input$VarForModel, horizon = 30, fam = "Poisson")),
          error = function(e){
            return("IL MODELLO NON HA RAGGIUNTO LA CONVERGENZA!")
          } 
        )
      )
    }
    
    
  })
  
  # Summary Italia
  output$SummaryModel <- reactive({
    
    if(!is.character(outputModello())){
      if(input$ModRegioneSiNo){
        return(
          summary_out_model(outputModello(), varest = input$VarForModel, resdata = residents, reg = input$SelRegModel, is.reg = T)
        )
      }else{
        summary_out_model(outputModello(), varest = input$VarForModel, resdata = residents)
      }
      
    }else{
      return(HTML(outputModello()))
    }
    
    
  })
  
  # Plot nuovi cumulati Italia
  output$PredCumCases <- renderPlotly({
    
    plot_mod1 <- plot_out_model(outputmod = outputModello(), hz = input$SelDateModel, what = "Cumulati", VarModel = input$VarForModel, addReduMirr = input$AddReducedMirrored)
    
   plot_mod1
    
  })
  
  # Plot nuovi predetti Italia
  output$PredNewCases <- renderPlotly({
    
    plot_mod2 <- plot_out_model(outputmod = outputModello(), hz = input$SelDateModel, what = "Nuovi", VarModel = input$VarForModel, addReduMirr = input$AddReducedMirrored)
    
    plot_mod2
  })
  
  output$DatPred <- renderDT({
    
    DT_out_model(outputmod = outputModello(), hz = input$SelDateModel, VarModel = input$VarForModel)
  })
  
  
  
  
  output$ICuTab <- renderDT({

    outmod_terapie_table %>% dplyr::select(-DataPred) %>% 
      datatable(rownames = F, options = list(dom = 'tp', pageLength = 10, scrollX = T, 
                                             columnDefs = list(list(className = 'dt-center', targets = "_all"))))

  })
  
  
  output$DownICUTomorrow <- downloadHandler(
    filename = function() {
      paste0("PrevisioneTerapieIntensive_", datasuTabella,".csv")
    },
    content = function(file) {
      write.csv(outmod_terapie_table, file, row.names = FALSE)
    }
  )
  
  dataICUbar <- reactive({
    
    levels(outmod_terapie_plot$Regione)[5] <- "Emilia-Romagna"
    levels(outmod_terapie_plot$Regione)[6] <- "Friuli Venezia Giulia"
    levels(outmod_terapie_plot$Regione)[17] <- "Trentino-Alto Adige"
    
    dplot <- outmod_terapie_plot %>% 
      rename(denominazione_regione = Regione, data = DataPred) %>% 
      left_join(dati_reg %>% spread(Key, Value) %>% 
                  filter(data <= max(outmod_terapie_plot$DataPred)) %>% 
                  dplyr::select(data, starts_with("Tera"), denominazione_regione), 
                by = c("denominazione_regione", "data")) 
    
    dplot
    
  })
  
  output$BarplotIcu <- renderPlotly({
  
    dataICUbar() %>% 
      filter(data == as.character(input$ICubarDate)) %>% 
      plot_ly(x = ~denominazione_regione, y = ~`Terapia intensiva`, type = "bar", 
              name = "Osservati", marker = list(color = "firebrick")) %>% 
      add_trace(y = ~Previsione, name = "Previsti", marker = list(color = "darkorange")) %>% 
      layout(title = paste("Previsione del ", as.character(input$ICubarDate)),
             xaxis = list(title = ""),
             yaxis = list(title = "Terapie intensive"))
    
  })
  
  output$CovICUModel <- renderUI({
    
    dplot2 <- dataICUbar() %>% filter(data == as.character(input$ICubarDate))
    quanteIn <- sum(dplot2$`Terapia intensiva` >= dplot2$`Limite Inferiore` & dplot2$`Terapia intensiva` <= dplot2$`Limite Superiore`)
    
    capts <- paste("per", quanteIn, "regioni su 20 il valore osservato è contenuto nell'intervallo di previsione.")
    HTML(paste("In data ", as.character(input$ICubarDate), ", ",capts))
    
  })
  
  output$DownICUWhatever <- downloadHandler(
    filename = function() {
      paste0("PrevisioneTerapieIntensive_", as.character(input$ICubarDate),".csv")
    },
    content = function(file) {
      write.csv(dataICUbar() %>% filter(data == as.character(input$ICubarDate)), file, row.names = FALSE)
    }
  )
  
  # output$HistICU <- renderPlot({
  #   
  #   dataICUbar() %>% 
  #     group_by(data) %>% 
  #     summarise(TOTICU = sum(`Terapia intensiva`)) %$% hist(TOTICU) 
  #   
  # })
}


shinyApp(ui = ui, server = server)