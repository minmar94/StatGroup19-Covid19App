# Packages
require(tidyverse)
require(magrittr)
require(shiny)
require(shinythemes)
require(shinyWidgets)
require(ggiraph)
require(mapIT)
require(lubridate)
require(DT)
require(stringi)
require(MASS)
require(rmutil)
require(nplr)
require(tscount)
require(foreach)
require(doSNOW)
require(cowplot)

rm(list=ls())

source("Script/_growthGLM.r")


paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "-")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% rispetto a ieri"))
} 


run_growth_model <- function(da, reg="Lombardia", wh="Totale casi", horizon = 10, fam="nb", useLog=T) {
  
  dat <- da[da$region==reg,]
  
  whidx <- which(colnames(dat)==wh)
  pc <- dat[, whidx]
  ti <- dat$ti
  
  if(any(pc==0)) {
    w <- min(which(pc!=0))
    pc <- pc[-c(1:w)]
    ti <- ti[-c(1:w)]
    ti <- ti-min(ti)+1}
    mti <- max(ti)        
    timax <- mti+horizon
  np=growthGLM(pc,ti,timax=timax,family=fam,nstart=1000,useLog=useLog)
  
  ps <- np$pars
  y <- np$linPred
  x <- 1:timax
  
  cc1<-data.frame(x1=c(x[2:mti],rep(NA,length(x[-c(1:mti)]))),
                  pc=c(diff(pc),rep(NA,length(x[-c(1:mti)]))), x=x[-1],y=diff(y))
  cc<-data.frame(x1=c(x[1:mti],rep(NA,length(x[-c(1:mti)]))),
                 pc=c(pc,rep(NA,length(x[-c(1:mti)]))), x=x,y=y)
 

  
  return(list(cc=cc, cc1=cc1)) 
}



ui <- navbarPage(theme = shinytheme("united"),
                 title = "Analysis of the outbreak of CoviD-19 syndrome in Italy",
                 
                 # Primo pannello: Overview
                 tabPanel(title = "Overview", 
                          
                          div(style="margin-top:-3.5em",
                              fluidRow(tags$hr(),
                                column(width = 6,
                                       htmlOutput(outputId = "headSummary"), htmlOutput(outputId = "CurrentSitua"),
                                       style="padding:20px;"),
                                column(width = 4, ggiraphOutput("TSIncrPerc")),
                              )
                          ), tags$hr(),
                          
                          fluidPage(
                            fluidRow(column(pickerInput(inputId = "SelIdxforMap", 
                                                             label = "Seleziona una variabile", 
                                                             choices = list("Totale casi", "Totale positivi", "Nuovi positivi",
                                                                            "Totale ricoverati", "Ricoverati con sintomi","Terapia intensiva",
                                                                            "Isolamento domiciliare","Dismessi/Guariti", "Deceduti", "Tamponi"),
                                                             selected = "Nuovi positivi"),
                                                 mainPanel(ggiraphOutput(outputId = "MapIta")), width = 6),
                                     column(uiOutput(outputId = "SelDateBar"),
                                            mainPanel(ggiraphOutput(outputId = "BarplotIta")), width = 6)
                                                 ), tags$hr(),
                            
                            fluidRow(
                              column(4, tags$strong("Cos'è la COVID-19?"), textOutput(outputId = "COV19"), style="text-align:justify"),
                              column(4, tags$strong("Vocabulary"), htmlOutput(outputId = "Defins"), style="text-align:justify"),
                              column(4, tags$strong("  "), htmlOutput(outputId = "Statquant"), style="text-align:justify")
                            ))),
                          tabPanel("Modeling", 
                                   fluidPage(sidebarLayout(sidebarPanel(pickerInput(inputId = "VarForModel", 
                                                         label = "Seleziona una variabile",
                                                         choices = list("Totale casi", "Totale positivi", 
                                                                        "Ricoverati con sintomi","Terapia intensiva",
                                                                        "Isolamento domiciliare", "Deceduti"),
                                                         selected = "Totale casi"), 
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
                                                         )), htmlOutput("SummaryModel")),
                                             mainPanel(tags$style(type="text/css",
                                                                  ".shiny-output-error { visibility: hidden; }",
                                                                  ".shiny-output-error:before { visibility: hidden; }"
                                             ), verticalLayout(
                                               fluidRow(
                                                 splitLayout(cellWidths = c("49%", "49%"), ggiraphOutput("PredCumCases"), ggiraphOutput("PredNewCases"))
                                               ), tags$hr(),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("49%", "49%"), DTOutput("DatPredCum"), DTOutput("DatPredNew"))
                                               )
                                             ))))),
                          tabPanel("Credits", div(style="margin-top:-2.5em", includeMarkdown("InfoandCredits.md")))
                 
)
                 


server <- function(input, output){
  
  # Read aggregated Italian data up to today 
  CoviD_Ita <- reactive({
    
    dat_Ita <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv") %>% 
      mutate(data = date(data)) %>% dplyr::select(-note_it, -note_en)
    
    colnames(dat_Ita)[-c(1,2)] <- c("Ricoverati con sintomi", "Terapia intensiva", "Totale ricoverati",
                                    "Isolamento domiciliare", "Totale positivi", "Variazione totale positivi", "Nuovi positivi", 
                                    "Dismessi/Guariti", "Deceduti",  "Totale casi", "Tamponi")
    
    dat_Ita
  })
  
  # Read regional data up to today
  CoviD_Ita_Reg <- reactive({
    
    # Leggo i dati sui casi totali per Regione
    dati_reg <- read_csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv") %>% 
      dplyr::select(-note_it, -note_en) %>% 
      # Creo la regione Trentino che appare sottoforma di province autonome
      mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"),
                                            "Trentino-Alto Adige", denominazione_regione),
             data = date(data)) %>% 
      gather(Key, Value, ricoverati_con_sintomi:tamponi) %>% 
      group_by(Key, data, denominazione_regione) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() 
    
    dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                           labels = c("Deceduti", "Dismessi/Guariti", "Isolamento domiciliare", "Nuovi positivi", "Ricoverati con sintomi", 
                                      "Tamponi", "Terapia intensiva", "Totale casi", "Totale ricoverati", "Totale positivi", 
                                      "Variazione totale positivi"))
    
    dati_reg
  })
  
  
  
  output$COV19 <- renderText({"La COVID19 (dall'inglese COronaVIrus Disease 2019) o malattia respiratoria acuta da SARS-CoV-2 (dall'inglese Severe acute respiratory syndrome coronavirus 2), è una malattia infettiva respiratoria causata dal virus denominato SARS-CoV-2 appartenente alla famiglia dei coronavirus. 
Una persona infetta può presentare sintomi dopo un periodo di incubazione che può variare tra 2 e 14 giorni circa, durante i quali può comunque essere contagiosa. Per limitarne la trasmissione devono essere prese precauzioni, come adottare un'accurata igiene personale, lavarsi frequentemente le mani e indossare mascherine. Coloro che ritengono di essere infetti devono rimanere in quarantena, indossare una mascherina chirurgica e chiamare immediatamente un medico al fine di ricevere appropriate indicazioni."})
  
  output$Defins <- renderUI(HTML("<ul><li><b>Pazienti attualmente positivi=</b> ricoverati con sintomi + terapia intensiva + isolamento domiciliare</li>
                                      <li><b>Nuovi attualmente positivi=</b> totale attualmente positivi del giorno corrente - totale attualmente positivi del giorno precedente</li>
                                 <li><b>Nuovi positivi=</b> totale casi giorno corrente - totale casi giorno precedente</li>
                                 <li><b>Totale casi=</b> totale attualmente positivi + totale deceduti + totale guariti</li></ul>"))
  
  output$Statquant <- renderUI(HTML("<ul><li>Mortality</li><li>Deadliness</li></ul>"))
  
  output$headSummary <- reactive({
    today <- max(CoviD_Ita()$data)
    paste0(h2("Situazione italiana relativa all'epidemia CoviD-19 ad oggi*:"),
           h5("*I dati sono aggiornati alle ore 18pm del",
              day(today), stri_trans_totitle(month(today, label = T, abbr = F)), year(today)))
  })
  
  output$CurrentSitua <- reactive({
    
    today <- max(CoviD_Ita()$data) 
    dat_today <- CoviD_Ita() %>% filter(data %in% c(today, today-1))
    
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
    
    paste0("<ul>",
           "<li><b>Pazienti attualmente positivi: </b>", attualmente_positivi_ultimo,"(",incr_attualmente_positivi,")","</li>",
           "<li><b>Ricoverati con sintomi: </b>", ricov_sintomi_ultimo, "(",incr_ricoverati,")","</li>",
           "<li><b>Isolamento domiciliare: </b>", isol_domic_ultimo, "(",incr_isolamento,")","</li>",
           "<li><b>Terapia intensiva: </b>", terapia_intens_ultimo, "(",incr_terapia,")","</li>",
           "<li><b>Totale casi: </b>", totale_casi_ultimo, "(",incr_totale_casi,")","</li>",
           "<li><b>Dimessi/Guariti: </b>", dimessi_guariti_ultimo,"(",incr_guariti,")","</li>",
           "<li><b>Decessi: </b>", deceduti_ultimo, "(",incr_deceduti,")","</li>",
           "</ul>"
    )
  })
  
  output$SelDateBar <- renderUI({
    sliderTextInput(
      inputId = "SelDateBar",
      label = "Seleziona un giorno",
      grid = FALSE,
      force_edges = TRUE,
      width = 250,
      selected = max(CoviD_Ita_Reg()$data),
      choices = seq(min(CoviD_Ita_Reg()$data), max(CoviD_Ita_Reg()$data),1)
    )
  })
  
  
  output$TSIncrPerc <- renderggiraph({
    
    dat_Ita_incrperc <-  CoviD_Ita() %>% 
      dplyr::select(`Ricoverati con sintomi`, `Terapia intensiva`, `Isolamento domiciliare`, `Dismessi/Guariti`, Deceduti, `Totale casi`) %>% 
      mutate_all(.funs = function(x) (c(0, diff(x)))/x)
    dat_Ita_incrperc <- dat_Ita_incrperc[-1,]
    
    incr_plot <- bind_cols(data = CoviD_Ita()$data[-1], dat_Ita_incrperc) %>% 
      gather(Key, Value, -data) %>% 
      ggplot(aes(x=data, y = Value, colour = Key)) + geom_line() + geom_point() +
      scale_color_brewer(direction = 1, palette = "YlOrRd") +
      labs(x="", y = "Incrementi percentuali", colour = "") +
      scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", expand = c(0,0)) +
      theme_bw() +
      theme(legend.position = "bottom", text = element_text(size = 14))
    
    ggiraph(ggobj = incr_plot)
    
  })
  
  output$MapIta <- renderggiraph({
    
    # Aggrego per regione
    dati_reg_formap <- CoviD_Ita_Reg() %>%
      filter(Key == as.character(input$SelIdxforMap), data == max(data)) 
    
    map_plot1 <- mapIT(values = Value, 
                       id = denominazione_regione,
                       data = dati_reg_formap %>% as.data.frame(),
                       graphPar = list(guide.label=as.character(input$SelIdxforMap), low="yellow", high="brown")) +
      ggtitle(paste("Situazione dell'epidemia in Italia al", max(dati_reg_formap$data))) +
      theme_light() +
      theme(axis.text = element_blank(), text = element_text(size = 14))
    
    ggiraph(ggobj = map_plot1)
  })
  
  output$BarplotIta <- renderggiraph({
    
    dati_forbar <- CoviD_Ita_Reg() %>% 
      group_by(data, Key) %>% 
      summarise(Tot = sum(Value)) %>%
      ungroup() %>% 
      filter(Key %in% c("Ricoverati con sintomi", "Terapia intensiva", "Isolamento domiciliare")) %>% 
      filter(data <= as.character(input$SelDateBar))
    
    barplot_1 <- dati_forbar %>% 
      ggplot(aes(x = data, y = Tot, fill = Key)) +
      geom_bar(stat = "identity") +
      labs(x = "", y = "Totale dei pazienti positivi", fill = "") +
      scale_x_date(date_labels = "%d/%m", date_breaks = "3 days", expand = c(0,0)) +
      scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0,0)) +
      scale_fill_manual(values = c("darkorange", "red2", "brown")) +
      theme_light() +
      theme(legend.position = "top", text = element_text(size = 14), legend.box = "horizontal")
    ggiraph(ggobj = barplot_1)
    
  })
  
  outputModello <- reactive({
    
    residents <- read.csv("Data/residenti2019.csv",header=TRUE,sep=",")
    ag <- CoviD_Ita_Reg() %>% spread(Key, Value) %>% arrange(denominazione_regione)
    
    fa <- factor(ag$data)
    fa2 <- factor(ag$denominazione_regione)
    ti <- unclass(fa)
    ti_orig <- attr(ti, "levels")
    
    levels(fa2)[6] <- "Friuli V. G."
    levels(fa2)[5] <- "Emilia Romagna"
    levels(fa2)[17] <- "TrentinoAltoAdige"
    
    residents[,1] <- as.character(residents[,1])
    residents[10,1] <- "Valle d'Aosta"
    
    ma <- match(residents[,1],levels(fa2))
    residents <- residents[which(!is.na(ma)),]
    ma <- match(fa2,residents[,1])
    
    fa2 <- as.character(fa2)
    
    da <- data.frame(ag[,-c(1:2)],ti=as.numeric(ti),region=fa2,residents=residents[ma,2])
    
    da$region <- factor(as.character(da$region))
    colnames(da) <- c("Deceduti", "Dismessi/Guariti", "Isolamento domiciliare", "Nuovi positivi", "Ricoverati con sintomi", "Tamponi",
                      "Terapia intensiva", "Totale casi", "Totale ricoverati",  "Totale positivi", "Variazione totale positivi", "ti", "region", "residents")
    
    outmod <- suppressWarnings(run_growth_model(da = da, reg = input$SelRegModel, wh = input$VarForModel))
    
    ll <- list(outmod, ti_orig)
    
  })
  
  output$PredCumCases <- renderggiraph({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
                               
    orig_date_obs <- date(ti_orig[na.omit(omod[[1]]$x1)])
    horiz_len <- sum(is.na(omod[[1]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[1]]$x <- date_forecast
    omod[[1]]$x1 <- c(date_forecast[1:(length(omod[[1]]$x)-horiz_len)], rep(NA, horiz_len))
    
    plot_mod1 <- ggplot(omod[[1]]) +
      geom_line(aes(x=x1,y=pc), alpha=0.2)+geom_point(aes(x=x1,y=pc))+
      geom_line(aes(x=x,y=y),col="red", size = 1.1)+
      labs(x = "", y = "Cumulati") +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = plot_mod1)
    #plot_row <- plot_grid(plot_mod1, plot_mod2, nrow = 1)
    
    # title <- ggdraw() + 
    #   draw_label(
    #     paste(as.character(input$VarForModel), "predetti in", as.character(input$SelRegModel)),
    #     fontface = 'bold',
    #     x = 0,
    #     hjust = 0
    #   ) 
    
    #p3 <- girafe(ggobj = plot_grid(title, plot_row,ncol = 1, rel_heights = c(0.1, 1)))
    #p3
    
  })
  
  output$PredNewCases <- renderggiraph({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[2]]$x1)])
    horiz_len <- sum(is.na(omod[[2]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[2]]$x <- date_forecast
    omod[[2]]$x1 <- c(date_forecast[1:(length(omod[[2]]$x)-horiz_len)], rep(NA, horiz_len))
    
    plot_mod2 <- ggplot(omod[[2]]) +
      geom_line(aes(x=x1,y=pc), alpha=0.2)+geom_point(aes(x=x1,y=pc)) +
      geom_line(aes(x=x,y=y),col="red", size = 1.1)+
      labs(x = "", y = "Giornalieri") +
      theme_bw() +
      theme(text = element_text(size = 14))
    
    ggiraph(ggobj = plot_mod2)
    
  })
  
  output$DatPredCum <- renderDT({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[1]]$x1)])
    horiz_len <- sum(is.na(omod[[1]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[1]]$x <- date_forecast
    omod[[1]]$x1 <- c(date_forecast[1:(length(omod[[1]]$x)-horiz_len)], rep(NA, horiz_len))
    
    dt_out <- omod[[1]] %>% as_tibble() %>% 
      filter(x > max(x1, na.rm = T)-2) %>% 
      dplyr::select(x, pc, y) %>% 
      mutate(y = round(y)) %>% 
      rename(Data = x, Osservati = pc, Predetti = y)
    
    datatable(dt_out, rownames = FALSE, caption = "Casi cumulati")
    
  })
  
  output$DatPredNew <- renderDT({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[2]]$x1)])
    horiz_len <- sum(is.na(omod[[2]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[2]]$x <- date_forecast
    omod[[2]]$x1 <- c(date_forecast[1:(length(omod[[2]]$x)-horiz_len)], rep(NA, horiz_len))
    
    dt_out2 <- omod[[2]] %>% as_tibble() %>% 
      filter(x > max(x1, na.rm = T)-2) %>% 
      dplyr::select(x, pc, y) %>% 
      mutate(y = round(y)) %>% 
      rename(Data = x, Osservati = pc, Predetti = y)
    
    datatable(dt_out2, rownames = FALSE, caption = "Nuovi casi giornalieri")
    
  })
  
  output$SummaryModel <- reactive({
    
    omod <- outputModello()[[1]]
    ti_orig <- outputModello()[[2]]
    
    orig_date_obs <- date(ti_orig[na.omit(omod[[2]]$x1)])
    horiz_len <- sum(is.na(omod[[2]]$x1))
    date_forecast <- seq(min(orig_date_obs), max(orig_date_obs) + horiz_len, 1)
    
    omod[[2]]$x <- date_forecast
    omod[[2]]$x1 <- c(date_forecast[1:(length(omod[[2]]$x)-horiz_len)], rep(NA, horiz_len))
    
    est_pick <- omod[[2]]$x[which.max(omod[[2]]$y)]
    estmax_val <- round(omod[[2]]$y[omod[[2]]$x==est_pick])
    obsmax_val <- ifelse(est_pick<=max(omod[[2]]$x1, na.rm=T), round(omod[[2]]$pc[omod[[2]]$x==est_pick]), NA)
    
    pdate <- paste0(day(est_pick), " ", stri_trans_totitle(month(est_pick, label = T, abbr = F)), " ",  year(est_pick))
    paste0(
      "<ul>",
      "<li><b>Punto di flessione (picco della curva stimato): </b>", pdate,"</li>",
      "<li><b>Massimo stimato di casi giornalieri: </b>", estmax_val, ifelse(!is.na(obsmax_val), paste0(" (osservati ", obsmax_val, ")"), ""),"</li>",
      "</ul>"
    )
    
  })

}

shinyApp(ui = ui, server = server)