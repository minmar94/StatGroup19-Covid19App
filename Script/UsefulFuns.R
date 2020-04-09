
# # Useful functions ------------------------------------------------------
# Read italian data
read_italian <- function(path){
  
  dati_Ita <- read_csv(path) %>% 
    mutate(data = date(data)) %>% dplyr::select(-note_it, -note_en)
  
  colnames(dati_Ita)[-c(1,2)] <- c("Ricoverati con sintomi", "Terapia intensiva", "Totale ricoverati",
                                  "Isolamento domiciliare", "Totale positivi", "Variazione totale positivi", "Nuovi positivi", 
                                  "Dismessi/Guariti", "Deceduti",  "Totale casi", "Tamponi")
  
  return(dati_Ita)
  
}

# Read and prepare regional data
read_regional <- function(path){
  
  dati_reg <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>% 
    # Creo la regione Trentino che appare sottoforma di province autonome
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    gather(Key, Value, ricoverati_con_sintomi:tamponi) %>% 
    group_by(Key, data, denominazione_regione) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    ungroup() 
  
  dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                         labels = c("Deceduti", "Dismessi/Guariti", "Isolamento domiciliare", "Nuovi positivi", "Ricoverati con sintomi", 
                                    "Tamponi", "Terapia intensiva", "Totale casi", "Totale ricoverati", "Totale positivi", 
                                    "Variazione totale positivi"))
  
  return(dati_reg)
}

# Read province data
read_province <- function(path){
  
  dati_prov <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>%
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    drop_na(sigla_provincia)
  
  
  
}

# Read italian data shapefile for map
read_shape_italy <- function(province = F){
  
  italy_sf <- read_rds("Data/gadm36_ITA_1_sp.rds") %>% st_as_sf() %>% dplyr::select(NAME_1, geometry) %>% 
    mutate(NAME_1 = factor(NAME_1, labels = c("Abruzzo", "Puglia", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli Venezia Giulia",
                                              "Lazio" ,"Liguria" , "Lombardia", "Marche", "Molise", "Piemonte", "Sardegna", "Sicilia", "Toscana",
                                              "Trentino-Alto Adige", "Umbria", "Valle d'Aosta", "Veneto")))
  colnames(italy_sf)[1] <- "NAME" 
  
  if(province){
    
    italy_sf <- read_rds("Data/gadm36_ITA_2_sp.rds") %>% st_as_sf() %>% dplyr::select(NAME_1, NAME_2, geometry)
    
    # Sistemo etichette
    italy_sf$NAME_1[italy_sf$NAME_1 == "Apulia"] <- "Puglia"
    italy_sf$NAME_1[italy_sf$NAME_1 == "Sicily"] <- "Sicilia"
    italy_sf$NAME_1[italy_sf$NAME_1 == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"
    
    italy_sf$NAME_2[italy_sf$NAME_2 %in% c("Forli' - Cesena", "Mantua", "Padua", 
                                                     "Monza and Brianza", "Pesaro E Urbino", "Syracuse",
                                                     "Florence", "Reggio Di Calabria", "Reggio Nell'Emilia")] <- c("Reggio di Calabria", "Forlì-Cesena", "Reggio nell'Emilia",
                                                                                                                   "Mantova", "Monza e della Brianza", "Pesaro e Urbino",
                                                                                                                   "Siracusa",  "Firenze",  "Padova")
    colnames(italy_sf)[1] <- "NAME"
    
  }
  
  return(italy_sf)
  
}


# Print sign header
paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% rispetto a ieri"))
} 

# print labels on map
# print_labels <- function(dat){
#   
#   sprintf(
#     "<strong>%s</strong><br/>%g people",
#     as.data.frame(dat)[,1], dat$Value
#   ) %>% lapply(htmltools::HTML)
# }

# Prepare data for model regional level
prepdata_for_model <- function(dftoprep, resdata){
  ag <- dftoprep
  
  # Converto in fattore i tempi
  fa <- factor(ag$data)
  # Converto in fattore la regione
  fa2 <- factor(ag$denominazione_regione)
  # Mi tengo i tempi come numerici
  ti <- unclass(fa)
  # I tempi originali
  ti_orig <- date(attr(ti, "levels"))
  
  # Aggiusto le etichette
  levels(fa2)[6] <- "Friuli V. G."
  levels(fa2)[5] <- "Emilia Romagna"
  levels(fa2)[17] <- "TrentinoAltoAdige"
  
  # Tengo solo i residenti delle regioni che matchano l'etichetta delle regioni
  ma <- match(resdata[,1],levels(fa2))
  residents <- resdata[which(!is.na(ma)),]
  # Prendo le etichette della regioni che ora matchano i residenti
  ma <- match(fa2,resdata[,1])
  
  # converto in carattere
  fa2 <- as.character(fa2)
  
  # Creo il dataset di input
  da <- data.frame(ag %>% 
                     # Lascio da parte le variabili per cui non faccio il modello
                     dplyr::select(-data, -denominazione_regione, -Tamponi, -`Variazione totale positivi`, -`Dismessi/Guariti`, -`Nuovi positivi`),
                   ti=as.numeric(ti), ti_orig = ti_orig,
                   region=fa2, 
                   residents=resdata[ma,2])
  
  colnames(da) <- c("Deceduti", "Isolamento domiciliare", "Ricoverati con sintomi", 
                    "Terapia intensiva", "Totale casi", "Totale ricoverati",  "Totale positivi", "ti", "ti_orig", "region", "residents")
  
  return(da)
}

# function for running the model by region
run_growth_model <- function(da, reg=NULL, wh="Totale casi", horizon = 10, fam="Poisson") {
  
  
  ti_orig_out <- da$ti_orig %>% unique()
  
  if(is.null(reg)){
    dat <- aggregate(da %>% dplyr::select(-region, -ti, -residents, -ti_orig), list(da$ti), sum)
    colnames(dat) <- c("ti", "Deceduti", "Isolamento domiciliare", "Ricoverati con sintomi", 
                       "Terapia intensiva", "Totale casi", "Totale ricoverati",  "Totale positivi")
    
  }else{
    dat <- da[da$region==reg,]
  }
  
  # Seleziono variabile da modellare
  whidx <- which(colnames(dat)==wh)
  pc <- dat[, whidx]
  ti <- dat$ti
  
  
  if(any(pc==0)) {
    w <- min(which(pc!=0))
    pc <- pc[-c(1:w)]
    ti <- ti[-c(1:w)]
    ti <- ti-min(ti)+1
    ti_orig_out <- dat$ti_orig[-c(1:w)]
  }
  
  mti <- max(ti)        
  timax <- mti+horizon
  mnt <- ifelse(any(diff(pc)<0), F, T)
  np <- growthGLM(count = pc, ti = ti, timax = timax, family = fam, maxiter = 5000, monotone = mnt, run = 100)
  
  if(any(is.na(np$se))){
    np <- growthGLM(count = pc, ti = ti, timax = timax, family = fam, maxiter = 10000, monotone = mnt, run = 500)
  }
  
  if(any(is.na(np$se))){
    np <- growthGLM(count = pc, ti = ti, timax = timax, family = fam, maxiter = 15000, monotone = mnt, run = 1000)
  }
  
  # Prediction dei cumulati
  y <- np$linPred
  # Creo tempi fittizi numerici
  x <- seq(min(ti_orig_out), max(ti_orig_out) + horizon, 1)
  x1 <- c(ti_orig_out[2:mti], rep(NA, horizon)) 
  pc_out_diff <- c(diff(pc), rep(NA, horizon))
  # Data frame con osservati e predetti nuovi 
  cc1<-data.frame(x1 = x1,
                  # Creo gli osservati facendo le differenze prime dei cumulati
                  pc = pc_out_diff, 
                  x = x[-1], y = diff(y))
  
  # Data frame con osservati e predetti cumulati
  cc<-data.frame(x1 = c(x1[1] - 1, x1),
                 pc = c(pc,rep(NA,horizon)),
                 x = x, y = y)
  
  return(list(cc=cc, cc1=cc1, R2 = round(np$R2, 4), pars = np$pars, stderrs = np$se)) 
}


# Plot result model (return plot)
plot_out_model <- function(outputmod, hz, what = "Cumulati"){
  
  if(what == "Cumulati"){
    dd <- outputmod$cc
    ylabel <- "Casi cumulati"
  } 
  if(what == "Nuovi"){
    dd <- outputmod$cc1
    ylabel <- "Nuovi casi"
  } 
  
  pp <- ggplot(dd %>% 
           filter(x <= max(x1, na.rm = T) + hz)) +
    geom_line(aes(x = x1, y = pc), alpha=0.2) + 
    geom_point(aes(x = x1,y = pc))+
    geom_line(aes(x = x, y = y),col="red", size = 1.1)+
    labs(x = "", y = ylabel) +
    theme_bw() +
    theme(text = element_text(size = 14))
  return(pp)
  
}


# Table output model (return table)
DT_out_model <- function(outputmod, hz) {
  
  sketch  <-  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Data'),
        th(colspan = 2, 'Cumulati'),
        th(colspan = 2, 'Nuovi')
      ),
      tr(
        lapply(rep(c('Osservati', 'Predetti'), 2), th)
      )
    )
  ))
  
  ddt <- bind_cols(outputmod$cc %>% set_colnames(value = c("Data", "Cumulati.Osservati", "Data2", "Cumulati.Predetti")),
                   outputmod$cc1 %>% add_row(data.frame(x1=outputmod$cc[1,1], pc=NA, x=outputmod$cc[1,1], y=NA), .before = T) %>%  
                     set_colnames(value = c("Data", "Nuovi.Osservati", "Data2", "Nuovi.Predetti")) %>% 
                     dplyr::select(-Data, -Data2)) %>% 
    filter(between(Data2, max(Data, na.rm = T) -2, max(Data, na.rm = T) + hz)) %>% 
    dplyr::select(-Data) %>% 
    mutate_if(is.numeric, round) 
  
  #cpt <- ifelse(what == "Cumulati", "Casi cumulati", "Nuovi casi giornalieri")
  
  ddtt <- datatable(ddt[,c(2, 1, 3:5)], rownames = FALSE, #caption = cpt, 
            options = list(dom = 'tp', pageLength = 5, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
            container = sketch)
  
  return(ddtt)
  
}

# print summary model (return html code)
summary_out_model <- function(outputmod){
  
  omod <- outputmod$cc1
  ti_orig_out <- c(outputmod$cc1$x1[1]-1, outputmod$cc1$x1 %>% na.omit())
  
  # Picco stimato
  l_pck <- min(ti_orig_out) + round(exp(outputmod$pars[4]-1.96*outputmod$stderrs[4]))
  u_pck <- min(ti_orig_out) + round(exp(outputmod$pars[4]+1.96*outputmod$stderrs[4]))
  est_pick <- min(ti_orig_out) + round(exp(outputmod$pars[4]))
  # Valore stimato nel giorno del picco
  estmax_val <- round(omod$y[omod$x==est_pick])
  # Se picco già avvenuto, metti valore osservato
  obsmax_val <- ifelse(est_pick<=max(omod$x1, na.rm=T), round(omod$pc[omod$x==est_pick]), NA)
  
  # Asintoto cumulati
  asi_est_l <- round(exp(outputmod$pars[1]-1.96*outputmod$stderrs[1])+exp(outputmod$pars[2]-1.96*outputmod$stderrs[2])) 
  asi_est_u <- round(exp(outputmod$pars[1]+1.96*outputmod$stderrs[2])+exp(outputmod$pars[2]+1.96*outputmod$stderrs[2])) 
  asi_est <- round(exp(outputmod$pars[1])+exp(outputmod$pars[2])) 
  
  pdate_l <- paste0(day(l_pck), " ", stri_trans_totitle(month(l_pck, label = T, abbr = F)), " ",  year(l_pck))
  pdate <- paste0(day(est_pick), " ", stri_trans_totitle(month(est_pick, label = T, abbr = F)), " ",  year(est_pick))
  pdate_u <- paste0(day(u_pck), " ", stri_trans_totitle(month(u_pck, label = T, abbr = F)), " ",  year(u_pck))
  
  out_string <- paste0(
    "<ul>",
    "<li><b>Picco della curva stimato: </b>", pdate, paste0(" (non prima del ", pdate_l, ", non dopo il ", pdate_u, ")"), "</li>",
    "<li><b>Massimo stimato di casi giornalieri: </b>", estmax_val, ifelse(!is.na(obsmax_val), paste0(" (osservati ", obsmax_val, ")"), ""),"</li>",
    "<li><b>Asintoto stimato casi cumulati: </b>", asi_est, paste0(" (non meno di ", asi_est_l, ", non piu' di ", asi_est_u, ")"),"</li>", 
    "<li><b>Goodness of fit</b> (R2): ", outputmod$R2, "</li>",
    "</ul>",
    collapse = ""
  )
  
  return(out_string)
}


# Build map default

# plot_leaflet_map <- function(df, variable, datetoselect){
#   
#   dd <- df %>% dplyr::filter(Key == variable, data == datetoselect)
#   
#   pal <- colorBin("YlOrRd", domain = dd$Value, bins = 6)
#   
#   leaflet(data = dd) %>% 
#     clearTiles() %>% 
#     clearShapes() %>%
#     clearControls() %>% 
#     addTiles() %>% 
#     addPolygons(
#       fillColor = ~pal(Value),
#       weight = 2,
#       opacity = 1,
#       color = "black",
#       dashArray = "1",
#       fillOpacity = 1,
#       highlight = highlightOptions(
#         weight = 5,
#         fillOpacity = 1,
#         bringToFront = TRUE),
#       label = print_labels(dd),
#       labelOptions = labelOptions(
#         style = list("font-weight" = "normal", padding = "3px 8px"),
#         textsize = "15px",
#         direction = "auto")) %>%
#     addLegend(pal = pal, values = ~Value, opacity = 1, title = variable, position = "topright", na.label = "Missing")
#   
# }
