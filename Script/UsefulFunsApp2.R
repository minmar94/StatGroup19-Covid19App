# # Useful functions ------------------------------------------------------
# Paste date in english
paste_eng_date <- function(date){
  dd <- day(date)
  mm <- month(date, label = T, abbr = F)
  yy <-year(date)
  
  l1 <- ifelse(dd %in% c(1, 21, 31), "st", ifelse(dd %in% c(2, 22), "nd", ifelse(dd %in% c(3, 23), "rd", "th")))
  outmonth <- paste0(dd, l1, " of ", mm, " ", yy)
  return(outmonth)
}

# Read italian data
read_italian <- function(path){
  
  dati_Ita <- read_csv(path) %>% 
    mutate(data = date(data)) %>% dplyr::select(-note_it, -note_en, -casi_testati)
  
  colnames(dati_Ita)[-c(1,2)] <- c("Hospitalized with symptoms", "Intensive care", "Current hospitalized",
                                   "Home isolation", "Current positives", "Variation Current positives", "New positives", 
                                   "Discharged recovered", "Deceased",  "Cumulative positives", "Swabs")
  
  return(dati_Ita)
  
}

# Read and prepare regional data
read_regional <- function(path){
  
  dati_reg <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en, -casi_testati) %>% 
    # 
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige",
                                          denominazione_regione),
           data = date(data)) %>% 
    gather(Key, Value, ricoverati_con_sintomi:tamponi) %>% 
    group_by(Key, data, denominazione_regione) %>% 
    dplyr::summarise(Value = sum(Value)) %>% 
    ungroup() 
  
  dati_reg$Key <- factor(dati_reg$Key, levels = unique(dati_reg$Key), 
                         labels = c("Deceased", "Discharged recovered", "Home isolation", "New positives", "Hospitalized with symptoms", 
                                    "Swabs", "Intensive care", "Cumulative positives", "Current hospitalized", "Current positives", 
                                    "Variation Current positives"))
  
  return(dati_reg)
}

# Read province data
read_province <- function(path){
  
  dati_prov <- read_csv(path) %>% 
    dplyr::select(-note_it, -note_en) %>%
    mutate(denominazione_regione = ifelse(denominazione_regione %in% c("P.A. Bolzano", "P.A. Trento"), "Trentino-Alto Adige", denominazione_regione),
           data = date(data)) %>% 
    drop_na(sigla_provincia) %>% 
    rename(`Cumulative positives` = totale_casi)
  
  
  
}

# Read italian data shapefile for map
read_shape_italy <- function(province = F){
  
  italy_sf <- read_rds("Data/gadm36_ITA_1_sp.rds") %>% 
    st_as_sf() %>% dplyr::select(NAME_1, geometry) %>% 
    mutate(NAME_1 = factor(NAME_1, labels = c("Abruzzo", "Puglia", "Basilicata", "Calabria", "Campania", "Emilia-Romagna", "Friuli Venezia Giulia",
                                              "Lazio" ,"Liguria" , "Lombardia", "Marche", "Molise", "Piemonte", "Sardegna", "Sicilia", "Toscana",
                                              "Trentino-Alto Adige", "Umbria", "Valle d'Aosta", "Veneto")))
  colnames(italy_sf)[1] <- "NAME" 
  
  if(province){
    
    italy_sf <- read_rds("Data/gadm36_ITA_2_sp.rds") %>% 
      st_as_sf() %>% dplyr::select(NAME_1, NAME_2, geometry)
    
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

# Not actually in use
print_current_situa <- function(da, TotPop){
  today <- max(da$data)
  dat_today <- da %>% dplyr::filter(data %in% c(today, today-1))
  
  attualmente_positivi_ultimo <- dat_today$`Current positives`[2]
  incr_attualmente_positivi <- paste_signpercent(dat_today$`Current positives`[2], dat_today$`Current positives`[1])
  
  deceduti_ultimo <- dat_today$Deceased[2]
  incr_deceduti <- paste_signpercent(dat_today$Deceased[2], dat_today$Deceased[1])
  
  dimessi_guariti_ultimo <- dat_today$`Discharged recovered`[2]
  incr_guariti <- paste_signpercent(dat_today$`Discharged recovered`[2], dat_today$`Discharged recovered`[1])
  
  totale_casi_ultimo <- dat_today$`Cumulative positives`[2]
  incr_totale_casi <- paste_signpercent(dat_today$`Cumulative positives`[2], dat_today$`Cumulative positives`[1])
  
  ricov_sintomi_ultimo <- dat_today$`Hospitalized with symptoms`[2]
  incr_ricoverati <- paste_signpercent(dat_today$`Hospitalized with symptoms`[2], dat_today$`Hospitalized with symptoms`[1])
  
  isol_domic_ultimo <- dat_today$`Home isolation`[2]
  incr_isolamento <- paste_signpercent(dat_today$`Home isolation`[2], dat_today$`Home isolation`[1])
  
  terapia_intens_ultimo <- dat_today$`Intensive care`[2]
  incr_terapia <- paste_signpercent(dat_today$`Intensive care`[2], dat_today$`Intensive care`[1])
  
  totale_ricoverati_ultimo <- dat_today$`Current hospitalized`[2]
  incr_currhosp <- paste_signpercent(dat_today$`Current hospitalized`[2], dat_today$`Current hospitalized`[1])
  
  
  quoz_let_ita <- round(dat_today$Deceased[2]/dat_today$`Cumulative positives`[2],4)*100
  
  tasso_mort_ita <- round(dat_today$Deceased[2]/TotPop,4)*100
  
  return(
    paste0(tags$h2(tags$strong("Italian current situation about the COVID-19 pandemic*")),
           h5("*most recent update: ", paste_eng_date(today)),
           "<ul>",
           "<li><b>Cumulative positives: </b>", totale_casi_ultimo, " (",incr_totale_casi,")","</li>",
           "<li><b>Current positives: </b>", attualmente_positivi_ultimo," (",incr_attualmente_positivi,")","</li>",
           "<li><b>Current hospitalized: </b>", totale_ricoverati_ultimo, " (",incr_currhosp,")","</li>",
           "<li><b>Intensive care units: </b>", terapia_intens_ultimo, " (",incr_terapia,")","</li>",
           "<li><b>Hospitalized with symptoms: </b>", ricov_sintomi_ultimo, " (",incr_ricoverati,")","</li>",
           "<li><b>Home isolation: </b>", isol_domic_ultimo, " (",incr_isolamento,")","</li>",
           "<li><b>Discharged recovered: </b>", dimessi_guariti_ultimo," (",incr_guariti,")","</li>",
           "<li><b>Deceased: </b>", deceduti_ultimo, " (",incr_deceduti,")","</li>",
           "<li><b>Death rate: </b>", tasso_mort_ita, "%","</li>",
           "<li><b>Fatality rate: </b>", quoz_let_ita, "%","</li>",
           "</ul>",
           collapse = ""
    )
  )
}


# Do donut plot
do_ciambella <- function(da, is.reg = F, reg, variable = "Cumulative positives"){
  
  colors <- c("firebrick", "darkorange", "salmon")
  
  if(variable == "Cumulative positives"){
    vartosel <- c("Deceased", "Current positives", "Discharged recovered")
  }
  if(variable == "Current positives"){
    vartosel <- c("Home isolation", "Hospitalized with symptoms", "Intensive care")
  }
  
  if(!is.reg){
    dperdonut <- da %>% 
      mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% 
      filter(data == max(data), Key %in% vartosel) %>% 
      arrange(Key) %>% 
      mutate(percentage = round(Value/sum(Value),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage) %>%
      mutate(stato = "Italy") %>% 
      rename(denominazione_regione = stato)
  }else{
    dperdonut <- da %>% 
      mutate(Key = as.character(Key)) %>% 
      filter(denominazione_regione == reg, data == max(data), 
             Key %in% vartosel) %>% arrange(Key) %>% 
      mutate(percentage = round(Value/sum(Value),4)*100,
             lab.pos = cumsum(percentage)-.5*percentage) 
  }
  
  dperdonut %>% 
    plot_ly(labels = ~Key, values = ~percentage, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            hole = 0.6,
            text = ~paste(Key, "\n", percentage, '%'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>% 
    layout(title = paste0("Distribution of ", tolower(variable),  " in ", "<b>",unique(dperdonut$denominazione_regione), "</b>", " today"), 
           showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}


# Do barplot TS
do_barplot_ts <- function(da, is.reg = F, reg, variable = "Cumulative positives"){
  
  if(variable == "Cumulative positives"){
    vartosel <- c("Deceased", "Current positives", "Discharged recovered")
  }
  if(variable == "Current positives"){
    vartosel <- c("Hospitalized with symptoms", "Home isolation", "Intensive care")
  }
  
  if(!is.reg){
    dperbar <- da %>%
      mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% spread(Key, Value) %>% 
      mutate(stato = "Italy") %>% 
      rename(denominazione_regione = stato) %>% 
      dplyr::select(data, denominazione_regione, vartosel)
    
    colnames(dperbar)[-c(1,2)] <- c("Y1", "Y2", "Y3")
    
  }else{
    dperbar <- da %>% 
      filter(denominazione_regione == reg, Key %in% vartosel) %>% 
      spread(Key, Value) %>% 
      dplyr::select(data, denominazione_regione, vartosel)
    
    colnames(dperbar)[-c(1,2)] <- c("Y1", "Y2", "Y3")
  }
  
  dperbar %>% 
    plot_ly(x = ~data, y = ~Y1, type = "bar", name = vartosel[1], color = I("darkorange")) %>% 
    add_trace(y = ~Y2, name = vartosel[2], color = I("firebrick")) %>% 
    add_trace(y = ~Y3, name = vartosel[3], color = I("salmon")) %>% 
    layout(title = paste0("Distribution of ", tolower(variable), " over time","\n", "<b>",unique(dperbar$denominazione_regione), "</b>"),
           xaxis = list(title = ""), legend = list(x = 0, y = 1),
           yaxis = list(title = 'Counts'), barmode = 'stack')
  
  
}


# Do map
draw_map <- function(dajoined, reg = NULL, varsel, datasel, dajoinedprov){
  
  annots <- list(x = 1, y = -0.1, text = as.character(datasel), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=15, color="black"))
  if(is.null(reg)){
    p <- plot_ly(data = dajoined %>% 
                   filter(Key == as.character(varsel), data == as.character(datasel)), 
                 stroke = I("black"),
                 split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                 text = ~paste0(NAME, "\n", Value, " people"),
                 hoveron = "fills",
                 hoverinfo = "text", 
                 showlegend = F) %>%
      layout(annotations = annots) %>% 
      colorbar( title = list(text = paste0("<b>", as.character(varsel), "</b>")), len = 0.6)
  }else{
    
    if(varsel == "Cumulative positives"){
      p <- plot_ly(data = dajoinedprov %>% 
                     filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg), 
                   stroke = I("black"),
                   split = ~NAME_2, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME_2," (",NAME, ")","\n", Value, " people"),
                   hoveron = "fills",
                   hoverinfo = "text", showlegend = F) %>% 
        layout(annotations = annots)  %>% 
        colorbar( title = list(text = paste0("<b>", as.character(varsel), "</b>")), len = 0.6)
    }else{
      p <- plot_ly(data = dajoined %>% 
                     filter(Key == as.character(varsel), data == as.character(datasel), NAME %in% reg), 
                   stroke = I("black"),
                   split = ~NAME, color = ~Value, colors = "YlOrRd", alpha = 1, 
                   text = ~paste0(NAME, "\n", Value, " people"),
                   hoveron = "fills",
                   hoverinfo = "text", showlegend = F) %>% 
        layout(annotations = annots)  %>% 
        colorbar( title = list(text = paste0("<b>", as.character(varsel), "</b>")), len = 0.6)
      if(length(reg) == 1){
        p <- p %>% hide_colorbar()
      }
    }
    
  }
  
  
  return(p)
  
  
}


# Do Time series
do_ts <- function(da, is.reg = F, reg, varsel, datasel, is.incrementi=T, tipo.incremento = "Absolute"){
  
  if(!is.reg){
    
    dts <- da %>% 
      mutate(Key = as.character(Key)) %>% 
      group_by(data, Key) %>% 
      summarise(Value = sum(Value)) %>% 
      ungroup() %>% 
      filter(data <= as.character(datasel), Key == varsel) %>% 
      arrange(Key) %>% 
      mutate(denominazione_regione = "Italy") 
    
  }else{
    
    dts <- da %>% 
      filter(data <= as.character(datasel), Key == varsel, denominazione_regione %in% reg) %>% 
      dplyr::select(data, denominazione_regione, Key, Value)
    
  }
  
  ylabel <- "Counts"
  
  if(is.incrementi){
    dts <- dts %>% 
      arrange(denominazione_regione) %>% 
      group_split(denominazione_regione) %>% 
      map_dfr(function(x){
        x %<>% 
          mutate(IncrAss = c(0, diff(Value)),
                 IncrPerc = round((c(0, diff(Value))/Value)*100,4)) %>% 
          ungroup() 
      })
    ylabel <- ifelse(tipo.incremento == "Absolute", "Absolute variations (w.r.t. yesterday)",
                     "Relative (%) variations (w.r.t. yesterday)") 
    
    if(tipo.incremento == "Absolute"){
      dts <- dts %>% dplyr::select(data, denominazione_regione, Key, IncrAss) %>% rename(Value = IncrAss)
    } else {
      dts <- dts %>% dplyr::select(data, denominazione_regione, Key, IncrPerc) %>% rename(Value = IncrPerc) %>% 
        filter(data > min(data))
    }
    
    
  }
  colors <- c("firebrick", "red", "orange3", "orange","gold")[1:length(reg)]
  
  dts %>% 
    plot_ly(x = ~data, y = ~Value, type = 'scatter', mode = 'lines+markers', 
            color = ~denominazione_regione, colors = colors) %>% 
    layout(xaxis = list(title = ""), yaxis = list(title = ylabel), title = paste("Time series of",tolower(varsel)),
           legend = list(orientation = "h"))
  
}


# Print sign header
paste_signpercent <- function(x_oggi,x_ieri){
  simbolo <- ifelse(x_oggi-x_ieri>0, "+", "")
  incr <- ((x_oggi-x_ieri)/x_ieri)*100
  return(paste0(simbolo, round(incr, 2),"% w.r.t. yesterday"))
} 


# Prepare data for model regional level
prepdata_for_model <- function(dftoprep, resdata){
  ag <- dftoprep %>% 
    mutate(denominazione_regione = ifelse(denominazione_regione == "Trentino-Alto Adige", "TrentinoAltoAdige", 
                                          ifelse(denominazione_regione == "Friuli Venezia Giulia", "Friuli V. G.",
                                                 ifelse(denominazione_regione == "Emilia-Romagna", "Emilia Romagna",
                                                        denominazione_regione)))) %>% 
    arrange(denominazione_regione, data)
  
  # Label
  resdata[10,1] <- "Valle d'Aosta"
  
  da <- ag %>% 
    left_join(resdata, by = c("denominazione_regione" = "Territorio")) %>% 
    mutate(ti = as.numeric(unclass(factor(data))), denominazione_regione = factor(denominazione_regione)) %>% 
    rename(ti_orig = data, region = denominazione_regione, residents = totale) 
  
  return(da)
}

# Run the model
run_growth_model <- function(da, reg=NULL, wh="Cumulative positives", horizon = 10, fam="Poisson") {
  
  ti_orig_out <- da$ti_orig %>% unique()
  
  if(is.null(reg)){
    dat <- aggregate(da %>% dplyr::select(-region, -ti, -residents, -ti_orig), list(da$ti), sum)
    colnames(dat)[1] <- "ti"
    
  }else{
    dat <- da[da$region==reg,]
  }
  
  # Pick the variable to model
  whidx <- which(colnames(dat)==wh)
  pc <- dat[, whidx, drop = T]
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
  # 
  diffpc <- diff(pc)
  lastidx <- (length(pc)-9):length(pc) 
  beta1 <- coef(lm(pc[lastidx]~ti[lastidx]))[2]
  
  mnt <- as.logical(ifelse(beta1<0, F, T))
  rrs <- 500
  
  np <- tryCatch(
    growthGLM(count = pc, ti = ti, monotone = mnt, nmirror = 8, tk = NA, family = fam, maxiter = 5000, runs = rrs, tPred = timax),
    #warning = function(w){print("Something happened")},
    error = function(e){return("Error")}
  )
  
    
  # Prediction
  y <- cbind(np$low, np$linPred, np$up)
  colnames(y) <- c("ly", "y", "uy")
  ydiff <- cbind(np$lowdiff, diff(np$linPred), np$updiff)
  colnames(ydiff) <- c("ly", "y", "uy")
  #
  x <- seq(min(ti_orig_out), max(ti_orig_out) + horizon, 1)
  x1 <- c(ti_orig_out[2:mti], rep(NA, horizon))
  
  pc_out <- c(diff(pc), rep(NA, horizon))
  
  # Data frame observed and predicted variation
  cc1<-data.frame(x1 = x1, pc = pc_out, x = x[-1], ydiff)
  
  # Data frame observed and predicted total
  cc<-data.frame(x1 = c(x1[1] - 1, x1), pc = c(pc,rep(NA,horizon)), x = x, y)
  
  ooo <- list(cc=cc, cc1=cc1, R2 = list(round(np$R2, 4)), pars = list(np$pars), stderrs = list(np$se), monot = mnt, 
              fam = fam, NoConv = np$NoConv, BandsError = np$BandsError)
    
  
  return(ooo)
}


# Plot result model (return plot) new
plot_out_model <- function(outputmod, horizon = 15, what = "Cumulati", VarModel = "Cumulative positives", showbands = T){
  
  ses <- outputmod$stderrs[[1]]
  
  if(what == "Cumulati"){
    dd <- outputmod$cc %>% 
      mutate(uy = ifelse(x<=max(x1, na.rm = T), NA, uy),
             ly = ifelse(x<=max(x1, na.rm = T), NA, ly))
    ylabel <- "Daily counts"
    if(VarModel %in% c("Deceased", "Cumulative positives")){
      ylabel <- "Cumulative counts"
    }
  }
  if(what == "Nuovi"){
    dd <- outputmod$cc1 %>% 
      mutate(uy = ifelse(x<=max(x1, na.rm = T), NA, uy),
             ly = ifelse(x<=max(x1, na.rm = T), NA, ly))
    ylabel <- "Daily variations"
    if(VarModel %in% c("Deceased", "Cumulative positives")){
      ylabel <- "Daily counts"
    }
  }
  
  namemodel <- ifelse(outputmod$monot, "Richards", "Mirrored Richards")
  
  pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + horizon),
                x=~x1, y = ~pc, name = 'Observed', color = I("black"),
                type = 'scatter', mode = 'lines+markers', alpha = 0.6) %>%
    add_trace(x=~x, y = ~y, name = namemodel, type = "scatter",
              mode = 'lines', alpha = 1,
              line = list(width = 3, color = "red")) %>%
    layout(xaxis = list(title = ""), yaxis = list(title = ylabel))
  
  if(!outputmod$NoConv){
    if(sum(is.na(ses)|is.nan(ses)) == 0){
      pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + horizon),
                    x=~x, y = ~y, type = "scatter", mode = 'lines',
                    line = list(width = 3, color = "red"), name = namemodel) %>%
        add_ribbons(ymin = ~ly, ymax = ~uy, name = '95% Conf. Int.', color = I('rgba(250,128,114, 0.8)'),
                    line = list(width = 0)) %>% 
        add_trace(y = ~pc,  name = "Observed", alpha = 0.7, mode = 'lines+markers', 
                  line = list(color = "black", width=0.5), marker = list(color = "black")) %>%
        layout(xaxis = list(title = ""), yaxis = list(title = ylabel))
    }
  }  else{
    if(sum(is.na(ses)|is.nan(ses)) == 0 & showbands){
      pp <- plot_ly(dd %>% filter(x <= max(x1, na.rm = T) + horizon), 
                   x=~x, y = ~y, type = "scatter", mode = 'lines',
                   line = list(width = 3, color = "red"), name = namemodel) %>%
        add_ribbons(ymin = ~ly, ymax = ~uy, name = '95% Conf. Int.', color = I('rgba(250,128,114, 0.8)'),
                    line = list(width = 0)) %>% 
        add_trace(y = ~pc,  name = "Observed", alpha = 0.7, mode = 'lines+markers', 
                  line = list(color = "black", width=0.5), marker = list(color = "black")) %>%
        layout(xaxis = list(title = ""), yaxis = list(title = ylabel))
    } 
  }
  
  
  return(pp)
  
}


# Table output model (return table) new
DT_out_model <- function(outputmod, horizon = 15, VarModel = "Cumulative positives", showbands = F) {
  
  ses <- outputmod$stderrs[[1]]
  
  sketch  <-  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Date'),
        th(colspan = 2, 'Total'),
        th(colspan = 2, 'Variation')
      ),
      tr(
        lapply(rep(c('Observed', 'Predicted'), 2), th)
      )
    )
  ))
  
  sketch2  <-  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Date'),
        th(colspan = 2, 'Cumulative'),
        th(colspan = 2, 'Total')
      ),
      tr(
        lapply(rep(c('Observed', 'Predicted'), 2), th)
      )
    )
  ))
  
  
  capts_out <-  htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: left;',
    'Table: ', htmltools::em(paste0('Predictions for ', tolower(VarModel), '.'))
  )
  
  
  ddt <- bind_cols(outputmod$cc %>% dplyr::select(x1, pc, x, y) %>%
                     set_colnames(value = c("Date", "Total.Observed", "Data2", "Total.Predicted")),
                   outputmod$cc1[, c(1,2,3,5)] %>% dplyr::select(x1, pc, x, y) %>%
                     add_row(data.frame(x1=outputmod$cc[1,1], pc=NA, x=outputmod$cc[1,1], y=NA), .before = T) %>%
                     set_colnames(value = c("Date", "Variation.Observed", "Data2", "Variation.Predicted")) %>%
                     dplyr::select(-Date, -Data2)) %>%
    filter(between(Data2, max(Date, na.rm = T) -2, max(Date, na.rm = T) + horizon)) %>%
    dplyr::select(-Date) %>%
    mutate_if(is.numeric, round)
  
  if(!outputmod$NoConv){
    if(sum(is.na(ses)|is.nan(ses)) == 0){
      
      sketch  <-  htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Date'),
            th(colspan = 2, 'Total'),
            th(colspan = 2, 'Variation')
          ),
          tr(
            lapply(rep(c('Observed', 'Predicted (Min., Max.)'), 2), th)
          )
        )
      ))
      sketch2  <-  htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Date'),
            th(colspan = 2, 'Cumulative'),
            th(colspan = 2, 'Total')
          ),
          tr(
            lapply(rep(c('Observed', 'Predicted (Min., Max.)'), 2), th)
          )
        )
      ))
      dcc <- outputmod$cc %>% mutate_if(is.numeric, round) %>% 
        mutate(IC = paste("(",ly,", ",uy,")" , sep = "")) %>% 
        unite("y", y, IC, sep = " ")
      
      dcc1 <- outputmod$cc1 %>% mutate_if(is.numeric, round) %>% 
        mutate(IC = paste("(",ly,", ",uy,")" , sep = "")) %>% 
        unite("y", y, IC, sep = " ")
      
      ddt <- bind_cols(dcc %>% dplyr::select(x1, pc, x, y) %>%
                         set_colnames(value = c("Date", "Total.Observed", "Data2", "Total.Predicted")),
                       dcc1 %>% dplyr::select(x1, pc, x, y) %>%
                         add_row(data.frame(x1=outputmod$cc[1,1], pc=NA, x=outputmod$cc[1,1], y=NA), .before = T) %>%
                         set_colnames(value = c("Date", "Variation.Observed", "Data2", "Variation.Predicted")) %>%
                         dplyr::select(-Date, -Data2)) %>%
        filter(between(Data2, max(Date, na.rm = T) -2, max(Date, na.rm = T) + horizon)) %>%
        dplyr::select(-Date) 
    }
    
  } else{
    if(sum(is.na(ses)|is.nan(ses)) == 0 & showbands){
      sketch  <-  htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Date'),
            th(colspan = 2, 'Total'),
            th(colspan = 2, 'Variation')
          ),
          tr(
            lapply(rep(c('Observed', 'Predicted (Min., Max.)'), 2), th)
          )
        )
      ))
      sketch2  <-  htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Date'),
            th(colspan = 2, 'Cumulative'),
            th(colspan = 2, 'Total')
          ),
          tr(
            lapply(rep(c('Observed', 'Predicted (Min., Max.)'), 2), th)
          )
        )
      ))
      dcc <- outputmod$cc %>% mutate_if(is.numeric, round) %>% 
        mutate(IC = paste("(",ly,", ",uy,")" , sep = "")) %>% 
        unite("y", y, IC, sep = " ")
      
      dcc1 <- outputmod$cc1 %>% mutate_if(is.numeric, round) %>% 
        mutate(IC = paste("(",ly,", ",uy,")" , sep = "")) %>% 
        unite("y", y, IC, sep = " ")
      
      ddt <- bind_cols(dcc %>% dplyr::select(x1, pc, x, y) %>%
                         set_colnames(value = c("Date", "Total.Observed", "Data2", "Total.Predicted")),
                       dcc1 %>% dplyr::select(x1, pc, x, y) %>%
                         add_row(data.frame(x1=outputmod$cc[1,1], pc=NA, x=outputmod$cc[1,1], y=NA), .before = T) %>%
                         set_colnames(value = c("Date", "Variation.Observed", "Data2", "Variation.Predicted")) %>%
                         dplyr::select(-Date, -Data2)) %>%
        filter(between(Data2, max(Date, na.rm = T) -2, max(Date, na.rm = T) + horizon)) %>%
        dplyr::select(-Date) 
    }
  }
  
  
  
  ddtt <- datatable(ddt[,c(2, 1, 3:5)], rownames = FALSE, class = 'cell-border stripe',
                    options = list(dom = 'tp', pageLength = 5, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    container = sketch, caption = capts_out)
  
  
  if(VarModel %in% c("Deceased",  "Cumulative positives")){
    
    
    
    ddt <- ddt %>% 
      rename(Cumulative.Observed = Total.Observed, Cumulative.Predicted = Total.Predicted, 
             Total.Observed = Variation.Observed, Total.Predicted = Variation.Predicted)
    
    
    ddtt <- datatable(ddt[,c(2, 1, 3:5)], rownames = FALSE, class = 'cell-border stripe',
                      options = list(dom = 'tp', pageLength = 5, scrollX = T, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                      container = sketch2, caption = capts_out)
  }
  
  return(ddtt)
  
}


# print summary model (return html code)
summary_out_model <- function(outputmod, VarModel = "Cumulative positives", resdata, is.reg = F, reg = NULL){
  
  famout <- outputmod$fam
  
  resdata[10,1]="Valle d'Aosta"
  
  if(is.reg & !is.null(reg)) mx_sel <- resdata[resdata$Territorio == reg, 2]
  if(!is.reg) mx_sel <- 6*10^7
  
  omod <- outputmod$cc1
  
  ti_orig_out <- c(outputmod$cc1$x[1]-1, outputmod$cc1$x)
  ti <- as.numeric(unclass(factor(ti_orig_out)))
  
  est_pick <- ti_orig_out[which.max(omod$y)]
  estmax_val_l <- round(omod$ly[which.max(omod$y)])
  estmax_val <- round(max(omod$y))
  estmax_val_u <- round(omod$uy[which.max(omod$y)])
  obsmax_val <- omod$pc[which.max(omod$y)]
  
  
  # 
  tasso_come <- ifelse(outputmod$monot, "Richards", "Mirrored Richards")
  
  # 
  pdate <- paste_eng_date(est_pick)
 
  
  if(VarModel %in% c("Deceased", "Cumulative positives") & !(is.na(outputmod$stderrs[[1]][1]))){
    
    # Asintoto cumulati
    asi_est_l <- round(exp(outputmod$pars[[1]][1]-1.96*outputmod$stderrs[[1]][1]))
    idx <- min(which(is.na(outputmod$cc$pc)))-1
    asi_est_l <- ifelse(asi_est_l < outputmod$cc$pc[idx], outputmod$cc$pc[idx], asi_est_l)
    asi_est_u <- round(exp(outputmod$pars[[1]][1]+1.96*outputmod$stderrs[[1]][1])) 
    asi_est_u <- ifelse(asi_est_u > mx_sel, mx_sel, asi_est_u)
    asi_est <- round(exp(outputmod$pars[[1]][1])) 
    
    
    out_string <- paste0(
      paste0("<h4><b>Model with ", famout, " - ", tasso_come, ": </b></h4>"),
      "<ul>",
      "<li><b>Estimated peak of daily variations: </b>", pdate, "</li>",
      ifelse(!is.na(estmax_val_l), paste0("<li><b>Max estimated value at the peak: </b>", "between ", 
                                          estmax_val_l, " and ", estmax_val_u, ifelse(!is.na(obsmax_val), paste0(" (observed ", obsmax_val, ")"), ""),"</li>"), ""),
      "<li><b>Estimated upper asymptote of cumulative cases: </b>", asi_est, paste0(" (not less than ", asi_est_l, ", no more than ", asi_est_u, ")"),"</li>", 
      "<li><b>Goodness of fit: </b> ", outputmod$R2[[1]], "</li>",
      "</ul>",
      collapse = ""
    )
  } else{
    out_string <- paste0(
      paste0("<h2><b>Model with ", famout, " - ", tasso_come, ": </b></h2>"),
      "<ul>",
      "<li><b>Estimated peak of daily variations: </b>", pdate, "</li>",
      ifelse(!is.na(estmax_val_l),paste0("<li><b>Max estimated value at the peak: </b>", "between ", 
                                         estmax_val_l, " and ", estmax_val_u, ifelse(!is.na(obsmax_val), paste0(" (observed ", obsmax_val, ")"), ""),"</li>"),""),
      "<li><b>Goodness of fit:</b> ", outputmod$R2[[1]], "</li>",
      "</ul>",
      collapse = ""
    )
    
  }
  
  return(out_string)
}


# Return current situa
return_current_situa <- function(da, TotPop){
  today <- max(da$data)
  dat_today <- da %>% dplyr::filter(data %in% c(today, today-1))
  
  attualmente_positivi_ultimo <- dat_today$`Current positives`[2]
  incr_attualmente_positivi <- paste_signpercent(dat_today$`Current positives`[2], dat_today$`Current positives`[1])
  
  deceduti_ultimo <- dat_today$Deceased[2]
  incr_deceduti <- paste_signpercent(dat_today$Deceased[2], dat_today$Deceased[1])
  
  dimessi_guariti_ultimo <- dat_today$`Discharged recovered`[2]
  incr_guariti <- paste_signpercent(dat_today$`Discharged recovered`[2], dat_today$`Discharged recovered`[1])
  
  totale_casi_ultimo <- dat_today$`Cumulative positives`[2]
  incr_totale_casi <- paste_signpercent(dat_today$`Cumulative positives`[2], dat_today$`Cumulative positives`[1])
  
  ricov_sintomi_ultimo <- dat_today$`Hospitalized with symptoms`[2]
  incr_ricoverati <- paste_signpercent(dat_today$`Hospitalized with symptoms`[2], dat_today$`Hospitalized with symptoms`[1])
  
  isol_domic_ultimo <- dat_today$`Home isolation`[2]
  incr_isolamento <- paste_signpercent(dat_today$`Home isolation`[2], dat_today$`Home isolation`[1])
  
  terapia_intens_ultimo <- dat_today$`Intensive care`[2]
  incr_terapia <- paste_signpercent(dat_today$`Intensive care`[2], dat_today$`Intensive care`[1])
  
  totale_ricoverati_ultimo <- dat_today$`Current hospitalized`[2]
  incr_currhosp <- paste_signpercent(dat_today$`Current hospitalized`[2], dat_today$`Current hospitalized`[1])
  
  swabs_ultimo <- dat_today$Swabs[2]
  incr_swabs <- paste_signpercent(dat_today$Swabs[2], dat_today$Swabs[1])
  
  new_pos_ultimo <- dat_today$`New positives`[2]
  incr_newpos <- paste_signpercent(dat_today$`New positives`[2], dat_today$`New positives`[1])
  
  
  quoz_let_ita <- round(dat_today$Deceased[2]/dat_today$`Cumulative positives`[2],4)*100
  
  tasso_mort_ita <- round(dat_today$Deceased[2]/TotPop,4)*100
  
  list_out <- list("Cumulative positives" = c(totale_casi_ultimo, incr_totale_casi),
                   "Current positives" = c(attualmente_positivi_ultimo, incr_attualmente_positivi),
                   "Current hospitalized" = c(totale_ricoverati_ultimo, incr_currhosp),
                   "Intensive care" = c(terapia_intens_ultimo, incr_terapia),
                   "Hospitalized with symptoms" = c(ricov_sintomi_ultimo, incr_ricoverati), 
                   "Home isolation" = c(isol_domic_ultimo, incr_isolamento),
                   "New positives" = c(new_pos_ultimo, incr_newpos),
                   "Discharged recovered" = c(dimessi_guariti_ultimo, incr_guariti),
                   "Deceased" = c(deceduti_ultimo, incr_deceduti),
                   "Swabs" = c(swabs_ultimo, incr_swabs),
                   "Letalita" = quoz_let_ita, 
                   "Mortalita" = tasso_mort_ita)
  return(
   list_out
  )
}


# Show raw data
show_raw_data <- function(daita, dareg){
  Dt_out_raw <- daita %>% 
    rename(Date = data, Country = stato) %>% 
    mutate(Country = "Italy",
           `New deaths` = c(Deceased %>% first, diff(Deceased))) %>% 
    dplyr::select(Date, Country, `Cumulative positives`, `Current positives`, `New positives`, 
                  `Current hospitalized`, `Deceased`, `New deaths`, `Hospitalized with symptoms`, 
                  `Intensive care`, `Home isolation`, `Discharged recovered`, Swabs) %>% 
    bind_rows(dareg %>% 
                spread(Key, Value) %>% 
                rename(Date = data, Country = denominazione_regione) %>% 
                group_by(Country) %>% 
                mutate(`New deaths` = c(Deceased %>% first, diff(Deceased))) %>% 
                ungroup() %>% 
                dplyr::select(Date, Country, `Cumulative positives`, `Current positives`, `New positives`, 
                              `Current hospitalized`, `Deceased`, `New deaths`, `Hospitalized with symptoms`, 
                              `Intensive care`, `Home isolation`, `Discharged recovered`, Swabs)
              
    ) 
  
  
  Dt_out_raw$Country <- factor(Dt_out_raw$Country,
                               levels = c("Italy", "Lombardia", "Liguria", "Piemonte", "Valle d'Aosta",
                                          "Emilia-Romagna", "Friuli Venezia Giulia", "Veneto", "Trentino-Alto Adige",
                                          "Lazio", "Marche", "Toscana", "Umbria",
                                          "Abruzzo", "Basilicata", "Calabria",
                                          "Campania", "Molise", "Puglia","Sardegna", "Sicilia"))
  
  Dt_out_raw %<>% arrange(desc(Date), Country) 
  
  return(Dt_out_raw)
}