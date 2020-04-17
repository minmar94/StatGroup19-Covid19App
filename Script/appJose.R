#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("_growthGLM.r")
source("download_data.R")



# Define UI for application that draws a histogram
ui <- pageWithSidebar(
  
  # Application title
  headerPanel("StatGroup-19-SP"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  
  #, "Acumulated (log-scale)"="logs"

  sidebarPanel(
    radioButtons("plotacum", "Employed data:",
                 c("Acumulated" = "acum",
                   "Daily changes" = "daily")),
    br(),    
    selectInput("count", "Database:",
                 list("Spain" = "spain","Belgium"="belgica","Italy" = "italy","Portugal"="portugal","USA"="USA","Other countries (ECDC)"="ECDC","Other countries (Johns Hopkins)"="hopkings")),
    selectizeInput("region", "Country/region:", 
                choices = spain$regiones,selected ="Galicia"),
    br(),
    selectInput("varused", "Variable:", 
                choices = spain$variablesen),
    br(),
    checkboxInput("lockdown", "Add date of the political measures"),
    conditionalPanel(
      condition = "input.lockdown == true",
      selectizeInput('lockdown2', 'Political measure(s):', choices=unique(df$category),selected ="Lockdown",multiple=T)
    ), 
    conditionalPanel(
      condition = "input.lockdown == true",
      checkboxInput('lockdown3', 'Add info of the measure(s)')
    ), 
    br(),
    checkboxInput("compare", "Compare with other region(s) on the graph"),
    conditionalPanel(
      condition = "input.compare == true",
      selectizeInput('count2', 'Database/s to be compared:', choices=list("Spain" = "spain","Belgium"="belgica","Italy" = "italy","Portugal"="portugal","USA"="USA","Other countries (ECDC)"="ECDC","Other countries (Johns Hopkins)"="hopkings"),selected ="belgica",multiple=T)
    ), 
    br(),
    conditionalPanel(
      condition = "input.compare == true",
      selectizeInput(paste("region",2,sep=""), "Regions 1 to be compared:",
                     choices = spain$regiones,multiple=T)
    ),
    conditionalPanel(
      condition = "input.compare == true",
      textInput('daysd1', 'Days of delay in database 1', "0")
    ),
    br(),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>1",
      selectizeInput(paste("region",3,sep=""), "Regions 2 to be compared:",
                     choices = spain$regiones,multiple=T)
    ),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>1",
      textInput('daysd2', 'Days of delay in database 2', "0")
    ),
    br(),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>2",
      selectizeInput(paste("region",4,sep=""), "Regions 3 to be compared:",
                     choices = spain$regiones,multiple=T)
    ),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>2",
      textInput('daysd3', 'Days of delay in database 3', "0")
    ),
    br(),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>3",
      selectizeInput(paste("region",5,sep=""), "Regions 4 to be compared:",
                     choices = spain$regiones,multiple=T)
    ),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>3",
      textInput('daysd4', 'Days of delay in database 4', "0")
    ),
    br(),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>4",
      selectizeInput(paste("region",6,sep=""), "Regions 5 to be compared:",
                     choices = spain$regiones,multiple=T)
    ),
    conditionalPanel(
      condition = "input.compare == true && input.count2.length>4",
      textInput('daysd5', 'Days of delay in database 5', "0"),
      br(),
      conditionalPanel(
        condition = "input.compare == true && input.count2.length>5",
        selectizeInput(paste("region",7,sep=""), "Regions 6 to be compared:",
                       choices = spain$regiones,multiple=T)
      ),
      conditionalPanel(
        condition = "input.compare == true && input.count2.length>5",
        textInput('daysd6', 'Days of delay in database 6', "0")
      ),
      br(),
      conditionalPanel(
        condition = "input.compare == true && input.count2.length>6",
        selectizeInput(paste("region",8,sep=""), "Regions 7 to be compared:",
                       choices = spain$regiones,multiple=T)
      ),
      conditionalPanel(
        condition = "input.compare == true && input.count2.length>6",
        textInput('daysd7', 'Days of delay in database 7', "0")
      )
    )
  ),
 
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot",
                                  dblclick = "plot1_dblclick",
                                  brush = brushOpts(
                                    id = "plot1_brush",
                                    resetOnNew = TRUE)),
               verbatimTextOutput("infold"),
               htmlOutput("infoplot")
               ), 
      tabPanel("Predictions", DT::dataTableOutput("table"),
               plotOutput('plot_pred1')
               ),
      tabPanel("Summary", htmlOutput("summary")), 
      tabPanel("Info", uiOutput("info"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  observe({
  countryi <- switch(input$count,spain=spaini,belgica=belgicai,italy=italyi,portugal=portugali,USA=USAi,ECDC=ECDCi,hopkings=hopkingsi)
  country <- switch(input$count,spain=spain,belgica=belgica,italy=italy,portugal=portugal,USA=USA,ECDC=ECDC,hopkings=hopkings)

  maxx=10^(100)
  maxy=10^(100)
  selectp=NULL

  updateSelectizeInput(session, "region",
                    label = "Country/Region:",
                    choices = countryi$regiones,
                    selected = countryi$regiones[1]
  )

  
  if(input$count=="USA"){
    updateSelectizeInput(session, "region",
                         label = "Country/Region:",
                         choices = countryi$regiones,
                         selected = "New York City, New York, US"
    )
  }
  


  countryi2=list()
  country2=list()
  
  if(length(input$count2)>0){
  for(j in 1:length(input$count2)){
    
    countryi2[[j]]=switch(input$count2[j],spain=spaini,belgica=belgicai,italy=italyi,portugal=portugali,USA=USAi,ECDC=ECDCi,hopkings=hopkingsi)
    country2[[j]]=switch(input$count2[j],spain=spain,belgica=belgica,italy=italy,portugal=portugal,USA=USA,ECDC=ECDC,hopkings=hopkings)
      
  updateSelectizeInput(session, paste("region",j+1,sep=""),
                    label = paste("Regions",j,"to be compared:"),
                    choices = countryi2[[j]]$regiones,
                    selected = countryi2[[j]]$regiones[1]
  )
  
  if(input$count2[j]=="USA"){
    updateSelectizeInput(session, paste("region",j+1,sep=""),
                         label = paste("Regions",j,"to be compared:"),
                         choices = countryi2[[j]]$regiones,
                         selected = "New York City, New York, US"
    )
  }
   
  }
  }

  updateSelectInput(session, "varused",
                    label = "Variable:",
                    choices = countryi$variablesen,
                    selected = countryi$variablesen[1]
  )
  
  
  observe({
    #region <- input$region
    varused <- countryi$variables[which(input$varused==countryi$variablesen)]
    dataused<-subset(country, country$region %in% input$region)
    dataused<-dataused %>% dplyr::select(!!!varused)
    dataused<-as.list(dataused)
    #country[country$region==region,]$varused
    np2 <<- try(growthGLM2(dataused[[1]],timax=NA,family="Poisson",maxiter=1e2,runs=20,monotone=TRUE,weight=1)
)
    if(class(np2)=="try-error"){
      
    }
    if(class(np2)!="try-error"){
      maxx<<-dataused[[1]][length(dataused[[1]])]+90
      maxy<<-exp(np2$pars[1])+exp(np2$pars[2])
    }
  })
  
  
  ranges <- reactiveValues(x = c(0,maxx), y =  c(0,maxy))
  pointclick<- reactiveValues(x = -100, y =  -100)

  
  observeEvent(input$plot1_dblclick, {
    #print(input$plot1_dblclick)
    doublec <- input$plot1_dblclick
    pointclick$x <- doublec$x
    pointclick$y <- doublec$y
    brush <- input$plot1_brush
    if(doublec$x<5){
      pointclick$x <- -1000
    }
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      pointclick$x <- -1000
      
    } else {
      ranges$x <- c(0,maxx)
      ranges$y <- c(0,maxy)
    }
  })
  

  
  
  output$plot <- renderPlot({
    
    tryCatch({
    varused <- countryi$variables[which(input$varused==countryi$variablesen)]
    datausedb<-subset(country, country$region %in% input$region)
    dataused<-datausedb %>% dplyr::select(!!!varused)
    if(class(np2)!="try-error"){
      bottom=exp(np2$pars[1])
      top=exp(np2$pars[2])+bottom
      future=list()
      richpred=Richards(1:(length(varused)+90),np2$pars)
      future$x=which(richpred<trunc(top))
      future$y=richpred[future$x]
      datos2=c(0,dataused[[1]])
      pdatos=which(datos2==0)
      pdatos=pdatos[length(pdatos)]
      xlimb<-max(c(ranges$x[1],pdatos-4))
      dbc=(input$plot1_dblclick$x)
      if(length(dbc)>0){
      if(dbc<xlimb){
        pointclick$x <- -1000
      }}
      if(input$plotacum=="acum"){
      xlimt<-min(max(c(future$x,length(dataused[[1]])),na.rm=T),ranges$x[2])
      ylimt<-min(max(future$y),ranges$y[2])
      plot(future$x,future$y,type="l",xlim = c(xlimb,xlimt),ylim=c(ranges$y[1],ylimt),xlab="Days",xaxt="n",ylab=paste(as.character(input$varused)," (",as.character(varused),")",sep=""),main=paste("Model for the cumulative number of cases in",as.character(input$region)))
      points(1:length(dataused[[1]]),dataused[[1]],pch=19)
      }
      if(input$plotacum=="daily"){
        xlimt<-min(max(c(future$x,length(dataused[[1]])),na.rm=T),ranges$x[2])
        ylimt<-min(max(c(diff(future$y),diff(dataused[[1]])),na.rm=T),ranges$y[2])
        plot(2:length(dataused[[1]]),diff(dataused[[1]]),xlim = c(xlimb,xlimt),ylim=c(ranges$y[1],ylimt),xlab="Days",xaxt="n",ylab=paste(as.character(input$varused)," (",as.character(varused),")",sep=""),main=paste("Model for the daily changes in",as.character(input$region)),bg="black",pch=21)
        lines(2:length(dataused[[1]]),diff(dataused[[1]]),col="grey")
        lines(future$x[2:length(future$x)],diff(future$y))
      }
      dateus=datausedb %>% dplyr::select(!!!"date")
      dateus=as.character(dateus[[1]])
      labs1<-as.Date(dateus[1], "%Y-%m-%d")
      labs2<-as.Date("2020-12-31", "%Y-%m-%d")
      labs3=seq(labs1,labs2,by=1)
      labs=labs3[seq(1,max(future$x),by=5)]
      labs<-format(labs, format="%m-%d")
      axis(1,at=seq(1,max(future$x),by=5),labels=labs,las=2,cex.axis=0.8)
      #text(cex=0.7, x=seq(1,max(future$x),by=5)-.25, y=-12-0.25, labs, xpd=TRUE, srt=45)
      if(input$lockdown==T){
        database1=firstup(input$count)
        if(input$count=="belgica"){database1="Belgium"}
        if(input$count=="USA"){database1="United States of America"}
        if(input$count=="hopkings"){
        database1=input$region
        database1=substr(database1,1,nchar(database1)-1)
        }
        if(input$count=="ECDC"){database1=df$country[which(df$iso3c==ECDC[ECDC$regiones==input$region,]$countryterritoryCode[1])][1]}
        if(input$lockdown3==T){
          info1ld<<-character()
        }
        for(ldj in 1:length(input$lockdown2)){
        lockd=(as.data.frame(df[df$country==database1&df$category==input$lockdown2[ldj]&is.na(df$admin_level_name),]))$date_implemented
        if(input$lockdown3==T){
          info1ld<<-c(info1ld,apply(as.data.frame(df[df$country==database1&df$category==input$lockdown2[ldj]&is.na(df$admin_level_name),c(2,11,9)]),1,paste,collapse=" - "))
        }
        lockd=as.Date(lockd)
        lockd=which(labs3 %in% lockd)
        if(length(lockd)>0){
        for(ldi in 1:length(lockd)){
        lines(rep(lockd[ldi],2),c(0,10^10),lty=ldj+1)
        }}
        }
        legend("bottomright",input$lockdown2,lty=(1:length(input$lockdown2))+1)
      }
      
    }
    

    
    if(input$compare){
      countryi2=list()
      country2=list()
      regionu=numeric()
      coloresu=0
      
      if(length(input$count2)>0){
      
      for(j in 1:length(input$count2)){
        lreg=length(eval(parse(text=paste("input$region",j+1,sep=""))))
        daysdu=eval(parse(text=paste("input$daysd",j,sep="")))
        daysd<- as.numeric(unlist(strsplit(daysdu,",")))
        if(length(daysd)!=lreg){
      updateTextInput(session, paste("daysd",j,sep=""), value=rep(0,lreg))
      }}
      
      
      for(j in 1:length(input$count2)){
        
        countryi2[[j]]=switch(input$count2[j],spain=spaini,belgica=belgicai,italy=italyi,portugal=portugali,USA=USAi,ECDC=ECDCi,hopkings=hopkingsi)
        country2[[j]]=switch(input$count2[j],spain=spain,belgica=belgica,italy=italy,portugal=portugal,USA=USA,ECDC=ECDC,hopkings=hopkings)
        
      region2tot=eval(parse(text=paste("input$region",j+1,sep="")))
      coloresu=coloresu+length(region2tot)
      }
      colorestot=hcl.colors(coloresu)
      colorestot2=hcl.colors(coloresu,alpha=0.5)
      pointstot=rep((15:20),trunc(coloresu)/6+1)[1:coloresu]
      l=0
      for(j in 1:length(input$count2)){
        region2tot=eval(parse(text=paste("input$region",j+1,sep="")))
      for(k in 1:length(region2tot)){
      l=l+1  
      region2=region2tot[k]
      varused2 <- countryi2[[j]]$variables[which(input$varused==countryi2[[j]]$variablesen)]
      dataused2b<-subset(country2[[j]], country2[[j]]$region %in% region2)
      dataused2<-dataused2b %>% dplyr::select(!!!varused2)
      dataused2<-as.list(dataused2)
      dateus2=as.list(dataused2b %>% dplyr::select(!!!"date"))
      dateus2=dateus2[[1]]
      daysdu=eval(parse(text=paste("input$daysd",j,sep="")))
      daysd<- as.numeric(unlist(strsplit(daysdu,",")))
      daysd=daysd[k]
      inid=daysd+as.double(as.Date(substring(dateus, 1, 10)))-as.double(as.Date(substring(dateus2, 1, 10)))
      
      if(input$lockdown==T){
        database1=firstup(input$count2[j])
        if(input$count2[j]=="belgica"){database1="Belgium"}
        if(input$count2[j]=="USA"){database1="United States of America"}
        if(input$count2[j]=="hopkings"){
          database1=input$region
          database1=substr(database1,1,nchar(database1)-1)
        }
        if(input$count2[j]=="ECDC"){database1=df$country[which(df$iso3c==ECDC[ECDC$regiones==input$region,]$countryterritoryCode[1])][1]}
        for(ldj in 1:length(input$lockdown2)){
          lockd=(as.data.frame(df[df$country==database1&df$category==input$lockdown2[ldj]&is.na(df$admin_level_name),]))$date_implemented
          if(input$lockdown3==T){
            info1ld<<-c(info1ld,apply(as.data.frame(df[df$country==database1&df$category==input$lockdown2[ldj]&is.na(df$admin_level_name),c(2,11,9)]),1,paste,collapse=" - "))
          }
          lockd=as.Date(lockd)
          lockd=which(labs3 %in% lockd)-daysd
          if(length(lockd)>0){
            for(ldi in 1:length(lockd)){
              lines(rep(lockd[ldi],2),c(0,10^10),lty=ldj+1,col=colorestot2[l])
            }}
        }
      }
      
      if(input$plotacum=="acum"){
      if(inid>=0){
      points(1:(length(dataused2[[1]])-inid), dataused2[[1]][(inid+1):length(dataused2[[1]])],col=colorestot[l],pch=pointstot[l])
      lines(1:(length(dataused2[[1]])-inid), dataused2[[1]][(inid+1):length(dataused2[[1]])],col=colorestot2[l])
      }else{
        points((1-inid):(length(dataused2[[1]])-inid), dataused2[[1]][1:length(dataused2[[1]])],col=colorestot[l],pch=pointstot[l])
        lines((1-inid):(length(dataused2[[1]])-inid), dataused2[[1]][1:length(dataused2[[1]])],col=colorestot2[l])        
      }
      }
      if(input$plotacum=="daily"){
        if(inid>=0){
          points(2:(length(dataused2[[1]])-inid), diff(dataused2[[1]][(inid+1):length(dataused2[[1]])]),col=colorestot[l],pch=pointstot[l])
          lines(2:(length(dataused2[[1]])-inid), diff(dataused2[[1]][(inid+1):length(dataused2[[1]])]),col=colorestot2[l])
        }else{
          points((2-inid):(length(dataused2[[1]])-inid), diff(dataused2[[1]][1:length(dataused2[[1]])]),col=colorestot[l],pch=pointstot[l])
          lines((2-inid):(length(dataused2[[1]])-inid), diff(dataused2[[1]][1:length(dataused2[[1]])]),col=colorestot2[l])        
        }
      }
      regionu=c(regionu,region2)
      }}
      legend("topleft",c(input$region,regionu),pch=c(21,pointstot),pt.bg=c("black",rep(NA,length(colorestot))),col=c(1,colorestot))
    }

    dfdataused=as.data.frame(cbind(1:length(as.list(dataused)[[1]]),as.list(dataused)[[1]]))
    names(dfdataused)=c("x","y")
   # if(!is.null(selectp)){
  #  points(selectp$x,selectp$y,cex=2)}
  #  text(selectp$x-15,selectp$y, labels=paste("(",selectp$x,",",selectp$y,")",sep=""), cex= 1)
#    if(!is.null(input$plot1_click)){
#      dfdataused=as.data.frame(cbind(1:length(dataused[[1]]),dataused[[1]]))
#      names(dfdataused)=c("x","y")
#      selectp<-nearPoints(dfdataused, input$plot1_click,"x","y", addDist = F)
#    }
    
#    if(!is.null(selectp)){ 
#      print(selectp)
#      points(selectp,cex=2)
#      text(selectp$x-15,selectp$y, labels=paste("(",selectp$x,",",selectp$y,")",sep=""), cex= 1)
#      }
    }
    
    points(pointclick,cex=2)
    if(input$plotacum=="acum"){
      valsumi=max(c(0,max((as.list(dataused)[[1]]))*0.1),na.rm=T)}
    if(input$plotacum=="daily"){
      valsumi=max(c(0,max(diff(as.list(dataused)[[1]]))*0.1),na.rm=T)}
    text(pointclick$x-17,pointclick$y+valsumi, labels=paste("(",labs3[round(pointclick$x)],",",round(pointclick$y),")",sep=""), cex= 1)
    
    output$infold<-renderText({ 
      if(input$lockdown3==T){
        paste(info1ld, collapse="\n")
      }
    })
    
    }, error=function(e){})
  })
  

  
  output$plot_pred1 <- renderPlot({
    
    tryCatch({
    varused <- countryi$variables[which(input$varused==countryi$variablesen)]
    datausedb<-subset(country, country$region %in% input$region)
    dataused<-datausedb %>% dplyr::select(!!!varused)
    dataused<-dataused[[1]]
    dateus=datausedb %>% dplyr::select(!!!"date")
    dateus=dateus[[1]]
    labs1<-as.Date(dateus[1], "%Y-%m-%d")
    labs2<-as.Date("2020-12-31", "%Y-%m-%d")
    labs3=seq(labs1,labs2,by=1)
    datas=labs3[(length(dataused)-6):(length(dataused)+7)]
    #np2<-try(funnplrmod(dataused))
    bottom=exp(np2$pars[1])
    top=exp(np2$pars[2])+bottom
    valfut=Richards((length(dataused)-6):(length(dataused)+7),np2$pars)
    valfut=round(valfut)
    valpast=c(dataused[(length(dataused)-6):length(dataused)],rep(NA,7))
    
    if(input$plotacum=="acum"){ylab1=paste("Cumulative number of cases in",as.character(input$region))}
    if(input$plotacum=="daily"){
      ylab1=paste("Daily changes in",as.character(input$region))
      valfut=c(0,diff(valfut))
      valpast=c(0,diff(valpast))
    }
    
    estpred   <- cbind(valfut[5:7], valpast[5:7])
    colnames(estpred) <- c("Observed", "Estimated")
    rownames(estpred) <- datas[5:7]
    
    barplot(t(estpred), beside=TRUE, ylab=ylab1, ylim=c(0,1.1*max(valfut[5:10],valpast[5:7],max(apply(estpred,1,"max")*1.1))), col=c("darkblue","red"),xaxt="n",xlim=c(1,19))
    barplot(c(rep(NA,5),valfut[8:12]),add=T,xaxt="n",col="darkblue",space=c(1,0))
    axis(1,c(2,5,8,10.5,12.5,14.5,16.5,18.5),datas[5:12], las=2,cex.axis=0.7)
    text(c(2,5,8),apply(estpred,1,"max")*1.05,paste("Dif:",round(abs(valfut[5:7]-valpast[5:7]))))
    if(valfut[12]>valfut[8]){
      legend("topleft",c("Observed","Estimated"),col=c("red","darkblue"),pch=c(15,15))}
    if(valfut[8]>valfut[12]){
      legend("topright",c("Observed","Estimated"),col=c("red","darkblue"),pch=c(15,15))}
    #barplot(valfut[8:10]~datas[8:10],add=T,col="red")
    #box(bty="l")
    
    }, error=function(e){})
  })
  
  
  output$summary <- renderUI({
    
    tryCatch({
    varused <- countryi$variables[which(input$varused==countryi$variablesen)]
    datausedb<-subset(country, country$region %in% input$region)
    dataused<-datausedb %>% dplyr::select(!!!varused)
    dataused<-dataused[[1]]
    dateus=datausedb %>% dplyr::select(!!!"date")
    dateus=dateus[[1]]
    labs1<-as.Date(dateus[1], "%Y-%m-%d")
    labs2<-as.Date("2020-12-31", "%Y-%m-%d")
    labs3=seq(labs1,labs2,by=1)
    
    database1=firstup(input$count)
    if(input$count=="belgica"){database1="Belgium"}
    if(input$count=="ECDC"){database1="European Centre for Disease Prevention and Control"}
    if(input$count=="hopkings"){database1="Johns Hopkins University Center for Systems Science and Engineering"}
    if(input$count=="USA"){database1="Centers for Disease Control and Prevention, USA"}
    bottom=exp(np2$pars[1])
    top=exp(np2$pars[2])+bottom
    xmid=which.max(diff(Richards(1:10000,np2$pars)))
    str1 <- paste("Database:", database1)
    str2 <- paste("Country/Region:", input$region)
    str3 <- paste("Variable:", input$varused)
    str4 <- paste("Variable (original name):", countryi$variables[which(input$varused==countryi$variablesen)])
    str5 <- paste("Inflexion point (estimated peak of the curve):",labs3[round(xmid)]) 
    str6 <- paste("Top asymptote (maximum number of accumulated cases):",round(top)) 
    HTML(paste(str1, str2,str3, str4,str5, str6, sep = '<br/>'))
    
    }, error=function(e){})
  })

  
  output$table <- DT::renderDataTable({
    
    tryCatch({
    varused <- countryi$variables[which(input$varused==countryi$variablesen)]
    datausedb<-subset(country, country$region %in% input$region)
    dataused<-datausedb %>% dplyr::select(!!!varused)
    dataused<-dataused[[1]]
    dateus=datausedb %>% dplyr::select(!!!"date")
    dateus=dateus[[1]]
    labs1<-as.Date(dateus[1], "%Y-%m-%d")
    labs2<-as.Date("2020-12-31", "%Y-%m-%d")
    labs3=seq(labs1,labs2,by=1)
    datas=labs3[(length(dataused)-2):(length(dataused)+7)]
    #np2<-try(funnplrmod(dataused))
    bottom=exp(np2$pars[1])
    top=exp(np2$pars[2])+bottom
    if(input$plotacum=="acum"){
      valfut=Richards((length(dataused)-2):(length(dataused)+7),np2$pars)
    valfut=round(valfut)
    valpast=c(dataused[(length(dataused)-2):length(dataused)],rep(NA,7))
    datapred=data.frame(datas=datas,valpast=valpast,valfut=valfut)
    }
    if(input$plotacum=="daily"){
      valfut=Richards((length(dataused)-3):(length(dataused)+7),np2$pars)
      valfut=diff(round(valfut))
      valpast=c(dataused[(length(dataused)-3):length(dataused)],rep(NA,7))
      valpast=diff(valpast)
      datapred=data.frame(datas=datas,valpast=valpast,valfut=valfut)
    }
    names(datapred)=c("Date of notification","Observed","Estimated")
    datapred
    }, error=function(e){})
  },options = list(pageLength = 5, lengthChange = FALSE,sDom  = '<"top">lrt<"bottom">ip'),rownames=F)
  
  
  
  })
  
  
  url1 <- a("Available in GitHub.", href="https://github.com/jose-ameijeiras/StatGroup-19-SP/blob/master/Stat-Group-19-SP_metodo.pdf",target="_blank")
  url2 <- a("Available in the StatGroup-19 webpage.", href="https://statgroup-19.blogspot.com/2020/03/the-logistic-curve-and-peak-in-lombardy.html",target="_blank")
  url3 <- a("Available in the StatGroup-19 webpage.", href="https://statgroup-19.blogspot.com/2020/03/la-curca-logistica-e-il-picco-della.html",target="_blank")
  url4 <-a("Jose Ameijeiras-Alonso.", href="https://jose-ameijeiras.netlify.com/",target="_blank")
  url5<- a("StatGroup-19:", href="https://statgroup-19.blogspot.com/",target="_blank")
  url7<- a("Available in GitHub.", href="https://github.com/pcm-dpc/COVID-19",target="_blank")
  url6<- a("Available in GitHub.", href="https://github.com/datadista/datasets/tree/master/COVID%2019",target="_blank")
  url8<- a("Available in GitHub.", href="https://github.com/CSSEGISandData/COVID-19",target="_blank")
  url9<- a("Available in the ECDC webpage.", href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",target="_blank")
  url10<- a("Project MTM2016-76969-P", href="http://eio.usc.es/pub/innpar2d/index.php/es/inicio",target="_blank")
  url11<- a("Available in the Belgian institute for health webpage.", href="https://epistat.wiv-isp.be/covid/",target="_blank")
  url12<- a("Available in the Assessment Capacities Project webpage,", href="https://epistat.wiv-isp.be/covid/",target="_blank")
  url13<- a("tidy_covid19 repository.", href="https://github.com/joachim-gassen/tidy_covid19",target="_blank")
  
  output$info <- renderUI({
    tagList(br(),"Shiny app developed by",url4, br(),br(),"Modeling and application made by StatGroup-19 and StatGroup-19-SP groups.", br(),br(),url5, br(),"Fabio Divino (University of Molise)", br(),"Alessio Farcomeni (Tor Vergata University of Rome)", br(),"Giovanna Jona Lasinio (University of Rome La Sapienza)", br(),"Gianfranco Lovison (University of Palermo)", br(),"Antonello Maruotti (LUMSA University, Rome)",br(),br(),"StatGroup-19-SP group:", br(),"Jose Ameijeiras-Alonso (KU Leuven)", br(),"Rosa Crujeiras (Universidade de Santiago de Compostela)", br(),br(),"Info in English:", url3, br(),"Info in Spanish:", url1, br(),"Info in Italian:", url2, br(),br(),"Data from the Spanish reports:", url6, br(),"Data from the Italian reports:", url7,br(),"Data from the Belgian reports:", url11, br(),"Data of USA and the Johns Hopkins University Center for Systems Science and Engineering:", url8, br(),"Data of the European Centre for Disease Prevention and Control (ECDC):", url9,br(),"COVID19 Government Measures Dataset:",url12,"see also",url13, br(), br(),"Acknowledgements",br(),"Supported by the", url10,"from the Spanish State Research Agency (AEI) co-funded by the European Regional Development Fund (ERDF), the Competitive Reference Groups 2017-2020 (ED431C 2017/38) from the Xunta de Galicia through the ERDF, FWO research project G.0826.15N (Flemish Science Foundation) and GOA/12/014 project (Research Fund KU Leuven).")
  })
  
  
  output$infoplot <- renderUI({
    info1="-Double-click to see the value of a point in the graph."
    info4="-Double-click in the left part of the graph to remove the value of the point."
    info2="-Brush to select and area and double click to zoom."
    info3="-Double-click to zoom out."
    info5="-The date displayed corresponds to the date of the report."
    info6="-All the data, except the Portuguese, are updated with respect to the last available report."
    info7="-Some variables may not be available for some regions."
    info8="-Political measures are shown when applied at the national level."
    HTML(paste(info1,info4,info2,info3,info5,info6,info7,info8, sep = '<br/>'))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

