
# New app that includes all states
# LGG June 2025
#--------------------------------------------------------------
# prelims and load data

#library(reshape2) #
#library(plyr) #
library (MASS)
library(RColorBrewer)#
library(shiny)#
library(directlabels)
library(shinyjs)
library(arrow)
library(tidyr)
library(ggplot2)
library(dplyr)

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

state <-    c("AK","AZ","CA","CO","ID","MT","NM","NV","OR","UT","WA","WY")
statelc <-  c("ak","az","ca","co","id","mt","nm","nv","or","ut","wa","wy")

datadir <- "zdata"

SNTLmeas <- c("Tavg","Acc_Precip","Snow_Depth","SWE", "Tsoil_8")

SNTLname <- c("Daily Average Temperature (F)","Accumulated Precipitation (in.)",
              "Snow Depth (in.)","Snow Water Equivalent (in.)","Soil Temperature at 8 in. (F)")

SNTLstaNM <- c("Bateman", "Chamita", "Elk.Cabin", "Frisco.Divide", "Gallegos.Peak",  "Garita.Peak", "Hopewell", "Lookout.Mountain",
               "Mcknight.Cabin",      "Navajo.Whiskey.Ck",   "North.Costilla",      "Palo",                "Quemazon",            "Red.River.Pass..2",  
              "Rice.Park",           "Rio.Santa.Barbara",   "San.Antonio.Sink",    "Santa.Fe",  "Senorita.Divide..2",  "Shuree",  "Sierra.Blanca",      
              "Signal.Peak",         "Silver.Creek.Divide", "Taos.Powderhorn",     "Taos.Pueblo",         "Tolby",               "Tres.Ritos",          
              "Vacas.Locas",  "Wesner.Springs")


translation <- schema(
  State = utf8(),
  Site = utf8(), 
  SiteNum=int64(),
  Date = date64(), # not the default
  Tavg = float64(),
  Snow_Depth = float64(),
  SWE=float64(),
  Tsoil_8=float64(),
  Acc_Precip=float64(),
  wateryear=int64(),
  waterdoy=float64(),
  juliand=float64(),
)

#-------------------------------------------------------------------------------------------
# shiny app

tit <- paste0("SNOTEL Yearly Comparisons - All States")

varlist <- SNTLmeas
narrowSidebar <- HTML('<style>.span4 {min-width: 700px; max-width: 700px; }</style>')
# Define UI
ui <- fluidPage(
  useShinyjs(),
  div(
    id="loading_page",h1("Loading...")
  ),
  hidden(
    div(
      id="main_content",
      
      
      # App title ----
      titlePanel(tit),
      
      fluidRow(
        column(12,
               tag("a", list(href = "https://nwcc-apps.sc.egov.usda.gov/imap/#version=169&elements=&networks=!&states=!&counties=!&hucs=&minElevation=&maxElevation=&elementSelectType=all&activeOnly=true&activeForecastPointsOnly=false&hucLabels=false&hucIdLabels=false&hucParameterLabels=false&stationLabels=&overlays=&hucOverlays=&basinOpacity=100&basinNoDataOpacity=100&basemapOpacity=100&maskOpacity=0&mode=data&openSections=dataElement,parameter,date,basin,elements,location,networks&controlsOpen=true&popup=&popupMulti=&popupBasin=&base=esriNgwm&displayType=station&basinType=6&dataElement=SNWD&depth=-8&parameter=OBS&frequency=DAILY&duration=I&customDuration=&dayPart=E&monthPart=E&forecastPubDay=1&forecastExceedance=50&useMixedPast=true&seqColor=1&divColor=3&scaleType=D&scaleMin=&scaleMax=&referencePeriodType=POR&referenceBegin=1981&referenceEnd=2010&minimumYears=20&hucAssociations=true&relativeDate=-1&lat=41.063&lon=-111.533&zoom=5.0",
                             "   NRCS SNOTEL Site Map", target="_blank"))
        )
      ),
      
      fluidRow(
        column(12, 
               p("This app reads NRCS SNOTEL data and is built in R Shiny."),
               p("A Water Year is October 1 (of the previous calendar year) through Sept 30."),
               p(" Questions about the app? Comments? Feedback? Send a message to Amy at farewelltospring.org or Leslie at lgermain@neptuneinc.org."),
               p(" Code for this app is open source. You can find it on ",
                 tag("a", list(href = "https://github.com/lesliegg1/SnowScraper",
                               "   this github repository.", target="_blank")))
        )
      ),
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(tags$head(narrowSidebar),
                     
                     selectizeInput('state', h4("State"), choices = state, selected = "NM"),
                     selectizeInput('site', h4("Site"), choices = ""),
                     selectInput("variable", h4("Parameter"), 
                                 #choices = varlist, selected = "Acc_Precip"),
                                 choices = varlist, selected = "Snow_Depth"),
                     checkboxGroupInput("hyears", "Highlight Years"),
                     width=2),
        
        # Main panel for displaying outputs ----
        mainPanel(
          fluidRow( 
            column(4, wellPanel(
              textInput("minx", label = h4("X Min, 0 = Oct. 1"), value = "0"),
              textInput("maxx", label = h4("X Max, 365 = Sep 30"), value = "280")
              #          textInput("maxx", label = h4("X Max, 365 = Sep 30"), value = "364")
            )),
            column(4, wellPanel(
              textInput("miny", label = h4("Y Min"), value = ""),
              textInput("maxy", label = h4("Y Max"), value = "")
            )),
            column(4, wellPanel(
              
              sliderInput("ma_value", label = h3("Moving Average (days)"), min = 1, 
                          max = 30, value = 1),
              h6("Recommended for smoothing temperature data")
            ))
          ),
          
          fluidRow(
            # Output: Main plot ----
            plotOutput(outputId = "Plot",  width = "100%")
          )
          
        )
        
      )
    )
    
    
  ))

# Define server logic
server <- function(input, output, session) {
  load_data()

  data <- reactive({
    
    open_dataset(file.path(datadir, input$state), schema=translation) %>% 
      na.exclude(Date) %>%
      na.exclude(SnowDepth) #%>%
      #collect

  })
  
  years <- reactive({
    unique(data()$wateryear)
  })
  
  observe({
    #site_select <- unique(data()$Site)[18]
    site_choices <- data() %>% pull(Site, as_vector=TRUE) %>% unique()
    site_select <- site_choices[18]
    if(is.na(site_select)) site_select <- site_choices[1]
    updateSelectizeInput(session, "site",
                      choices = site_choices,
                      selected = site_select
    )
  })
  
  sub_data <- reactive({
    newdf1 <- filter(data(), Site == input$site) 
    newdf <- newdf1 %>% 
      select(Date, Site, SiteNum, State, wateryear, waterdoy, juliand, var=all_of(input$variable)) %>%
      na.exclude(var) %>%
      collect
  })
  
  observe({
    updateCheckboxGroupInput(session, "hyears",
                             choices = unique(sub_data()$wateryear, selected=NULL)
    )
  })
  
  output$Plot <- renderPlot({
    var <- input$variable
    ma_value <- input$ma_value
    sitename <- input$site

    req(unique(sub_data()$Site)==input$site) # require site to have already been selected
    newdf <- sub_data() 
    years_site <- unique(newdf$wateryear)
    nyears_site <- length(years_site)
    
    maxday <- as.Date(max(newdf$juliand), origin=as.Date("1970-01-01"))
    
    ma <- function(x,b=5){stats::filter(x,rep(1/b,b), sides=1)}
    movav <- ma(newdf$var, ma_value)
    newdf$var <- as.numeric(movav)
    
    hyears <- input$hyears
    nh <- length(hyears)
    qq <-subset(newdf, wateryear %in% hyears)
    hyearsindex <- as.numeric(hyears)-as.numeric(as.character(years()[1]))
    colorrmp <- rev(c(colorRampPalette(brewer.pal(9,"Set1"))(nyears_site)))
    colorh <- colorrmp[hyearsindex]
    z <- match(var, SNTLmeas)
    yaxislab <- SNTLname[z]
    titlelab <- paste0(gsub("\\.", " ", sitename),", ", input$state,": Data through 00:00am on ",maxday)
    
    if(nh >= 1){
      p1 <- ggplot(data=newdf, aes(x=waterdoy, y=var, group=factor(wateryear), colour=factor(wateryear))) + 
        xlab("Day of Water Year") + ylab(yaxislab) + 
        ylim(c(as.numeric(input$miny),as.numeric(input$maxy))) + ggtitle(titlelab)
      p1  + geom_line(alpha=0.5,size=1.5) + scale_x_continuous(breaks = c(-1,28,58,89,120,151,182,212,243,273,304,335),
                                                               labels = c("Oct","Nov","Dec","Jan", "Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep"), 
                                                               limits=c(as.numeric(input$minx),as.numeric(input$maxx))) +
        scale_color_manual(values=colorrmp) +
        geom_line(data=qq, aes(x=as.numeric(waterdoy), y=var, group=factor(wateryear), colour=factor(wateryear)), size=3) +
        geom_dl(aes(label = factor(wateryear)), method = list(dl.combine("top.bumpup"), cex = 0.8)) +
        guides(colour = guide_legend(override.aes = list(alpha = 0.5))) 
      
    } else {
      p1 <- ggplot(data=newdf, aes(x=waterdoy, y=var, group=factor(wateryear), colour=factor(wateryear))) + 
        xlab("Day of Water Year") + ylab(yaxislab)+ 
        ylim(c(as.numeric(input$miny),as.numeric(input$maxy))) + ggtitle(titlelab)
      p1  + geom_line(alpha=0.5,size=1.5) + scale_x_continuous(breaks = c(-1,28,58,89,120,151,182,212,243,273,304,335),
                                                               labels = c("Oct","Nov","Dec","Jan", "Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep"), 
                                                               limits=c(as.numeric(input$minx),as.numeric(input$maxx))) +
        scale_color_manual(values=colorrmp) +
        geom_dl(aes(label = factor(wateryear)), method = list(dl.combine("top.bumpup"), cex = 0.8)) 
    }
  }, height = 600)
  
}

shinyApp(ui = ui, server = server)
