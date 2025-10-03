library(shiny)
# library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
# library(shinyscreenshot)
library(ggplot2)
library(mgcv)
library(formatR)
library(tinytex)
library(gh)
library(png)
library(gridExtra)
library(grid)
library(bslib)

ui <- fluidPage(style = 'margin-left: 10%; margin-right: 10%;',
                theme = bslib::bs_theme(bootswatch = "cerulean"),
                helpText(strong("Calculator Version:", style = "font-size:18px;")),
                textOutput("githubversion"),
                helpText(br()),
                
                mainPanel(
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Calculator", 
                             tags$img(src='landing1.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, University of New England logo, and a grower harvesting kelp"),
                             titlePanel(h1("Seaweed Nutrient Removal Calculator"), windowTitle = "Seaweed Nutrient Removal Calculator"),
                             helpText(br()),
                             
                             ### add text box with black border ### #5761C0  style = "border-style: solid; border-color: #C6E6F0#5EB6D9; background-color: #5EB6D9;",
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("This calculator predicts the amount of nitrogen farmed seaweed removes from the water when harvested, an important environmental benefit that seaweed aquaculture provides. This tool applies to kelp farms located within the Gulf of Maine, USA.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px;color: white"),
                                  p("To use the tool, please fill in information about your farm in sections 1-2 below.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white"),
                                  p("To download a report, click on ",strong("Download PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             helpText(br()),
                             
                             ### 1 FARM PRACTICES ###
                             helpText(h3("1) Farm Practices")),
                             ## Name
                             textAreaInput("farmname", div(strong("Project Name:"), " Please enter the name of the farm"),value = "", width="100%", rows=1, placeholder = NULL),
                             helpText(br()),
                             textAreaInput("projloc", div(strong("Location:"),"Please enter the name of the water body where the farm is located"), value = "", width ="100%", rows=1, placeholder = NULL),
                             helpText(h6("Farm Location: "),"Please scroll or pinch to zoom to the farm area, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then the errant marker", style = "font-size:18px;"),
                             leafletOutput("mymap", width="100%", height=400),
                             ## Location table
                             tableOutput('loctable'),
                             ## Species
                             selectInput("species", div(strong("Species:")," Please select the species of seaweed that was harvested"),c("Sugar kelp (Saccharina latissima)", "..."), width="100%"),
                             helpText(br()),
                             
                             ## Number
                             helpText(br()),
                             numericInput("Hlength", div(strong("Length of line harvested (ft):")," Please enter the total length in feet of line harvested at the selected size"), 0, min=0, max=NA, width="100%"),
                             helpText(br()),
                             # dateInput("Htime", div(strong("Harvest date (yyyy-mm-dd):")), min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
                             # dateRangeInput("Htime", div(strong("Period of harvest (yyyy-mm-dd):")), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
                             dateInput("HarvestDate", div(strong("Date of harvest (yyyy-mm-dd):")), value = NULL, min=Sys.Date()-(5*365), max=Sys.Date(), format = "yyyy-mm-dd", startview = "month", width = "100%"),
                             br(),
                             ## Units
                             radioButtons(
                               "units",
                               div(strong("Units:")," Select the units for nutrient removal"),
                               choices =c("Pounds (lbs)", "Kilograms (kg)"),
                               selected ="Pounds (lbs)",
                               inline = T,
                               width="100%"),
                             helpText(br()),
                             tableOutput("mytable")
                             
                    )
                  )
                )
)

server <- function(input, output, session) {
  session$onSessionEnded(function() { stopApp() })
  
  # Add github version to top of page
  output$githubversion <- renderText({
    releases <- gh("GET /repos/{owner}/{repo}/releases", 
                   owner = "RMORSEcode",
                   repo = "Seaweed-Calculator")
    releases[[1]][["name"]]
  })
  
  ## Load GAM models to estimate biomass and percent N at time (week)
  biomass_model <- readRDS("GAMpctN.rds")
  N_model <- readRDS("GAMbiomass.rds")
  
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery) %>%
      addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap", options = providerTileOptions(opacity = 0.6)) %>%
      # addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 0.4)) %>%
      # fitBounds(-70, 40, -65, 45) %>%
      setView(lng = -68.5, lat = 43, zoom = 7) %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        polygonOptions=FALSE,
        markerOptions = T,
        rectangleOptions =F,
        circleOptions = F,
        circleMarkerOptions = F,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    output$loctable <- renderTable(
      data.frame("Lon"=feature$geometry$coordinates[[1]],"Lat"=feature$geometry$coordinates[[2]]),
      striped = T,
      hover = F,
      bordered = T,
      spacing = c("s", "xs", "m", "l"),
      width = "auto",
      align = NULL,
      rownames = FALSE,
      colnames = TRUE,
      digits = 4,
      na = "NA",
      quoted = FALSE
    )
  })
  
  table <- reactive({
    # S. latissima N percent (g N /g dry weight)
    Npctlo=0.02
    Npcthi=0.04
    # g wet weight to g dry weight ratio 9:1
    dw2ww=0.1111111 #1/9
    ft2m=0.3048 #convert feet (input length) to meter
    
    # g WW / m of line (ESTIMATE NEEDS REVISION)
    # gWWperM=5000 # use gam predict(kg/m) here
    
    # GROWOUT_WEEK=week(input$HarvestDate)
    # weekin=data.frame(GROWOUT_WEEK)
    # gWWperM=(predict(biomass_model, weekin))*1000
    DOY=yday(input$HarvestDate)
    doy=data.frame(DOY)
    
    biomass.mod=(predict(biomass_model, doy))
    gWWperM=exp(biomass.mod)*1000 # model prediction kg/m -> g/m
    
    Ngam=predict(N_model, doy)/100
    Nmodel=Ngam * dw2ww * gWWperM * input$Hlength * ft2m
    Nlo=Npctlo*dw2ww*gWWperM*input$Hlength*ft2m
    Nhi=Npcthi*dw2ww*gWWperM*input$Hlength*ft2m
    
    #convert grams N to lbs or kg
    cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    tNlo=round((Nlo*cnvrt),1)
    tNhi=round((Nhi*cnvrt),1)
    tNmodel=round((Nmodel*cnvrt),1)
    df=data.frame(matrix(c(tNlo, tNhi, tNmodel), nrow=1, ncol=3))
    colnames(df)=c("Low estimate", "High estimate", "model")
    df$Units=input$units
    row.names(df)=c("Nitrogen Removed")
    df
  })
  
  output$mytable <-
    renderTable(
      table(),
      rownames = TRUE
    )
}

# Run the application
shinyApp(ui = ui, server = server)