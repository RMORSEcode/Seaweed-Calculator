# C:/Users/ryan.morse/Documents/GitHub/Seaweed-Calculator
# https://test-connect.fisheries.noaa.gov/content/852a0e82-8e29-48d0-b98d-efae8ef07d35

library(shiny)
# library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
# library(shinyscreenshot)
library(ggplot2)
library(mgcv)
library(lubridate)
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
                                  p("Seaweed can remove excess nitrogen from coastal waters, which is an important environmental benefit. This calculator predicts the amount of nitrogen removed at harvest by a seaweed farm. This tool was developed using data from the Gulf of Maine, USA, and can be applied to kelp farms located within this geographic region. As data from other species and locations becomes available, the tool can be updated and expanded in the future.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px;color: white"),
                                  p("To use the tool, please fill in information about an existing or proposed farm in sections 1-2 below.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white"),
                                  p("To download a report, click on ",strong("Download PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             helpText(br()),
                             
                             ### 1 FARM PRACTICES ###
                             helpText(h3("1) Farm Practices")),
                             ## Name
                             textAreaInput("farmname", div(strong("Project Name:"), " Please enter the name of the farm"),value = "", width="100%", rows=1, placeholder = NULL),
                             helpText(br()),
                             textAreaInput("projloc", div(strong("Location:"),"Please enter the name of the water body where the farm is located"), value = "", width ="100%", rows=1, placeholder = NULL),
                             helpText(h6("Farm Location: "),"Please scroll or pinch to zoom to the farm area, then click once on the marker pin and select the site to record the coordinates. To remove a marker, click on the trash icon and then click on the marker to be removed", style = "font-size:18px;"),
                             leafletOutput("mymap", width="100%", height=400),
                             ## Location table
                             tableOutput('loctable'),
                             helpText(br()),
                             ## Species
                             selectInput("species", div(strong("Species:")," This tool is currently only available for sugar kelp, additional species will be added as data become available. Please select the species of seaweed that was harvested."),c("Sugar kelp (Saccharina latissima)", "..."), width="100%"),
                             ## Number
                             helpText(br()),
                             numericInput("Hlength", div(strong("Length of line harvested (ft):")," Please enter the total length in feet of line harvested at the selected size"), 0, min=0, max=NA, width="100%"),
                             helpText(br()),
                             # dateInput("Htime", div(strong("Harvest date (yyyy-mm-dd):")), min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
                             # dateRangeInput("Htime", div(strong("Period of harvest (yyyy-mm-dd):")), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
                             dateInput("HarvestDate", div(strong("Date of harvest (yyyy-mm-dd):")), value =NULL , min=Sys.Date()-(5*365), max=Sys.Date(), format = "yyyy-mm-dd", startview = "year", width = "100%"),
                             # sliderInput(
                             #   inputId = "HarvestDate",
                             #   label = "Date (Month-Day)",
                             #   min = as.Date("2021-01-01"),
                             #   max = as.Date("2021-07-31"),
                             #   value = c(as.Date("2021-02-01")),
                             #   timeFormat = "%m/%d",
                             #   step=1
                             # ),br(),
                             ## Units
                             radioButtons(
                               "units",
                               div(strong("Units:")," Select the units for nitrogen removal"),
                               choices =c("Pounds (lbs)", "Kilograms (kg)"),
                               selected ="Pounds (lbs)",
                               inline = T,
                               width="100%"),
                             helpText(br()),
                             tableOutput("mytable"),
                             br(),
                             plotOutput("fertplot", width="100%"),
                             br(),
                             # # radioButtons("extension", "Save As:",
                             # #              choices = c("png", "svg"), inline = TRUE),
                             downloadButton(
                               outputId = "download",
                               label = "Download Infographic"
                             ),
                             downloadButton(
                               outputId = "downloader",
                               label = "Download PDF Report"
                             ),
                             
                    ),
                    tabPanel("Harvest Optimizer",
                             tags$img(src='landing1.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, University of New England logo, and a grower harvesting kelp"),
                             titlePanel(h1("Seaweed Nutrient Removal Calculator"), windowTitle = "Seaweed Nutrient Removal Calculator"),
                             helpText(br()),
                             ### add text box with black border ### "border-style: solid; border-color: gray; background-color: #838B8B;"
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("Published data indicates that nitrogen concentration in kelp varies seasonally. The Harvest optimizer predicts the amount of nitrogen removed from the Gulf of Maine, USA, based on harvest date.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("To use the tool, please enter a length of line to harvest and drag the date slider to see how the amount of nitrogen removed varies with time.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;")),
                             helpText(br()),
                             helpText(br()),
                             numericInput("Hlength2", div(strong("Length of line harvested (ft):")," Please enter the total length in feet of line harvested at the selected size"), 0, min=0, max=NA, width="100%"),
                             helpText(br()),
                             sliderInput(
                               inputId = "monthSlider",
                               label = "Date (Month-Day)",
                               min = as.Date("2021-01-01"),
                               max = as.Date("2021-07-31"),
                               value = c(as.Date("2021-02-01")),
                               timeFormat = "%m/%d",
                               step=5
                             ),
                             tableOutput("OptimizeTable"),
                             # fluidRow(
                             #   splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,300), 
                             #               plotOutput("OptiBiomassplot", width="80%"), 
                             #               plotOutput("OptiNplot", width="80%")
                             #   )
                             # )
                             plotOutput("OptiNplot", width="50%"),
                    ),
                    
                    tabPanel("About", 
                             tags$img(src='landing1.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, University of New England logo, and a grower harvesting kelp"),
                             titlePanel(h1("Seaweed Nutrient Removal Calculator"), windowTitle = "Seaweed Nutrient Removal Calculator"),
                             helpText(br()),
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("About this Tool:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:20px; color: white;"),
                                  p("The Seaweed Nutrient Removal Calculator can be used to generate nitrogen removal estimates for permit applications for new or expanding farms by estimating farmed kelp production at the prospective site, or to provide information on nitrogen removed at an existing kelp farm using actual harvest numbers. The grower provides information on:", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- The date of harvest"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p(strong("- The length of line harvested"), style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                                  p("- Farm location will be included as inputs for use in generating the report, but will not affect the calculation.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white;"),
                             ),
                             helpText(br()),
                             tags$p(
                               h4("Background"),
                               helpText(strong("Excess nutrients in coastal waters"), style = "font-size:18px;"),
                               p("Nitrogen (N) is an essential nutrient, but excess levels of N in coastal waters can lead to algal blooms, low oxygen concentrations, fish kills, and other detrimental effects. Like all primary producers, seaweed incorporates nutrients into its tissue as it grows. At harvest, these nutrients are permanently removed from the coastal environment, providing a benefit to water quality and coastal communities. Nitrogen is also an indicator of protein content in seaweed, which may contribute to product quality or nutritional capacity."
                               ),
                               tags$img(src='infographic.png', width = "100%", alt="This illustration shows a landscape in the background with agricultural fields, houses with lawns, and a river washing nutrients from those sources into an underwater scene in the foreground where the nitrogen is assimilated by plankton and seaweed."),
                               br(),
                               helpText(strong("The Seaweed Nutrient Removal Calculator"), style = "font-size:18px;"),
                               p("The calculator is a tool designed for seaweed farmers and resource managers to inform seaweed aquaculture permitting. Resource managers have expressed interest in easy-to-use tools that produce location and operation-appropriate values for the environmental benefits, or ecosystem services, seaweed farms provide. The calculator provides estimated values for nutrient removal in a format that aligns with the seaweed aquaculture permitting process."
                               ),
                               p("The nitrogen removal calculations are based on published data measuring seaweed biomass and the average nitrogen concentration in seaweed over a range of harvest dates. This tool estimates the weight and nitrogen concentration of the seaweed based on the day of year it is harvested under typical conditions on a farm in the Gulf of Maine. The weight estimates are based on non-linear generalized additive model (GAM) output of biomass regressed on day of the year. Similarly, the nitrogen concentration estiamte is based on GAM output of nitrogen concentration regressed on day of the year. Finally, the nitrogen concentration is multiplied by a dry-weight to wet-weight conversion factor,  the total kelp biomass, and the length of line harvested to obtain the amount of nitrogen removed."
                               ),
                               br(),
                               tags$img(src='schema.png', width = "100%", alt="Inforgraphic showing calculator development and data processing"),
                               
                               # p("We have synthesized available literature for eastern oyster farms across the Northeast region, from North Carolina to Maine, and applied methodology used by the Chesapeake Bay Program to calculate nutrient removal at harvest ",
                               #   tags$a(style="font-weight:bold", target="_blank", href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0310062",
                               #          "(Rose et al. 2024)."),
                               #   " Variability in oyster tissue and shell nutrient concentration was low, and an assessment of farm location, ploidy, and cultivation practice (with vs. without gear) suggested that a single average value could reasonably be applied across all farms."
                               # ),
                             ),
                             # h6(tags$a(target="_blank", href="https://doi.org/10.5281/zenodo.11966672",
                             #           "Access publicly available data used to create this tool >")
                             # ),
                             br(),
                             h4("Location of seaweed", em("(Saccharina latissima)"), "samples from aquaculture farm sites used to develop the calculator"
                             ),
                             br(),
                             leafletOutput("contmap", width="100%", height=400),
                             br(),
                             tags$p(
                               h4("Project Team"),
                               tags$a(target="_blank", href="https://sites.une.edu/byronlab/", "Carrie Byron,"),
                               tags$a(target="_blank", href="https://www.researchgate.net/profile/Gretchen-Grebe", "Gretchen Schott Grebe,"), #https://www.linkedin.com/in/gretchen-schott-grebe/
                               tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/renee-mercaldo-allen", "Renee Mercaldo-Allen"),
                               tags$a(target="_blank", href="https://www.linkedin.com/in/julie-m-rose/", "Julie Rose,"),
                               tags$a(target="_blank", href="https://www.fisheries.noaa.gov/contact/ryan-morse-phd","Ryan Morse"),
                             ),
                             div( style = "border-style: solid; border-radius: 10px; border-color: #0085CA; background-color: #0085CA;",
                                  p("Send questions or comments to:",style="text-align:center; padding-left:10px; padding-right:10px; font-size:16px; color: white"),
                                  p("ES.Tools@noaa.gov",style="text-align:center; padding-left:10px; padding-right:10px; font-size:16px; color: white"),
                             ),
                             tags$p(
                               h4("References:"),
                               p("Bolduc, W., Griffin, R.M. & Byron, C.J. Consumer willingness to pay for farmed seaweed with education on ecosystem services. J Appl Phycol 35, 911–919 (2023). https://doi.org/10.1007/s10811-023-02914-3"
                               ),
                               p("Grebe, G. S., Byron, C. J., Brady, D. C., St. Gelais, A. T., & Costa-Pierce, B. A. (2021). The effect of distal-end trimming on Saccharina latissima morphology, composition, and productivity. Journal of the World Aquaculture Society, 52(5), 1081–1098. https://doi.org/10.1111/jwas.12814"
                               ),
                               p("Grebe, G.S., Byron, C.J., Brady, D.C. et al. The nitrogen bioextraction potential of nearshore Saccharina latissima cultivation and harvest in the Western Gulf of Maine. J Appl Phycol 33, 1741–1757 (2021). https://doi.org/10.1007/s10811-021-02367-6"
                               ),
                               p("Rose, J. M., Morse, R., & Schillaci, C. (2024). Development and application of an online tool to quantify nitrogen removal associated with harvest of cultivated eastern oysters. PLOS ONE, 19(9), e0310062. https://doi.org/10.1371/JOURNAL.PONE.0310062"
                               ),
                               p("Schutt, E., Francolini, R., Price, N., Olson, Z., & Byron, C. J. (2023). Supporting ecosystem services of habitat and biodiversity in temperate seaweed (Saccharina spp.) farms. Marine Environmental Research, 191, 106162. https://doi.org/10.1016/J.MARENVRES.2023.106162"
                               ),
                               br(),
                               h4("Disclaimer:"),
                               p("This is a scientific product and is not an official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
                               ),
                             ),
                    ),
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
  N_model <- readRDS("GAMpctN.rds")
  biomass_model <- readRDS("GAMbiomass.rds")
  
  ## Lat/Lon data for farms ussed to develop tool
  stations=readxl::read_xlsx("Swd_Location_data.xlsx",sheet='final', range='A1:F5')
  
  # add  data contributor map to 'about' page
  output$contmap <- renderLeaflet({
    leaflet(height="100%") %>%
      addTiles() %>%
      setView(lng = -69.95, lat = 43.65, zoom = 9) %>%
      addMarkers(stations$Longitude, stations$Latitude, popup = stations$Waterbody_Name, label =stations$Site )
  })
  
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery) %>%
      addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap", options = providerTileOptions(opacity = 0.6)) %>%
      # addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(opacity = 0.4)) %>%
      # fitBounds(-70, 40, -65, 45) %>%
      # setView(lng = -68.5, lat = 43, zoom = 7) %>%
      setView(lng = -69.95, lat = 43.65, zoom = 9) %>%
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
    # S. latissima N content (g N /g dry weight)
    Npctlo=0.02
    Npcthi=0.04
    # g wet weight to g dry weight ratio 9:1
    dw2ww=0.083 #average from Gretchen 2018-2019  #previously estimate of 1/9 DW:WW
    ft2m=0.3048 #convert feet (input length) to meter
    # DOY=yday(input$HarvestDate)
    day_of_year_shifted=ifelse(month(input$HarvestDate) < 12,
                               yday(input$HarvestDate) + 365, # Shift Jan-Nov to after Dec
                               yday(input$HarvestDate)        # Dec remains as is
    )
    # doy=data.frame(DOY); colnames(doy)='day_of_year_shifted'# 0-365 for biomass model
    sdoy=data.frame(day_of_year_shifted) #December based start, +365 shifted after December for N model
    biomass.mod=(predict(biomass_model, sdoy, se=F))
    kgWWperM=exp(biomass.mod) # model prediction kg/m -> g/m
    Ngam=predict(N_model, sdoy, se=F)
    Nmodel=Ngam * dw2ww * kgWWperM * input$Hlength * ft2m * 1000 # kg to g
    Nlo=Npctlo*dw2ww*kgWWperM*input$Hlength*ft2m * 1000# kg to g
    Nhi=Npcthi*dw2ww*kgWWperM*input$Hlength*ft2m * 1000# kg to g
    
    #convert grams N to lbs or kg
    cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    tNlo=round((Nlo*cnvrt),1)
    tNhi=round((Nhi*cnvrt),1)
    tNmodel=round((Nmodel*cnvrt),1)
    Bio.model=kgWWperM * 1000 * input$Hlength * ft2m
    tBio=round((Bio.model*cnvrt),-1)
    # df=data.frame(matrix(c(tNlo, tNhi, tNmodel, Ngam*100,  kgWWperM, tBio), nrow=1, ncol=6))
    # colnames(df)=c("Lo", "Hi", "Estimate (unit)","N (%DW)", "WW biomass (kg/m)", "Harvested Biomass (unit)")
    # df=data.frame(matrix(c(tNmodel, tBio, Ngam*100,  kgWWperM), nrow=1, ncol=4))
    # colnames(df)=c("N Removal (unit)","Harvested Biomass (unit)","Seaweed N (%DW)", "WW biomass (kg/m)")
    df=data.frame(matrix(c(tNmodel, tBio), nrow=1, ncol=2))
    colnames(df)=c("Nitrogen Removed","Harvested Biomass")
    df$Units=input$units
    # row.names(df)=c("Nitrogen Removed:")
    df
  })
  
  output$mytable <-
    renderTable(
      table()#,
      # rownames = TRUE
    )
  
  # fertilplot <- function(){
  fertilplot <- reactive({
    dw2ww=0.083 #average from Gretchen 2018-2019  #previously estimate of 1/9 DW:WW
    ft2m=0.3048 #convert feet (input length) to meter
    day_of_year_shifted=ifelse(month(input$HarvestDate) < 12,
                               yday(input$HarvestDate) + 365, # Shift Jan-Nov to after Dec
                               yday(input$HarvestDate)        # Dec remains as is
    )
    sdoy1=data.frame(day_of_year_shifted) #December based start, +365 shifted after December for N model
    biomass.mod=(predict(biomass_model, sdoy1, se=F))
    kgWWperM=exp(biomass.mod) # model prediction kg/m -> g/m
    Ngam=predict(N_model, sdoy1, se=F)
    Nmodel=Ngam * dw2ww * kgWWperM * input$Hlength * ft2m * 1000 # kg to g
    #convert grams N to lbs
    cnvrt=0.00220462
    tNmodel=round((Nmodel*cnvrt),1)
    nBags=round((tNmodel/5),1)
    sqftlawns=round(tNmodel,0)*1000
    img1<-readPNG("kelp_infographic_cropped.PNG")
    #get size
    h<-dim(img1)[1]
    w<-dim(img1)[2]
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    #fill plot with image
    usr<-par("usr")
    F=rasterImage(img1, usr[1], usr[3], usr[2], usr[4])
    text(0,.90, "Nitrogen removal", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0,.80, "equal to:", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0,.70, nBags, cex=3, col='red', pos=4)
    text(0,.60, "50-lb bags", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0,.50, "of fertilizer*, or", cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0,.40, sqftlawns, cex=3, col='red', pos=4)
    text(0,.30, "square feet of",cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0,.20,"lawns fertilized**",cex=2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0.4,.15, "* Equivalency based on fertilizer", cex=1.2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0.4,.10, "with 10% nitrogen content", cex=1.2, col=rgb(.2,.2,.2,.7), pos=4)
    text(0.4,.05,"** Using 1-lb of N per 1000 sq. ft.", cex=1.2, col=rgb(.2,.2,.2,.7), pos=4)
    # text(0.0,0.0,"https://connect.fisheries.noaa.gov/ANRC/", cex=1.2, col='blue', pos=4)
    F
  })
  
  output$fertplot <-
    renderPlot({
      fertilplot()
    })

  infoplot <- reactive({
    dw2ww=0.083 #average from Gretchen 2018-2019  #previously estimate of 1/9 DW:WW
    ft2m=0.3048 #convert feet (input length) to meter
    day_of_year_shifted=ifelse(month(input$HarvestDate) < 12,
                               yday(input$HarvestDate) + 365, # Shift Jan-Nov to after Dec
                               yday(input$HarvestDate)        # Dec remains as is
    )
    sdoy1=data.frame(day_of_year_shifted) #December based start, +365 shifted after December for N model
    biomass.mod=(predict(biomass_model, sdoy1, se=F))
    kgWWperM=exp(biomass.mod) # model prediction kg/m -> g/m
    Ngam=predict(N_model, sdoy1, se=F)
    Nmodel=Ngam * dw2ww * kgWWperM * input$Hlength * ft2m * 1000 # kg to g
    #convert grams N to lbs
    cnvrt=0.00220462
    tNmodel=round((Nmodel*cnvrt),1)
    nBags=round((tNmodel/5),1)
    sqftlawns=round(tNmodel,0)*1000
    img2<-readPNG("kelp_infographic_cropped.PNG")
    #get size
    h<-dim(img2)[1]
    w<-dim(img2)[2]
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    QR1=png::readPNG("swd_qr1.png")
    #fill plot with image
    Z=rasterImage(img2, usr[1], usr[3], usr[2], usr[4])
    text(0,0.65, "Nitrogen removed =", cex=1.1, col='#003366', pos=4)
    text(0.15,.57, prettyNum(nBags, big.mark = ",", scientific = FALSE), cex=1.5, col='red')
    text(0,.48, "50-lb bags of fertilizer*", cex=1.1, col='#003366', pos=4)
    text(.35,.4, "Which is equal to:", cex=1.1, col='#003366', pos=4)
    text(0.5,0.35, prettyNum(sqftlawns, big.mark = ",", scientific = FALSE), cex=1.5, col='red')
    text(.35,.30, "sq. ft. of land fertilized**",cex=1.1, col='#003366', pos=4) 
    text(0.3,.15, "* Based on fertilizer with 10% nitrogen content", cex=0.7, col='#003366', pos=4)
    text(0.3,.1, "** Using 1-lb of nitrogen per 1000 sq. ft.", cex=0.7, col='#003366', pos=4)
    text(0.5,0.98,"https://connect.fisheries.noaa.gov/SNRC/", cex=0.75, col='black', pos=4)
    mtext(input$farmname, side=3, line=-1, outer=T, cex=ifelse(nchar(input$farmname)<40,1.3,1), font=2, col='black')
    rasterImage(QR1,0,0,0.2,0.2) #xleft,ybottom,xright,ytop
    Z
  })
  
  ## save infographic to file
  output$download <- downloadHandler(
    filename = paste0("Infographic_",Sys.Date(),".png"),
    content = function(file) {
      png(file, width = 1000,
          height = 1000,
          res = 200)
      #fertilplot()
      infoplot()
      dev.off()
    })
  
  OptTable <- reactive({
    # S. latissima N content (g N /g dry weight)
    dw2ww=0.083 #average from Gretchen 2018-2019  #previously estimate of 1/9 DW:WW
    ft2m=0.3048 #convert feet (input length) to meter
    day_of_year_shifted=ifelse(month(input$monthSlider) < 12,
                               yday(input$monthSlider) + 365, # Shift Jan-Nov to after Dec
                               yday(input$monthSlider)        # Dec remains as is
    )
    # doy=data.frame(DOY); colnames(doy)='day_of_year_shifted'# 0-365 for biomass model
    sdoy2=data.frame(day_of_year_shifted) #December based start, +365 shifted after December for N model
    biomass.mod=(predict(biomass_model, sdoy2, se=F))
    kgWWperM=exp(biomass.mod) # model prediction kg/m -> g/m
    Ngam=predict(N_model, sdoy2, se=F)
    Nmodel=Ngam * dw2ww * kgWWperM * input$Hlength2 * ft2m * 1000 # kg to g
    #convert grams N to lbs
    cnvrt=0.00220462 #ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    tNmodel=round((Nmodel*cnvrt),1)
    Bio.model=kgWWperM * 1000 * input$Hlength2 * ft2m
    tBio=round((Bio.model*cnvrt),-1)
    df1=data.frame(matrix(c(tNmodel, Ngam*100, tBio), nrow=1, ncol=3))
    colnames(df1)=c("N removal (lbs)","Model N %", "Harvest Biomass (lbs)")
    df1
  })
  
  output$OptimizeTable <-
    renderTable(
      OptTable(),
      rownames = TRUE
    )
  
  Nplot <- reactive({
    # S. latissima N content (g N /g dry weight)
    dw2ww=0.083 #average from Gretchen 2018-2019  #previously estimate of 1/9 DW:WW
    ft2m=0.3048 #convert feet (input length) to meter
    day_of_year_shifted=ifelse(month(input$monthSlider) < 12,
                               yday(input$monthSlider) + 365, # Shift Jan-Nov to after Dec
                               yday(input$monthSlider)        # Dec remains as is
    )
    # doy=data.frame(DOY); colnames(doy)='day_of_year_shifted'# 0-365 for biomass model
    sdoy2=data.frame(day_of_year_shifted) #December based start, +365 shifted after December for N model
    biomass.mod=(predict(biomass_model, sdoy2, se=F))
    kgWWperM=exp(biomass.mod) # model prediction kg/m -> g/m
    Ngam=predict(N_model, sdoy2, se=F)
    Nmodel=Ngam * dw2ww * kgWWperM * input$Hlength2 * ft2m * 1000 # kg to g
    #convert grams N to lbs
    cnvrt=0.00220462 #ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    tNmodel=round((Nmodel*cnvrt),1)
    Bio.model=kgWWperM * 1000 * input$Hlength2 * ft2m
    tBio=round((Bio.model*cnvrt),-1)
    # df=data.frame(matrix(c(tNmodel, Ngam*100, kgWWperM, tBio), nrow=1, ncol=4))
    # colnames(df)=c("Estimate (lbs)","N (%DW)", "WW biomass (kg/m)", "Harvested Biomass (lbs)")
    # df$var=input$monthSlider
    # P1=ggplot(df, aes(x=var, y=tNmodel))+
    #   geom_bar(stat="identity" , fill="firebrick", width = 0.65)+
    #   theme_minimal()+
    #   ylab("Pounds of N")+
    #   xlab("Date")+
    #   theme(axis.title.x = element_text(size = 16),
    #         axis.text.x = element_text(size = 14),
    #         axis.text.y = element_text(size = 14),
    #         axis.title.y = element_text(size = 16))
    # P1
    
    
    # df=data.frame(matrix(c(tNmodel), nrow=1, ncol=1)) ### if not using second axis ---> NB
    df=data.frame(matrix(c(tNmodel)/ 0.01, nrow=1, ncol=1)) ### note that N is being fit to second axis here ---> NB
    colnames(df)="Pounds"
    df$var="Nitrogen"
    df=rbind(df, list(Pounds=tBio, var="Biomass" ))
    # df=mutate(Pounds = ifelse(var == "Nitrogen", Pounds / 0.01, Variable)) 
    P1=ggplot(df, aes(x=var, y=Pounds))+
      geom_bar(stat="identity" , fill='firebrick4', width = 0.65)+
      theme_minimal()+
      scale_y_continuous(name="Biomass (lbs)", sec.axis = sec_axis( transform=~ . * 0.01, name = "Nitrogen (lbs)")) +
      ylab("Pounds")+
      xlab("Seaweed")+
      theme(axis.title.x = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16))
    P1
  })
  # Biomassplot <- reactive({
  #   # S. latissima N content (g N /g dry weight)
  #   dw2ww=0.083 #average from Gretchen 2018-2019  #previously estimate of 1/9 DW:WW
  #   ft2m=0.3048 #convert feet (input length) to meter
  #   day_of_year_shifted=ifelse(month(input$monthSlider) < 12,
  #                              yday(input$monthSlider) + 365, # Shift Jan-Nov to after Dec
  #                              yday(input$monthSlider)        # Dec remains as is
  #   )
  #   # doy=data.frame(DOY); colnames(doy)='day_of_year_shifted'# 0-365 for biomass model
  #   sdoy=data.frame(day_of_year_shifted) #December based start, +365 shifted after December for N model
  #   biomass.mod=(predict(biomass_model, sdoy, se=F))
  #   kgWWperM=exp(biomass.mod) # model prediction kg/m -> g/m
  #   Ngam=predict(N_model, sdoy, se=F)
  #   Nmodel=Ngam * dw2ww * kgWWperM * input$Hlength2 * ft2m * 1000 # kg to g
  #   #convert grams N to lbs
  #   cnvrt=0.00220462 #ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
  #   tNmodel=round((Nmodel*cnvrt),1)
  #   Bio.model=kgWWperM * 1000 * input$Hlength2 * ft2m
  #   tBio=round((Bio.model*cnvrt),-1)
  #   df=data.frame(matrix(c(tNmodel, Ngam*100, kgWWperM, tBio), nrow=1, ncol=4))
  #   colnames(df)=c("Estimate (lbs)","N (%DW)", "WW biomass (kg/m)", "Harvested Biomass (lbs)")
  #   df$var=input$monthSlider
  #   P2=ggplot(df, aes(x=var, y=tBio))+
  #     geom_bar(stat="identity" , fill="firebrick", width = 0.65)+
  #     theme_minimal()+
  #     ylab("Pounds of Seaweed")+
  #     xlab("Date")+
  #     theme(axis.title.x = element_text(size = 16),
  #           axis.text.x = element_text(size = 14),
  #           axis.text.y = element_text(size = 14),
  #           axis.title.y = element_text(size = 16))
  #   P2
  # })
  
  # output$OptiBiomassplot <- 
  #   renderPlot({
  #     Biomassplot()
  #   })
  # 
  output$OptiNplot <-
    renderPlot({
      Nplot()
    })
  
  
  output$downloader <- 
    downloadHandler(
      paste0(Sys.Date(),"_Seaweed_Farm_Nitrogen_Report.pdf"),
      content = 
        function(file)
        {
          rmarkdown::render(
            input = "reportout.Rmd",
            output_file = "built_report.pdf",
            params = list(
              Location=input$projloc,
              Species=input$species,
              Units=input$units, 
              Length=input$Hlength,
              Farm=input$farmname,
              Date=input$HarvestDate,
              table = table(),
              HLat=input$mymap_draw_new_feature$geometry$coordinates[[2]],
              HLon=input$mymap_draw_new_feature$geometry$coordinates[[1]]
            )
          ) 
          readBin(con = "built_report.pdf", 
                  what = "raw",
                  n = file.info("built_report.pdf")[, "size"]) %>%
            writeBin(con = file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)