library(shiny)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
# library(shinyscreenshot)
library(ggplot2)
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
                             # tags$img(src='swooshgn2.png'),
                             # tags$img(src='gn_swoosh_shellfish3.png'),
                             tags$img(src='landing1.png', width = "100%", alt="NOAA branding, NOAA Fisheries Logo, University of New England logo, and a grower harvesting kelp"),
                             titlePanel(h1("Seaweed Nutrient Removal Calculator"), windowTitle = "Seaweed Nutrient Removal Calculator"),
                             helpText(br()),
                             
                             ### add text box with black border ### #5761C0  style = "border-style: solid; border-color: #C6E6F0#5EB6D9; background-color: #5EB6D9;",
                             div( style = "border-style: solid; border-radius: 5px; border-color: #0085CA; background-color: #0085CA;",
                                  p("This calculator predicts the amount of nitrogen and phosphorus farmed seaweed removesfrom the water when harvested, a key environmental benefit that seaweed aquaculture provides. This tool applies to kelp farms located within the Gulf of Maine, USA.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px;color: white"),
                                  p("To use the tool, please fill in information about your farm in sections 1-2 below.", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white"),
                                  p("To download a report, click on ",strong("Download PDF Report")," at the bottom", style="text-align:justify; padding-left:10px; padding-right:10px; font-size:18px; color: white")),
                             helpText(br()),
                             
                             ### 1 FARM PRACTICES ###
                             helpText(h3("1) Farm Practices")),
                             ## Name
                             textAreaInput("farmname", div(strong("Project Name:"), " Please enter the name of the oyster farm"),value = "", width="100%", rows=1, placeholder = NULL),
                             helpText(br()),
                             ## Ploidy
                             selectInput("ploidy", div(strong("Oyster Ploidy:")," Please select the ploidy of the oysters that were harvested", em("(will not affect calculation)")),c("Diploid", "Triploid", "Combination"), width="100%"),
                             helpText(br()),
                             
                             
                             
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
  
  output$mymap <- renderLeaflet({
    leaflet(height="50%") %>%
      # addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
      addTiles() %>%
      setView(lng = -70, lat = 40, zoom = 5) %>%
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
  
  table <- reactive({
    taval=1.42E-05
    tbval=2.60727827
    saval=0.00039042
    sbval=2.579747757
    if(input$seedonly==T){
      tdw=taval*(input$seedSizeOut)^tbval
      sdw=saval*(input$seedSizeOut)^sbval
      tNi=0.0770*tdw
      sNi=0.0019*sdw
      tPi=0.008345*tdw
      sPi=0.000438*sdw
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN=round((tNi*cnvrt*input$seedNum),1)
      sN=round((sNi*cnvrt*input$seedNum),1)
      tP=round((tPi*cnvrt*input$seedNum),1)
      sP=round((sPi*cnvrt*input$seedNum),1)
      df=data.frame(matrix(c(sN, tN, tN+sN), nrow=1, ncol=3))
      colnames(df)=c("Shell", "Tissue", "Total")
      df=rbind(df, list(Shell=sP, Tissue=tP, Total=sP+tP))
      df$Units=input$units
      row.names(df)=c("Nitrogen", "Phosphorus")
    }
    
    else{
      tdw1=taval*(input$sizeIn)^tbval
      sdw1=saval*(input$sizeIn)^sbval
      tdw2=taval*(input$sizeOut*25.4)^tbval
      sdw2=saval*(input$sizeOut*25.4)^sbval
      tNi1=0.0770*tdw1
      sNi1=0.0019*sdw1
      tPi1=0.008345*tdw1
      sPi1=0.000438*sdw1
      tNi2=0.0770*tdw2
      sNi2=0.0019*sdw2
      tPi2=0.008345*tdw2
      sPi2=0.000438*sdw2
      #convert grams N to lbs or kg
      cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
      tN1=round((tNi1*cnvrt*input$HNum),1)
      sN1=round((sNi1*cnvrt*input$HNum),1)
      tP1=round((tPi1*cnvrt*input$HNum),1)
      sP1=round((sPi1*cnvrt*input$HNum),1)
      tN2=round((tNi2*cnvrt*input$HNum),1)
      sN2=round((sNi2*cnvrt*input$HNum),1)
      tP2=round((tPi2*cnvrt*input$HNum),1)
      sP2=round((sPi2*cnvrt*input$HNum),1)
      sN=sN2-sN1
      tN=tN2-tN1
      sP=sP2-sP1
      tP=tP2-tP1
      df=data.frame(matrix(c(sN, tN, tN+sN), nrow=1, ncol=3))
      colnames(df)=c("Shell", "Tissue", "Total")
      df=rbind(df, list(Shell=sP, Tissue=tP, Total=sP+tP))
      df$Units=input$units
      row.names(df)=c("Nitrogen", "Phosphorus")
    }
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