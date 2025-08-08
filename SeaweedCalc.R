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
                             textAreaInput("farmname", div(strong("Project Name:"), " Please enter the name of the farm"),value = "", width="100%", rows=1, placeholder = NULL),
                             helpText(br()),
                             textAreaInput("projloc", div(strong("Location:"),"Please enter the name of the water body where the farm is located"), value = "", width ="100%", rows=1, placeholder = NULL),
                             leafletOutput("mymap", width="100%", height=400),
                             ## Location table
                             tableOutput('loctable'),
                             ## Species
                             selectInput("species", div(strong("Species:")," Please select the species of seaweed that was harvested"),c("Sugar kelp (Saccharina latissima)", "..."), width="100%"),
                             helpText(br()),
                             
                             ## Number
                             helpText(br()),
                             numericInput("Hlength", div(strong("Length of line harvested (m):")," Please enter the total length in meters of line harvested at the selected size"), 0, min=0, max=NA, width="100%"),
                             helpText(br()),
                             dateRangeInput("Htime", div(strong("Period of harvest (yyyy-mm-dd):"), em("(does not affect calculation)")), start=NULL, end=NULL, min=Sys.Date()-(5*365), max=Sys.Date(), startview = "month", width="100%"),
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
    Npctlo=1.04
    Npcthi=3.82
    # g wet weight to g dry weight ratio 9:1
    dw2ww=1/9
    # g WW / m of line (ESTIMATE NEEDS REVISION)
    gWWperM=500
    Nlo=Npctlo*dw2ww*gWWperM*input$Hlength
    Nhi=Npcthi*dw2ww*gWWperM*input$Hlength
    
    #convert grams N to lbs or kg
    cnvrt=ifelse(input$units=="Pounds (lbs)",0.00220462,0.001)
    tNlo=round((Nlo*cnvrt),1)
    tNhi=round((Nhi*cnvrt),1)
    
    df=data.frame(matrix(c(tNlo, tNhi), nrow=1, ncol=2))
    colnames(df)=c("Low estimate", "High estimate")
    df$Units=input$units
    row.names(df)=c("Nitrogen")
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