# need this to run the plotly chart
library(plotly)

fluidPage(id = 'pageBackground', 
          tags$style('#pageBackground {background-color: #D9D9D9;}'), # set dashboard background
  
  
  # Application title
  titlePanel(h1(id = "title", strong("ACCIDENTIAL DRUG RELATED DEATHS DASHBOARD"), # dashboard name
                tags$style(HTML("#title{font-size: 48px; background-color:#7B7B7B; color:white;}")))), # the html for the header
                # setting the size and background color and text color
  
  
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel( id = 'sidebar' , tags$style('#sidebar{background-color:white;}'),
      selectInput("sexParameter", 
                  label = h3("Filter Data Based on Sex"),
                  choices = list("Male" = "Male", "Female" = "Female", "All" = "All"), 
                  selected = "All"),
      selectInput("raceParameter", 
                  label = h3("Filter Data Based on Race"),
                  choices = list("Asian Indian" = "Asian Indian", "Asian, Other" = "Asian, Other", 
                                 "White" = "White", "Black" = "Black", "Hispanic, White" = "Hispanic, White", 
                                 "Hispanic, Black" = "Hispanic, Black", "Other" = "Other", "Chinese" = "Chinese",
                                 "Native American, Other" = "Native American, Other", "Hawaiian" = "Hawaiian", 
                                 "All" = "All"), 
                  selected = "All"),
    width = 2), # the width of the sidebar
    

    
    
    # where the plots go
    mainPanel(
      fluidRow( # allows me to put the charts side by side
        splitLayout(cellWidths = c("40%", "40%", "40%"), plotlyOutput("genderbar"), plotOutput("OpioidBar", width = "100%"), plotOutput("mannerBar"))
      ),
      br(), # for putting space between the plots
      br(),
      fluidRow(
        splitLayout(cellWidths = c("60%", "60%"), plotOutput("countryBar"),  plotOutput("locationBar"))
      ),
      br(),
      br(),
      fluidRow(
        splitLayout(cellWidths = c("70%", "40%"), plotOutput("injuryBar", height = 900, width = 800), plotOutput("ageBar", width = 500))
      ),
    )
  )
)