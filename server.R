a <- read.csv("Accidental_Drug_Related_Deaths_2012-2018.csv")
function(input, output) {
  # the required libraries
  library(ggplot2)
  library(plotly)
  library(dplyr)

  
  
  
  # PLOT 1
  # gender bar plot 
  inputPlot1 <- reactive({
    # get rid of unknown sex 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    # get rid of unknown race
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    # get rid of unknown age
    a <- a[!is.na(a$Age),]
    # get rid of unknown location 
    a <- a[!(a$Location == ""),]
    # get rid of unknown manner of deaths 
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    
    sexLabel <- "Males and Females"
    
    # get the percentage
    a %>% 
      count(Sex) %>% 
      mutate(perc = n / nrow(a)) -> a
    
    # the y and x label and the chart title
    y <- a$perc
    x<- a$Sex
    chartTitle <- paste("What Percentage of " , sexLabel , " Died ", sep = " ")
    
    
    ggplot(a, aes(x,y))+
      geom_bar(stat = "identity", width = 0.5, fill = "#C39BC8")+ # the fill of the bars and the width of them as well
      theme_classic()+
      ggtitle(chartTitle)+
      ylim(0,1)+ # the limits for the y axis
      theme_bw()+
      theme(
        plot.title = element_text( # plot title; setting the size and making it bold and moving it up alittle
          size = 10,
          face = "bold",
          vjust = 2,
        ),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9") # make the border outside the panel the same color as the website background
      )
  })
  output$genderbar <- renderPlotly({
    print(inputPlot1())

  })
  
  
  
  
  
  
  
  
  # PLOT 2
  # how many deaths involved opioids graph 
  output$OpioidBar <-renderPlot({
    # get rid of the people whos sex and race and age and location and manner of death is unknown this is done to keep the data among the plot consistent 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    a <- a[!is.na(a$Age),]
    a <- a[!(a$Location == ""),]
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    # make all the blank values in the column to unknowns 
    a$AnyOpioid[a$AnyOpioid == ""] <- "UnKnown"
    
    
    
    raceChoice <- input$raceParameter
    sexChoice <- input$sexParameter
    
    raceLabel <- ""
    
    
    # the race choice filter
    if(raceChoice == "Asian Indian"){
      a<- a[a$Race == "Asian Indian",]
      raceLabel <- "who were Asian Indian"
    }
    else if(raceChoice == "Asian, Other"){
      a<- a[a$Race == "Asian, Other",]
      raceLabel <- "who were Asian, Other"
    }
    else if(raceChoice == "White"){
      a<- a[a$Race == "White",]
      raceLabel <- "who were White"
    }
    else if(raceChoice == "Black"){
      a<- a[a$Race == "Black",]
      raceLabel <- "who were Black"
    }
    else if(raceChoice == "Hispanic, White"){
      a<- a[a$Race == "Hispanic, White",]
      raceLabel <- "who were Hispanic, White"
    }
    else if(raceChoice == "Hispanic, Black"){
      a<- a[a$Race == "Hispanic, Black",]
      raceLabel <- "who were Hispanic, Black"
    }
    else if(raceChoice == "Other"){
      a<- a[a$Race == "Other",]
      raceLabel <- "who were Other"
    }
    else if(raceChoice == "Chinese"){
      a<- a[a$Race == "Chinese",]
      raceLabel <- "who were Chinese"
    }
    else if(raceChoice == "Native American, Other"){
      a<- a[a$Race == "Native American, Other",]
      raceLabel <- "who were Native American, Other"
    }
    else if(raceChoice == "Hawaiian"){
      a<- a[a$Race == "Hawaiian",]
      raceLabel <- "who were Hawaiian"
    }
    
    
    sexLabel <- "Male and Female"
    
    # the sex choice filter 
    if(sexChoice == "Male"){
      a <- a[a$Sex != "Female",]
      sexLabel <- "Males"
    }else if(sexChoice == "Female"){
      a <- a[a$Sex != "Male",]
      sexLabel <- "Females"
    }
    
    chartTitle <- paste("Were Opioids Involved in the Deaths of\n", sexLabel,raceLabel, sep = " ")
    
    
    # use this to sort the bars in ascending order
    a <- a %>% group_by(AnyOpioid) %>%   mutate(count_name_occurr = n())
    
    ggplot(a, aes(reorder(AnyOpioid,count_name_occurr))) + 
      geom_bar(width = 0.6, fill = "#C39BC8")+
      ggtitle(chartTitle) + 
      geom_text(aes(label = ..count..), vjust = -0.3, stat = "count", size=4, fontface = "bold")+ # put the numbers on top of the bars 
      theme_bw()+ # this theme gives me a border around the charts 
      theme(
        panel.grid.major = element_blank(), # get rid of major grid lines
        panel.grid.minor = element_blank(), # get rid of minor grid lines
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(
          size = 15,
          hjust = 0.5,
          vjust = 2,
          face = "bold"
        ),
        axis.title.x = element_blank(),
        axis.text.x = element_text(
          size = 12
        ),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9") 
        
      )
    
    
  })
  
  
  
  
  
  
  
  # Plot 3
  # what country had the highest death 
  output$countryBar<- renderPlot({
    # get rid of the people whos sex and race and age  and location and manner of death is unknown this is done to keep the data among the plot consistent 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    a <- a[!is.na(a$Age),]
    a <- a[!(a$Location == ""),]
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    # set the blanks equal to unknown for death country 
    a$DeathCounty[a$DeathCounty == ""] <- "Unknown"
    
    raceChoice <- input$raceParameter
    sexChoice <- input$sexParameter
    
    raceLabel <- ""
    
    
    # the race choice filter
    if(raceChoice == "Asian Indian"){
      a<- a[a$Race == "Asian Indian",]
      raceLabel <- "who were Asian Indian"
    }
    else if(raceChoice == "Asian, Other"){
      a<- a[a$Race == "Asian, Other",]
      raceLabel <- "who were Asian, Other"
    }
    else if(raceChoice == "White"){
      a<- a[a$Race == "White",]
      raceLabel <- "who were White"
    }
    else if(raceChoice == "Black"){
      a<- a[a$Race == "Black",]
      raceLabel <- "who were Black"
    }
    else if(raceChoice == "Hispanic, White"){
      a<- a[a$Race == "Hispanic, White",]
      raceLabel <- "who were Hispanic, White"
    }
    else if(raceChoice == "Hispanic, Black"){
      a<- a[a$Race == "Hispanic, Black",]
      raceLabel <- "who were Hispanic, Black"
    }
    else if(raceChoice == "Other"){
      a<- a[a$Race == "Other",]
      raceLabel <- "who were Other"
    }
    else if(raceChoice == "Chinese"){
      a<- a[a$Race == "Chinese",]
      raceLabel <- "who were Chinese"
    }
    else if(raceChoice == "Native American, Other"){
      a<- a[a$Race == "Native American, Other",]
      raceLabel <- "who were Native American, Other"
    }
    else if(raceChoice == "Hawaiian"){
      a<- a[a$Race == "Hawaiian",]
      raceLabel <- "who were Hawaiian"
    }
    
    
    sexLabel <- "Male and Female"
    
    # the sex choice filter 
    if(sexChoice == "Male"){
      a <- a[a$Sex != "Female",]
      sexLabel <- "Males"
    }else if(sexChoice == "Female"){
      a <- a[a$Sex != "Male",]
      sexLabel <- "Females"
    }
    
    
    
    chartTitle <- paste("How Many", sexLabel,raceLabel, "Died Based on Country", sep = " ")
    
    # use this to sort the bars in ascending order
    a <- a %>% group_by(DeathCounty) %>%   mutate(count_name_occurr = n())
    
    ggplot(a, aes(reorder(DeathCounty,-count_name_occurr)))+ 
      geom_bar(fill = "#C39BC8")+ 
      coord_flip()+
      geom_text(aes(label = ..count..), hjust = -0.10, stat = "count", size=4, fontface = "bold")+
      ggtitle(chartTitle)+
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          size = 15,
          hjust = 0.5,
          vjust = 2, 
          face = "bold"
        ),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(
          size = 12
        ),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9")
      )
    
  })
  
  
  
  
  
  
  # PLOT 4
  # Which age group has the lowest deaths? (group by 10's)
  output$ageBar <- renderPlot({
    # get rid of the people whos sex and race and age and location and manner of death is unknown this is done to keep the data among the plot consistent 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    a <- a[!is.na(a$Age),]
    a <- a[!(a$Location == ""),]
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    raceChoice <- input$raceParameter
    sexChoice <- input$sexParameter
    
    raceLabel <- ""
    
    
    # the race choice filter
    if(raceChoice == "Asian Indian"){
      a<- a[a$Race == "Asian Indian",]
      raceLabel <- "who were Asian Indian"
    }
    else if(raceChoice == "Asian, Other"){
      a<- a[a$Race == "Asian, Other",]
      raceLabel <- "who were Asian, Other"
    }
    else if(raceChoice == "White"){
      a<- a[a$Race == "White",]
      raceLabel <- "who were White"
    }
    else if(raceChoice == "Black"){
      a<- a[a$Race == "Black",]
      raceLabel <- "who were Black"
    }
    else if(raceChoice == "Hispanic, White"){
      a<- a[a$Race == "Hispanic, White",]
      raceLabel <- "who were Hispanic, White"
    }
    else if(raceChoice == "Hispanic, Black"){
      a<- a[a$Race == "Hispanic, Black",]
      raceLabel <- "who were Hispanic, Black"
    }
    else if(raceChoice == "Other"){
      a<- a[a$Race == "Other",]
      raceLabel <- "who were Other"
    }
    else if(raceChoice == "Chinese"){
      a<- a[a$Race == "Chinese",]
      raceLabel <- "who were Chinese"
    }
    else if(raceChoice == "Native American, Other"){
      a<- a[a$Race == "Native American, Other",]
      raceLabel <- "who were Native American, Other"
    }
    else if(raceChoice == "Hawaiian"){
      a<- a[a$Race == "Hawaiian",]
      raceLabel <- "who were Hawaiian"
    }
    
    
    sexLabel <- "Male and Female"
    
    # the sex choice filter 
    if(sexChoice == "Male"){
      a <- a[a$Sex != "Female",]
      sexLabel <- "Males"
    }else if(sexChoice == "Female"){
      a <- a[a$Sex != "Male",]
      sexLabel <- "Females"
    }
    
    # create the grouping by doing binning 
    a$Age <- cut(a$Age, c(10,20,30,40,50,60,70,80,90))
    chartTitle <- paste(sexLabel,raceLabel,"Deaths Based on \nAge Range")
    
    # sort the data in ascending order
    a <- a %>% group_by(Age) %>%   mutate(count_name_occurr = n())
    
    
    ggplot(a, aes(reorder(Age,count_name_occurr)))+
      geom_bar(width = 0.7, fill = "#C39BC8") +
      geom_text(aes(label = ..count..), vjust = -0.30, stat = "count", size=4, fontface = "bold")+
      ggtitle(chartTitle)+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(
          size = 12
        ),
        plot.title = element_text(
          size = 15,
          hjust = 0.5,
          vjust = 2,
          face = "bold"
        ),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9")
      )
    
  })
  
  
  
  
  
  
  
  # PLOT 5
  # Which location had the second most deaths?
  output$locationBar <- renderPlot({
    # get rid of the people whos sex and race and age and location and manner of death is unknown this is done to keep the data among the plot consistent 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    a <- a[!is.na(a$Age),]
    a <- a[!(a$Location == ""),]
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    raceChoice <- input$raceParameter
    sexChoice <- input$sexParameter
    
    raceLabel <- ""
    
    
    # the race choice filter
    if(raceChoice == "Asian Indian"){
      a<- a[a$Race == "Asian Indian",]
      raceLabel <- "who were Asian Indian"
    }
    else if(raceChoice == "Asian, Other"){
      a<- a[a$Race == "Asian, Other",]
      raceLabel <- "who were Asian, Other"
    }
    else if(raceChoice == "White"){
      a<- a[a$Race == "White",]
      raceLabel <- "who were White"
    }
    else if(raceChoice == "Black"){
      a<- a[a$Race == "Black",]
      raceLabel <- "who were Black"
    }
    else if(raceChoice == "Hispanic, White"){
      a<- a[a$Race == "Hispanic, White",]
      raceLabel <- "who were Hispanic, White"
    }
    else if(raceChoice == "Hispanic, Black"){
      a<- a[a$Race == "Hispanic, Black",]
      raceLabel <- "who were Hispanic, Black"
    }
    else if(raceChoice == "Other"){
      a<- a[a$Race == "Other",]
      raceLabel <- "who were Other"
    }
    else if(raceChoice == "Chinese"){
      a<- a[a$Race == "Chinese",]
      raceLabel <- "who were Chinese"
    }
    else if(raceChoice == "Native American, Other"){
      a<- a[a$Race == "Native American, Other",]
      raceLabel <- "who were Native American, Other"
    }
    else if(raceChoice == "Hawaiian"){
      a<- a[a$Race == "Hawaiian",]
      raceLabel <- "who were Hawaiian"
    }
    
    
    sexLabel <- "Male and Female"
    
    # the sex choice filter 
    if(sexChoice == "Male"){
      a <- a[a$Sex != "Female",]
      sexLabel <- "Males"
    }else if(sexChoice == "Female"){
      a <- a[a$Sex != "Male",]
      sexLabel <- "Females"
    }
    
    
    chartTitle <- paste("Where Did",sexLabel,raceLabel,"Deaths Occur",sep=" ")
    
    a <- a %>% group_by(Location) %>%   mutate(count_name_occurr = n())
    
    ggplot(a, aes(reorder(Location,-count_name_occurr ))) + 
      geom_bar(fill = "#C39BC8") + 
      coord_flip() +
      ggtitle(chartTitle)+
      geom_text(aes(label = ..count..), hjust = -0.1, stat = "count", size=4, fontface = "bold")+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(
          face = "bold",
          size = 15,
          hjust = 0.5,
          vjust = 2
        ),
        axis.text.y = element_text(
          size = 12
        ),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9")
      )
  })
  
  
  
  
  
  
  # PLOT 6
  # the manner of death 
  output$mannerBar <- renderPlot({
    # get rid of the people whos sex and race and age and location and manner of death is unknown this is done to keep the data among the plot consistent 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    a <- a[!is.na(a$Age),]
    a <- a[!(a$Location == ""),]
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    # make sure the accident are all grouped together even if they are spelled differently 
    a$MannerofDeath[a$MannerofDeath == "accident" | a$MannerofDeath == "ACCIDENT"] <- "Accident"
    
    raceChoice <- input$raceParameter
    sexChoice <- input$sexParameter
    
    raceLabel <- ""
    
    
    # the race choice filter
    if(raceChoice == "Asian Indian"){
      a<- a[a$Race == "Asian Indian",]
      raceLabel <- "who were Asian Indian"
    }
    else if(raceChoice == "Asian, Other"){
      a<- a[a$Race == "Asian, Other",]
      raceLabel <- "who were Asian, Other"
    }
    else if(raceChoice == "White"){
      a<- a[a$Race == "White",]
      raceLabel <- "who were White"
    }
    else if(raceChoice == "Black"){
      a<- a[a$Race == "Black",]
      raceLabel <- "who were Black"
    }
    else if(raceChoice == "Hispanic, White"){
      a<- a[a$Race == "Hispanic, White",]
      raceLabel <- "who were Hispanic, White"
    }
    else if(raceChoice == "Hispanic, Black"){
      a<- a[a$Race == "Hispanic, Black",]
      raceLabel <- "who were Hispanic, Black"
    }
    else if(raceChoice == "Other"){
      a<- a[a$Race == "Other",]
      raceLabel <- "who were Other"
    }
    else if(raceChoice == "Chinese"){
      a<- a[a$Race == "Chinese",]
      raceLabel <- "who were Chinese"
    }
    else if(raceChoice == "Native American, Other"){
      a<- a[a$Race == "Native American, Other",]
      raceLabel <- "who were Native American, Other"
    }
    else if(raceChoice == "Hawaiian"){
      a<- a[a$Race == "Hawaiian",]
      raceLabel <- "who were Hawaiian"
    }
    
    
    sexLabel <- "Male and Female"
    
    # the sex choice filter 
    if(sexChoice == "Male"){
      a <- a[a$Sex != "Female",]
      sexLabel <- "Males"
    }else if(sexChoice == "Female"){
      a <- a[a$Sex != "Male",]
      sexLabel <- "Females"
    }
    
    chartTitle <- paste("Manner Of Death of",sexLabel,raceLabel, sep = " ")
    
    a <- a %>% group_by(MannerofDeath) %>%   mutate(count_name_occurr = n())
    
    ggplot(a, aes(reorder(MannerofDeath,count_name_occurr))) + 
      geom_bar(width = 0.4, fill = "#C39BC8") +
      geom_text(aes(label = ..count..), vjust = -0.30, stat = "count", size=4, fontface = "bold")+
      ggtitle(chartTitle)+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(
          face = "bold",
          size = 15,
          hjust = 0.5,
          vjust = 2
        ),
        axis.text.x = element_text(
          size = 12
        ),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9")
      )
    
  })
  
  
  
  
  
  
  
  
  # PLOT 7
  # where was the injury place 
  output$injuryBar <- renderPlot({
    # get rid of the people whos sex and race and age and location and manner of death is unknown this is done to keep the data among the plot consistent 
    a$Sex[a$Sex == ""] <- "Unknown"
    a<- a[a$Sex != "Unknown",]
    a$Race[a$Race == ""] <- "Unknown"
    a<- a[a$Race != "Unknown",]
    a <- a[!is.na(a$Age),]
    a <- a[!(a$Location == ""),]
    a <- a[!(a$MannerofDeath == ""),]
    a <- a[!(a$InjuryPlace == ""),]
    
    
    raceChoice <- input$raceParameter
    sexChoice <- input$sexParameter
    
    raceLabel <- ""
    
    
    # the race choice filter
    if(raceChoice == "Asian Indian"){
      a<- a[a$Race == "Asian Indian",]
      raceLabel <- "who were Asian Indian"
    }
    else if(raceChoice == "Asian, Other"){
      a<- a[a$Race == "Asian, Other",]
      raceLabel <- "who were Asian, Other"
    }
    else if(raceChoice == "White"){
      a<- a[a$Race == "White",]
      raceLabel <- "who were White"
    }
    else if(raceChoice == "Black"){
      a<- a[a$Race == "Black",]
      raceLabel <- "who were Black"
    }
    else if(raceChoice == "Hispanic, White"){
      a<- a[a$Race == "Hispanic, White",]
      raceLabel <- "who were Hispanic, White"
    }
    else if(raceChoice == "Hispanic, Black"){
      a<- a[a$Race == "Hispanic, Black",]
      raceLabel <- "who were Hispanic, Black"
    }
    else if(raceChoice == "Other"){
      a<- a[a$Race == "Other",]
      raceLabel <- "who were Other"
    }
    else if(raceChoice == "Chinese"){
      a<- a[a$Race == "Chinese",]
      raceLabel <- "who were Chinese"
    }
    else if(raceChoice == "Native American, Other"){
      a<- a[a$Race == "Native American, Other",]
      raceLabel <- "who were Native American, Other"
    }
    else if(raceChoice == "Hawaiian"){
      a<- a[a$Race == "Hawaiian",]
      raceLabel <- "who were Hawaiian"
    }
    
    
    sexLabel <- "Male and Female"
    
    # the sex choice filter 
    if(sexChoice == "Male"){
      a <- a[a$Sex != "Female",]
      sexLabel <- "Males"
    }else if(sexChoice == "Female"){
      a <- a[a$Sex != "Male",]
      sexLabel <- "Females"
    }
    
    
    
    chartTitle <- paste("Places were the injuries for",sexLabel,raceLabel,sep = " ")
    
    a <- a %>% group_by(InjuryPlace) %>%   mutate(count_name_occurr = n())
    
    ggplot(a, aes(reorder(InjuryPlace,-count_name_occurr)))+
      geom_bar(fill = "#C39BC8")+
      coord_flip()+
      ggtitle(chartTitle)+
      geom_text(aes(label = ..count..), hjust = -0.20, stat = "count", size=4, fontface = "bold")+
      theme_bw()+
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(
          size = 12
        ),
        axis.title.y = element_blank(),
        plot.title = element_text(
          face = "bold",
          size = 15,
          hjust = 0.5,
          vjust = 2
        ),
        plot.background = element_rect(fill = "#D9D9D9", color = "#D9D9D9")
      )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
