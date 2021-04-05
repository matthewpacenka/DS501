library(shiny)

#map libraries
library(usmap)
library(ggplot2)
library(usdata)

#kmeans libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

#for the grid
library(gridExtra)

#prepare data
myData = readRDS("data/BeeData.rds")
myData = na.omit(myData)
myData = data.frame(myData)
myData$State = state2abbr(myData$State)

#data
#URL = https://quickstats.nass.usda.gov/results/7B6FD5B3-D13B-310B-BACE-BE7B027E2FCF
#shiny URL = https://m-pacenka.shinyapps.io/bee-app/

#ui <- fluidPage("Hello World"
               #*Input(inputId = "name",), #need comma between inputs of fluidPage()  
               #*Output(outputId = "name",)
               
 #              )
ui <- fluidPage(
  titlePanel("Honey Bee Colony Loss Visualization"),

 sidebarLayout(
   sidebarPanel(
     helpText("Compare percent of bee colonies lost by time of year. Data taken over the past 5 years."),

     selectInput('varType',
                 label = "Choose reason for colony loss",
                 choices = c("AFFECTED.BY.DISEASE",
                             "AFFECTED.BY.OTHER.CAUSES",
                             "AFFECTED.BY.PESTICIDES", 
                             "AFFECTED.BY.PESTS...EXCL.VARROA.MITES..",
                             "AFFECTED.BY.UNKNOWN.CAUSES",
                             "AFFECTED.BY.VARROA.MITES"),    #this would be input$dropdown of causes
                 selected = "AFFECTED.BY.DISEASE"), #default value
     
     selectInput("varMonth",
                 label = "Choose time of year",
                 choices = c("JAN THRU MAR",
                             "APR THRU JUN",
                             "JUL THRU SEP",
                             "OCT THRU DEC"),    #this would be input$dropdown of month
                 selected = "JAN THRU MAR"),

     sliderInput("varYear",
                 label = "Year of interest:",
                 min = 2015, max = 2020, value = 2015, sep="", animate=TRUE),
     
     numericInput('clusters', 'Cluster count', 3, min = 1, max = 6)
    ),

    mainPanel(
      h4("Evaluate Colony loss and what afflictions they were affected by"),
      plotOutput('plot1'),
      h6("All values are in percentages of colonies per state."),
      
      h6("The plot demonstrates the relevance of each affliction towards the loss of colonies each year. The higher the percent affected (y-axis), the 
      greater influence that affliction has on the colonies. This correlation does not necessarily imply that colonies died from this affliction, it merely
      suggests that beekeeper had higher levels of this affliction."),
      
      h6("By showing the percent of colonies lost, a deeper understanding of the influence of the affliction can be grasped. In each state, a higher percentage of 
      colonies lost AND a higher percentage of colonies affected (y-axis), tells us that the likely cause for colonies lost in that state was the 
      inputted affliction."),
      
      h6("It is important to note the time of year that is inputted since some afflictions are seasonally driven (e.g. Varroa Mites often kill colonies 
      during the winter, hence an increase in the percent of hives with Varroa Mites during the winter months.)."),
         
      h6("Note: the survey ran until the fall of 2020 so data for OCT THRU DEC of 2020 will be missing"),
      h4("US without CA"),
      plotOutput('mapUS'),
      h6("CA has been excluded due to the disproportionately high numbers of colonies used there for almond and cherry (and other fruit) blossoms. Each year, 
         commercial honey bee companies ship thousands of colonies around the state to pollinate the trees. This effort surpases any scale in the
         rest of the country by (often) hundreds of thousands of colonies."),
      
      h4("Explanation of the K-Means Algorithm"),
      h6("The K-means algorithm involves selecting K initial centroids where K is a user defined number of desired clusters. 
         Each point is then assigned to a closest centroid and the collection of points nearest to each centroid form a cluster. 
         The centroid gets updated according to the points in the cluster. This process continues until the points stop changing their clusters."),
      
      h4("Report Info"),
      h6("Data - The information collected was from the USDA National Agricultural Statistics Service. This data set incorporates survey information
         from beekeepers of each state in the continental United States."),
      h6("Motivation - As a beekeeper, I (and many other Worcester County Beekeepers) lost hives this past winter to Varroa Mites, and 
         was curious to learn if this affliction was specific to New England, and if Varroa was the major cause behind colony loss. Using the information
         provided by the USDA, I was able to determine that Varroa is not just an affliction of the North East, and, in-fact, has an even greater affect on other 
         crutial honey-producing states. Since Florida and Texas are two major honey producing states, it is concerning to see those two states losing
         a large portion of their colonies each year."),
      h6("Findings - (For my personal interest) Varroa is not just a problem in the North East. It impacts all regions of the country and it does so at various
         times during the year. This concludes that Varroa populations are local issues and require attention by the collectives of beekeepers in those
         areas. Second, from the clustering, we are also able to see that other afflictions are not regional. Interestingly enough, some afflicitons appear during
         businesses of scale (as we can see in 2015 that Pesticides affect disproportionately high numbers of colonies in Texas, Florida, and California - the 
         three largest commercial honey bee states.")
      
      # h4("Regional Analysis"),
      # h6("North East"),
      # plotOutput('mapNorthEast'),
      # h6("Midwest"),
      # plotOutput('mapMidwest'),
      # h6("South East"),
      # plotOutput('mapSouthEast'),
      # h6("South West"),
      # plotOutput('mapSouthWest'),
      # h6("North West"),
      # plotOutput('mapNorthWest'),
      # h6("West"),
      # plotOutput('mapWest')
    )
  )
)


#server <- function(input, output){
  
  #Rule 1 and 2: output$"outputId name from above" = render*({})
  #Rule 3: reference input values inside render*() by:
      #input$"inputId from above"
  
#}

server <- function(input, output) {
#--------------K means----------------
  selectedData <- reactive({
    myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("LOSS.PERCENTAGE",input$varType)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#377EB8","#FF7F00","#4DAF4A","#F781BF","#A65628","#FFFF33"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 8, cex = 4, lwd = 4)
    text(selectedData(), labels = myData$State, pos = 3)
    
  })

#----------------MAPS----------------
#All US
  output$mapUS <- renderPlot({
    temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
    colnames(temp)[colnames(temp) == "State"] <- "state"
    
    plot_usmap(exclude = "CA", data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
      scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
      theme(legend.position = "right")
  })
#   #NE
#   output$mapNorthEast <- renderPlot({
#     temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
#     colnames(temp)[colnames(temp) == "State"] <- "state"
#     states = c("ME","MA","RI","CT","VT","NH","NY","PA")
#     
#     plot_usmap(include = states, data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
#       scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
#       theme(legend.position = "right")
#   })
# #SE  
#   output$mapSouthEast <- renderPlot({
#     temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
#     colnames(temp)[colnames(temp) == "State"] <- "state"
#     states = c("FL","GA","AR","LA","TN","AL","SC","NC","VA","MD","DE","KY","WV")
#     
#     plot_usmap(include = states, data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
#       scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
#       theme(legend.position = "right")
#   })
# #MidW  
#   output$mapMidwest <- renderPlot({
#     temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
#     colnames(temp)[colnames(temp) == "State"] <- "state"
#     states = c("WI","OH","MI","IL","IN","KE","KA","MO","IA","NE","SD","ND")
#     
#     plot_usmap(include = states, data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
#       scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
#       theme(legend.position = "right")
#   })
# #SW
#   output$mapSouthWest <- renderPlot({
#     temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
#     colnames(temp)[colnames(temp) == "State"] <- "state"
#     states = c("TX","NM","OK","UT","CO","AZ")
#     
#     plot_usmap(include = states, data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
#       scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
#       theme(legend.position = "right")
#   })
# #NW
#   output$mapNorthWest <- renderPlot({
#     temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
#     colnames(temp)[colnames(temp) == "State"] <- "state"
#     states = c("OR","WA","WY","MT","ID")
#     
#     plot_usmap(include = states, data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
#       scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
#       theme(legend.position = "right")
#   })
# 
# #W    
#   output$mapWest <- renderPlot({
#     temp = myData[myData$Period == input$varMonth & myData$Year == input$varYear, c("State","LOSS.COUNT")]
#     colnames(temp)[colnames(temp) == "State"] <- "state"
#     
#     plot_usmap(include = c("CA","NV"), data = temp, values = "LOSS.COUNT", color = "blue", labels = TRUE, label_color = "gray") + 
#       scale_fill_continuous(low = "white", high = "blue", name = "Count of Colonies Lost", label = scales::comma) + 
#       theme(legend.position = "right")
#   })
}

shinyApp(ui = ui, server = server)
