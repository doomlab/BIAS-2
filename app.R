#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bias Function"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        ##pick the effect size type 
        selectInput("estype", helpText("Type of Effect Size:"), 
                    choices = list("Eta" = "fes", 
                                   "Partial Eta" = "pes",
                                   "Omega" = "fos",
                                   "Partial Omega" = "pos",
                                   "Generalized Eta" = "ges"), selected = "fes"),
        
        ##pick the correlation between levels
        selectInput("correl", helpText("Correlation of Levels:"), 
               choices = list(0, .1, .3, .5, .7, .9), selected = 0),
        
        ##pick the research design
        selectInput("rdtype", helpText("Research Design Type:"), 
                    choices = list("One-Way Between Subjects" = "BN1", 
                                   "One-Way Repeated Measures" = "RM1",
                                   "Two-Way Between Subjects" = "BN2",
                                   "Two-Way Repeated Measures" = "RM2"), selected = "BN1"),
        
        ##pick the number of levels
        selectInput("levels", helpText("Number of Levels:"), 
                    choices = list(3, 4, 5, 6), selected = 3),
        
        ##pick the size of the effect
        selectInput("esstrength", helpText("Strength of Effect:"), 
                    choices = list("Small" = 5, 
                                   "Medium" = 3,
                                   "Large" = 1), selected = 5)
      ), ##close side bar panel
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("biasplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$biasplot <- renderPlot({
     
     ##dataset must be in folder
     fulldata = read.csv("BiasDataAverage.csv")
     
     ##create the graph data
     graphdata = subset(fulldata, 
                        Corr == input$correl & 
                          Levels == input$levels & 
                          stdev == input$esstrength &
                          ES == input$estype &
                          Design == input$rdtype)

     ##create the graph
     library(ggplot2)
     ggplot(graphdata, aes(N, Bias)) +
       geom_point() + 
       geom_smooth() + 
       coord_cartesian(ylim = c(-.30,.30)) + 
       geom_abline(slope = 0, intercept = 0) +
       theme(panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), 
             panel.background = element_blank(), 
             axis.line.x = element_line(colour = "black"), 
             axis.line.y = element_line(colour = "Black"),
             legend.key = element_rect(fill = "white"),
             text = element_text(size = 15))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

