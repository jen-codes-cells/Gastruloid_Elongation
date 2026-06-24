# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(bslib)

# load data
df <- read.csv("data/Wnt_3_ways_R2_clean_summary.csv", header = T)

# global parameters
TreatmentColours <- c("#b4d4b2", "#c96f7a", "#f3e4c2", "#aab1cf", "#f1a993")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simple Gastruloid Elongation"),

    # Sidebar with a select input for cell line (group) 
    sidebarLayout(
        sidebarPanel(
          title = "Gastruloid Elongation",
            # table input here
          # select cell line here
          selectInput("group",
            label = "Select Cell Line:",
            choices = c("E14tg2a", "E14_SL", "S2TB"),
            selected = "E14_SL"
          ),
          
          selectInput("condition",
            label = "Select Condition:",
            choices = c("CHIR", "Canonical Only", "PCP Only", "no CHIR", "no Wnt"),
            selected = "CHIR",
            multiple = TRUE
          )
        ),

        # Main panel showing the table and plot
        mainPanel(
          
              plotOutput("gloidPlot"),
              tableOutput("sum_table")
        )
    )
)

# Define server logic required to display table and plot
server <- function(input, output) {
  
  output$gloidPlot <- renderPlot({
    # plot here
    p <- df |> filter(Group == input$group, Condition %in% input$condition)  
    # the %in% operator allows none or multiple selections!!, input$condition is basically a vector when multiple choice
    
    ggplot(p, aes(x = factor(Timepoint, levels=c('72h', '96h', '120h')), y = EI_Avg, group = Condition, colour = Condition)) +
      geom_line(linewidth = 2) +
      # geom_ribbon(alpha=0.25) + # add error shading
      theme_minimal() +
      scale_color_manual(values = TreatmentColours) +
      ggtitle(paste(input$group, " Elongation Index")) +
      ylab("Elongation Index") +
      xlab("Timepoint")
      })
  
  output$sum_table <- renderTable({
    t <-  df |> filter(Group == input$group , Condition %in% input$condition) 
    }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
