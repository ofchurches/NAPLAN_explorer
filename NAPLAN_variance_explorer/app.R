library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(tidyverse)
library(viridis)

NAPLAN_Numeracy_2019 <- read_excel("NAPLAN Numeracy 2019.xlsx")

NAPLAN_Numeracy_2019_pull <- NAPLAN_Numeracy_2019 %>%
  filter(`NAPLAN Scale Score` < max(`NAPLAN Scale Score`)) %>%
  pull(`NAPLAN Scale Score`)

NAPLAN_Numeracy_2019_pull_2 <- c(min(NAPLAN_Numeracy_2019_pull), NAPLAN_Numeracy_2019_pull)

NAPLAN_Numeracy_2019_rect <- NAPLAN_Numeracy_2019 %>%
  mutate(NAPLAN_Numeracy_2019_push = NAPLAN_Numeracy_2019_pull_2) %>%
  group_by(Year, Band) %>%
  mutate(band_min = min(NAPLAN_Numeracy_2019_push)) %>%
  ungroup() %>%
  mutate(band_plus_one = Band + 1) %>%
  group_by(Year, band_plus_one) %>%
  mutate(band_max = max(`NAPLAN Scale Score`)) %>%
  ungroup() %>%
  group_by(Year) %>%
  mutate(SEA = fct_collapse(as.factor(Band), 
                             
                            
                            "Demonstrated High Achievement" = c(as.character(min(Band) + 4), as.character(min(Band) + 5)),
                            "Demonstrated SEA" = c(as.character(min(Band) + 2), as.character(min(Band) + 3)),
                            "Did not demonstrate SEA" = c(as.character(min(Band)), as.character(min(Band) + 1))
  )
  ) %>%
  mutate(SEA = fct_rev(SEA)) %>%
  ungroup()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NAPLAN Variance Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("y3",
                        "Raw score in Year 3:",
                        min = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 3) %>%
                          pull(`Raw Score`) %>%
                          min(),
                        max = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 3) %>%
                          pull(`Raw Score`) %>%
                          max(),
                        value = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 3) %>%
                          pull(`Raw Score`) %>%
                          sample(1)),
            sliderInput("y5",
                        "Raw score in Year 5:",
                        min = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 5) %>%
                          pull(`Raw Score`) %>%
                          min(),
                        max = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 5) %>%
                          pull(`Raw Score`) %>%
                          max(),
                        value = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 5) %>%
                          pull(`Raw Score`) %>%
                          sample(1)),
            sliderInput("y7",
                        "Raw score in Year 7:",
                        min = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 7) %>%
                          pull(`Raw Score`) %>%
                          min(),
                        max = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 7) %>%
                          pull(`Raw Score`) %>%
                          max(),
                        value = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 7) %>%
                          pull(`Raw Score`) %>%
                          sample(1)),
            sliderInput("y9",
                        "Raw score in Year 9:",
                        min = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 9) %>%
                          pull(`Raw Score`) %>%
                          min(),
                        max = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 9) %>%
                          pull(`Raw Score`) %>%
                          max(),
                        value = NAPLAN_Numeracy_2019 %>%
                          filter(Year == 9) %>%
                          pull(`Raw Score`) %>%
                          sample(1))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h4("The plot below shows confidence intervals around the scores recorded for a single student in the NAPLAN Numeracy Domain using data from 2019."), 
          h4("Use the sliders to set the raw score for each year level. This will move the points on the plot to the right and set the whiskers automatically in terms of the corresponding NAPLAN Scale Score. The whiskers above and below each score show the 95% confidence interval for that score. Note that many scores extend over SEA thresholds."),
          plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      selected_y3 <- NAPLAN_Numeracy_2019 %>%
        filter(Year == 3 & `Raw Score` == input$y3)
      
      selected_y5 <- NAPLAN_Numeracy_2019 %>%
        filter(Year == 5 & `Raw Score` == input$y5)
      
      selected_y7 <- NAPLAN_Numeracy_2019 %>%
        filter(Year == 7 & `Raw Score` == input$y7)
      
      selected_y9 <- NAPLAN_Numeracy_2019 %>%
        filter(Year == 9 & `Raw Score` == input$y9)
      
      selected <- selected_y3 %>%
        bind_rows(selected_y5) %>%
        bind_rows(selected_y7) %>%
        bind_rows(selected_y9)
      
      
      
      ggplot() + 
        geom_rect(data = NAPLAN_Numeracy_2019_rect, mapping = aes(xmin = Year - 1, xmax = Year + 1, 
                                                                  ymin = band_min, ymax = band_max,
                                                                  fill = SEA)) +
        scale_fill_manual(values = c("#457645", "#b0bf1a", "#cee3ae")) + 
        geom_rect(data = NAPLAN_Numeracy_2019_rect, mapping = aes(xmin = Year - 1, xmax = Year + 1, 
                                                                  ymin = band_min, ymax = band_max
        ), 
        alpha = 0, 
        colour = "white") + 
        geom_text(data = NAPLAN_Numeracy_2019_rect, mapping = aes(x = Year, 
                                                                  y = band_min, 
                                                                  label = paste("Band ", Band) 
                                                                  ), 
                  colour = "white", 
                  nudge_y = 20, 
                  nudge_x = -.7) +

        geom_point(data = selected, mapping = aes(x = Year, y = `NAPLAN Scale Score`)) + 
        geom_errorbar(data = selected, mapping = aes(x = Year, 
                                                     ymin = `NAPLAN Scale Score` - (`Scale Score SE` * 1.96), 
                                                     ymax = `NAPLAN Scale Score` + (`Scale Score SE` * 1.96)), 
                      size = .9) + 
        
        theme_minimal() +
        theme(axis.text.x=element_text(size=15), 
              axis.text.y=element_text(size=15), 
              axis.title.x=element_text(size=15), 
              axis.title.y=element_text(size=15)) + 
        labs(title = "NAPLAN Numeracy 2019", 
             y = "Scale Score", 
             x = "Year") + 
        scale_x_continuous(breaks = c(3, 5, 7, 9)) + 
        geom_label(data = selected, mapping = aes(x = Year, 
                                                  y = `NAPLAN Scale Score`, 
                                                  label = `Raw Score`))
        
        
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
