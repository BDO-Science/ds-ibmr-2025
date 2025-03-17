#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Load 
x2a <- read_csv(here("data/data_processed/IBMR_X2_SF2025_input.csv")) %>%
    mutate(hydro = "AdjHist")

x2b <- read_csv(here("data/data_processed/IBMR_X2_SF2022MED_input.csv")) %>%
    mutate(hydro = "2022MED")

x2 <- bind_rows(x2a, x2b)

x2_long <- x2 %>%
    pivot_longer(cols = 3:14, values_to = "x2", names_to = "month") %>%
    mutate(month = as.numeric(month)) %>%
    mutate(month_abb = factor(month.abb[month], levels = month.abb)) %>%
    mutate(scenario = forcats::fct_relevel(scenario,  c("StatusQuo", "MaxDS_Even","MaxDS_Hist", "SummerFall_Even", "Summer_Even", "Summer_Even_AltSMSCG",
                                                        "SummerFall_Hist", "Summer_Hist", "June", "MaxWater", "MaxWater_noSMSCG"))) 


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Summer-Fall Delta Smelt SDM Data Input Exploration"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("month_slider", 
                        "Select Range of Months:", 
                        min = 1, 
                        max = 12, 
                        value = c(6, 9), 
                        step = 1),
            checkboxGroupInput("options", 
                               "Select Alternatives:", 
                               choices = list("Status Quo" = "StatusQuo", 
                                              "MaxDS Even" = "MaxDS_Even", 
                                              "MaxDS Hist" = "MaxDS_Hist", 
                                              "MaxWater" = "MaxWater",
                                              "MaxWater noSMSCG" = "MaxWater_noSMSCG",
                                              "Summer Even" = "Summer_Even",
                                              "Summer Even AltSMSCG" = "Summer_Even_AltSMSCG",
                                              "Summer Hist" = "Summer_Hist",
                                              "SummerFall Even" = "SummerFall_Even",
                                              "SummerFall Hist" = "SummerFall_Hist"), 
                               selected = c("StatusQuo","SummerFall_Hist")),
            checkboxGroupInput("hydrology", 
                               "Select Hydrology:", 
                               choices = list("Historical Adjusted Hydrology" = "AdjHist", 
                                              "2022 Median" = "2022MED"), 
                               selected = c("AdjHist"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("x2plot", width = "1200px", height = "1000px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$x2plot <- renderPlot({
        print(ggplot() +
            geom_line(data = x2_long %>% 
                          filter(month %in% c(input$month_slider[1]:input$month_slider[2])) %>%
                          filter(scenario %in% input$options) %>%
                          filter(hydro %in% c(input$hydrology)),
                      aes(x = year, y = x2, color = scenario, linetype = scenario), alpha = 0.9)+
            viridis::scale_color_viridis(discrete = TRUE, option = "turbo") + 
            scale_x_continuous(breaks = seq(1995, 2014, 1))+ 
            scale_y_continuous(breaks = seq(55, 95, 10)) + 
            facet_wrap(month_abb~hydro, nrow = length(c(input$month_slider[1]:input$month_slider[2]))) + 
            labs(y = "X2 (km)")+
            theme_bw() +
                theme(axis.text = element_text(size = 14),  # Increase tick mark font size
                      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                      legend.text=element_text(size=14),
                      legend.title=element_text(size=14),
                      axis.title.x = element_text(size=14),
                      axis.title.y = element_text(size=14),
                      legend.key.size = unit(1.2, "cm")))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
