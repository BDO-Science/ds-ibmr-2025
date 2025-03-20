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
library(rsconnect)

# Load data
consequence_table <-read.csv("ConsequenceTable_2025-03-19.csv")
# Standardize
consequence_table_std <- consequence_table %>%
    mutate(DeltaSmelt=(DeltaSmelt-min(consequence_table$DeltaSmelt))/(max(consequence_table$DeltaSmelt)-min(consequence_table$DeltaSmelt)),
           CVP_export=(CVP_export-min(consequence_table$CVP_export))/(max(consequence_table$CVP_export)-min(consequence_table$CVP_export)),
           SWP_export=(SWP_export-min(consequence_table$SWP_export))/(max(consequence_table$SWP_export)-min(consequence_table$SWP_export)),
           Redds_dewatered=(Redds_dewatered-max(consequence_table$Redds_dewatered))/(min(consequence_table$Redds_dewatered)-max(consequence_table$Redds_dewatered)),
           Folsom_probability300TAF=(Folsom_probability300TAF-min(consequence_table$Folsom_probability300TAF))/(max(consequence_table$Folsom_probability300TAF)-min(consequence_table$Folsom_probability300TAF)),
           DeltaHumanHealth=(DeltaHumanHealth-min(consequence_table$DeltaHumanHealth))/(max(consequence_table$DeltaHumanHealth)-min(consequence_table$DeltaHumanHealth)),
           Shasta_storage=(Shasta_storage-min(consequence_table$Shasta_storage))/(max(consequence_table$Shasta_storage)-min(consequence_table$Shasta_storage)),
           Oroville_storage=(Oroville_storage-min(consequence_table$Oroville_storage))/(max(consequence_table$Oroville_storage)-min(consequence_table$Oroville_storage)))

# Specify the columns you want to gather
columns_to_gather <- c("DeltaSmelt", "CVP_export","SWP_export","Redds_dewatered","Folsom_probability300TAF","DeltaHumanHealth",
                       "Shasta_storage","Oroville_storage")

consequence_table_std_long <- consequence_table_std %>% 
    pivot_longer(cols = all_of(columns_to_gather),  # Use all_of() to select columns
                 names_to = "Objective",                   # New column for the names
                 values_to = "Value")                  # New column for the values

# Define custom colors for barplots
#custom_colors_alt <- c("StatusQuo" = "#000000", "Alt F74" = "#E69F00",
 #                      "Alt S74" = "yellow4" , "Alt S74F80" = "#56B4E9","Alt NoX2"= "#999999")

unique(consequence_table_std_long$Alternative)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("2025 Summer-Fall Habitat Action SDM"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Hypothesis Weights"),
            sliderInput("H1_weight",
                        "Weight for Hypothesis 1 - Adjusted Historical Hydrology (the rest will be allocated towards 2022 Median Hydrology):",
                        min = 0.00,
                        max = 1.00,
                        value = 0.50),
            HTML("<strong>Note:</strong> Objective weights must add up to 1."),
            uiOutput("warning"),  # Dynamic warning output
            h3("Objective Weights"),
            numericInput("O1_weight", "1. Delta Smelt Persistence Weight", 0.125, min = 0.0, max = 1),
            numericInput("O2_weight", "2. Water Supply - CVP Exports", 0.125, min = 0.0, max = 1),
            numericInput("O3_weight", "3. Water Supply - SWP Exports", 0.125, min = 0.0, max = 1),
            numericInput("O4_weight", "4. Winter-run Chinook redd dewatering", 0.125, min = 0.0, max = 1),
            numericInput("O5_weight", "5. Steelhead and Fall-run Chinook (Folsom)", 0.125, min = 0.0, max = 1),
            numericInput("O6_weight", "6. In-Delta Water Quality and Human Health", 0.125, min = 0.0, max = 1),
            numericInput("O7_weight", "7. Coldwater Pool CVP (Shasta)", 0.125, min = 0.0, max = 1),
            numericInput("O8_weight", "8. Coldwater Pool SWP (Oroville)", 0.125, min = 0.0, max = 1),
            h3("Hypothesis Information"),
            p("Hypothesis 1: Adjust Historical Hydrology"),
            p("Hypothesis 2: Climate Change adjusted based on 2022 median"),
            HTML("<strong>Note:</strong> Actions would only occur in W or AN years. Old and Middle River + Suisun Marsh Salinity Control Gate actions are present across all alternatives unless noted otherwise."),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(tabPanel("Information",h3("Background"),
                                       p("This effort is intended to aid federal and state agencies decide on how update the Delta Smelt Summer and Fall Habitat conservation measure described for the Long-Term Operation of the Central Valley Project and State Water Project in coordination with interested parties. The action would be for a single year (2025) and then revisited in 2026."),
                                       h3("How to use this page:"),
                                       h3("Models"),
                                       h4("CalSim3"),
                                       p("To calculate water operation impacts for each alternative, CalSim3 model was used. CalSim3 is a water resources planning model that simulate operations of the SWP and CVP and much of the water resources infrastructure in the Central Valley of California and the Sacramento-San Joaquin Delta region."),
                                       h4("IBMR v1"),
                                       p("To evaluate population growth under the alternatives and hypotheses, Delta Smelt Individual-Based Model in R (IBMR) was used. IBMR v1 is a monthly time-step simulation/agent-based model for Delta Smelt with an explicit bioenergetics component calibrated to observed conditions in 1995-2014. Bioenergetic parameters came from Rose et al. (2013) publication, which used information from Rainbow Smelt with an assumed temperature effect for Delta Smelt."),
                                       h3("Alternatives")),
        tabPanel("Line Plot",plotOutput("Plot")),
        tabPanel("Utility Score Table",tableOutput("tableSum")),tabPanel("VOI",textOutput("VOI_calc1"),textOutput("VOI_calc2")),
        tabPanel("Raw Consequence Table",tableOutput("tableRaw"))))
    )
)



# Define server logic required to do things
server <- function(input, output, session) {
    #Pull together line plot data
    line_plot_data <- reactive({consequence_table_std_long %>% 
            mutate(obj_weight= case_when(Objective == "DeltaSmelt" ~ input$O1_weight,
                                         Objective == "CVP_export" ~ input$O2_weight,
                                         Objective == "SWP_export" ~ input$O3_weight,
                                         Objective == "Redds_dewatered" ~ input$O4_weight, 
                                         Objective == "Folsom_probability300TAF" ~ input$O5_weight,
                                         Objective == "DeltaHumanHealth" ~ input$O6_weight,
                                         Objective == "Shasta_storage" ~ input$O7_weight,
                                         Objective == "Oroville_storage" ~ input$O8_weight)) %>%
            mutate(score_obj = obj_weight*Value) %>% group_by(Alternative,Hydrology) %>%
            summarise(comp_score = sum(score_obj)) %>% mutate(Hypo_weight = case_when(Hydrology == "AdjustedHist" ~ 1.0,
                                                                                    Hydrology == "2022MED" ~ 0))
    })
    
    output$Plot <- renderPlot({
        #Plot here
        print(ggplot(data=line_plot_data(), aes(x=Hypo_weight, y=comp_score, color=Alternative,linetype=Alternative)) +
                  geom_line(linewidth= 1.2) +
                  geom_vline(xintercept = input$H1_weight, linetype="dotted", color = "red", size=1) +
                  theme_minimal()+
                  labs(title = NULL,
                       x = "Hypothesis 1 weight",
                       y = "Composite score (objective-weighted linear value function)") +
                  theme(axis.text = element_text(size = 14),  # Increase tick mark font size
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                        legend.text=element_text(size=14),
                        legend.title=element_text(size=14),
                        axis.title.x = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        legend.key.size = unit(1.2, "cm")))
    })
    
    # Create final utility score table
    cons_table_std_new <- reactive({consequence_table_std_long %>% mutate(obj_weight= case_when(Objective == "DeltaSmelt" ~ input$O1_weight,
                                                                                    Objective == "CVP_export" ~ input$O2_weight,
                                                                                    Objective == "SWP_export" ~ input$O3_weight,
                                                                                    Objective == "Redds_dewatered" ~ input$O4_weight, 
                                                                                    Objective == "Folsom_probability300TAF" ~ input$O5_weight,
                                                                                    Objective == "DeltaHumanHealth" ~ input$O6_weight,
                                                                                    Objective == "Shasta_storage" ~ input$O7_weight,
                                                                                    Objective == "Oroville_storage" ~ input$O8_weight)) %>%
            mutate(score_obj = obj_weight*Value) %>% group_by(Alternative,Hydrology) %>%
            summarise(comp_score = sum(score_obj)) %>% mutate(hypo_weight = case_when(Hydrology == "AdjustedHist" ~ input$H1_weight,
                                                                                      Hydrology == "2022MED" ~ 1-input$H1_weight)) %>%
            mutate(comp_score_hyp = comp_score*hypo_weight) %>% ungroup() %>% group_by(Alternative) %>%
            summarise(CompositeScore = sum(comp_score_hyp))})
    
    output$tableSum <- renderTable(cons_table_std_new() %>% arrange(desc(CompositeScore)))
    
    # Add VOI calculation tables
    cons_table_reconfig <- reactive({consequence_table_std_long %>% mutate(obj_weight= case_when(Objective == "DeltaSmelt" ~ input$O1_weight,
                                                                                                 Objective == "CVP_export" ~ input$O2_weight,
                                                                                                 Objective == "SWP_export" ~ input$O3_weight,
                                                                                                 Objective == "Redds_dewatered" ~ input$O4_weight, 
                                                                                                 Objective == "Folsom_probability300TAF" ~ input$O5_weight,
                                                                                                 Objective == "DeltaHumanHealth" ~ input$O6_weight,
                                                                                                 Objective == "Shasta_storage" ~ input$O7_weight,
                                                                                                 Objective == "Oroville_storage" ~ input$O8_weight))  %>%
            mutate(hypo_weight = case_when(Hydrology == "AdjustedHist" ~ input$H1_weight,
                                           Hydrology == "2022MED" ~ 1-input$H1_weight)) %>%
            dplyr::select(Alternative, Hypothesis, Objective, Value, hypo_weight) %>% 
            spread(Objective, Value) %>% 
            mutate(composite_score = (DeltaSmelt * input$fish_weight) + (WaterCost * (1 - input$fish_weight))) })
    
    certainty_calc <- reactive({ cons_table_reconfig() %>% 
            group_by(Hypothesis) %>% 
            summarise(composite_score = max(composite_score), hypo_weight = mean(hypo_weight)) %>%
            mutate(hypothesis_score = composite_score * hypo_weight) })
    
    uncertainty_calc <- reactive({ cons_table_reconfig() %>% 
            mutate(composite_score_hypo = hypo_weight * composite_score) %>% 
            group_by(Alternatives) %>% 
            summarise(composite_score = sum(composite_score_hypo)) })
    # Show text on VOI
    output$VOI_calc1 <- renderText({
        paste("Value of Perfect Information (Composite Score):",round(sum(certainty_calc()$hypothesis_score)-max(uncertainty_calc()$composite_score),digits=3))
    })
    output$VOI_calc2 <- renderText({
        paste("Value of Perfect Information (% of Best composite Score):",paste(round((sum(certainty_calc()$hypothesis_score)-max(uncertainty_calc()$composite_score))/max(cons_table_std_new()$CompositeScore)*100,digits=3)),"%")
    })
    
    # Show table for swing weighting
    output$tableSwing <- renderTable(swing_table)
    # Results of swing weighting
    observeEvent(input$submit, {
        output$swingText_Water <- renderText({
            paste("Objective Weight for Water Cost:", round(input$numInputAlt_A/(input$numInputAlt_A+input$numInputAlt_B),2))
        })
        output$swingText_DeltaSmelt <- renderText({
            paste("Objective Weight for Delta Smelt:", round(input$numInputAlt_B/(input$numInputAlt_A+input$numInputAlt_B),2))
        })
    })
    # Show warning if the hypothesis weight number exceeds 1
    output$warning <- renderUI({
        if (!all.equal(1,input$O1_weight+input$O2_weight+input$O3_weight+input$O4_weight+input$O5_weight+input$O6_weight+input$O7_weight+input$O8_weight)) {
            tags$div(style = "color: red;", "Warning: The total objective weights do not add up to 1!")
        }
    })
    
    # Plot performance metric bar plots
    # Delta Smelt plot
    output$PlotSmelt <- renderPlot({
        print(ggplot(data_plot_dsm, aes(x=Alternatives, y=Score, fill=Alternatives)) +
                  geom_bar(stat = "identity") +
                  labs(title = "Delta Smelt Objective",
                       x = "Alternative",
                       y = "Lambda") + 
                  facet_grid(cols = vars(Hypothesis)) +
                  scale_fill_manual(values = custom_colors_alt,guide="none")  +   # Use a color palette
                  theme_bw() +                          # Classic theme 
                  coord_cartesian(ylim=c(0.75,1.05)) +
                  theme(axis.text.y = element_text(size = 14),
                        axis.text.x = element_text(size = 14,angle = 45, hjust = 1),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                        axis.title.x = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        strip.text = element_text(size = 14)) )
    })
    # Water cost plot
    output$PlotWater <- renderPlot({
        print(ggplot(data_plot_water, aes(x=Alternatives, y=Score, fill=Alternatives)) +
                  geom_bar(stat = "identity") +
                  labs(title = "Water Cost Objective",
                       x = "Alternative",
                       y = "Thousand Acre Feet") + 
                  scale_fill_manual(values = custom_colors_alt,guide="none")  +   # Use a color palette
                  theme_bw() +                          # Classic theme 
                  theme(axis.text = element_text(size = 14),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                        axis.title.x = element_text(size=14),
                        axis.title.y = element_text(size=14)) )
    })
    
    
    # Raw consequence table
    output$tableRaw <- renderTable(consequence_table)
}

# Run the application 
shinyApp(ui = ui, server = server)
