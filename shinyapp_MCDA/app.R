library(shiny)
library(tidyverse)
library(rsconnect)
library(viridis)
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



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(
      # Logo
      title = div(
        # logo
        style = "display: flex; align-items: center; justify-content: space-between;",
        a(img(src = "USBR_logo_oct_2019.jpg", height = 80, style = "margin-right: 20px;"),
          href = "http://www.usbr.gov"),
        # title
        div(
          style = "flex-grow: 1;",
          h2("2025 Summer-Fall Habitat Action SDM",
             style = "font-family:Segoe UI Semibold; font-size: 34px"),
          h4("Version 1.0.0", style = "font-size: 14px; font-style: italic"),
          tags$hr(style = "border-top: 1px solid #0a7e8c; margin-top: 10px; margin-bottom: 10px;opacity: 0.7"),
          # links and contact
          h5("Code associated with SDM effort available at: ", 
             a("https://github.com/BDO-Science/ds-ibmr-2025", href="https://github.com/BDO-Science/ds-ibmr-2025")),
          h5("Preliminary results for SDM effort available at: ", 
             a("Delta Smelt Summer-Fall Habitat Action SDM Results", href="https://www.researchgate.net/publication/390212804_Structured_Decision_Making_for_Delta_Smelt_Summer-Fall_Habitat_Actions_Multi-criteria_Decision_Analysis_Results")),
          h5(uiOutput("contact"))
          
        )
      ),
      windowTitle = "2025 Summer-Fall Habitat Action SDM"),
    tags$hr(style = "border-top: 3px solid #0a7e8c; margin-top: 10px; margin-bottom: 20px; opacity: 0.9"),
      
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Hypothesis Weights"),
            sliderInput("H1_weight",
                        "Weight for Hypothesis 1 - Adjusted Historical Hydrology (the rest will be allocated towards 2022 Median Hydrology):",
                        min = 0.00,
                        max = 1.00,
                        value = 0.50),
            HTML("<strong>Note:</strong> At least one objective need to have >0 score."),
            uiOutput("warning"),  # Dynamic warning output
            h3("Objective Scores"),
            numericInput("O1_score", "1. Delta Smelt Persistence", 100, min = 0, max = 100),
            numericInput("O2_score", "2. Water Supply - CVP Exports", 100, min = 0, max = 100),
            numericInput("O3_score", "3. Water Supply - SWP Exports", 100, min = 0, max = 100),
            numericInput("O4_score", "4. Winter-run Chinook redd dewatering", 100, min = 0, max = 100),
            numericInput("O5_score", "5. Steelhead and Fall-run Chinook (Folsom)", 100, min = 0, max = 100),
            numericInput("O6_score", "6. In-Delta Water Quality and Human Health", 100, min = 0, max = 100),
            numericInput("O7_score", "7. Coldwater Pool CVP (Shasta)", 100, min = 0, max = 100),
            numericInput("O8_score", "8. Coldwater Pool SWP (Oroville)", 100, min = 0, max = 100),
            h3("Hypothesis Information"),
            p("Hypothesis 1: Adjust historical hydrology"),
            p("Hypothesis 2: Climate change adjusted hydrology based on 2022 median"),
            HTML("<strong>Note:</strong> Actions would only occur in Wet (W) or Above Normal (AN) years. Old and Middle River + Suisun Marsh Salinity Control Gate actions are present across all alternatives unless noted otherwise.")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(tabPanel("Information",h3("Background"),
                                       p("This effort is intended to help federal and state agencies decide on how update the Delta Smelt Summer and Fall Habitat conservation measure described for the Long-Term Operation of the Central Valley Project and State Water Project in coordination with interested parties. The action would be for a single year (2025) and then revisited in 2026."),
                                       h3("How to use this page:"),
                                       p("Use the swing weight excel sheet to conduct your swing weighting. Scores from the sheet can then be plugged into the Shinyapp here. Once swing weight scores are entered, objective weights are automatically calculated along with utility/composite scores. Utility Score Table tab will show the ranking of alternatives based on your objective weights, from best to worst. The line plot tab will show how your ranking of alternatives change between hypotheses/hydrology, if any (where line with highes utility score is your best-ranked alternative). VOI tab shows whether or not there is Value of Information, benefit/gain if you were to wait on the decision by gathering perfect information on which hydrology is more accurate."),
                                       h3("Models"),
                                       h4("CalSim3"),
                                       p("To calculate water operation impacts for each alternative, CalSim3 model was used. CalSim3 is a water resources planning model that simulate operations of the SWP and CVP and much of the water resources infrastructure in the Central Valley of California and the Sacramento-San Joaquin Delta region."),
                                       h4("IBMR v1"),
                                       p("To evaluate population growth under the alternatives and hypotheses, Delta Smelt Individual-Based Model in R (IBMR) was used. IBMR v1 is a monthly time-step simulation/agent-based model for Delta Smelt with an explicit bioenergetics component calibrated to observed conditions in 1995-2014. Bioenergetic parameters came from Rose et al. (2013) publication, which used information from Rainbow Smelt with an assumed temperature effect for Delta Smelt."),
                                       h3("Alternatives"),
                                       p("Below are the brief descriptions for the alternatives evaluated in this structured decision-making effort. Note: Suisun Marsh Salinity Control Gate action is modeled as 60 continuous days of operation unless noted otherwise."),
                                       h4("MaxDS_Even"),
                                       p("Target 20,400 cfs Delta outflow in Jun-Aug and then X2 at 74 km in Sep-Oct, with 20,400 cfs average for Jun-Aug."),
                                       h4("MaxDS_Hist"),
                                       p("Target 20,400 cfs Delta outflow in Jun-Aug and then X2 at 74 km in Sep-Oct, with 30,400 cfs in Jun, 20,400 cfs in Jul, and 10,400 cfs in Aug."),
                                       h4("SummerFall_Even"),
                                       p("Target 10,200 cfs Delta outflow in Jun-Aug and then X2 at 80 km in Sep-Oct , with 10,400 cfs average for Jun-Aug."),
                                       h4("SummerFall_Hist"),
                                       p("Target 10,200 cfs Delta outflow in Jun-Aug and then X2 at 80 km in Sep-Oct , with 17,000 cfs in Jun, and 7,000 cfs in Jul-Aug."),
                                       h4("Summer_Even"),
                                       p("Target 10,200 cfs Delta outflow in Jun-Aug, no fall X2, with 10,400 cfs average for Jun-Aug."),
                                       h4("Summer_Hist"),
                                       p("Target 10,200 cfs Delta outflow in Jun-Aug, no fall X2, with 17,000 cfs in Jun, and 7,000 cfs in Jul-Aug."),
                                       h4("Summer_Even_AltSMSCG"),
                                       p("Target 10,200 cfs Delta outflow in Jun-Aug, no fall X2, with 10,400 cfs average for Jun-Aug. Alternate Suisun Marsh Salinity Control Gate Action with 7 days on/7 days off."),
                                       h4("June"),
                                       p("Target 10,200 cfs Delta outflow in June only, no fall X2."),
                                       h4("StatusQuo"),
                                       p("Operate to 2024 ROD & ITP, with Fall X2 at 80 km in Sep-Oct."),
                                       h4("MaxWater"),
                                       p("Operate to 2024 ROD & ITP, but remove Fall X2 (continue Suisun Marsh Salinity Control Gate action)."),
                                       h4("MaxWater_NoSMSCG"),
                                       p("Operate to 2024 ROD & ITP, but remove Fall X2 and Suisun Marsh Salinity Control Gate action.")),
        tabPanel("Line Plot",plotOutput("Plot")),
        tabPanel("Utility Score Table",tableOutput("tableSum")),tabPanel("VOI",textOutput("VOI_calc1"),textOutput("VOI_calc2")),
        tabPanel("Raw Consequence Table",tableOutput("tableRaw")),
        tabPanel("Normalized Consequence Table",tableOutput("tableNormal")),
        tabPanel("Objective Weights",tableOutput("tableObjWeights"))))
    )
)



# Define server logic required to do things
server <- function(input, output, session) {
  
    # Contact Info
    contacturl <- a("bmahardja@usbr.gov", href="mailto:bmahardja@usbr.gov")
    output$contact <- renderUI({
    tagList(p(HTML("For questions or comments, please contact: <span style='font-weight: bold;'>Brian Mahardja | </span>"), 
              contacturl, style = "font-size: 14px"))
    })
  
    #Reactive objective weight
    O1_weight <- reactive({input$O1_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O2_weight <- reactive({input$O2_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O3_weight <- reactive({input$O3_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O4_weight <- reactive({input$O4_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O5_weight <- reactive({input$O5_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O6_weight <- reactive({input$O6_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O7_weight <- reactive({input$O7_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    O8_weight <- reactive({input$O8_score/(input$O1_score + input$O2_score + input$O3_score + input$O4_score + input$O5_score + input$O6_score + input$O7_score + input$O8_score)})
    
    #Pull together line plot data
    line_plot_data <- reactive({consequence_table_std_long %>% 
            mutate(obj_weight= case_when(Objective == "DeltaSmelt" ~ O1_weight(),
                                         Objective == "CVP_export" ~ O2_weight(),
                                         Objective == "SWP_export" ~ O3_weight(),
                                         Objective == "Redds_dewatered" ~ O4_weight(), 
                                         Objective == "Folsom_probability300TAF" ~ O5_weight(),
                                         Objective == "DeltaHumanHealth" ~ O6_weight(),
                                         Objective == "Shasta_storage" ~ O7_weight(),
                                         Objective == "Oroville_storage" ~ O8_weight())) %>%
            mutate(score_obj = obj_weight*Value) %>% group_by(Alternative,Hydrology) %>%
            summarise(comp_score = sum(score_obj)) %>% mutate(Hypo_weight = case_when(Hydrology == "AdjustedHist" ~ 1.0,
                                                                                    Hydrology == "2022MED" ~ 0)) })
 
    
    output$Plot <- renderPlot({
        #Plot here
        print(ggplot(data=line_plot_data(), aes(x=Hypo_weight, y=comp_score, color=Alternative,linetype=Alternative)) +
                  geom_line(linewidth= 1.2) +
                  geom_vline(xintercept = input$H1_weight, linetype="dotted", color = "red", linewidth=1) +
                  theme_minimal()+
                  labs(title = NULL,
                       x = "Hypothesis 1 weight",
                       y = "Composite score (objective-weighted linear value function)") +
                scale_color_viridis(option = "turbo", discrete = TRUE) + 
                  theme(axis.text = element_text(size = 14),  # Increase tick mark font size
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                        legend.text=element_text(size=14),
                        legend.title=element_text(size=14),
                        axis.title.x = element_text(size=14),
                        axis.title.y = element_text(size=14),
                        legend.key.size = unit(1.2, "cm")))
    })
    
    # Create final utility score table
    cons_table_std_new <- reactive({consequence_table_std_long %>% mutate(obj_weight= case_when(Objective == "DeltaSmelt" ~ O1_weight(),
                                                                                    Objective == "CVP_export" ~ O2_weight(),
                                                                                    Objective == "SWP_export" ~ O3_weight(),
                                                                                    Objective == "Redds_dewatered" ~ O4_weight(), 
                                                                                    Objective == "Folsom_probability300TAF" ~ O5_weight(),
                                                                                    Objective == "DeltaHumanHealth" ~ O6_weight(),
                                                                                    Objective == "Shasta_storage" ~ O7_weight(),
                                                                                    Objective == "Oroville_storage" ~ O8_weight())) %>%
            mutate(score_obj = obj_weight*Value) %>% group_by(Alternative,Hydrology) %>%
            summarise(comp_score = sum(score_obj)) %>% mutate(hypo_weight = case_when(Hydrology == "AdjustedHist" ~ input$H1_weight,
                                                                                      Hydrology == "2022MED" ~ 1-input$H1_weight)) %>%
            mutate(comp_score_hyp = comp_score*hypo_weight) %>% ungroup() %>% group_by(Alternative) %>%
            summarise(CompositeScore = sum(comp_score_hyp))})
    
    output$tableSum <- renderTable({cons_table_std_new() %>% arrange(desc(CompositeScore))},digits=5)
    
    # Add VOI calculation tables
    cons_table_reconfig <- reactive({consequence_table_std_long %>% mutate(obj_weight= case_when(Objective == "DeltaSmelt" ~ O1_weight(),
                                                                                                 Objective == "CVP_export" ~ O2_weight(),
                                                                                                 Objective == "SWP_export" ~ O3_weight(),
                                                                                                 Objective == "Redds_dewatered" ~ O4_weight(), 
                                                                                                 Objective == "Folsom_probability300TAF" ~ O5_weight(),
                                                                                                 Objective == "DeltaHumanHealth" ~ O6_weight(),
                                                                                                 Objective == "Shasta_storage" ~ O7_weight(),
                                                                                                 Objective == "Oroville_storage" ~ O8_weight()))  %>%
            mutate(composite_score = Value*obj_weight) %>%
            group_by(Alternative,Hydrology) %>%
            summarise(composite_score = sum(composite_score)) %>%
            mutate(hypo_weight = case_when(Hydrology == "AdjustedHist" ~ input$H1_weight,
                                           Hydrology == "2022MED" ~ 1-input$H1_weight))})
            
    certainty_calc <- reactive({ cons_table_reconfig() %>% 
            group_by(Hydrology) %>% 
            summarise(composite_score = max(composite_score), hypo_weight = mean(hypo_weight)) %>%
            mutate(hypothesis_score = composite_score * hypo_weight) })
    
    uncertainty_calc <- reactive({ cons_table_reconfig() %>% 
            mutate(composite_score_hypo = hypo_weight * composite_score) %>% 
            group_by(Alternative) %>% 
            summarise(composite_score = sum(composite_score_hypo)) })
    # Show text on VOI
    output$VOI_calc1 <- renderText({
        paste("Value of Perfect Information (Composite Score):",round(sum(certainty_calc()$hypothesis_score)-max(uncertainty_calc()$composite_score),digits=4))
    })
    output$VOI_calc2 <- renderText({
        paste("Value of Perfect Information (% of Best composite Score):",paste(round((sum(certainty_calc()$hypothesis_score)-max(uncertainty_calc()$composite_score))/max(cons_table_std_new()$CompositeScore)*100,digits=4)),"%")
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
        if ((input$O1_score+input$O2_score+input$O3_score+input$O4_score+input$O5_score+input$O6_score+input$O7_score+input$O8_score)<=0) {
            tags$div(style = "color: red;", "Warning: Score need to be >0 for at least one objective")
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
    # Normalized consequence table
    output$tableNormal <- renderTable({consequence_table_std},digits=4)
    # Summary of Objective Weights
    output$tableObjWeights <- renderTable({data.frame(Objective=c("Delta Smelt Persistence","CVP Export","SWP Export","Winter-run: Redds dewatered","Fall-run/Steelhead: Folsom","Delta Water Quality","CVP Coldwater Pool (Shasta)","SWP Coldwater Pool (Oroville)"),Weight=c(O1_weight(),O2_weight(),O3_weight(),O4_weight(),O5_weight(),O6_weight(),O7_weight(),O8_weight()))},digits=4)
}

# Run the application 
shinyApp(ui = ui, server = server)
