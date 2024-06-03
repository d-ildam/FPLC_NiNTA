library(shiny)
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(shinythemes)
library(shinyFiles)
library(rsconnect)

# Define UI
ui <- fluidPage(
  theme = shinytheme("readable"),
  titlePanel("AKTAprime plus FPLC NiNTA Plots"),
  tabsetPanel(
    tabPanel("Time Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_time", "Please select your file:"),
                 textInput("plot_title_time", "Plot Title:"),
                 textInput("x_axis_time", "Please write x-axis name:", "Time (minute)"),
                 textInput("left_y_axis_time", "Please write left y-axis name:", "Absorbance at 280 nm (mAu)"),
                 textInput("right_y_axis_time", "Please write right y-axis name:", "Elution Buffer Percentage (%B)"),
                 numericInput("plot_height_time", "Plot Height (pixels):", value = 600),
                 numericInput("plot_width_time", "Plot Width (pixels):", value = 800),
                 downloadButton("download_time_plot", "Download Plot")
               ),
               mainPanel(
                 plotOutput("plot_time"),
                 uiOutput("time_plot_error")
               )
             )
    ),
    tabPanel("Volume Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_volume", "Please select your file:"),
                 numericInput("flow_rate_volume", "Enter the flow rate:", value = 0.8),
                 textInput("plot_title_volume", "Plot Title:"),
                 textInput("x_axis_volume", "Please write x-axis name:", "Volume (mL)"),
                 textInput("left_y_axis_volume", "Please write left y-axis name:", "Absorbance at 280 nm (mAu)"),
                 textInput("right_y_axis_volume", "Please write right y-axis name:", "Elution Buffer Percentage (%B)"),
                 numericInput("plot_height_volume", "Plot Height (pixels):", value = 600),
                 numericInput("plot_width_volume", "Plot Width (pixels):", value = 800),
                 downloadButton("download_volume_plot", "Download Plot")
               ),
               mainPanel(
                 plotOutput("plot_volume"),
                 uiOutput("volume_plot_error")
               )
             )
    ),
    tabPanel("Fraction Tube Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_tube", "Please select your file:"),
                 numericInput("flow_rate_tube", "Enter the flow rate:", value = 0.8),
                 textInput("plot_title_tube", "Plot Title:"),
                 textInput("x_axis_tube", "Please write x-axis name:", "Fraction Number"),
                 textInput("left_y_axis_tube", "Please write left y-axis name:", "Absorbance at 280 nm (mAu)"),
                 textInput("right_y_axis_tube", "Please write right y-axis name:", "Elution Buffer Percentage (%B)"),
                 numericInput("plot_height_tube", "Plot Height (pixels):", value = 600),
                 numericInput("plot_width_tube", "Plot Width (pixels):", value = 800),
                 downloadButton("download_tube_plot", "Download Plot")
               ),
               mainPanel(
                 plotOutput("plot_tube"),
                 uiOutput("tube_plot_error")
               )
             )
    )
  ),
  
  
  tags$div(
    tags$p("This shiny application allows to upload FPLC data files of Nickel-NTA purification and generate publication-ready plots."),
    tags$p("The data have been taken from AKTAprime plus FPLC system and app may be inapplicable to other systems."),
    tags$p("AKTAprime plus FPLC NiNTA Plots by Ildam, D. (OKE Lab)"),
    style = "margin-bottom: 20px;"
  )
)


# Define server logic
server <- function(input, output) {
  
  # Time Plot
  time_plot <- reactive({
    req(input$file_time)
    raw_df <- read_excel(input$file_time$datapath)
    selected_df <- raw_df[, 1:4]
    cleaned_df <- selected_df[-(1:3), -3] %>%
      setNames(c("min", "mAu", "%B")) %>%
      mutate(across(everything(), as.numeric))
    filtered_df <- cleaned_df %>%
      filter(`%B` > 0)
    result_df <- filtered_df %>%
      mutate(subtracted_min = min - first(min))
    scaleFactor <- max(result_df$mAu) / max(result_df$`%B`)
    
    ggplot(result_df, aes(x = subtracted_min)) +     
      geom_line(aes(y = mAu), linewidth = 1, colour = "black") +  
      geom_line(aes(y = `%B` * scaleFactor), linewidth = 0.8, colour = "blue") +  
      scale_y_continuous(
        name = input$left_y_axis_time, 
        n.breaks = 10, 
        sec.axis = sec_axis(~./scaleFactor, name = input$right_y_axis_time),
      ) + 
      scale_x_continuous(name = input$x_axis_time, breaks = seq(0, max(result_df$min) + 1)) + 
      ggtitle(input$plot_title_time) +    
      theme_pubr() +   
      labs_pubr() +   
      theme(
        axis.title.y = element_text(face = "bold", color = "black", size = 13),    
        axis.title.y.right = element_text(face = "bold", color = "blue", size = 13),   
        axis.text.y.right = element_text(color = "blue"),
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.x = element_text(face = "bold", size = 13),   
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$plot_time <- renderPlot({
    time_plot()
  }, height = function() { input$plot_height_time }, width = function() { input$plot_width_time })
  
  output$download_time_plot <- downloadHandler(
    filename = function() {
      paste("time_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = time_plot(), dpi = "retina", width = input$plot_width_time/100, height = input$plot_height_time/100, limitsize = FALSE)
    }
  )
  
  # Volume Plot
  volume_plot <- reactive({
    req(input$file_volume)
    raw_df <- read_excel(input$file_volume$datapath)
    selected_df <- raw_df[, 1:4]
    cleaned_df <- selected_df[-(1:3), -3] %>%
      setNames(c("min", "mAu", "%B")) %>%
      mutate(across(everything(), as.numeric))
    filtered_df <- cleaned_df %>%
      filter(`%B` > 0)
    flow_rate <- input$flow_rate_volume
    result_df <- filtered_df %>%
      mutate(subtracted_min = min - first(min)) %>% 
      mutate(volume = subtracted_min * flow_rate)
    scaleFactor <- max(result_df$mAu) / max(result_df$`%B`)
    
    ggplot(result_df, aes(x = volume)) +     
      geom_line(aes(y = mAu), linewidth = 1, colour = "black") +  
      geom_line(aes(y = `%B` * scaleFactor), linewidth = 0.8, colour = "blue") +  
      scale_y_continuous(
        name = input$left_y_axis_volume, 
        n.breaks = 10, 
        sec.axis = sec_axis(~./scaleFactor, name = input$right_y_axis_volume)
      ) + 
      scale_x_continuous(name = input$x_axis_volume, breaks = seq(0, max(result_df$min))) + 
      ggtitle(input$plot_title_volume) +    
      theme_pubr() +   
      labs_pubr() +   
      theme(
        axis.title.y = element_text(face = "bold", color = "black", size = 13),    
        axis.title.y.right = element_text(face = "bold", color = "blue", size = 13),   
        axis.text.y.right = element_text(color = "blue"),
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.x = element_text(face = "bold", size = 13),   
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$plot_volume <- renderPlot({
    volume_plot()
  }, height = function() { input$plot_height_volume }, width = function() { input$plot_width_volume })
  
  output$download_volume_plot <- downloadHandler(
    filename = function() {
      paste("volume_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = volume_plot(), dpi = "retina", width = input$plot_width_volume/100, height = input$plot_height_volume/100, limitsize = FALSE)
    }
  )
  
  # Fraction Tube Plot
  tube_plot <- reactive({
    req(input$file_tube)
    raw_df <- read_excel(input$file_tube$datapath)
    selected_df <- raw_df[, 1:4]
    cleaned_df <- selected_df[-(1:3), -3] %>%
      setNames(c("min", "mAu", "%B")) %>%
      mutate(across(everything(), as.numeric))
    filtered_df <- cleaned_df %>%
      filter(`%B` > 0)
    flow_rate <- input$flow_rate_tube
    result_df <- filtered_df %>%
      mutate(subtracted_min = min - first(min)) %>% 
      mutate(volume = subtracted_min * flow_rate) %>% 
      mutate(tube = volume / 1 + 1) 
    scaleFactor <- max(result_df$mAu) / max(result_df$`%B`)
    rounded_tube_numbers <- round(result_df$tube)
    minor_label <- seq(min(rounded_tube_numbers), max(rounded_tube_numbers))
    minor_break_seq <- seq(min(rounded_tube_numbers) + 0.5 ,max(rounded_tube_numbers) + 0.5)
    
    ggplot(result_df, aes(x = tube)) +     
      geom_line(aes(y = mAu), linewidth = 1, colour = "black") +  
      geom_line(aes(y = `%B` * scaleFactor), linewidth = 0.8, colour = "blue") +  
      scale_y_continuous(
        name = input$left_y_axis_tube, 
        n.breaks = 10, 
        sec.axis = sec_axis(~./scaleFactor, name = input$right_y_axis_tube)
      ) + 
      scale_x_continuous(
        name = input$x_axis_tube, 
        breaks = rounded_tube_numbers, 
        minor_breaks = minor_break_seq, 
        guide = guide_axis(minor.ticks = TRUE),
        labels = NULL
      ) +
      ggtitle(input$plot_title_tube) +    
      theme_pubr() + 
      labs_pubr() +    
      geom_segment(
        aes(x = rounded_tube_numbers, xend = rounded_tube_numbers, y = -Inf, yend = 0),
        color = "black"
      ) +  
      theme(
        axis.title.y = element_text(face = "bold", color = "black", size = 13),     
        axis.title.y.right = element_text(face = "bold", color = "blue", size = 13),   
        axis.line.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.x = element_text(face = "bold", size = 13),  
        plot.title = element_text(hjust = 0.5)
      ) +
      annotate(
        "text",
        x = minor_break_seq,
        y = rep(-0.5, length(minor_break_seq)),
        label = minor_label,
        fontface = 2,
        vjust = +1.7
      )
  })
  
  output$plot_tube <- renderPlot({
    tube_plot()
  }, height = function() { input$plot_height_tube }, width = function() { input$plot_width_tube })
  
  output$download_tube_plot <- downloadHandler(
    filename = function() {
      paste("tube_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = tube_plot(), dpi = "retina", width = input$plot_width_tube/100, height = input$plot_height_tube/100, limitsize = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

