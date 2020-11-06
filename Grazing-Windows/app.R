rm(list = ls())

library(shiny)
library(tidyverse)
library(lubridate)
library(gamm4)
library(ggrepel)

# pre processing
load(file = 'model_data.rda')

top_gam <- readRDS(file = 'top_gam.rds')

pred_grid <-
  pred_grid %>%
  select(treatment:Cattle)

duration_df <-
  pred_grid %>%
  filter(Cattle == "Present") %>%
  group_by(Rotation) %>%
  summarise(duration = n())

ytitle <- expression(Predicted~italic(E.~coli)~MPN~100*ml^-1)
ytitle2 <- "Predicted proportion of days above Utah DWQ Limit (320 MPN)"
xtitle <- 'Day of Year'

# Make dataframe for plot annotations: ----------- # 
label_df1 <- expand.grid( xpos = 300, 
                          Limit = c(320, 410, 668), 
                          Rotation = c('Continuous-Turnout', 
                                       'Deferred-Rotation', 
                                       'Time-Controlled Rotation')) %>% 
  mutate( Rotation = factor( Rotation, 
                             levels = c('Continuous-Turnout', 
                                        'Deferred-Rotation', 
                                        'Time-Controlled Rotation'), 
                             ordered = T)) 

label_df2 <- data.frame( Limit = c(668, 320, 410), 
                         label = c('Utah DWQ', 'EPA 75th', 'EPA 90th'))

label_df <- 
  label_df1 %>%
  left_join(label_df2 ) %>% 
  filter( Rotation == 'Continuous-Turnout')

# ------------ # 

plot_title <- expression('Predicted Daily Stream'~italic(E.~coli))  

# Define UI for application

ui <- fluidPage(
  titlePanel(p("Effects of Grazing Rotation on Stream", em('E. coli'), "Levels")), 
  hr(), 
  fluidRow( 
    column(7,  
      mainPanel('The number of days that stream E. coli levels are expected to exceed regulatory limits depends on the start date and duration of the grazing treatment. Explore the effects of changing the grazing start date using the sliders to the left 
                of the plots. ', align = 'left')
      )
    ), 
  hr(), 
  fluidRow(
    column( 2,
            sliderInput(
              "long_DOY",
              "Start date for Continuous Turnout",
              min = 130,
              max = 307 - 123,
              value = 150
            ),
            sliderInput(
              "medium_DOY",
              "Start date for Deferred Rotation",
              min = 130,
              max = 307 - 49,
              value = 150
            ),
            sliderInput(
              "short_DOY",
              "Start date for Time-Controlled Rotation",
              min = 130,
              max = 307 - 18,
              value = 150
            ),
            tags$style(".shiny-plot-output{height:80vh !important;}")
    ), 
    column( 5, 
            plotOutput("calendar_plot")),
    column( 4,
            plotOutput("days_exceeding_plot")
            )
  ),
  hr(), 
  fluidRow(
    column(5, 
      offset = 1, 
      tableOutput("ndays")
    )
  )
)

# Define server logic
server <- function(input, output) {
  default_start_DOY <- reactive({
    data.frame(
      "Rotation" = c('Continuous-Turnout', 
                     'Deferred-Rotation', 
                     'Time-Controlled Rotation'),
      "start_DOY" = c(input$long_DOY, 
                      input$medium_DOY, 
                      input$short_DOY)
    )
  })
  
  pred_grid_1 <- reactive({
    pred_grid %>%
      left_join(default_start_DOY()) %>%
      left_join(duration_df) %>%
      mutate(
        Cattle = "Absent",
        grazing_start = start_DOY,
        grazing_end = start_DOY + duration
      ) %>%
      mutate(Cattle = ifelse(DOY >= grazing_start &
                               DOY <= grazing_end, "Present", Cattle)) %>%
      ungroup() %>% 
      filter( DOY >= 121,  
              DOY <= 273) # Only count days inside recreation water quality season
  })
  
  yhat <- reactive({
    predict(
      top_gam$gam,
      newdata = pred_grid_1(),
      type = 'response',
      se.fit = T
    ) %>%
      as.data.frame()
  })
  
  pred_ndays <- reactive({ 
    pred_ndays_above(top_gam,
                     new_data = pred_grid_1(), 
                     threshholds = threshholds) %>%
      mutate( y = q50/n, ymin = q2.5/n, ymax = q97.5/n) %>%  
      mutate( label = paste0( q50, ' out of\n', n, ' days')) %>% 
      mutate( Rotation = 
                factor( treatment, 
                labels = c('Continuous-Turnout', 
                           'Deferred-Rotation',  
                           'Time-Controlled Rotation')))  
  })
  
  pred_grid_2 <- reactive({
    pred_grid_1() %>%
      mutate(yhat = 10 ^ (yhat()$fit)) %>%
      mutate(lcl = 10 ^ (yhat()$fit - 2 * yhat()$se.fit)) %>%
      mutate(ucl = 10 ^ (yhat()$fit + 2 * yhat()$se.fit))
  })
  
  
  pred_grid_windows <- reactive({
    pred_grid_1() %>%
      group_by(Rotation, Cattle) %>%
      filter(Cattle == 'Present') %>%
      summarise(DOY1 = min(DOY), 
                DOY2 = max(DOY)) %>%
      mutate(DOY = 200, 
             ecoli_MPN = 100) %>%
      ungroup()
  })
  

  output$calendar_plot <- renderPlot({
    # re-draw plot with new data
    fig2 +
      geom_point(color = NA) +
      geom_ribbon(
        data = pred_grid_2(),
        aes(
          x = DOY,
          y = yhat,
          ymin = lcl,
          ymax = ucl,
          fill = Rotation,
          color = NULL
        ),
        alpha = 0.4
      ) +
      geom_line(
        data = pred_grid_2(),
        aes(x = DOY, y = yhat, color = Rotation),
        alpha = 1,
        size = 1.5
      ) +
      geom_rect(
        data = pred_grid_windows(),
        aes(
          xmin = DOY1,
          xmax = DOY2,
          ymin = 0,
          ymax = Inf
        ),
        alpha = 0.2,
        color = NA,
        fill = 'gray'
      ) +
      geom_label_repel( 
        data = label_df, 
        aes( x = xpos, 
             y = Limit, 
             label = label), 
        show.legend = F, fill = 'white', color = 'black') + 
      ylab(ytitle) + 
      xlab(xtitle) + 
      scale_color_manual(values = mycolors2, name = 'Rotation', guide = 'none') + 
      scale_fill_manual(values = mycolors2, name = 'Rotation', guide = 'none') +
      scale_linetype_manual(guide = "none", values = c(1:3)) +
      facet_wrap(~ Rotation , ncol = 1) + 
      theme(axis.title = element_text(size = 14), 
            legend.text = element_text( size = 12), 
            legend.title =  element_text(size = 14), 
            axis.text = element_text(size = 10))

  })
  
  output$days_exceeding_plot <- renderPlot({
    
    pred_ndays() %>%
      filter( Limit == 668) %>% 
      ggplot(aes(
        x = Rotation,
        y = y,
        ymin = ymin,
        ymax = ymax, 
        color = Rotation)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +
      geom_errorbar(position = position_dodge(width = 0.5)) +
      geom_label( aes(label = label),
                  position = position_dodge(width = 0.5),
                  color = 'black',
                  fill = 'white',
                  hjust = -0.09 ) +
      scale_color_manual(values = mycolors2[3:1], guide = 'none') +
      ylab(ytitle2) +
      theme_bw() +
      theme(
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank()
      ) + 
      ylim( c(0, 0.75))
    
  })

  output$ndays <- renderTable({

    pred_ndays() %>% 
      select(Rotation, Limit, q50, n ) %>%
      spread( Rotation, q50) %>%
      rename( 'Total Number of Days' = n) %>% 
      left_join(threshholds) %>% 
      select( Threshold, Limit, 
              `Continuous-Turnout`:`Time-Controlled Rotation`, 
              `Total Number of Days`) %>%
      rename( 'E. coli (MPN/100 ml)' = Limit )
  
  }, 
  digits = 0,
  caption = 'Table 1. Number of days per year that E. coli 
  levels would be predicted to exceed regulatory limits in 
  each grazing treatment. Adjust the grazing schedule with 
  the sliders on the left.',
  
  caption.placement = getOption("xtable.caption.placement", "bottom"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )

}

# Run the application
shinyApp(ui = ui, server = server)
