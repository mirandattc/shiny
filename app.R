########################################
# Shiny app for HIV spend Model comparison
# created on 10/13/17
# Update on 09/18/18
# Author: Miranda Tao
#####################################

# load packages
library(shinydashboard);library(shiny);library(DT);library(scales)
library(magrittr);library(tools);library(ini)
library(dplyr);library(data.table);
library(ggplot2); library(plotly);library(gridExtra); library(grid)
library(leaflet); library(sp)
library(shinythemes); library(rgeos)
library(RColorBrewer); library(tidyr)
library(gtools)


########### GET DATA ###########
hiv_dt <- fread("./data/hiv_models.csv")
hiv_dt <- hiv_dt[,model := as.character(factor(model, labels = c('model 1','model 2')))]

mal_dt <- fread("./data/malaria_models.csv")
mal_dt[,loc_label := paste0(ihme_loc_id," | ",location_name)]

category <- unique(hiv_dt$value_code)

#====================================================================================================

## Get map shape file and remove subnationals
load("./data/map.RData")
map_sf <- map[!grepl("_",map@data$ihme_lc_id),]


############# UI ############
# create user interface
ui <- shinyUI(fluidPage(theme = shinytheme("united"),

                        titlePanel(h2("Global Disease Expenditure Data Vetting & Model Evaluation")),

                        # horirzontal line
                        hr(),
                        navbarPage("Disease",
                                   tabPanel("HIV",
                                            # box1: all selections in side bar
                                            sidebarPanel(width = 3,

                                                         # select a measurement to display (mult by prevalence?)
                                                         conditionalPanel(condition = "input.tabselected==1 | input.tabselected==2 |input.tabselected==3 | input.tabselected==5",
                                                                          radioButtons('measure', h4("Type of Measurement"),
                                                                                       choices=c(
                                                                                         "Expenditure Per HIV case" = 2,
                                                                                         "Expenditure, in Million" = 3,
                                                                                         "Expenditure Per ART covered life" = 4,
                                                                                         "Proportion of total spend on HIV" = 5,
                                                                                         "Proportion of overall health spend" = 6,
                                                                                         "Proportion of subtotal (DAH/Domestic/Function)" = 7),
                                                                                       selected = 3),
                                                                          hr()
                                                         ),

                                                         # select a value code to display, for map
                                                         conditionalPanel(condition = "input.tabselected==1 | input.tabselected==5",
                                                                          selectInput("category_map",
                                                                                      label = h4("Pick an HIV spend category"),
                                                                                      choices = category),
                                                                          hr()
                                                         ),

                                                         # select a value code to display, for time series and line graph
                                                         conditionalPanel(condition = "input.tabselected==2 | input.tabselected==3",
                                                                          selectInput("category_ts",
                                                                                      label = h4("Pick HIV spend category(s)"),
                                                                                      choices = category,
                                                                                      multiple = T),
                                                                          hr()
                                                         ),



                                                         # select model(s) to display/compare, for map and line graph
                                                         conditionalPanel(condition = "input.tabselected==1 | input.tabselected==3",
                                                                          radioButtons("model", h4("Pick model"),
                                                                                       choices = unique(hiv_dt$model)
                                                                          ),
                                                                          hr()
                                                         ),

                                                         ## conditional on line graph: select countries to display
                                                         conditionalPanel(condition = "input.tabselected==3",
                                                                          selectInput("country_line", label = h4("Pick your countries"),
                                                                                      choices = sort(unique(hiv_dt$loc_label)),
                                                                                      multiple = T
                                                                          ),
                                                                          hr()
                                                         ),

                                                         ## conditional on line graph: which level division? by income? super-region? HIV burden?
                                                         conditionalPanel(condition = "input.tabselected==3",
                                                                          radioButtons("level", h4("Pick a high level division"),
                                                                                       choices=c("WB income level" = 1,
                                                                                                 "GBD super-region" = 2,
                                                                                                 "HIV burden" = 3),
                                                                                       selected = 1)
                                                         ),



                                                         ## conditional on time series plot(2): select a country to display
                                                         conditionalPanel(condition = "input.tabselected==2",
                                                                          selectInput("country", label = h4("Pick a Country"),
                                                                                      choices = sort(unique(hiv_dt$loc_label)),
                                                                                      selected = "MWI | Malawi"),
                                                                          hr()
                                                         ),



                                                         # conditional on time series plot(2): which model to show, multiple
                                                         conditionalPanel(condition = "input.tabselected==2",
                                                                          selectInput("model_ts",
                                                                                      label = h4("Pick your model(s)"),
                                                                                      choices = unique(hiv_dt$model),
                                                                                      multiple = T,
                                                                                      selected = unique(hiv_dt$model)
                                                                          ),
                                                                          hr()
                                                         ),

                                                         # conditional on scatter plot(2): which two models to compare, 2 max
                                                         conditionalPanel(condition = "input.tabselected==5",
                                                                          selectizeInput("model_scatter",
                                                                                      label = h4("Pick two models to compare"),
                                                                                      choices = unique(hiv_dt$model),
                                                                                      multiple = T,options = list(maxItems = 2),
                                                                                      selected = unique(hiv_dt$model)
                                                                          )
                                                         ),

                                                         ## conditional on times series and line plot: select years
                                                         conditionalPanel(condition = "input.tabselected==2|input.tabselected==3",
                                                                          sliderInput("year_range",
                                                                                      label = h4("Year Range"),
                                                                                      min = 2000,
                                                                                      max = 2040,
                                                                                      value = c(2000, 2040), sep = "")
                                                         ),

                                                         ## conditional on time series plot(2): show original data?
                                                         conditionalPanel(condition = 'input.tabselected==2',
                                                                          radioButtons("data", label = h4("Show data"),
                                                                                       choices = c(
                                                                                         "data: cookd outlier detection, logit fraction" = 4,
                                                                                         "none" = 5
                                                                                       ),
                                                                                       selected = 5)
                                                         ),



                                                         ## conditional on map(1): select a year to display
                                                         conditionalPanel(condition = "input.tabselected==1|input.tabselected==5",
                                                                          sliderInput("year", label = h4("Slide to a year"),
                                                                                      min = 2000,
                                                                                      max = 2040,
                                                                                      value = 2015, sep = "")
                                                         )
                                            ),


                                            # box2: output tabs
                                            mainPanel(width = 9,
                                                      tabsetPanel(type = 'pills',
                                                                  tabPanel("Time series", value =2,
                                                                           checkboxGroupInput("show_lines",
                                                                                              label = '',
                                                                                              choices = c('Show mean'=1, 'Show UIs'=2),
                                                                                              selected = c(1,2),
                                                                                              inline = T),
                                                                           plotlyOutput("ts_plot", height = 800)
                                                                           ),
                                                                  tabPanel("Map", value=1, leafletOutput("map_plot" ,width = "100%",height = 800)),
                                                                  tabPanel("Scatter plot", value=5, plotlyOutput("scatter_plot", height = 800)),
                                                                  tabPanel("Line graph", value=3, plotlyOutput("line_plot", height = 800)),
                                                                  tabPanel("Table", value = 4, DT::dataTableOutput("table")),
                                                                  id = 'tabselected')
                                                      )
                                            ),
                                   #========================================================================================================
                                   #                                                MALARIA UI
                                   #========================================================================================================

                                   tabPanel("Malaria",
                                            sidebarPanel(width = 3,
                                                         # Category for time series
                                                         conditionalPanel("input.tabselected_mal==1",
                                                                          selectInput("country_mal", label = h4("Pick a Country"),
                                                                                      choices = sort(unique(mal_dt$loc_label)),
                                                                                      selected = "MWI | Malawi"),
                                                                          hr()
                                                         ),



                                                         radioButtons('measure_mal', h4("Type of Measurement"),
                                                                     choices=c("Logit fraction (model space)" = 0,
                                                                               "Proportion of overall health spend minus HIV" = 1,
                                                                               "Expenditure per malaria case" = 2,
                                                                               "Expenditure per capita" = 3,
                                                                               "Expenditure, in Million" = 4,
                                                                               "Expenditure" = 5
                                                                       ),
                                                                     selected = 1),
                                                         hr(),



                                                         # Category for time series
                                                         conditionalPanel("input.tabselected_mal==1",
                                                                          selectInput("category_ts_mal",
                                                                                      label = h4("Pick Malaria spend category(s)"),
                                                                                      choices = unique(mal_dt$value_code),
                                                                                      multiple = T),
                                                                          hr()
                                                         ),

                                                         # Category for map
                                                         conditionalPanel("input.tabselected_mal==2 | input.tabselected_mal==4",
                                                                          selectInput("category_map_mal",
                                                                                      label = h4("Pick a Malaria spend category"),
                                                                                      choices = unique(mal_dt$value_code),
                                                                                      multiple = F),
                                                                          hr()
                                                                          ),

                                                         # conditional on time series plot(2): which model to show, multiple
                                                         conditionalPanel(condition = "input.tabselected_mal==1",
                                                                          selectInput("model_ts_mal",
                                                                                      label = h4("Pick your model(s)"),
                                                                                      choices = unique(mal_dt$model),
                                                                                      multiple = T,
                                                                                      selected = unique(mal_dt$model)
                                                                          )),

                                                         # conditional on time series plot(2): which model to show, multiple
                                                         conditionalPanel(condition = "input.tabselected_mal==4",
                                                                          selectizeInput("model_scatter_mal",
                                                                                      label = h4("Pick two models to compare"),
                                                                                      choices = unique(mal_dt$model),
                                                                                      multiple = T,options = list(maxItems = 2),
                                                                                      selected = unique(mal_dt$model)
                                                                          )),


                                                         # conditional on map: which model to show, single
                                                         conditionalPanel(condition = "input.tabselected_mal==2",
                                                                          selectInput("model_map_mal",
                                                                                      label = h4("Pick a model"),
                                                                                      choices = unique(mal_dt$model),
                                                                                      multiple = F,
                                                                                      selected = unique(mal_dt$model)
                                                                          )),

                                                         ## conditional on map: select a year to display
                                                         conditionalPanel(condition = "input.tabselected_mal==2 | input.tabselected_mal==4",
                                                                          sliderInput("year_mal", label = h4("Slide to a year"),
                                                                                      min = 2000,
                                                                                      max = 2015,
                                                                                      value = 2015, sep = ""))
                                                         ),



                                            mainPanel(width = 9,
                                                      tabsetPanel(type = 'pills',

                                                                  tabPanel("Time series", value =1,
                                                                           checkboxGroupInput("show_lines_mal",
                                                                                              label = '',
                                                                                              choices = c('Show gpr mean'=1,
                                                                                                          'Show gpr UIs'=2,
                                                                                                          'Show data'=3),
                                                                                              selected = c(1,2,3),
                                                                                              inline = T),
                                                                           plotlyOutput("ts_plot_mal", height = 800)),
                                                                  tabPanel("Map", value=2, leafletOutput("map_plot_mal" ,width = "100%",height = 800)),
                                                                  tabPanel("Scatter plot", value=4, plotlyOutput("scatter_plot_mal", height = 800)),
                                                                  tabPanel("Table", value = 3, DT::dataTableOutput("table_mal")),
                                                                  id = 'tabselected_mal')
                                            )
                                   ),

                              #==================================================================================================
                              #                                         Turberculosis UI
                              #==================================================================================================

                                   tabPanel("TB")
                                   )

                        )
)








server <- function(input, output,session) {


  ###############################################################################################################################################################
  #                                                                     HIV MODULE
  ###############################################################################################################################################################
  ## data transformation, log space, per prev, pr expenditure?
  first_pass <- function() {

    # Expenditure space
    dtp <- copy(hiv_dt)

    value <- c('mean', 'upper', 'lower','data')

    # Per prev, logged
    if(input$measure == 1) {
      dtp[, eval(value):= lapply(eval(value), function(x) log(get(x)/val_prev))]

      # per prev, unlogged
    }else if (input$measure == 2){
      dtp[, eval(value):= lapply(eval(value), function(x) get(x)/val_prev)]

    }else if (input$measure == 3){
      dtp[, eval(value):= lapply(eval(value), function(x) get(x)/1000000)]

    }else if (input$measure == 4){
      dtp[, eval(value):= lapply(eval(value), function(x) get(x)/(art_coverage*val_prev))]
      dtp[is.infinite(mean), mean := 0]
      dtp[is.infinite(upper), upper := 0]
      dtp[is.infinite(lower), lower := 0]
      dtp[is.infinite(data), data := 0]

    }else if (input$measure == 5){
      dtp[, eval(value) := lapply(c('frachiv_mean','frachiv_upper','frachiv_lower'), function(x) get(x))]
      dtp[,data := NA]
    }else if (input$measure == 6){
      dtp[, eval(value) := lapply(c('fractot_mean','fractot_upper','fractot_lower'), function(x) get(x))]
      dtp[,data := NA]
    } else{
      dtp[, eval(value) := lapply(c('fracsubtot_mean','fracsubtot_upper','fracsubtot_lower'), function(x) get(x))]
      dtp[,data := NA]
    }

    return(dtp)

  }
  dt_first_pass <- reactive({ first_pass() })


  ## filter data for map: value_code, year and model
  map_dt <- reactive({

    dt0 <- unique(dt_first_pass()[,.(value_code, ihme_loc_id, location_name, year_id, model, lower, mean, upper,super_region_name, income_group)])
    dt1 <- dt0[value_code == input$category_map & year_id == input$year & model == input$model]
    return(dt1)

  })


  #
  observe({
    code_update <- unique(dt_first_pass()[!is.na(mean),value_code])
    updateSelectInput(session= session, inputId="category_ts", choices=code_update, selected = code_update[1])
    updateSelectInput(session= session, inputId="category_map", choices=code_update, selected = code_update[1])
  })


  ## filter data for time series: value_code, country and model
  ts_dt <- reactive({

    dt <- dt_first_pass()[value_code %in% input$category_ts
                          & loc_label %in% input$country
                          & model %in% input$model_ts
                          & year_id %in% c(input$year_range[1]:input$year_range[2])
                          ]

    return(dt)

  })

  ## filter data for line graph: value_code, level division and model
  line_dt <- reactive({

    dt <- dt_first_pass()[value_code %in% input$category_ts
                          & model %in% input$model
                          & year_id %in% c(input$year_range[1]:input$year_range[2])
                          ]

    dt[, highlight := ifelse(loc_label %in% input$country_line, T, F)]

    if (input$level == 1){
      dt[, level := income_group]
    }else if(input$level == 2){
      dt[, level := super_region_name]
    }else{
      dt[, level := hiv_prev_group]
    }

    return(dt)

  })

  ## filter data for scatter plot: value_code, model, year
  scatter_dt <- reactive({

    dt <- dt_first_pass()[value_code %in% input$category_map
                          & model %in% input$model_scatter
                          & year_id == input$year
                          ,.(location_name, year_id, value_code, mean, model)]

    # print(dt)
    dt <- unique(dt) %>%
      mutate(model = as.character(factor(model, labels = c('model1','model2')))) %>%
      spread(model, mean) %>% data.table

    print(dt)
    return(dt)

  })



  #======================
  #--- Leaflet Map ---
  #======================
  map_reactive <- reactive({

    ## merge map data with shape file
    map1 <- sp::merge(map_sf,map_dt(), by.x = "ihme_lc_id", by.y = "ihme_loc_id", all.y = T)

    ## Set color palette
    clrs <- brewer.pal(10, "RdYlGn")
    # if(input$measure != 1){
    #    clrs <- brewer.pal(30, "Greens")
    # }

    pal <- colorNumeric(palette = clrs, domain = map1@data$mean)


    ## Create label with ensemble_mean, ensemble_upper and ensemble_lower, DO NOT ROUND?
    map_label <- lapply(seq(nrow(map1@data)), function(i)
      paste0('<p>',map1@data[i,'location_name'],": ", round(map1@data[i,'mean'])," (",
             round(map1@data[i,'lower'])," - ",round(map1@data[i,'upper']),")",'<p></p>',
             'Super-reg: ',map1@data[i,'super_region_name'],'</p><p>',
             "WB group: ", map1@data[i,'income_group'], '</p>'))

    initial_lat = 35
    initial_lng =20
    initial_zoom = 2

    # Contruct the map, based on selected year
    leaflet(data=map1) %>%
      addPolygons(data=map1,
                  stroke = T,
                  fillOpacity = 0.75,
                  smoothFactor = 0.8,
                  weight=1,
                  color = ~pal(map1@data$mean),
                  noClip=T,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label=lapply(map_label, HTML)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~map1@data$mean,
                title = paste0(input$model," ", input$year),
                opacity = 1)  %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = initial_lng,lat = initial_lat, zoom = initial_zoom)

  })

  ## Render the plot as a leaflet object
  output$map_plot <- renderLeaflet({ map_reactive() })



  #=========================
  #--- Time series plot ---
  #==========================

  # plot data?
  plot_data <- function() {
    if (input$data == 5) {
      return(geom_point(color = NA))

    } else if(input$data == 4){
      return(geom_point(aes(#shape = factor(source_type),
        text= paste0("source ID: ",source_id,
                     "\nsource_type: ", source_type,
                     "\nflag: ", flag_cd_logit),
        color = factor(source_type)
      ),
      shape = 17,
      size = 3,
      show.legend = F, na.rm = T))

    }
  }

   # plot rescaled?
  plot_rescale <- function() {
    if (1 %in% input$show_lines) {
      ## Plot the data
      return(geom_line(aes(x = year_id, y = mean, group = model, color = model), linetype = "dotted", size = 0.9))
    } else {
      return(NULL)
    }
  }


  # plot uncertainty for ensemble model?
  plot_uncertainty_rescale <- function() {
    if (2  %in% input$show_lines) {
      return(geom_ribbon(aes(x = year_id, ymin = lower, ymax = upper,
                             group = model, fill = model), alpha=0.2))
    } else {
      return(NULL)
    }
  }

  ts_plot <- reactive({

    # transform data based on checkbox input(T/F)
    ggplot(ts_dt(), aes(x = year_id, y = data)) +

      ## plot underlying data used for model
      plot_data() +


      # plot rescaled?
      plot_rescale() +

      #plot uncertainty for ensemble
      plot_uncertainty_rescale() +

      scale_color_brewer(palette = "Set2", direction = -1) +
      scale_fill_brewer(palette = "Set2", direction = -1) +

      facet_wrap( ~ value_code, scales = "fixed", drop = T) +

      theme(panel.spacing.y = unit(0.3, "lines"),
            panel.spacing.x = unit(0.2, "lines")) +
      theme_bw() +

      xlab("Year")

  })

  ## Render the plot as a plotly object
  output$ts_plot <- renderPlotly({
    ggplotly(ts_plot(), dynamicTicks=F)
  })


  ##=====================================
  # line graph
  #=====================================
  ## Render the plot as a plotly object
  output$line_plot <- renderPlotly({

    # if we don't want to highlight a country
    if (length(input$country_line) == 0){
      g <- ggplot(line_dt()) +
        geom_line(aes(x = year_id, y = mean,
                      group = interaction(location_name, level),
                      color = level,
                      country = location_name)) +
        facet_wrap(~ value_code, scales = "fixed") +
        theme_bw()
      xlab("Year")
    } else {
      g <- ggplot(line_dt()) +
        geom_line(aes(x = year_id, y = mean,
                      group = interaction(location_name, level),
                      color = highlight,
                      country = location_name,
                      level = level)) +
        scale_color_manual(values = c("grey75","deeppink")) +
        geom_text(data = line_dt()[year_id == 2000 & highlight == T], aes(x = year_id, y = mean, color = highlight, label = ihme_loc_id),check_overlap = T) +
        facet_wrap(~ value_code, scales = "fixed") +
        xlab("Year") +
        theme_bw() + theme(legend.position = "none")
    }



    ggplotly(g, tooltip = c("x", "y", "country","level"))

  })


  ##=====================================
  # scatter comparison of two selected models
  ##=====================================
  scatter_plot <- reactive({
    ggplot(scatter_dt()) +
      geom_abline(linetype = "dashed") +
      geom_point(aes(x = model1, y = model2, color = location_name)) +
      facet_wrap(~value_code) +
      xlab(paste0('model 1: ',input$model_scatter[1])) + ylab(paste0('model 2: ', input$model_scatter[2])) +
      theme_bw()
  })

  output$scatter_plot <- renderPlotly({
    ggplotly(scatter_plot(), dynamicTicks=F)
  })


  ##=====================================
  # table
  ##=====================================
  # Table with selected properties (value_code and country)
  output$table <- DT::renderDataTable({
    dt <- unique(dt_first_pass()[!is.na(data),.(value_code,location_name,year_id, data, source_type, super_region_name, income_group, hiv_prev_group, val_prev, art_cov_life)])
    DT::datatable(dt, filter = 'top', options = list(lengthMenu = c(10,30,50)), rownames = F, height = 800)
  })



  ###############################################################################################################################################################
  #                                                                     MALARIA MODULE
  ###############################################################################################################################################################
  first_pass_mal <- function(){
    values <- c('data','gpr_mean','gpr_lower','gpr_upper')
    dt <- copy(mal_dt)

    if (input$measure_mal == 0){
      dt[, eval(values):= lapply(eval(values), function(x) logit(get(x),  min = 0, max = non_hiv_spend))]
    } else if (input$measure_mal == 1){
      dt[, eval(values):= lapply(eval(values), function(x) get(x)/non_hiv_spend)]

    } else if (input$measure_mal == 2){
      dt[, eval(values):= lapply(eval(values), function(x) get(x)/incidence_number)]
      dt[is.infinite(gpr_mean), gpr_mean := 0]
      dt[is.infinite(gpr_lower), gpr_lower := 0]
      dt[is.infinite(gpr_upper), gpr_upper := 0]

    } else if (input$measure_mal == 3){
      dt[, eval(values):= lapply(eval(values), function(x) get(x)/population)]
    } else if (input$measure_mal == 4){
      dt[, eval(values):= lapply(eval(values), function(x) get(x)/1000000)]
    }

    return(dt)
  }
  dt_first_pass_mal <- reactive({ first_pass_mal() })

  # update selection for value codes based on country input
  observe({
    code_update <- unique(mal_dt[loc_label %in% input$country_mal, value_code])
    updateSelectInput(session= session, inputId="category_ts_mal", choices=code_update, selected = code_update[1])
  })

  ## filter data for time series: value_code, country and model
  ts_dt_mal <- reactive({

    dt <- dt_first_pass_mal()[value_code %in% input$category_ts_mal
                              & loc_label %in% input$country_mal
                              & model %in% input$model_ts_mal
                ]
    return(dt)
  })

  ## filter data for map: value_code, year and model
  map_dt_mal <- reactive({

    dt0 <- unique(dt_first_pass_mal()[,.(value_code, ihme_loc_id, location_name, year_id, model, gpr_lower, gpr_mean, gpr_upper)])
    dt1 <- dt0[value_code == input$category_map_mal & year_id == input$year_mal & model == input$model_map_mal]
    return(dt1)

  })

  ## filter data for map: value_code, year and model
  scatter_dt_mal <- reactive({

    dt <- dt_first_pass_mal()[value_code %in% input$category_map_mal
                          & model %in% input$model_scatter_mal
                          & year_id == input$year_mal
                          ,.(location_name, year_id, value_code, gpr_mean, model)]

    # print(dt)
    dt <- unique(dt) %>%
      mutate(model = as.character(factor(model, labels = c('model1','model2')))) %>%
      spread(model, gpr_mean) %>% data.table

    print(dt)
    return(dt)

  })

  #=====================================
  # Time Series Plot for malaria
  #======================================
  ts_plot_mal <- reactive({

    # plot model estimates?
    plot_gpr_mal <- function() {
      if (1 %in% input$show_lines_mal) {
        ## Plot the data
        return(geom_line(aes(x = year_id, y = gpr_mean, group = model, color = model), size = 1))
      } else {
        return(NULL)
      }
    }

    # plot uncertainty for mean model?
    plot_gpr_ui_mal <- function() {
      if (2 %in% input$show_lines_mal) {
        ## Plot the data
        return(geom_ribbon(aes(x = year_id, ymin = gpr_lower, ymax = gpr_upper,
                               group = model, fill = model), alpha=0.5))
      } else {
        return(NULL)
      }
    }

    plot_data_mal <- function(){
      if (3 %in% input$show_lines_mal){
        return(geom_point(aes(x = year_id, y = data,
                              shape = as.factor(source_type),
                              color = as.factor(flag_cd)#,
                              # text= paste0("\nnotes: ", notes)
                              ),
                          size = 3,
                          show.legend = F, na.rm = T))
      }else{
        return(NULL)
      }
    }

    # transform data based on checkbox input(T/F)
    ggplot(ts_dt_mal()) +

      plot_data_mal() +

      plot_gpr_mal() +
      plot_gpr_ui_mal() +

      scale_color_brewer(palette = "Set2", direction = -1) +
      scale_fill_brewer(palette = "Set2", direction = -1) +

      facet_wrap( ~ value_code, scales = "fixed", drop = T) +

      theme(panel.spacing.y = unit(0.3, "lines"),
            panel.spacing.x = unit(0.2, "lines")) +
      theme_bw() +

      xlab("Year")

  })

  ## Render the plot as a plotly object
  output$ts_plot_mal <- renderPlotly({
    ggplotly(ts_plot_mal(), dynamicTicks=F)
  })


  #======================
  #--- Leaflet Map ---
  #======================
  map_reactive_mal <- reactive({

    ## merge map data with shape file
    map1 <- sp::merge(map_sf,map_dt_mal(), by.x = "ihme_lc_id", by.y = "ihme_loc_id", all.y = T)

    ## Set color palette
    clrs <- brewer.pal(10, "RdYlGn")
    # if(input$measure != 1){
    #    clrs <- brewer.pal(30, "Greens")
    # }

    pal <- colorNumeric(palette = clrs, domain = map1@data$gpr_mean)


    ## Create label with ensemble_mean, ensemble_upper and ensemble_lower, DO NOT ROUND?
    map_label <- paste0(map1@data$location_name,": ", map1@data$gpr_mean,
                        "\n\t(", map1@data$gpr_lower," - ",map1@data$gpr_upper,")")

    initial_lat = 35
    initial_lng =20
    initial_zoom = 2

    # Contruct the map, based on selected year
    leaflet(data=map1) %>%
      addPolygons(data=map1,
                  stroke = T,
                  fillOpacity = 0.75,
                  smoothFactor = 0.8,
                  weight=1,
                  fillColor = ~pal(map1@data$gpr_mean),
                  noClip=T,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label=map_label) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~map1@data$gpr_mean,
                title = paste0(input$model_map_mal," ", input$year_mal),
                opacity = 1)  %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = initial_lng,lat = initial_lat, zoom = initial_zoom)

  })

  ## Render the plot as a leaflet object
  output$map_plot_mal <- renderLeaflet({ map_reactive_mal() })

  ##=====================================
  # scatter comparison of two selected models
  ##=====================================
  scatter_plot_mal <- reactive({
    ggplot(scatter_dt_mal()) +
      geom_abline(linetype = "dashed") +
      geom_point(aes(x = model1, y = model2, color = location_name)) +
      facet_wrap(~value_code) +
      scale_x_continuous(name = paste0('model 1: ',input$model_scatter_mal[1])) +
      scale_y_continuous(name = paste0('model 2: ', input$model_scatter_mal[2])) +
      theme_bw()
  })

  output$scatter_plot_mal <- renderPlotly({
    ggplotly(scatter_plot_mal(), dynamicTicks=F)
  })


  #=====================================#
  # table for malaria
  #=====================================
  output$table_mal <- DT::renderDataTable({
    dt <- unique(ts_dt_mal()[,.(source_type, source_id, value_code, location_name, year_id, data, flag_cd, incidence_number,gpr_mean,gpr_lower, gpr_upper)])
    dt[, source_id := as.character(source_id)]
    dt[, year_id := as.character(year_id)]
    DT::datatable(dt, filter = 'top', options = list(lengthMenu = c(10,30,50)), rownames = F, height = 800)
  })

}

shinyApp(ui, server)
