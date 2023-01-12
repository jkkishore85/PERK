#' mec_dash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils head
#' @importFrom stats sd
#' @importFrom zoo as.Date as.yearmon
#' @importFrom plotly ggplotly renderPlotly layout
#' @importFrom grDevices dev.off pdf postscript
#' @importFrom DT renderDT dataTableOutput datatable
#' @importFrom bs4Dash box boxSidebar boxPad
#' @importFrom shinyWidgets pickerInput
#' @import dplyr tidyr ggplot2
mod_mec_dash_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(

    ## fluidRow start ----
    shiny::fluidRow(
      shinyjs::useShinyjs(),
      column(9,
             ## Box start start ----
             bs4Dash::box(width = NULL,
                 closable = FALSE,
                 collapsible = TRUE,
                 maximizable = TRUE,
                 ## Sidebar ----
                 sidebar = bs4Dash::boxSidebar(
                   startOpen = TRUE,
                   id = "mecsidebar11",
                   br(),
                   bs4Dash::boxPad(
                     width = 8,
                     shiny::fluidRow(
                       column( width = 4,
                               shinyWidgets::pickerInput(
                                 inputId = ns("selFeature"),
                                 label = "Compare",
                                 choices = c("Compound" = "compound",
                                             "Matrices" = "matrices",
                                             "Site" = "site"
                                 ),
                                 choicesOpt = list(
                                   icon = c("fas fa-capsules",
                                            "fas fa-water",
                                            "fas fa-map-marker-alt"
                                   )
                                 )
                               )
                       ),
                       column( width = 4,
                               shinyWidgets::pickerInput(
                                 inputId = ns("select_plot"),
                                 label = "Plot type:",
                                 choices = c("Monthly" = "bar",
                                             "Selected Period" = "box"
                                 ),
                                 choicesOpt = list(
                                   icon = c("fas fa-chart-column",
                                            "fas fa-chart-gantt"
                                   ) )
                               )
                       ),
                     ),
                     shiny::dateRangeInput(ns('date_range'),
                                           label = 'Date range (yyyy-mm-dd):',
                                           start ="2014-12-31",
                                           end = Sys.Date() + 2
                     ),
                     uiOutput(ns("selz_type")),
                     selectInput(
                       ns('select_target'),
                       'Target type:',
                       c(
                         'Compound' = 'Compound'
                       ),
                       selected = 'Compound'
                     ),
                     uiOutput(ns("selz_y")),
                     uiOutput(ns("selz_site")),
                     uiOutput(ns("selz_compound")),
                     actionButton(inputId = ns("gen_plot"),
                                  label = "Generate Graph",
                                  class="btn btn-success action-button")
                   )
                 ),
                 shiny::fluidRow(
                   column(
                     width = 12,
                     conditionalPanel("input.select_plot == 'bar'", ns = ns,
                                      conditionalPanel("input.selFeature == 'compound'", ns = ns,
                                                       plotly::plotlyOutput(ns("mec_plot_bar01"), height="600px"),
                                      ),
                                      conditionalPanel("input.selFeature == 'matrices'", ns = ns,
                                                       plotly::plotlyOutput(ns("mec_plot_bar02"), height="600px")),
                                      conditionalPanel("input.selFeature == 'site'", ns = ns,
                                                       plotly::plotlyOutput(ns("mec_plot_bar03"), height="600px"))
                     ),
                     conditionalPanel("input.select_plot == 'box'", ns = ns,
                                      conditionalPanel("input.selFeature == 'compound'", ns = ns,
                                                       plotly::plotlyOutput(ns("mec_plot_box01"), height="600px"),
                                      )),
                     tags$hr(),
                     uiOutput(ns("uidownload_btn")),
                     tags$hr(),
                     checkboxInput(ns("mec_show_tab"),
                                   label = "Show Datatable", value = FALSE),
                     conditionalPanel(
                       "input.mec_show_tab == true",  ns =ns,
                       DT::dataTableOutput(ns("tab_plot_data"))
                     )
                   ) # End of Column
                 ) # End of Fluid row
             )## End of Box
      )# End of column
    )

  )
}

#' mec_dash Server Functions
#'
#' @noRd
mod_mec_dash_server <- function(id,
                                mec_dat,
                                table_dt,
                                sel_target,
                                api_family,
                                wwtp_info,
                                re_info,
                                fx_info,
                                flow_info,
                                global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    global <- global

    mec_inputs <- reactive({
      req(table_dt$up_file)
      req(api_family$up_file)

      site_id <- input$mec_site_select
      cpd_name <- input$selz_cpd
      MEC_Key <- input$yaxis_mec
      EV_Type <- input$mec_env_type

      ggplot_dark_theme <-
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, angle = 45, hjust = 1, colour = "snow"),
                       axis.text.y = ggplot2::element_text(size = 12, colour = "snow"),
                       axis.title.x = ggplot2::element_text(size = 15, face = "bold", colour = "snow" ),
                       axis.title.y = ggplot2::element_text(size = 15, face = "bold", colour = "snow" ),
                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                       panel.grid.major = ggplot2::element_line(color = "#42484e", size = 0.4), # get rid of major grid
                       panel.grid.minor = ggplot2::element_line(color = "#42484e", size = 0.3), # get rid of minor grid
                       strip.background = ggplot2::element_rect(color="snow",
                                                                fill="transparent"),
                       strip.text.x = ggplot2::element_text(size = 15, color = "snow"),
                       strip.text.y = ggplot2::element_text(size = 15, color = "snow"),
                       legend.title= ggplot2::element_text(size = 12, colour = "snow"),
                       legend.background = ggplot2::element_rect(fill = "transparent"), # get rid of legend bg
                       legend.box.background = ggplot2::element_rect(fill = "transparent") ,# get rid of legend panel bg
                       legend.text = ggplot2::element_text(size = 12, colour = "snow") ,
                       title = ggplot2::element_text(size = 12, color = "snow")
        )

      ggplot_light_theme <-
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, angle = 45, hjust = 1, colour = "black"),
                       axis.text.y = ggplot2::element_text(size = 12, colour = "black"),
                       axis.title.x = ggplot2::element_text(size = 15, face = "bold", colour = "black"),
                       axis.title.y = ggplot2::element_text(size = 15, face = "bold", colour = "black"),
                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                       panel.grid.major = ggplot2::element_line(color = "gray93", size = 0.4), # get rid of major grid
                       panel.grid.minor = ggplot2::element_line(color = "gray93", size = 0.3), # get rid of minor grid
                       strip.background = ggplot2::element_rect(colour="black",
                                                                fill="transparent"),
                       strip.text.x = ggplot2::element_text(size = 15, color = "black"),
                       strip.text.y = ggplot2::element_text(size = 15, color = "black"),
                       legend.title= ggplot2::element_text(size = 12, colour = "black"),
                       legend.background = ggplot2::element_rect(fill = "transparent"), # get rid of legend bg
                       legend.box.background = ggplot2::element_rect(fill = "transparent") ,# get rid of legend panel bg
                       legend.text = ggplot2::element_text(size = 12, colour = "black"),
                       title = ggplot2::element_text(size = 12, color = "black")
        )
      return(
        list(
          site_id = site_id,
          cpd_name = cpd_name,
          ggplot_dark_theme = ggplot_dark_theme,
          ggplot_light_theme = ggplot_light_theme,
          MEC_Key = MEC_Key,
          EV_Type =  EV_Type
        )
      )
    })

    getData <- reactive ({

      req(mec_dat$up_file)
      req(api_family$up_file)
      req(wwtp_info$up_file)
      req(re_info$up_file)
      req(fx_info$up_file)
      req(flow_info$up_file)

      api_family_inFile <- api_family$up_file
      wwtp_info_inFile <- wwtp_info$up_file
      re_info_inFile <- re_info$up_file
      fx_info_inFile <- fx_info$up_file
      flow_info_inFile <- flow_info$up_file
      mec_inFile <- mec_dat$up_file

      api_family_fn <- file_input(name = api_family_inFile$name,
                                  path = api_family_inFile$datapath)

      wwtp_info_fn <- file_input(name = wwtp_info_inFile$name,
                                 path = wwtp_info_inFile$datapath)

      re_info_fn <- file_input(name = re_info_inFile$name,
                               path = re_info_inFile$datapath)

      fx_info_fn <- file_input(name = fx_info_inFile$name,
                               path = fx_info_inFile$datapath)

      flow_info_fn <- file_input(name = flow_info_inFile$name,
                               path = flow_info_inFile$datapath)

      mec_info_fn <- file_input(name = mec_inFile$name,
                           path = mec_inFile$datapath)

      return(
        list(
          mec_data = mec_info_fn$dataInput %>%
            dplyr::mutate(PERIOD = zoo::as.Date(PERIOD, "%d/%m/%Y")),
          apifamily = api_family_fn$dataInput,
          reinfo = re_info_fn$dataInput,
          fexcretainfo = fx_info_fn$dataInput,
          flowinfo = flow_info_fn$dataInput %>%
            dplyr::mutate(
              Flow = `Total_Flow(m3)`*1000,
              PERIOD = zoo::as.Date(Date, "%d/%m/%Y"),
              Type = replace(Type, Type == "influent","INF"),
              Type = replace(Type, Type == "effluent","EFF"),
              Type = replace(Type, Type == "RiverUP","R_UP"),
              Type = replace(Type, Type == "RiverDown","R_Down")),
          wwtpinfo = wwtp_info_fn$dataInput %>%
            dplyr::mutate(Year = zoo::as.yearmon(Year, "%Y")) %>%
            dplyr::mutate(Year = format(Year, format ="%Y"))
        )
      )
    })

   measured_full <- reactive({

      req(getData()$mec_data)
      req(getData()$apifamily)
      req(getData()$wwtpinfo)
      req(getData()$reinfo)
      req(getData()$fexcretainfo)
      req(getData()$flowinfo)

      df <- getData()$mec_data
      api <- getData()$apifamily
      wwtp <- getData()$wwtpinfo
      re <-  getData()$reinfo
      fexcreta <-  getData()$fexcretainfo
      flow <- getData()$flowinfo

      mec_01 <- df %>%
        dplyr::select(Compound, catchment, PERIOD, Type, replicate, Concentration) %>%
        dplyr::group_by(Compound, catchment, Type, PERIOD ) %>%
        dplyr::summarise(mean_val = mean(Concentration, na.rm = TRUE),
                         SD = sd(Concentration, na.rm = TRUE) ) %>%
        dplyr::ungroup() %>%
        dplyr::rename(`Concentration (ng/L)` = mean_val ) %>%
        dplyr::mutate(
          Compound = tolower(Compound),
          Type = replace(Type, Type == "influent","INF"),
          Type = replace(Type, Type == "effluent","EFF"),
          Type = replace(Type, Type == "RiverUP","R_UP"),
          Type = replace(Type, Type == "RiverDown","R_Down")) %>%
        dplyr::left_join(flow, by = c("catchment","PERIOD","Type")) %>%
        dplyr::mutate(`DL (mg/day)` = ((`Concentration (ng/L)` /1000000) * Flow)) %>%
        dplyr::mutate(period = as.Date(PERIOD)) %>%
        dplyr::mutate(PERIOD =as.Date(PERIOD, "%d-%m-%Y")) %>%
        tidyr::separate(period, c("Year","month","Date" ),sep = "-"  ) %>%
        dplyr::mutate(Year = zoo::as.yearmon(Year, "%Y")) %>%
        dplyr::mutate(Year = format(Year, format ="%Y")) %>%
        dplyr::left_join(dplyr::select(wwtp, c("Total_PE","catchment", "Year")), by = c("catchment", "Year")) %>%
        dplyr::mutate(`PNDL (mg/day/1000)` = (`DL (mg/day)`/Total_PE)*1000  ) %>%
        dplyr::select(Compound, catchment, PERIOD, Type, `Concentration (ng/L)`, `DL (mg/day)`, `PNDL (mg/day/1000)`, SD) %>%
        tidyr::pivot_longer(!c(Compound, catchment, PERIOD, Type,SD), names_to = "MEC_Key", values_to = "mean_val")

      return(
        list(
          mec_raw = df,
          mec_data_full = mec_01
        )
      )
    })

    plot_data <- reactive({
      req(measured_full()$mec_data_full)
      req(mec_inputs()$cpd_name)
      req(mec_inputs()$site_id)
      req(mec_inputs()$MEC_Key)
      req(mec_inputs()$EV_Type)

      cpdname <- mec_inputs()$cpd_name
      sitename <- mec_inputs()$site_id
      MECKey <- mec_inputs()$MEC_Key
      EVType <- mec_inputs()$EV_Type
      df01 <- measured_full()$mec_data_full

      tryCatch(
        {
          df <- df01 %>%
            dplyr::filter(Compound %in% cpdname) %>%
            dplyr::filter(MEC_Key %in% MECKey) %>%
            dplyr::filter(catchment %in% sitename) %>%
            dplyr::filter(Type %in% EVType) %>%
            dplyr::filter(PERIOD > !!input$date_range[1] & PERIOD < !!input$date_range[2])

        },
        error = function(e) {
          stop(safeError(e))
        }
      )
    })

    # bar plot - Compound ----
    output$mec_plot_bar01 <- plotly::renderPlotly (
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(mec_inputs()$cpd_name)
        req(mec_inputs()$site_id)
        req(mec_inputs()$MEC_Key)
        req(mec_inputs()$EV_Type)
        req(plot_data())

        cpdname <- mec_inputs()$cpd_name
        sitename <- mec_inputs()$site_id
        MECKey <- mec_inputs()$MEC_Key
        EVType <- mec_inputs()$EV_Type

        plot01 <- plotly::plot_ly(plot_data(),
                                  x = ~ as.POSIXct(PERIOD), y = ~mean_val,
                                  color = ~Compound,
                                  #colors = color_palette(),
                                  type = "bar",
                                  marker = list(
                                    line = list(color = input$colPlotOutline,
                                                width = input$widthPlotOutline)),
                                  error_y = ~list(array = SD,
                                                  color = input$colPlotOutline)) %>%
          plotly::layout(
            title = list(
              text =  paste('Measured', MECKey, 'of pharmaceutical in <br>',
                            EVType, 'at WWTP',sitename)
            ),
            legend = list(title=list(text='Compounds')),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            xaxis = list(
              title='<b> Period (Date Month Year (Day)) </b>',
              type = 'date',
              tickformat = "%d %B %Y <br> (%a)"),
            yaxis = list(
              title=paste(MECKey))
          )

        plot01

        if (global$dark_mode)
        {
          plot01 <- plot01 %>%
            plotly::layout(
              title = list(
                font = list(
                  color = "#C6C8C9")),
              xaxis = list(
                color = "#C6C8C9"),
              yaxis = list(
                color = "#C6C8C9"),
              legend = list(
                font = list(
                  color = "#C6C8C9")
              ))
        }
        else
        {
          plot01
        }
      })
    )

    # bar plot - matrices ----
    output$mec_plot_bar02 <- renderPlotly (
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(mec_inputs()$cpd_name)
        req(mec_inputs()$site_id)
        req(mec_inputs()$MEC_Key)
        req(mec_inputs()$EV_Type)
        req(plot_data())

        cpdname <- mec_inputs()$cpd_name
        sitename <- mec_inputs()$site_id
        MECKey <- mec_inputs()$MEC_Key
        EVType <- mec_inputs()$EV_Type

        plot01 <- plotly::plot_ly(plot_data(),
                                  x = ~ as.POSIXct(PERIOD), y = ~mean_val,
                                  color = ~Type,
                                  #colors = color_palette(),
                                  type = "bar",
                                  marker = list(
                                    line = list(color = input$colPlotOutline,
                                                width = input$widthPlotOutline)),
                                  error_y = ~list(array = SD,
                                                  color = input$colPlotOutline)) %>%
          plotly::layout(
            title = list(
              text =  paste('Measured Concentrations (ng/L) of pharmaceutical in <br>',
                            EVType, 'at WWTP',sitename)
            ),
            legend = list(title=list(text='<b> Matrices</b>')),
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            xaxis = list(
              title='Period (Date Month Year (Day))',
              type = 'date',
              tickformat = "%d %B %Y <br> (%a)"),
            yaxis = list(
              title=paste('<b> Concentration (ng/L) </b>'))
          )

        plot01
      })
    )

    # bar plot - site ----
    output$mec_plot_bar03 <- renderPlotly (
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(mec_inputs()$ggplot_dark_theme)
        req(plot_data())

        ggplot_dark <- mec_inputs()$ggplot_dark_theme

        plot01 <- ggplot2::ggplot(data=plot_data(),
                                   ggplot2::aes(x=PERIOD, y=mean_val, fill=catchment)) +
          ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(), colour="black")


        plotly::ggplotly(
          plot01 + ggplot_dark
          )

      })
    )

    # box plot - Compound ----
    output$mec_plot_box01 <- renderPlotly (
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        req(mec_inputs()$ggplot_dark_theme)

        ggplot_dark <- mec_inputs()$ggplot_dark_theme

        plot01 <- plotly::plot_ly(plot_data(), x = ~ Compound, y = ~mean_val,
                                  color = ~Compound,
                                  #colors = color_palette(),
                                  type = "box",
                                  marker = list(
                                    line = list(color = input$colPlotOutline,
                                                width = input$widthPlotOutline))
        ) %>%
          plotly::layout(
            paper_bgcolor = "transparent", plot_bgcolor = "transparent",
            xaxis = list(title='Compound'),
            yaxis = list(title='Concentration')
          )

        plot01

      })
    )

    targets <- reactive({

      req(sel_target$up_file)
      tryCatch(
        {
          df <- readr::read_csv(sel_target$up_file$datapath)
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
    })

    catchment <- reactive({
      req(table_dt$up_file)
      tryCatch(
        {
          df <- readr::read_csv(table_dt$up_file$datapath) %>%
            dplyr::select(catchment) %>%
            unique()

        },
        error = function(e) {
          stop(safeError(e))
        }
      )
    })

    # UI Output - Compounds ----
    output$selz_compound <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(api_family$up_file)
        req(targets()$Compound)
        req(input$selFeature)

        target_cpd <- unique(targets()$Compound)

        if(input$selFeature %in% c("site","matrices" ))
        {
          selectInput(inputId= ns("selz_cpd"),
                      label="Select Compound:",
                      choices= target_cpd
          )
        }
        else {
          shinyWidgets::pickerInput(
            inputId =  ns("selz_cpd"),
            label = "Select Compound(s):",
            choices= target_cpd,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(target_cpd,1)
          )
        }
      })
    })

    # UI Output - site ----
    output$selz_site <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(table_dt$up_file)
        req(catchment()$catchment)

        site_name <- unique(catchment()$catchment)

        if(input$selFeature %in% c("site" ))
        {
          shinyWidgets::pickerInput(
            inputId =  ns("mec_site_select"),
            label="Select the site:",
            choices= site_name,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(site_name,1)
          )
        }
        else{
          selectInput(
            inputId=ns("mec_site_select"),
            label="Select the site:",
            choices= site_name,
            selected = head(site_name,1)
          )
        }
      })
    })

    # UI Output - Y axis ----
    output$selz_y <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(measured_full()$mec_data_full)

        df01 <- measured_full()$mec_data_full

        MEC_Key <- unique(df01$MEC_Key)

        selectInput(
          inputId=ns("yaxis_mec"),
          label="Select Y axis:",
          choices= MEC_Key,
          selected = head(MEC_Key,1)
        )
      })
    })

    # UI Output - Type ----
    output$selz_type <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(measured_full()$mec_data_full)

        df01 <- measured_full()$mec_data_full

        Type <- unique(df01$Type)

        if(input$selFeature %in% c("matrices" ))
        {
          shinyWidgets::pickerInput(
            inputId =  ns("mec_env_type"),
            label="Select Sample Type:",
            choices = Type,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(Type,1)
          )
        }
        else{
          selectInput(
            inputId=ns("mec_env_type"),
            label="Select Sample Type:",
            choices= Type,
            selected = head(Type,1)
          )
        }
      })
    })

    # DT - Tab plot data ----
    output$tab_plot_data <- DT::renderDT({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        options(
          DT.options = list(
            pageLength = nrow(plot_data()),
            autoWidth = FALSE,
            scrollX = TRUE,
            scrollY = "600px"
          )
        )

        DT::datatable(
          plot_data(),
           filter = "top",
           rownames = FALSE,
           caption = 'List of Entries.',
           options = list(
             columnDefs = list(
               list(className = 'dt-center', targets = "_all")))
        )
      })
    })

    output$tab_flow_data <- DT::renderDT({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        req(getData()$flowinfo)

        flow <- getData()$flowinfo

        options(
          DT.options = list(
            pageLength = nrow(flow),
            autoWidth = FALSE,
            scrollX = TRUE,
            scrollY = "600px"
          )
        )
        DT::datatable(
          flow,
          filter = "top",
          rownames = FALSE,
          caption = 'List of Entries.',
          options = list(
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")))
        )

      })
    })

    # return list ----
    return(
      list(
        mec_full = reactive({
          measured_full()$mec_data_full}),
        mec_raw = reactive({
          measured_full()$mec_raw})
        )
    )

  })
}

## To be copied in the UI
# mod_mec_dash_ui("mec_dash_1")

## To be copied in the server
# mod_mec_dash_server("mec_dash_1")
