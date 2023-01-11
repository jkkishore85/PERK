#' pecvsmec_dash UI Function
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
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom grDevices dev.off pdf postscript
#' @importFrom DT renderDT dataTableOutput datatable
#' @importFrom shinyWidgets pickerInput
#' @import dplyr tidyr ggplot2
mod_pecvsmec_dash_ui <- function(id){
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
                   id = "pcmcsidebar11",
                   br(),
                   bs4Dash::boxPad(
                     width = 8,
                     shiny::fluidRow(
                       column( width = 4,
                               shinyWidgets::pickerInput(
                                 inputId = ns("selFeature"),
                                 label = "Compare",
                                 choices = c("Compound" = "compound"
                                 ),
                                 choicesOpt = list(
                                   icon = c("fas fa-capsules"
                                   )
                                 )
                               )
                       ),
                       column( width = 4,
                               shinyWidgets::pickerInput(
                                 inputId = ns("select_plot"),
                                 label = "Plot type:",
                                 choices = c(
                                   "Bar" = "bar"
                                   ),
                                 choicesOpt = list(
                                   icon = c("fas fa-chart-column"
                                   ) )
                               )
                       ),
                     ),
                     shiny::fluidRow(
                       column(width = 4,
                              shiny::selectInput(ns('sel_trend'), 'Trend:',
                                          c(
                                            'Month' = 'month'
                                          ), selected = 'month'
                              )),
                       column(width = 4,
                              shiny::uiOutput(ns("selz_period"))
                       )),
                     shiny::uiOutput(ns("selz_type")),
                     shiny::selectInput(
                       ns('select_target'),
                       'Target type:',
                       c(
                         'Compound' = 'Compound'
                       ),
                       selected = 'Compound'
                     ),
                     shiny::uiOutput(ns("selz_y")),
                     shiny::uiOutput(ns("selz_site")),
                     shiny::uiOutput(ns("selz_compound")),
                     actionButton(inputId = ns("gen_plot"),
                                  label = "Generate Graph",
                                  class="btn btn-success action-button")
                     )
                   ),
                 shiny::fluidRow(
                   column(
                     width = 12,
                     conditionalPanel("input.select_plot == 'bar'", ns = ns,
                                      plotly::plotlyOutput(ns("pecvsmec_plot_bar01"), height="600px")),
                     tags$hr(),
                     uiOutput(ns("uidownload_btn")),
                     tags$hr(),
                     checkboxInput(ns("show_tab"),
                                   label = "Show Datatable", value = FALSE),
                     conditionalPanel(
                       "input.show_tab == true",  ns =ns,
                       DT::dataTableOutput(ns("tab_plot_data"))
                     )
                   ) # End of Column
                 ) # End of Fluid row
             )## End of Box
      )# End of column
    )

  )
}

#' pecvsmec_dash Server Functions
#'
#' @noRd
mod_pecvsmec_dash_server <- function(id,
                                     pec_dat,
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

    pcvsmc_inputs <- reactive({
      req(table_dt$up_file)
      req(api_family$up_file)

      site_id <- input$pcvsmc_site_select
      cpd_name <- input$pcvsmc_selz_cpd
      PEC_Key <- input$pcvsmc_yaxis
      EV_Type <- input$pcvsmc_env_type
      pcvcmcdate <- input$pcvsmc_date

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
          PEC_Key = PEC_Key,
          EV_Type =  EV_Type,
          pcvcmcdate = pcvcmcdate
        )

      )
    })

    getData <- reactive ({
      req(pec_dat$pec_data_full())
      req(pec_dat$pec_sd())
      req(mec_dat$mec_full())
      req(mec_dat$mec_raw())
      req(api_family$up_file)
      req(wwtp_info$up_file)
      req(re_info$up_file)
      req(fx_info$up_file)

      api_family_inFile <- api_family$up_file
      wwtp_info_inFile <- wwtp_info$up_file
      re_info_inFile <- re_info$up_file
      fx_info_inFile <- fx_info$up_file

      api_family_fn <- file_input(name = api_family_inFile$name,
                                  path = api_family_inFile$datapath)

      wwtp_info_fn <- file_input(name = wwtp_info_inFile$name,
                                 path = wwtp_info_inFile$datapath)

      re_info_fn <- file_input(name = re_info_inFile$name,
                               path = re_info_inFile$datapath)

      fx_info_fn <- file_input(name = fx_info_inFile$name,
                               path = fx_info_inFile$datapath)

      pec_dt <- pec_dat$pec_data_full()
      pc_sd <- pec_dat$pec_sd()
      mec_dt <- mec_dat$mec_full()
      mec_raw <- mec_dat$mec_raw()

      return(
        list(
          pc_sd_dt = pc_sd,
          pc_data = pec_dt,
          mc_data = mec_dt,
          mc_raw = mec_raw,
          apifamily = api_family_fn$dataInput,
          reinfo = re_info_fn$dataInput,
          fexcretainfo = fx_info_fn$dataInput,
          wwtpinfo = wwtp_info_fn$dataInput %>%
            dplyr::mutate(Year = zoo::as.yearmon(Year, "%Y")) %>%
            dplyr::mutate(Year = format(Year, format ="%Y"))
        )
      )
    })

    pcvsmc_data <- reactive({

      req(getData()$pc_sd_dt)
      req(getData()$pc_data)

      req(getData()$mc_raw)
      req(getData()$mc_data)

      req(getData()$apifamily)
      req(getData()$wwtpinfo)
      req(getData()$reinfo)
      req(getData()$fexcretainfo)

      api <- getData()$apifamily
      wwtp <- getData()$wwtpinfo
      re <-  getData()$reinfo
      fexcreta <-  getData()$fexcretainfo

      pcSD <- getData()$pc_sd_dt
      pcdata <- getData()$pc_data
      mcraw <- getData()$mc_raw
      mcdata <- getData()$mc_data

      mc_month <- mcraw %>%
        dplyr::mutate(period = format(PERIOD, format ="%d %B %Y")) %>%
        tidyr::separate(period, c("Date","month", "Year"),sep = " "  ) %>%
        tidyr::unite("date", c(month, Year), sep = " ", remove = FALSE) %>%
        dplyr::group_by(Compound, catchment, Type, date) %>%
        dplyr::summarise(SD = sd(Concentration, na.rm = TRUE),
                         Concentration = mean(Concentration, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(PERIOD =as.Date(as.POSIXct(zoo::as.yearmon(date)))) %>%
        dplyr::mutate(Type = replace(Type, Type == "influent", "MEC_influent")) %>%
        dplyr::mutate(Type = replace(Type, Type == "effluent","MEC_effluent")) %>%
        dplyr::mutate(Type = replace(Type, Type == "RiverDown","MEC_riverdown")) %>%
        dplyr::mutate(Type = replace(Type, Type == "RiverUP","MEC_riverup")) %>%
        dplyr::mutate(Type = replace(Type, Type == "SPM","MEC_SPM"))


      pcvsmc01 <- mc_month %>%
        tidyr::pivot_wider(names_from = Type,
                           names_glue = "{Type}_{.value}",
                           values_from = c(Concentration,SD)
        ) %>%
        dplyr::left_join( pcSD, by =c("PERIOD","Compound", "catchment", "date")) %>%
        dplyr::mutate(PA_influent_I =  (PEC0I_influent_Concentration/MEC_influent_Concentration)) %>%
        dplyr::mutate(PA_influent_II =  (PEC0II_influent_Concentration/MEC_influent_Concentration)) %>%
        dplyr::mutate(PA_effluent_I =  (PEC0I_effluent_Concentration/MEC_effluent_Concentration)) %>%
        dplyr::mutate(PA_effluent_II =  (PEC0II_effluent_Concentration/MEC_effluent_Concentration)) %>%
        dplyr::mutate(PA_riverdown_I =  (PEC0I_riverdown_Concentration/MEC_riverdown_Concentration)) %>%
        dplyr::mutate(PA_riverdown_II =  (PEC0II_riverdown_Concentration/MEC_riverdown_Concentration))

      pcvsmc02 <- pcvsmc01 %>%
        dplyr::select(-c(kgmonth_SM,kgmonth_SD,
                         kgmonth_YA, kgmonth_YD,
                         PNDP_SM, PNDP_YA,
                         kg_year,
                         low, high, avg,
                         dil_avg, month, Year, Total_PE,
                         rm_eff, rm_eff_SD,WWinhab,
                         MEC_riverup_Concentration, MEC_SPM_Concentration,
                         MEC_riverup_SD, MEC_SPM_SD,
                         PA_influent_I,PA_influent_II,
                         PA_effluent_I,PA_effluent_II,
                         PA_riverdown_I, PA_riverdown_II)) %>%
        tidyr::pivot_longer(
          cols = -c(Compound,catchment, date, PERIOD),
          names_to = c("compare", "Type", "outcome"),
          names_pattern = "(.*)_(.*)_(.*)",
          values_to = "Concentrations"
        ) %>%
        tidyr::unite(key, c(compare, outcome), sep = "_") %>%
        tidyr::pivot_wider(names_from = key,
                           values_from =Concentrations) %>%
        tidyr::pivot_longer(
          cols = -c(Compound,catchment, date, PERIOD, Type, MEC_Concentration, MEC_SD),# PEC_influent_I:MEC_effluent_SD,#MEC_influent_SD, # need to work on this - effluent
          names_to = c("PEC_Key", "Measure_Type"),
          names_pattern = "(.*)_(.*)",
          values_to = "values"
        ) %>%
        dplyr::mutate(PEC_Key = replace(PEC_Key, PEC_Key == "PEC0I","PEC_I"),
                      PEC_Key = replace(PEC_Key, PEC_Key == "PEC0II","PEC_II"),
                      Measure_Type = replace(Measure_Type, Measure_Type == "Concentration","PEC_Concentration"),
                      Measure_Type = replace(Measure_Type, Measure_Type == "SD","PEC_SD")) %>%
        tidyr::pivot_wider(names_from = Measure_Type, values_from = values, values_fill = 0) %>%
        dplyr::mutate(Type = replace(Type, Type == "influent","INF")) %>%
        dplyr::mutate(Type = replace(Type, Type == "effluent","EFF")) %>%
        dplyr::mutate(Type = replace(Type, Type == "riverdown","RDOWN")) %>%
        dplyr::mutate(pcvsmc_SD = sqrt(MEC_SD^2+PEC_SD^2))

      return(
        list(
          mcmonth = mc_month,
          pcvsmc = pcvsmc01,
          pcvsmcfull = pcvsmc02
        )
      )

    })


    plot_data <- reactive({
      req(pcvsmc_inputs()$cpd_name)
      req(pcvsmc_inputs()$site_id)
      req(pcvsmc_inputs()$PEC_Key)
      req(pcvsmc_inputs()$EV_Type)
      req(pcvsmc_inputs()$pcvcmcdate)

      req(pcvsmc_data()$pcvsmcfull)

      cpdname <- pcvsmc_inputs()$cpd_name
      sitename <- pcvsmc_inputs()$site_id
      PECKey <- pcvsmc_inputs()$PEC_Key
      EVType <- pcvsmc_inputs()$EV_Type
      pcvsmc_dat <- pcvsmc_inputs()$pcvcmcdate

      df01 <- pcvsmc_data()$pcvsmcfull

      tryCatch(
        {
          df <- df01 %>%
            dplyr::filter(Compound %in% cpdname) %>%
            dplyr::filter(PEC_Key %in% PECKey) %>%
            dplyr::filter(catchment %in% sitename) %>%
            dplyr::filter(Type %in% EVType) %>%
            dplyr::filter(date %in% pcvsmc_dat)
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
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

    pecvsmec_bar01 <- reactive({
      req(pcvsmc_inputs()$cpd_name)
      req(pcvsmc_inputs()$site_id)
      req(pcvsmc_inputs()$PEC_Key)
      req(pcvsmc_inputs()$EV_Type)
      req(pcvsmc_inputs()$pcvcmcdate)

      req(plot_data())

      cpdname <- pcvsmc_inputs()$cpd_name
      sitename <- pcvsmc_inputs()$site_id
      PECKey <- pcvsmc_inputs()$PEC_Key
      EVType <- pcvsmc_inputs()$EV_Type
      pcvsmc_dat <- pcvsmc_inputs()$pcvcmcdate

      plot01 <- ggplot2::ggplot(plot_data(),
                                ggplot2::aes(x=Compound, y=MEC_Concentration,
                                             fill=Compound)) +
        ggplot2::geom_bar(stat = "identity", color = "#42484e",
                          position = ggplot2::position_dodge())+
        #scale_fill_manual(values = color_palette_ggplot()) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin=MEC_Concentration-MEC_SD,
                       ymax = MEC_Concentration+MEC_SD),
          width=.2,
          position=ggplot2::position_dodge(.9)) +
        ggplot2::labs(title = paste("Measured vs Predicted Concentrations (ng/L)","in the ",
                                    EVType, " at WWTP ",
                                    sitename, " during ",
                                    pcvsmc_dat))+
        ggplot2::geom_point(data=plot_data(),
                            ggplot2::aes(x=Compound, y=PEC_Concentration,
                                         fill = Compound)#,
                            #                              color = input$colPlotOutline
        ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin=PEC_Concentration-PEC_SD,
                       ymax=PEC_Concentration+PEC_SD),
          width = 0.2,
          position=ggplot2::position_dodge(0.05)) +
        ggplot2::labs(x = "Compounds", y = "Concentration (ng/L)")

      return(
        list(
          plot01 = plot01
        )
      )
    })
    # bar plot - Compound ----
    output$pecvsmec_plot_bar01 <- renderPlotly (
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(pcvsmc_inputs()$ggplot_dark_theme)
        req(pcvsmc_inputs()$ggplot_light_theme)

        ggplot_dark <- pcvsmc_inputs()$ggplot_dark_theme
        ggplot_light <- pcvsmc_inputs()$ggplot_light_theme

        req(pecvsmec_bar01()$plot01)

        plot01 <- pecvsmec_bar01()$plot01

        plotly::ggplotly(
          if (global$dark_mode)
            plot01 + ggplot_dark
          else
            plot01 + ggplot_light
        )

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

        shinyWidgets::pickerInput(
            inputId =  ns("pcvsmc_selz_cpd"),
            label = "Select Compound(s):",
            choices= target_cpd,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(target_cpd,1)
            )
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
            inputId =  ns("pcvsmc_site_select"),
            label="Select the site:",
            choices= site_name,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(site_name,1)
          )
        }
        else{
          selectInput(
            inputId=ns("pcvsmc_site_select"),
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
        req(pcvsmc_data()$pcvsmcfull)

        df01 <- pcvsmc_data()$pcvsmcfull

        PEC_Key <- unique(df01$PEC_Key)

        selectInput(
          inputId=ns("pcvsmc_yaxis"),
          label="Select Y axis:",
          choices= PEC_Key,
          selected = head(PEC_Key,1)
        )
      })
    })

    # UI Output - PERIOD ----
    output$selz_period <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        req(pcvsmc_data()$pcvsmcfull)

        df01 <- pcvsmc_data()$pcvsmcfull

        date <- unique(df01$date)

        selectInput(
          inputId=ns("pcvsmc_date"),
          label="Select Month:",
          choices= date,
          selected = head(date,1)
        )
      })
    })


    # UI Output - Type ----
    output$selz_type <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(pcvsmc_data()$pcvsmcfull)

        df01 <- pcvsmc_data()$pcvsmcfull

        Type <- unique(df01$Type)

        if(input$selFeature %in% c("matrices" ))
        {
          shinyWidgets::pickerInput(
            inputId =  ns("pcvsmc_env_type"),
            label="Select Sample Type:",
            choices = Type,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(Type,1)
          )
        }
        else{
          selectInput(
            inputId=ns("pcvsmc_env_type"),
            label="Select Sample Type:",
            choices= Type,
            selected = head(Type,1)
          )
        }
      })
    })

    # Download Buttons ----
    output$uidownload_btn <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        tags$span(
          downloadButton(ns('downloaddata'), 'Download CSV') ,
          downloadButton(ns('downloadpdf'), 'Download PDF'),
          downloadButton(ns('downloadeps'), 'Download EPS')
        )
      })
    })

    # Download csv01
    output$downloaddata <- downloadHandler(
      filename = function (){ paste0('plot_data', '.csv')},
      content = function(file) {
        write_csv(as.data.frame(plot_data()), file)
      }
    )

    # Download PDF
    output$downloadpdf <- downloadHandler(
      filename = function(){ paste('prescplot.pdf',sep = '')},
      content = function(file) {
        # pdf ----
        pdf(file, paper = "a4r",width = 14)

        req(pcvsmc_inputs()$ggplot_light_theme)

        ggplot_light <- pcvsmc_inputs()$ggplot_light_theme

        req(pecvsmec_bar01()$plot01)

        plot01 <- pecvsmec_bar01()$plot01

        print(plot01 + ggplot_light)

        dev.off()
      })

    # Download EPS
    output$downloadeps <- downloadHandler(
      filename = function(){ paste('prescplot.eps',sep = '')},
      content = function(file) {
        # eps ----
        postscript(file,
                   width = 11.69 , height = 8.27, # inches
                   horizontal = TRUE, onefile = TRUE, paper = "special")
        pdf(file, paper = "a4r",width = 14)

        req(pcvsmc_inputs()$ggplot_light_theme)

        ggplot_light <- pcvsmc_inputs()$ggplot_light_theme

        req(pecvsmec_bar01()$plot01)

        plot01 <- pecvsmec_bar01()$plot01

        print(plot01 + ggplot_light)

        dev.off()
      })

    ## End of Download buttons ---


    # return list ----
    return(
      list(
        mcmonth = reactive({
          pcvsmc_data()$mcmonth}),
        pcvsmc_01 = reactive({
          pcvsmc_data()$pcvsmc}),
        pcvsmc_full = reactive({
          pcvsmc_data()$pcvsmcfull})
      )
    )


  })
}

## To be copied in the UI
# mod_pecvsmec_dash_ui("pecvsmec_dash_1")

## To be copied in the server
# mod_pecvsmec_dash_server("pecvsmec_dash_1")
