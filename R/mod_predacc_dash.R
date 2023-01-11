#' predacc_dash UI Function
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
mod_predacc_dash_ui <- function(id){
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
                          id = "predaccsidebar11",
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
                            shiny::uiOutput(ns("selz_period")),
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
                                             plotly::plotlyOutput(ns("predacc_plot_bar01"), height="600px")),
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

#' predacc_dash Server Functions
#'
#' @noRd
mod_predacc_dash_server <- function(id,
                                    pcvsmc_dat,
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

    predacc_inputs <- reactive({
      req(table_dt$up_file)
      req(api_family$up_file)

      site_id <- input$site_select
      cpd_name <- input$selz_cpd
      PA_Key <- input$select_yaxis
      EV_Type <- input$env_type
      pa_date <- input$select_date

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
          PA_Key = PA_Key,
          EV_Type =  EV_Type,
          pa_date = pa_date
        )

      )
    })

    getData <- reactive ({
      req(pcvsmc_dat$pcvsmc_01())
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

      pcvsmc_dt <- pcvsmc_dat$pcvsmc_01()

      return(
        list(
          pcvsmc_data = pcvsmc_dt,
          apifamily = api_family_fn$dataInput,
          reinfo = re_info_fn$dataInput,
          fexcretainfo = fx_info_fn$dataInput,
          wwtpinfo = wwtp_info_fn$dataInput %>%
            dplyr::mutate(Year = zoo::as.yearmon(Year, "%Y")) %>%
            dplyr::mutate(Year = format(Year, format ="%Y"))
        )
      )
    })

   accuracy_data <- reactive({

      req(getData()$pcvsmc_data)

      pcmc_data <- getData()$pcvsmc_data

      pred_acc_data <- pcmc_data %>%
        dplyr::select(-c(
          MEC_effluent_Concentration, MEC_influent_Concentration,
          MEC_riverdown_Concentration,
          MEC_riverup_Concentration, MEC_SPM_Concentration,
          MEC_effluent_SD, MEC_influent_SD,
          MEC_riverdown_SD,
          MEC_riverup_SD, MEC_SPM_SD,
          month, Year,
          kgmonth_SM,
          kgmonth_YA, kgmonth_YD,
          PNDP_SM, PNDP_YA,
          kg_year, kgmonth_SD,
          dil_avg, Total_PE,
          rm_eff, rm_eff_SD,
          WWinhab,
          low, high, avg,
          PEC0I_influent_Concentration, PEC0II_influent_Concentration,
          PEC0I_effluent_Concentration, PEC0II_effluent_Concentration,
          PEC0I_riverdown_Concentration, PEC0II_riverdown_Concentration,
          PEC0I_influent_SD, PEC0II_influent_SD,
          PEC0I_effluent_SD, PEC0II_effluent_SD,
          PEC0I_riverdown_SD, PEC0II_riverdown_SD
        )) %>%
        tidyr::pivot_longer(
          cols = -c(Compound,catchment, date, PERIOD),
          names_to = c("compare", "Type", "outcome"),
          names_pattern = "(.*)_(.*)_(.*)",
          values_to = "Concentrations"
        ) %>%
        tidyr::unite(key, c(compare, outcome), sep = "_") %>%
        tidyr::pivot_wider(names_from = key,
                           values_from = Concentrations) %>%
        tidyr::pivot_longer(
          cols =c(PA_I,PA_II),
          names_to = "PA_Key",
          values_to = "PA"
        ) %>%
        dplyr::mutate(Type = replace(Type, Type == "influent","INF")) %>%
        dplyr::mutate(Type = replace(Type, Type == "effluent","EFF")) %>%
        dplyr::mutate(Type = replace(Type, Type == "riverdown","RDOWN"))

      return(
        list(
          pred_acc_data = pred_acc_data
        )
      )
    })

    plot_data <- reactive({
      req(predacc_inputs()$cpd_name)
      req(predacc_inputs()$site_id)
      req(predacc_inputs()$PA_Key)
      req(predacc_inputs()$EV_Type)
      req(predacc_inputs()$pa_date)

      req(accuracy_data()$pred_acc_data)

      cpdname <- predacc_inputs()$cpd_name
      sitename <- predacc_inputs()$site_id
      PECKey <- predacc_inputs()$PA_Key
      EVType <- predacc_inputs()$EV_Type
      pcvsmc_dat <- predacc_inputs()$pa_date

      df01 <- accuracy_data()$pred_acc_data

      tryCatch(
        {
          df <- df01 %>%
            dplyr::filter(Compound %in% cpdname) %>%
            dplyr::filter(PA_Key %in% PECKey) %>%
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

    #bar plot - Compound----
    predacc_bar <- reactive({

      req(predacc_inputs()$cpd_name)
      req(predacc_inputs()$site_id)
      req(predacc_inputs()$PA_Key)
      req(predacc_inputs()$EV_Type)
      req(predacc_inputs()$pa_date)

      cpdname <- predacc_inputs()$cpd_name
      sitename <- predacc_inputs()$site_id
      PECKey <- predacc_inputs()$PA_Key
      EVType <- predacc_inputs()$EV_Type
      pcvsmc_dat <- predacc_inputs()$pa_date

      plot01 <- ggplot2::ggplot(data = plot_data() ,
                                ggplot2::aes(x=Compound, y=PA,
                                             fill=Compound)) +
        ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge(),color = "#42484e") +
        #ggplot2::scale_fill_manual(values = color_palette_ggplot()) +
        ggplot2::facet_grid(catchment~., scales = "free",space = "free")  +
        ggplot2::xlab("Compound") +
        ggplot2::ylab("PEC/MEC") +
        ggplot2::geom_hline(yintercept=0.5, linetype="dotted", color = "palegreen4", size = 1) +
        ggplot2::geom_hline(yintercept=2, linetype="dashed", color = "firebrick3", size = 1) +
        ggplot2::geom_text(
          ggplot2::aes(0,0.25,label =  "Low (PEC/MEC < 0.5)", family = "sans", vjust = -1), color = "palegreen4", nudge_y = 0.2) +
        ggplot2::geom_text(
          ggplot2::aes(0,2.8,label =  "High (PEC/MEC > 2)", family = "sans", vjust = -1), color = "firebrick3", nudge_y = 0.2)

      return(
        list(
          plot01 = plot01
        )
      )
    })

    output$predacc_plot_bar01 <- renderPlotly (
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(predacc_bar()$plot01)
        req(predacc_inputs()$ggplot_dark_theme)
        req(predacc_inputs()$ggplot_light_theme)

        plot01 <- predacc_bar()$plot01
        ggplot_dark <- predacc_inputs()$ggplot_dark_theme
        ggplot_light <- predacc_inputs()$ggplot_light_theme

        plotly::ggplotly(
          if (global$dark_mode)
            plot01 + ggplot_dark
          else
            plot01 + ggplot_light
        )
      })
    )

    # Targets ----
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

    # Catchments ----
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
          inputId =  ns("selz_cpd"),
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
            inputId =  ns("site_select"),
            label="Select the site:",
            choices= site_name,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(site_name,1)
          )
        }
        else{
          selectInput(
            inputId=ns("site_select"),
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
        req(accuracy_data()$pred_acc_data)

        df01 <- accuracy_data()$pred_acc_data

        PA_Key <- unique(df01$PA_Key)

        selectInput(
          inputId=ns("select_yaxis"),
          label="Select Y axis:",
          choices= PA_Key,
          selected = head(PA_Key,1)
        )
      })
    })

    # UI Output - PERIOD ----
    output$selz_period <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        req(accuracy_data()$pred_acc_data)

        df01 <- accuracy_data()$pred_acc_data

        date <- unique(df01$date)

        selectInput(
          inputId=ns("select_date"),
          label="Select Month:",
          choices= date,
          selected = head(date,1)
        )
      })
    })


    # UI Output - Type ----
    output$selz_type <- renderUI({
      withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

        req(accuracy_data()$pred_acc_data)

        df01 <- accuracy_data()$pred_acc_data

        Type <- unique(df01$Type)

        if(input$selFeature %in% c("matrices" ))
        {
          shinyWidgets::pickerInput(
            inputId =  ns("env_type"),
            label="Select Sample Type:",
            choices = Type,
            options = list(`actions-box` = TRUE),
            multiple = TRUE,
            selected = head(Type,1)
          )
        }
        else{
          selectInput(
            inputId=ns("env_type"),
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

        req(predacc_bar()$plot01)
        req(predacc_inputs()$ggplot_dark_theme)
        req(predacc_inputs()$ggplot_light_theme)

        plot01 <- predacc_bar()$plot01
        ggplot_light <- predacc_inputs()$ggplot_light_theme

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

        req(predacc_bar()$plot01)
        req(predacc_inputs()$ggplot_dark_theme)
        req(predacc_inputs()$ggplot_light_theme)

        plot01 <- predacc_bar()$plot01
        ggplot_light <- predacc_inputs()$ggplot_light_theme

        ggplot_light <- perk_inputs()$ggplot_light_theme

        print(plot01 + ggplot_light)

        dev.off()
      })

    ## End of Download buttons ---


  })
}

## To be copied in the UI
# mod_predacc_dash_ui("predacc_dash_1")

## To be copied in the server
# mod_predacc_dash_server("predacc_dash_1")
