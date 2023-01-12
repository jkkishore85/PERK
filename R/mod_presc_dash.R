#' presc_dash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny fluidRow column selectInput
#'   conditionalPanel checkboxInput actionButton tags
#'   reactive NS tagList moduleServer br downloadButton downloadHandler
#'   renderUI req safeError uiOutput withProgress
#' @importFrom utils head
#' @importFrom stats sd
#' @importFrom zoo as.Date as.yearmon
#' @importFrom plotly ggplotly renderPlotly
#' @importFrom grDevices dev.off pdf postscript
#' @importFrom DT renderDT dataTableOutput datatable
#' @importFrom shinyWidgets pickerInput
#' @importFrom bs4Dash box boxSidebar boxPad
#' @import dplyr tidyr ggplot2
mod_presc_dash_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    ## fluidRow start ----
    shiny::fluidRow(
      shinyjs::useShinyjs(),
      shiny::column(9,
             ## Box start start ----
             bs4Dash::box(width = NULL,
                 closable = FALSE,
                 collapsible = TRUE,
                 maximizable = TRUE,
                 ## Sidebar ----
                 sidebar = bs4Dash::boxSidebar(
                   startOpen = TRUE,
                   id = "pecsidebar12",
                  shiny::br(),
                   bs4Dash::boxPad(
                     width = 8,
                     shiny::fluidRow(
                       shiny::column( width = 4,
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
                       shiny::column( width = 4,
                               shinyWidgets::pickerInput(
                                 inputId = ns("select_plot"),
                                 label = "Plot type:",
                                 choices = c("Line" = "line"
                                             ),
                                 choicesOpt = list(
                                   icon = c("fas fa-chart-line"
                                            ) )
                                 )
                               ),
                       ),
                     shiny::dateRangeInput(ns('date_range'),
                                           label = 'Date range (yyyy-mm-dd):',
                                           start ="2014-12-31",
                                           end = Sys.Date() + 2
                                           ),
                     shiny::selectInput(ns('select_col'), 'Target type:',
                                 c(
                                   'kg/month' = 'kgmonth',
                                   'PNDP' = 'pndp'
                                 ), selected = 'kgmonth'
                     ),
                     shiny::selectInput(
                       ns('select_target'),
                       'Target type:',
                       c(
                         'Compound' = 'Compound'
                         ),
                       selected = 'Compound'
                       ),
                     shiny::uiOutput(ns("selz_site_pec")),
                     shiny::uiOutput(ns("selz_compound_pec")),
                    shiny::actionButton(inputId = ns("gen_plot"),
                                  label = "Generate Graph",
                                  class="btn btn-success action-button")
                     )
                   ),
                 shiny::fluidRow(
                   shiny::column(
                     width = 12,
                     plotly::plotlyOutput(ns("presc_plot_line01"), height="600px"),
                     shiny::tags$hr(),
                    shiny::uiOutput(ns("uidownload_btn")),
                     shiny::tags$hr(),
                     shiny::checkboxInput(ns("pec_show_tab"),
                                   label = "Show Datatable", value = FALSE),
                     shiny::conditionalPanel(
                       "input.pec_show_tab == true",  ns =ns,
                       DT::dataTableOutput(ns("tab_plot_data"))
                       )
                     ) # End of Column
                   ) # End of Fluid row
                 )## End of Box
             )# End of column
      )
  )# End of Taglist
  }

#' presc_dash Server Functions
#'
#' @noRd
mod_presc_dash_server <- function(id,
                                  table_dt,
                                  sel_target,
                                  api_family,
                                  wwtp_info,
                                  global){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    global <- global

    presc_inputs <-shiny::reactive({
     shiny::req(table_dt$up_file)
     shiny::req(api_family$up_file)
      site_id <- input$select_site
      cpd_name <- input$select_compound
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
          ggplot_light_theme = ggplot_light_theme
        )
      )
    })

    getData <-shiny::reactive({
     shiny::req(table_dt$up_file)
     shiny::req(api_family$up_file)

      table_dt_inFile <- table_dt$up_file
      api_family_inFile <- api_family$up_file
      wwtp_info_inFile <- wwtp_info$up_file

      presc_data_fn <- file_input(name = table_dt_inFile$name,
                                 path = table_dt_inFile$datapath)

      api_family_fn <- file_input(name = api_family_inFile$name,
                               path = api_family_inFile$datapath)

      wwtp_info_fn <- file_input(name = wwtp_info_inFile$name,
                               path = wwtp_info_inFile$datapath)
      return(
        list(
          prescdata = presc_data_fn$dataInput,
          apifamily = api_family_fn$dataInput,
          wwtpinfo = wwtp_info_fn$dataInput %>%
            dplyr::mutate(Year = zoo::as.yearmon(Year, "%Y")) %>%
            dplyr::mutate(Year = format(Year, format ="%Y"))
          )
        )
    })

    prescription_full <-shiny::reactive({
     shiny::req(getData()$prescdata)
     shiny::req(getData()$apifamily)
     shiny::req(getData()$wwtpinfo)
     shiny::req(presc_inputs()$cpd_name)
     shiny::req(presc_inputs()$site_id)
     shiny::req(presc_inputs()$ggplot_dark_theme)
     shiny::req(presc_inputs()$ggplot_light_theme)

      df <- getData()$prescdata
      api <- getData()$apifamily
      wwtp <- getData()$wwtpinfo
      cpdname <- presc_inputs()$cpd_name
      sitename <- presc_inputs()$site_id
      ggplot_light_theme <- presc_inputs()$ggplot_light_theme
      ggplot_dark_theme <- presc_inputs()$ggplot_dark_theme

      df_presc_month <- df %>%
        dplyr::left_join(api, by = "NM") %>%
        dplyr::group_by(Compound, catchment, date) %>%
        dplyr::summarise(CPD_kg_month = sum(kg.month, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(date2 = date) %>%
        tidyr::separate(date2, c("month", "Year"),sep = " ") %>%
        dplyr::left_join(dplyr::select(wwtp, c("Total_PE","catchment", "Year")), by = c("catchment", "Year")) %>%
        dplyr::mutate(PNDP_CPD_month = (((CPD_kg_month * 1000000)/30.4167) /Total_PE)*1000 ) %>%
        dplyr::mutate(Compound = forcats::fct_reorder(Compound, dplyr::desc(Compound)))

      df_presc_year <- df %>%
        dplyr::left_join(api, by = "NM") %>%
        dplyr::group_by(Compound, catchment,date) %>%
        dplyr::summarise(kg.month = sum(kg.month, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(date2 = date) %>%
        tidyr::separate(date2, c("month", "Year"), sep = " "  ) %>%
        dplyr::group_by(Compound, catchment,Year) %>%
        dplyr::summarise( CPD_kg_year = sum(kg.month, na.rm = TRUE),
                          CPD_kg_month_YA = mean(kg.month, na.rm = TRUE),
                          CPD_kg_year_SD = sd(kg.month, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(dplyr::select(wwtp, c("Total_PE","catchment", "Year")), by = c("catchment", "Year")) %>%
        dplyr::mutate(PNDP_CPD_year = (((CPD_kg_year * 1000000)/365) /Total_PE)*1000 ) %>%
        dplyr::mutate(Compound = forcats::fct_reorder(Compound, dplyr::desc(Compound)))

      df_presc_full <- df_presc_month %>%
        dplyr::left_join(df_presc_year, by = c("Compound", "catchment",
                                               "Year","Total_PE")) %>%
        dplyr::select(
          c(Compound,catchment, date,
            kgmonth_SM = CPD_kg_month,
            kgmonth_YA = CPD_kg_month_YA,
            kgmonth_YD = CPD_kg_year_SD,
            PNDP_SM = PNDP_CPD_month,
            PNDP_YA = PNDP_CPD_year,
            kg_year = CPD_kg_year
          )) %>%
        dplyr::group_by(Compound,catchment, date ) %>%
        dplyr::mutate(
          kgmonth_SD = sd(c(kgmonth_YA,kgmonth_SM), na.rm = TRUE),
          PERIOD =as.Date(as.POSIXct(zoo::as.yearmon(date)))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Compound = forcats::fct_reorder(Compound, dplyr::desc(Compound)))

      df_kgmonth <- df_presc_full %>%
        dplyr::select(-c(PNDP_SM, PNDP_YA, kg_year, date)) %>%
        tidyr::pivot_longer(!c(Compound, catchment, PERIOD),
                            names_to = "key", values_to = "values") %>%
        dplyr::mutate(Type = "test") %>%
        dplyr::mutate(Type = replace(Type, key %in% c("kgmonth_SM","kgmonth_YA"),"kgMonth"),
                      Type = replace(Type, key %in% c("kgmonth_SD","kgmonth_YD") ,"SD")) %>%
        dplyr::mutate(Key = "test") %>%
        dplyr::mutate(Key = replace(Key, key %in% c("kgmonth_SM","kgmonth_SD"),"SM"),
                      Key = replace(Key, key %in% c("kgmonth_YA","kgmonth_YD") ,"YD")) %>%
        dplyr::select(-c(key)) %>%
        tidyr::pivot_wider(names_from = Type, values_from = values)

      df_pndp <- df_presc_full %>%
        dplyr::select(-c(kgmonth_SM, kgmonth_YA, kg_year,kgmonth_YD,kgmonth_SD, date)) %>%
        tidyr::pivot_longer(!c(Compound, catchment, PERIOD),
                            names_to = "key", values_to = "values") %>%
        dplyr::mutate(Key = "test") %>%
        dplyr::mutate(Key = replace(Key, key %in% c("PNDP_SM"),"SM"),
                      Key = replace(Key, key %in% c("PNDP_YA") ,"YD")) %>%
        dplyr::select(-c(key))

      return(
        list(
          df_presc_month = df_presc_month,
          df_presc_year = df_presc_year,
          df_full = df_presc_full,
          df_kgmonth = df_kgmonth,
          df_pndp = df_pndp
        )
      )
    })

      plot_data <-shiny::reactive({
     shiny::req(prescription_full()$df_kgmonth)
     shiny::req(prescription_full()$df_pndp)
     shiny::req(presc_inputs()$cpd_name)
     shiny::req(presc_inputs()$site_id)

      cpdname <- presc_inputs()$cpd_name
      sitename <- presc_inputs()$site_id

       if (input$select_col == "kgmonth"){
          df01 <- prescription_full()$df_kgmonth
      }
      else if (input$select_col == "pndp"){
        df01 <- prescription_full()$df_pndp
      }
      tryCatch(
        {
          df <- df01 %>%
            dplyr::filter(Compound %in% cpdname) %>%
            dplyr::filter(catchment %in% sitename) %>%
            dplyr::filter(PERIOD > !!input$date_range[1] & PERIOD < !!input$date_range[2])
        },
        error = function(e) {
          stop(shiny::safeError(e))
        }
      )
    })

    catchment <-shiny::reactive({
     shiny::req(table_dt$up_file)
      tryCatch(
        {
          df <- readr::read_csv(table_dt$up_file$datapath) %>%
            dplyr::select(catchment) %>%
            unique()
        },
        error = function(e) {
          stop(shiny::safeError(e))
        }
      )
    })

    targets <-shiny::reactive({

     shiny::req(sel_target$up_file)
      tryCatch(
        {
          df <- readr::read_csv(sel_target$up_file$datapath)
        },
        error = function(e) {
          stop(shiny::safeError(e))
        }
      )
    })

    ## Outputs

    # DT Output: Plot Data ----
    output$tab_plot_data <- DT::renderDT({
      shiny::withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
     shiny::req(plot_data())
      options(DT.options = list(pageLength = nrow(plot_data()),
                                autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(plot_data(),
                    filter = "top",
                    rownames = FALSE,
                    caption = 'List of Entries.',
                    options = list(
                      columnDefs = list(
                        list(className = 'dt-center', targets = "_all")))
      )
      })
    })

    # UI Output - Compounds ----
    output$selz_compound_pec <- shiny::renderUI({
      shiny::withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

       shiny::req(api_family$up_file)
       shiny::req(targets()$Compound)

        target_cpd <- targets()$Compound

        shinyWidgets::pickerInput(
          inputId =  ns("select_compound"),
          label = "Select Compound(s):",
          choices= target_cpd,
          options = list(`actions-box` = TRUE),
          multiple = TRUE,
          selected = head(target_cpd,1)
          )
      })
    })

    # UI Output - site ----
    output$selz_site_pec <- shiny::renderUI({
      shiny::withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

       shiny::req(table_dt$up_file)
       shiny::req(catchment()$catchment)

        site_name <- catchment()$catchment

       shiny::selectInput(
          inputId=ns("select_site"),
          label="Select the site:",
          choices= site_name,
          selected = head(site_name,1)
          )
      })
    })

    # Plot ----
    output$presc_plot_line01 <- plotly::renderPlotly (
      shiny::withProgress(message = 'Data is loading, please wait ...', value = 1:100, {

       shiny::req(presc_inputs()$ggplot_dark_theme)
       shiny::req(presc_inputs()$ggplot_light_theme)

        ggplot_light <- presc_inputs()$ggplot_light_theme
        ggplot_dark <- presc_inputs()$ggplot_dark_theme

        ## line plot object ----
        lineplot_obj <-
          if (input$select_col == "kgmonth")
          {

            plot01 <-  plot_data() %>%
              ggplot2::ggplot(
                ggplot2::aes(x = PERIOD,y= kgMonth,
                             group =  interaction(Key, Compound))) +
              ggplot2::geom_line(
                ggplot2::aes(linetype = Key, color = Compound)
              ) +
              ggplot2::geom_point(
                ggplot2::aes(color=Compound)) +
              ggplot2::scale_linetype_manual(
                "Type",
                values=c(SM = "solid", YD= "twodash")) +
              ggplot2::scale_color_manual(values = grDevices::colorRampPalette(viridis::viridis(8))(10)) +
              ggplot2::labs(title = "Prescription per month",
                            subtitle = "kg month<sup>-1</sup>",
                            y = "kg month<sup>-1</sup>", x = "PERIOD" ) +
              ggplot2::scale_x_date(date_breaks = "1 month",
                                    date_labels = "%b %Y")
          }
        else
        {
          plot01 <-  plot_data() %>%
            ggplot2::ggplot(
              ggplot2::aes(x = PERIOD,y= values,
                           group =  interaction(Key, Compound))) +
            ggplot2::geom_line(
              ggplot2::aes(linetype = Key, color = Compound)
            ) +
            ggplot2::geom_point(
              ggplot2::aes(color=Compound)) +
            ggplot2::scale_linetype_manual(
              "Type",
              values=c(SM = "solid", YD= "twodash")) +
            ggplot2::scale_color_manual(values = grDevices::colorRampPalette(viridis::viridis(8))(10)) +
            ggplot2::labs(
              title = "Population normalised daily load per month",
              subtitle = "PNDP mg day<sup>-1</sup>",
              y = "PNDP mg day<sup>-1</sup>",
              x = "PERIOD" ) +
            ggplot2::scale_x_date(date_breaks = "1 month",
                                  date_labels = "%b %Y")
        }

        plotly::ggplotly(
          if (global$dark_mode) lineplot_obj + ggplot_dark
          else lineplot_obj + ggplot_light
        )
      })
    )

    # Download Buttons ----
    output$uidownload_btn <- shiny::renderUI({
      shiny::withProgress(message = 'Data is loading, please wait ...', value = 1:100, {
        shiny::tags$span(
         shiny::downloadButton(ns('downloaddata_01'), 'Download CSV') ,
         shiny::downloadButton(ns('downloadpdf_01'), 'Download PDF'),
         shiny::downloadButton(ns('downloadeps_01'), 'Download EPS')
        )
      })
    })

    # Download csv01
    output$downloaddata_01 <-shiny::downloadHandler(
      filename = function (){ paste0('plot_data', '.csv')},
      content = function(file) {
        write_csv(as.data.frame(plot_data()), file)
      }
    )

    # Download PDF
    output$downloadpdf_01 <-shiny::downloadHandler(
      filename = function(){ paste('prescplot.pdf',sep = '')},
      content = function(file) {
        # pdf ----
        pdf(file, paper = "a4r",width = 14)
       shiny::req(presc_inputs()$ggplot_dark_theme)
       shiny::req(presc_inputs()$ggplot_light_theme)

        ggplot_light <- presc_inputs()$ggplot_light_theme
        ggplot_dark <- presc_inputs()$ggplot_dark_theme

        lineplot <- if (input$select_col == "kgmonth")
          {
          plot01 <-  plot_data() %>%
            ggplot2::ggplot(
              ggplot2::aes(x = PERIOD,y= kgMonth,
                           group =  interaction(Key, Compound))) +
            ggplot2::geom_line(
              ggplot2::aes(linetype = Key, color = Compound)
            ) +
            ggplot2::geom_point(
              ggplot2::aes(color=Compound)) +
            ggplot2::scale_linetype_manual(
              "Type",
              values=c(SM = "solid", YD= "twodash")) +
            ggplot2::scale_color_manual(values = grDevices::colorRampPalette(viridis::viridis(8))(10)) +
            ggplot2::labs(title = "Prescription per month",
                          subtitle = "kg month<sup>-1</sup>",
                          y = "kg month<sup>-1</sup>", x = "PERIOD" ) +
            ggplot2::scale_x_date(date_breaks = "1 month",
                                  date_labels = "%b %Y")
        }
        else
        {
          plot01 <-  plot_data() %>%
            ggplot2::ggplot(
              ggplot2::aes(x = PERIOD,y= values,
                           group =  interaction(Key, Compound))) +
            ggplot2::geom_line(
              ggplot2::aes(linetype = Key, color = Compound)
            ) +
            ggplot2::geom_point(
              ggplot2::aes(color=Compound)) +
            ggplot2::scale_linetype_manual(
              "Type",
              values=c(SM = "solid", YD= "twodash")) +
            ggplot2::scale_color_manual(values = grDevices::colorRampPalette(viridis::viridis(8))(10)) +
            ggplot2::labs(
              title = "Population normalised daily load per month",
              subtitle = "PNDP mg day<sup>-1</sup>",
              y = "PNDP mg day<sup>-1</sup>",
              x = "PERIOD" ) +
            ggplot2::scale_x_date(date_breaks = "1 month",
                                  date_labels = "%b %Y")
        }
          if (global$dark_mode) lineplot + ggplot_dark

          else lineplot + ggplot_light

        print(lineplot)
        dev.off()
      })

    # Download EPS
    output$downloadeps_01 <-shiny::downloadHandler(
      filename = function(){ paste('prescplot.eps',sep = '')},
      content = function(file) {
        # eps ----
        postscript(file,
                   width = 11.69 , height = 8.27, # inches
                   horizontal = TRUE, onefile = TRUE, paper = "special")
        pdf(file, paper = "a4r",width = 14)
       shiny::req(presc_inputs()$ggplot_dark_theme)
       shiny::req(presc_inputs()$ggplot_light_theme)

        ggplot_light <- presc_inputs()$ggplot_light_theme
        ggplot_dark <- presc_inputs()$ggplot_dark_theme

        lineplot <- if (input$select_col == "kgmonth")
        {
          plot01 <-  plot_data() %>%
            ggplot2::ggplot(
              ggplot2::aes(x = PERIOD,y= kgMonth,
                           group =  interaction(Key, Compound))) +
            ggplot2::geom_line(
              ggplot2::aes(linetype = Key, color = Compound)
            ) +
            ggplot2::geom_point(
              ggplot2::aes(color=Compound)) +
            ggplot2::scale_linetype_manual(
              "Type",
              values=c(SM = "solid", YD= "twodash")) +
            ggplot2::scale_color_manual(values = grDevices::colorRampPalette(viridis::viridis(8))(10)) +
            ggplot2::labs(title = "Prescription per month",
                          subtitle = "kg/month",
                          y = "kg/month", x = "PERIOD" ) +
            ggplot2::scale_x_date(date_breaks = "1 month",
                                  date_labels = "%b %Y")
        }
        else
        {
          plot01 <-  plot_data() %>%
            ggplot2::ggplot(
              ggplot2::aes(x = PERIOD,y= values,
                           group =  interaction(Key, Compound))) +
            ggplot2::geom_line(
              ggplot2::aes(linetype = Key, color = Compound)
            ) +
            ggplot2::geom_point(
              ggplot2::aes(color=Compound)) +
            ggplot2::scale_linetype_manual(
              "Type",
              values=c(SM = "solid", YD= "twodash")) +
            ggplot2::scale_color_manual(values = grDevices::colorRampPalette(viridis::viridis(8))(10)) +
            ggplot2::labs(
              title = "Population normalised daily load per month",
              subtitle = "PNDP mg/day",
              y = "PNDP mg/day",
              x = "PERIOD" ) +
            ggplot2::scale_x_date(date_breaks = "1 month",
                                  date_labels = "%b %Y")
        }
        print(lineplot)
        dev.off()
      })

    ## End of Download buttons -----

    return(
      list(
        presc_data_full =shiny::reactive({
          prescription_full()$df_full})
      )
    )
  })
}

## To be copied in the UI
# mod_presc_dash_ui("presc_dash_1")

## To be copied in the server
# mod_presc_dash_server("presc_dash_1")
