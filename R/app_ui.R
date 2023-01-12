#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom bs4Dash bs4DashPage dashboardHeader bs4DashSidebar sidebarUserPanel bs4DashBrand bs4SidebarMenu bs4SidebarMenuItem bs4SidebarMenuSubItem bs4DashBody bs4TabItem bs4TabItems box bs4DashControlbar controlbarMenu controlbarItem skinSelector
#' @importFrom colourpicker colourInput
#' @importFrom shinyWidgets sliderTextInput
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bs4Dash::bs4DashPage(
      #dark = TRUE,
      help = FALSE,
      fullscreen = TRUE,
      scrollToTop = TRUE,

      # Header
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::bs4DashBrand(
          title = "PERK",
          opacity = 0.8),
        fixed = TRUE,
        controlbarIcon = shiny::icon("tools")
),

sidebar = bs4Dash::bs4DashSidebar(
        fixed = TRUE,
        skin = "light",
        status = "primary",
        id = "sidebar",
        bs4Dash::sidebarUserPanel(
          name = "Welcome Onboard!"
        ),
        bs4Dash::bs4SidebarMenu (
          id = "current_tab",
          flat = FALSE,
          compact = FALSE,
          childIndent = TRUE,
          tags$hr(),
          bs4Dash::bs4SidebarMenuItem("Info",
                                      bs4Dash::bs4SidebarMenuSubItem("About", tabName = "about_info"),
                                      icon = icon("circle-info")),
          bs4Dash::bs4SidebarMenuItem("Upload Data",
                            tabName = "user_input",
                            icon = icon("upload")),
          bs4Dash::bs4SidebarMenuItem("Predicted",
                            icon = icon("long-arrow-alt-left"),
                           bs4Dash::bs4SidebarMenuSubItem("Prescription", tabName = "pec_12"),
                           bs4Dash::bs4SidebarMenuSubItem("Predicted Concentrations", tabName = "pec_11")
          ),
          bs4Dash::bs4SidebarMenuItem("Measured",
                            tabName = "mec_11",
                            icon = icon("long-arrow-alt-right")),
          bs4Dash::bs4SidebarMenuItem("Predicted v Measured",
                   icon = icon("exchange-alt"),
                   bs4Dash::bs4SidebarMenuSubItem ("Predicted Vs Measured", tabName = "pcvsmc_11"),
                   bs4Dash::menuSubItem("Prediction Accuracy", tabName = "pcvsmc_12")
          ),

          tags$hr()

        )
      ), # Close of sidebar
body = bs4Dash::bs4DashBody(
#body = bs4Dash::dashboardBody(
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "about_info",
            tabPanel("About", value = "about",
                     sidebarPanel(width=1),
                     mainPanel(width=8,
                               h4("About:"),
                               p("PERK: An in-built application to predict and visualize environmental concentration and risk using pharmaceutical prescription data collected at fine spatial resolution."
                               ),
                               p("PERK acronym for Predicting Environmental concentrations and RisK assessment, is an R package with in-built application tool, aims to facilitate automated modelling and reporting of predicted environmental concentrations of a comprehensive set of pharmaceuticals derived from a wide range of therapeutic classes with different mode of action."),
                               p("The tool helps users,",
                                 tags$li("to input their measured concentration,"),
                                 tags$li("to compare the predicted and measured concentrations of the APIs by means of the PEC/MEC ratio, "),
                                 tags$li("to establish whether the predicted equations used tend to underestimate or overestimate measured values."),
                                 tags$li("It provides a consistent interactive user interface in a familiar dashboard layout, enabling users to visualise predicted values and compare with their measured values without any hassles."),
                                 tags$li("Users can download data and graphs generated using the tool in .csv or publication ready images (.pdf, .eps).")
                               ),
                               h4("Acknowledgments:"),
                               p("This work is a part of the Wastewater Fingerprinting for Public Health Assessment (ENTRUST) project funded by Wessex Water and EPSRC IAA (grant no. EP/R51164X/1)."),
                               br()
                     )
            )
          ),
          bs4Dash::bs4TabItem(
            tabName = "user_input",
            shiny::fluidRow(
              bs4Dash::box(
                title = "Pharmaceuticals Data",
                width = 4,
                closable = FALSE,
                collapsible = TRUE,
                maximizable = TRUE,
                mod_user_input_ui("user_input_1", "Pharmaceuticals list (PL)"),
                mod_user_input_ui("user_input_2", "Prescription Data (Rx)"),
                mod_user_input_ui("user_input_3", "API Data"),
                mod_user_input_ui("user_input_6", "Metabolism Data (Fx)"),
                mod_user_input_ui("user_input_7", "Measured Data (MC)")
              ),
              bs4Dash::box(
                title = "WWTP Data",
                width = 4,
                closable = FALSE,
                collapsible = TRUE,
                maximizable = TRUE,
                mod_user_input_ui("user_input_8", "Flow Data (FL)"),
                mod_user_input_ui("user_input_4", "WWTP Site Info (SI)"),
                mod_user_input_ui("user_input_5", "Removal Efficiency (RE)")
              ),
              bs4Dash::box(
                title = "Environmental Data",
                width = 4,
                closable = FALSE,
                collapsible = TRUE,
                maximizable = TRUE
              )
            )
            ),
          bs4Dash::bs4TabItem (
            tabName = "pec_12",
            mod_presc_dash_ui("presc_dash_1")
          ),
          bs4Dash::bs4TabItem (
            tabName = "pec_11",
            mod_pec_dash_ui("pec_dash_1")
          ),
          bs4Dash::bs4TabItem (
            tabName = "mec_11",
            mod_mec_dash_ui("mec_dash_1")
          ),
          bs4Dash::bs4TabItem (
            tabName = "pcvsmc_11",
            mod_pecvsmec_dash_ui("pecvsmec_dash_1")
          ),
          bs4Dash::bs4TabItem (
            tabName = "pcvsmc_12",
            mod_predacc_dash_ui("predacc_dash_1")
          )
        )# End of bs4TabItems
      ), #close of body

controlbar =  bs4Dash::bs4DashControlbar (
        id = "controlbar",
        skin = "light",
        pinned = FALSE,
        overlay = FALSE,
        width = 400,
        bs4Dash::controlbarMenu(
          id = "controlbarMenu",
          type = "pills",
          bs4Dash::controlbarItem(
            "Skin",
            #icon =  "fa-solid fa-palette",
            bs4Dash::skinSelector()
          ),
          bs4Dash::controlbarItem(
            "Plot",
            shiny::selectInput('plot_colors',
                               'Plot Fill Colours:',
                               c(
                                 'default' = 'viridis',
                                 'Grey' = 'Greys',
                                 'YlOrRd' = 'YlOrRd',
                                 'Pastel1' = 'Pastel1',
                                 'Pastel2' = 'Pastel2',
                                 'Dark2' = 'Dark2',
                                 'Accent' = 'Accent'),
                               selected = 'default'),
            shinyWidgets::sliderTextInput(
              inputId = "widthPlotOutline",
              label = "Plot width:",
              choices = seq(from = 0,
                            to = 5,
                            by = 0.25),
              grid = TRUE
            ),
            shinyWidgets::sliderTextInput(
              inputId = "geomTextDodgeY",
              label = "Text Y (points):",
              choices = seq(from = 0,
                            to = 2,
                            by = 0.1),
              grid = TRUE
            ),
            shinyWidgets::sliderTextInput(
              inputId = "geomTextDodgeX",
              label = "Text X (points):",
              choices = seq(from = 0,
                            to = 2,
                            by = 0.1),
              grid = TRUE
            ),
            colourpicker::colourInput("colPlotOutline",
                                      "Points outline",
                                      "black", allowTransparent = TRUE)

          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PERK"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
