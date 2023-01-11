datatables_tab <- bs4Dash::tabItem (
  tabName = "data_table",
  shiny::fluidRow(

    column(12,
           bs4Dash::tabBox(
             closable = FALSE,
             collapsible = TRUE,
             maximizable = TRUE,
             width = NULL,
             id = "data_table_01",

             shiny::tabPanel("Compounds",
                      mod_data_table_ui("data_table_1")),
             shiny::tabPanel("Rx",
                      mod_data_table_ui("data_table_2")),
             shiny::tabPanel("API",
                      mod_data_table_ui("data_table_3")),
             shiny::tabPanel("SI",
                      mod_data_table_ui("data_table_4")),
             shiny::tabPanel("RE",
                      mod_data_table_ui("data_table_5")),
             shiny::tabPanel("Fx",
                      mod_data_table_ui("data_table_6")),
             shiny::tabPanel("MC",
                      mod_data_table_ui("data_table_7")),
             shiny::tabPanel("FL",
                      mod_data_table_ui("data_table_8"))
           )
    )
  )
)
