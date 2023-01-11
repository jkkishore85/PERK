#' user_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyWidgets switchInput
#' @importFrom readr write_csv
mod_user_input_ui <- function(id, input_name){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::switchInput(
      inputId = ns("switch"),
      label = input_name,
      value = TRUE,
      labelWidth = "100px",
      offLabel = "Sample Dataset",
      onLabel = "User Input"
    ),
    conditionalPanel(
      condition = "input.switch == false", ns = ns,
                     shiny::fluidRow(
                       column(5,
                                     style=list("padding-right: 28px;"),
                                     shiny::uiOutput(ns("uifile_cpd"))
                       ),
                       column(3,
                                     style=list("padding-left: 5px;"),
                                     tags$b("Download Template:"),
                                     tags$br(),
                                     downloadButton(ns('dwnld_cpd'))
                       )
                     )
    )
  )
}

#' user_input Server Functions
#'
#' @noRd
mod_user_input_server <- function(id,
                                  id_label,
                                  placeholder,
                                  dwnld_name,
                                  dwnld_file
                                  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    vals <- reactiveValues()

    output$uifile_cpd <- renderUI({
      fileInput(ns('up_file'), id_label,
                multiple = FALSE,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                ),
                placeholder =placeholder
      )
    })

    output$dwnld_cpd <-  shiny::downloadHandler(
      filename = function (){ paste0(dwnld_name)},
      content = function(file) {
        readr::write_csv(as.data.frame(dwnld_file), file)
      }
    )

    observe({vals$up_file <- input$up_file})

    return(vals)
  })
}

## To be copied in the UI
# mod_user_input_ui("user_input_1")

## To be copied in the server
# mod_user_input_server("user_input_1")
