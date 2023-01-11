#' data_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom DT renderDT dataTableOutput datatable
mod_data_table_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
      # Output: Data file ----
      DT::dataTableOutput(ns("contents"))
  )
}

#' data_table Server Functions
#'
#' @noRd
mod_data_table_server <- function(id, input_mod){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$contents <- DT::renderDT({

      req(input_mod$up_file)

      tryCatch(
        {
          df <- readr::read_csv(input_mod$up_file$datapath)
        },
        error = function(e) {
          stop(safeError(e))
        }
      )

    })

  })
}

## To be copied in the UI
# mod_data_table_ui("data_table_1")

## To be copied in the server
# mod_data_table_server("data_table_1")
