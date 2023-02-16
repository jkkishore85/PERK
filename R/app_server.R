#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#' @importFrom tibble tibble
#' @importFrom bs4Dash toast
#' @importFrom shiny reactiveValues observe observeEvent
app_server <- function(input, output, session) {
  # Global variables
  global <- shiny::reactiveValues()
  shiny::observe({
    global$dark_mode <-  input$dark_mode})

  ## Templates - start ## ----

  # vectors
  Compound <- c("ibuprofen","ibuprofen","ibuprofen","ibuprofen","ibuprofen",
                "ibuprofen","ibuprofen","ibuprofen","ibuprofen","ibuprofen")

  catchment <- c("A","A","A","A","A",
                   "A","B","B","B","B")
  PERIOD <- c("DD/MM/YYYY","DD/MM/YYYY","DD/MM/YYYY","DD/MM/YYYY","DD/MM/YYYY",
              "DD/MM/YYYY","DD/MM/YYYY","DD/MM/YYYY","DD/MM/YYYY","DD/MM/YYYY")

  Type <- c("RiverUP","RiverUP","RiverDown","RiverDown","effluent",
            "effluent","influent","influent","RiverUP","RiverUP"
  )
  key <- c("Concentration","Concentration","Concentration","Concentration","Concentration",
           "Concentration","Concentration","Concentration","Concentration","Concentration")
  date <- c("2021-01-01", "2021-01-01", "2021-01-01", "2021-01-01", "2021-01-01",
            "2021-01-01", "2021-01-01", "2021-01-01", "2021-01-02", "2021-01-02")
  mean_val <- c(100:109)
  SD <- c(1:10)

  compound_list <- c("carbamazepine",
                     "citalopram",
                     "fluoxetine",
                     "venlafaxine")

  # measured template
  mc_template <- tibble::tibble(Compound = Compound,
                                catchment = c("A","A","A","A","A",
                                              "A","A","A","B","B"),
                                PERIOD = date,
                                Type = Type,
                                replicate = c(1,2,1,2,1,2,1,2,1,2),
                                Concentration = c(1.01:10.01)
                                )

  # prescription data template
  rx_template <- tibble::tibble(NM = Compound,
                                catchment = c("A","A","A","A","A",
                                              "A","A","A","A","A"),
                                PERIOD = c(202101:202110),
                                gram = c(1010, 2010, 3010, 4010, 5010,
                                         6010, 7010, 8010, 9010, 10010),
                                kg.month = c(1.01:10.01),
                                date = date )

  # Flow data template
  flow_template <-  tibble::tibble(catchment = c("A","A","A","A","A",
                                                 "A","A","A","A","A"),
                                   Date = PERIOD,
                                   Type = Type,
                                   `Total_Flow(m3)`= mean_val)

  # Compound list template
  cpd_template <- tibble::tibble(Compound = compound_list)

  apifamily_template <- tibble::tibble (NM = c("citalopram",	"citalopram hydrobromide", "citalopram hydrochloride"),
                                       Compound	= c("citalopram","citalopram","citalopram"),
                                       family	 = c("Antidepressants","Antidepressants","Antidepressants"),
                                       bnf_chapter = c("Central Nervous System","Central Nervous System","Central Nervous System"))

  # Site information template
  SI_template <- tibble::tibble (Total_PE = c(109543,
                                             867244,
                                             37714,
                                             18274,
                                             68453),
                                catchment = c("C", "E", "A", "D", "B"),
                                Year = c(2015,2015,2015,2015,2015))

  # Removal efficiency template
  RE_template <- tibble::tibble (Compound = c("carbamazepine","carbamazepine","carbamazepine","carbamazepine","carbamazepine"),
                                 catchment = c("A", "B", "C", "D", "E"),
                                rm_eff = c(-2.53,-20.26, 2.2, 13.01,-20.31 ),
                                rm_eff_SD = c(24.6,16.58, 19.85, 47.37, 168.15))
  # Fexcreta template
  Fx_template <- tibble::tibble (Compound = c("carbamazepine", "citalopram", "venlafaxine", "fluoxetine" ),
                                 low = 	c(13.5, 38, 10.4, 10.3),
                                 high = c(13.5, 38, 10.4, 10.3),
                                 avg = c(13.5, 38, 10.4, 10.3))
  ## Templates - end ##

  mod01 <- mod_user_input_server("user_input_1",
                        id_label = 'Upload Compounds list:',
                        placeholder = "Upload Compounds (.csv)",
                        dwnld_name = 'cpd_template.csv',
                        dwnld_file = cpd_template)
  mod02 <- mod_user_input_server("user_input_2",
                        id_label = 'Upload Prescription Data:',
                        placeholder = "Upload Rx (.csv)",
                        dwnld_name = 'rx_template.csv',
                        dwnld_file = rx_template)
  mod03 <- mod_user_input_server("user_input_3",
                                id_label = 'API Family:',
                                placeholder = "Upload API (.csv)",
                                dwnld_name = 'api_family.csv',
                                dwnld_file = apifamily_template)
  mod04 <- mod_user_input_server("user_input_4",
                                id_label = 'WWTP Site Info (SI):',
                                placeholder = "Upload SI (.csv)",
                                dwnld_name = 'SI_template.csv',
                                dwnld_file = SI_template)
  mod05 <- mod_user_input_server("user_input_5",
                                id_label = 'Removal Efficiency (RE):',
                                placeholder = "Upload RE (.csv)",
                                dwnld_name = 'RE_template.csv',
                                dwnld_file = RE_template)
  mod06 <- mod_user_input_server("user_input_6",
                                id_label = 'Metabolism Data (Fx):',
                                placeholder = "Upload Fx (.csv)",
                                dwnld_name = 'Fx_template.csv',
                                dwnld_file = Fx_template)
  mod07 <- mod_user_input_server("user_input_7",
                                id_label = 'Measured Data (MC):',
                                placeholder = 'Upload MC (.csv):',
                                dwnld_name = 'MC_template.csv',
                                dwnld_file = mc_template)
  mod08 <- mod_user_input_server("user_input_8",
                                id_label = 'WWTP Flow (FL):',
                                placeholder = 'Upload FL (.csv):',
                                dwnld_name = 'FL_template.csv',
                                dwnld_file = flow_template)
  mod09 <- mod_user_input_server("user_input_9",
                                 id_label = 'Environmental Data (EV):',
                                 placeholder = 'Upload EV (.csv):',
                                 dwnld_name = 'EV_template.csv',
                                 dwnld_file = ev_template)

  mod_data_table_server("data_table_1", mod01)
  mod_data_table_server("data_table_2", mod02)
  mod_data_table_server("data_table_3", mod03)
  mod_data_table_server("data_table_4", mod04)
  mod_data_table_server("data_table_5", mod05)
  mod_data_table_server("data_table_6", mod06)
  mod_data_table_server("data_table_7", mod07)
  mod_data_table_server("data_table_8", mod08)
  mod_data_table_server("data_table_9", mod09)

  presc_mod <- mod_presc_dash_server(id = "presc_dash_1",
                      table_dt = mod02,
                      sel_target = mod01,
                      api_family = mod03,
                      wwtp_info =  mod04,
                      global = global)

 pec_mod <- mod_pec_dash_server(id = "pec_dash_1",
                     presc_dat = presc_mod,
                     table_dt = mod02,
                     sel_target = mod01,
                     api_family = mod03,
                     wwtp_info =  mod04,
                     re_info = mod05,
                     fx_info = mod06,
                     global = global)

 mec_mod <- mod_mec_dash_server(id = "mec_dash_1",
                                mec_dat = mod07,
                                table_dt = mod02,
                                sel_target = mod01,
                                api_family = mod03,
                                wwtp_info =  mod04,
                                re_info = mod05,
                                fx_info = mod06,
                                flow_info = mod08,
                                global = global)

 pcmc_mod <- mod_pecvsmec_dash_server(id = "pecvsmec_dash_1",
                                      pec_dat = pec_mod,
                                      mec_dat = mec_mod,
                                      table_dt = mod02,
                                      sel_target = mod01,
                                      api_family = mod03,
                                      wwtp_info =  mod04,
                                      re_info = mod05,
                                      fx_info = mod06,
                                      flow_info = mod08,
                                      global = global
                                      )

 pa_mod <- mod_predacc_dash_server(id = "predacc_dash_1",
                                   pcvsmc_dat = pcmc_mod,
                                      table_dt = mod02,
                                      sel_target = mod01,
                                      api_family = mod03,
                                      wwtp_info =  mod04,
                                      re_info = mod05,
                                      fx_info = mod06,
                                      flow_info = mod08,
                                      global = global)
  # current theme info ---------------------------------------------------------

 shiny::observeEvent(input$dark_mode, {
   bs4Dash::toast(
     title = if (input$dark_mode) "Dark theme on!" else "Light theme on",
     options = list(position = "topRight", class = "bg-warning", autohide = TRUE)
   )
 })
}
