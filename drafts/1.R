shiny_analysis_tool <- function(
  data_for_shiny = NULL,
  port_number = NULL,
  num_of_sigfigs = 3,
  shiny_list_max = 5000,
  ezr_saved_analysis_csv_file_name = "ezr_saved_analysis.csv",
  ezr_analysis_history_csv_file_name = "ezr_analysis_history.csv"
) {
  # import packages
  import_package(shiny)
  import_package(shinydashboard)
  import_package(shinyjs)
  import_package(DT)
  import_package(magrittr)
  import_package(sodium)
  import_package(effsize)

  # Main login screen
  loginpage <- div(
    id = "loginpage",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    wellPanel(
      tags$h2("LOG IN", class = "text-center",
              style = "padding-top: 0;color:#333; font-weight:600;"),
      textInput("userName", placeholder="Username",
                label = tagList(icon("user"), "Username")),
      passwordInput("passwd", placeholder="Password",
                    label = tagList(icon("unlock-alt"), "Password")),
      br(),
      div(
        style = "text-align: center;",
        actionButton("login", "SIGN IN",
                     style = "color: white; background-color:#3c8dbc;
      padding: 10px 15px; width: 150px; cursor: pointer;
      font-size: 18px; font-weight: 600;"),
        shinyjs::hidden(
          div(
            id = "nomatch",
            tags$p("Oops! Incorrect username or password!",
                   style = "color: red; font-weight: 600;
                 padding-top: 5px;font-size:16px;",
                   class = "text-center"))))))

  # login passwords
  credentials = data.frame(
    username_id = c("1", "shane"),
    passod = sapply(c("1", "105"), password_store),
    permission  = c("basic", "advanced"),
    stringsAsFactors = F
  )

  # create sidebar menu analysis type
  analysis_type_label <-
    c("Saved Analysis",
      "Descriptive Stats",
      "Frequency Table",
      "Histogram (base R function)",
      "Scatterplot",
      "Raincloud plots + Table",
      "Histogram by Group",
      "Multiple Regression",
      "IV / DV Table",
      "Pivot Table",
      "View Data")
  analysis_type <-
    c("saved_analysis",
      "desc_stats",
      "freq_table",
      "histogram",
      "scatterplot",
      "raincloud",
      "histogram_by_group",
      "regression",
      "iv_dv_table",
      "pivot_table",
      "view_data")
  analysis_icon <-
    c("bookmark", # saved analysis
      "list", # desc stats
      "sort-amount-down", # freq table
      "chart-bar", # histogram
      "braille", # scatterplot
      "cloud", # raincloud
      "sliders-h", # histogram by group
      "registered", # regression
      "table", # iv dv table
      "th", # pivot table
      "file-alt" # view data
    )
  sidebar_menu_dt <- data.table(
    analysis_type, analysis_type_label, analysis_icon)

  # set defaults
  number_of_dynamic_ui <- 10
  number_of_static_ui <- 3
  number_of_dynamic_buttons <- 4
  number_of_dynamic_output_sections <- 10
  number_of_max_filter_vars <- 10
  plot_1_height <- 600
  debounce_ms_for_resetting_loaded_inputs <- 500

  # shiny server title
  shiny_update_time <- format(Sys.time(), "%b %d %Y, %H:%M:%S")

  # open saved analysis csv
  ezr_saved_analysis <-
    import_data(ezr_saved_analysis_csv_file_name,
                open_file_if_missing = F)

  # open run analysis csv
  ezr_analysis_history <-
    import_data(ezr_analysis_history_csv_file_name,
                open_file_if_missing = F)

  # create the saved analysis csv if it doesn't exist
  if(is.null(ezr_saved_analysis)) {
    # initialize the csv for saving analysis
    ezr_saved_analysis <-
      data.table(
        id = numeric(),
        time = character(),
        ip = character(),
        input_type = character(),
        input_value = character())
    output_to_csv(
      ezr_saved_analysis,
      gsub(".csv", "", ezr_saved_analysis_csv_file_name),
      timestamp = F)
    paste_message("The following file was created: ",
                  ezr_saved_analysis_csv_file_name)
  }

  # create the run analysis csv if it doesn't exist
  if(is.null(ezr_analysis_history)) {
    # initialize the csv for saving analysis
    ezr_analysis_history <-
      data.table(
        id = numeric(),
        time = character(),
        ip = character(),
        input_type = character(),
        input_value = character())
    output_to_csv(
      ezr_analysis_history,
      gsub(".csv", "", ezr_analysis_history_csv_file_name),
      timestamp = F)
    paste_message("The following file was created: ",
                  ezr_analysis_history_csv_file_name)
  }

  # import saved analysis csv for the first time
  ezr_saved_analysis <-
    import_data(ezr_saved_analysis_csv_file_name)

  # import run analysis csv for the first time
  ezr_analysis_history <-
    import_data(ezr_analysis_history_csv_file_name)

  # var names
  var_names <- c("", names(data_for_shiny))

  # functions to be put inside shiny
  one_var_input <- function() {
    selectizeInput(
      inputId = "var",
      label = "Select a variable:",
      choices = var_names,
      selected = NULL,
      multiple = F,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  iv_input <- function(multiple = T) {
    selectizeInput(
      inputId = "iv",
      label = "Select IV(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  dv_input <- function(multiple = T) {
    selectizeInput(
      inputId = "dv",
      label = "Select DV(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  iv_order_input <- function(iv_order, backup_iv_order) {
    selectizeInput(
      inputId = "iv_order",
      label = "Order for values in IV (Top to Bottom):",
      choices = "",
      selected = NULL,
      multiple = T,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  include_totals_input <- function() {
    selectizeInput(
      inputId = "include_totals",
      label = "Include or exclude Totals?",
      choices = c("Include Totals", "Exclude Totals"),
      selected = NULL,
      multiple = F,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  function_name_input <- function() {
    selectizeInput(
      inputId = "function_name",
      label = "Select a function for the cells:",
      choices = c(
        "count_after_removing_na",
        "mean_after_removing_na",
        "median_after_removing_na",
        "sd_after_removing_na",
        "min_after_removing_na",
        "max_after_removing_na",
        "geometric_mean_add_1_to_all_values"),
      selected = NULL,
      multiple = F,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  row_var_input <- function(multiple = T) {
    selectizeInput(
      inputId = "row_vars",
      label = "Select row variable(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  col_var_input <- function(multiple = T) {
    selectizeInput(
      inputId = "col_vars",
      label = "Select column variable(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  cell_var_input <- function() {
    selectizeInput(
      inputId = "cell_var",
      label = "Select a cell variable:",
      choices = var_names,
      selected = NULL,
      multiple = F,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  saved_analysis_input <- function(saved_analysis_reactive_dt) {
    if(is.null(saved_analysis_reactive_dt)) {
      saved_analysis_to_choose_from <- ""
    } else if(missing(saved_analysis_reactive_dt)) {
      saved_analysis_to_choose_from <- ""
    } else {
      dt01 <- saved_analysis_reactive_dt
      saved_analysis_ids <- sort(unique(dt01$id))
      saved_analysis_to_choose_from <- rep(NA, length(saved_analysis_ids))
      for(i in seq_along(saved_analysis_ids)) {
        specific_saved_analysis_dt <-
          dt01[id == saved_analysis_ids[i]]
        saved_analysis_temp_1 <-
          specific_saved_analysis_dt[
            input_type == "sidebar_menu"][["input_value"]]
        saved_analysis_temp_2_var_type <-
          unique(specific_saved_analysis_dt[
            input_type != "sidebar_menu"][["input_type"]])
        saved_analysis_temp_3_string <-
          paste0(
            sapply(saved_analysis_temp_2_var_type, function(x) {
              paste0(
                x, ": ", paste0(
                  specific_saved_analysis_dt[
                    input_type == x][["input_value"]],
                  collapse = ", "))}),
            collapse = " / ")
        saved_analysis_to_choose_from[i] <-
          paste0(saved_analysis_ids[i], " - ",
                 saved_analysis_temp_1, " / ",
                 saved_analysis_temp_3_string)
      }
    }
    selectizeInput(
      inputId = "saved_analysis_choices",
      label = "Choose the saved analysis to view: ",
      choices = saved_analysis_to_choose_from,
      multiple = T,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  filter_var_ids <-
    paste0("filter_var_", seq_len(number_of_max_filter_vars))
  names_of_all_inputs_for_analysis <- c(
    "sidebar_menu", "var", "iv", "dv", "iv_order", "include_totals",
    "function_name", "row_vars", "col_vars", "cell_var", "sigfig",
    "saved_analysis_choices", "vars_for_outliers", "sigfig",
    "names_of_filter_vars",
    filter_var_ids
  )
  outlier_input <- function() {
    selectizeInput(
      inputId = "vars_for_outliers",
      label = "Remove outliers in the following variable(s):",
      choices = var_names,
      multiple = T,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  sigfig_input <- function() {
    numericInput(
      inputId = "sigfig",
      label = "Number of significant figures to display:",
      value = num_of_sigfigs,
      min = 0, max = 20, step = 1)
  }
  filter_vars_input_1 <- function() {
    selectizeInput(
      inputId = "names_of_filter_vars",
      label = "Choose filtering variables:",
      choices = var_names,
      multiple = T,
      options = list(maxOptions = shiny_list_max),
      width = "100%")
  }
  static_input_uis <-
    list(renderUI({sigfig_input()}),
         renderUI({outlier_input()}),
         renderUI({filter_vars_input_1()}))
  analysis_input_1 <- function(active_tab, saved_analysis_reactive_dt) {
    if(active_tab %in% c("saved_analysis")) {
      uis <- list(renderUI({saved_analysis_input(saved_analysis_reactive_dt)}))
    }
    if(active_tab %in% c("desc_stats", "freq_table", "histogram")) {
      uis <- list(renderUI({one_var_input()}))
    }
    if(active_tab == "scatterplot") {
      uis <- list(renderUI({iv_input(multiple = F)}),
                  renderUI({dv_input(multiple = F)}))
    }
    if(active_tab %in% c("raincloud", "histogram_by_group")) {
      uis <- list(renderUI({iv_input(multiple = F)}),
                  renderUI({dv_input(multiple = F)}),
                  renderUI({iv_order_input()}))
    }
    if(active_tab == "regression") {
      uis <- list(renderUI({iv_input(multiple = T)}),
                  renderUI({dv_input(multiple = T)}))
    }
    if(active_tab == "iv_dv_table") {
      uis <- list(renderUI({iv_input(multiple = T)}),
                  renderUI({dv_input(multiple = T)}),
                  renderUI({include_totals_input()}),
                  renderUI({function_name_input()}))
    }
    if(active_tab == "pivot_table") {
      uis <- list(renderUI({row_var_input(multiple = T)}),
                  renderUI({col_var_input(multiple = T)}),
                  renderUI({cell_var_input()}),
                  renderUI({function_name_input()}),
                  renderUI({include_totals_input()}))
    }
    if(active_tab %in% c("view_data")) {
      uis <- list()
    }
    return(uis)
  }
  # buttons
  run_btn_input <- function() {
    actionButton(inputId = "run_btn", label = "Run", width = "80%")
  }
  save_btn_input <- function() {
    actionButton(inputId = "save_btn", label = "Save", width = "80%")
  }
  load_saved_btn_input <- function() {
    actionButton(inputId = "load_saved_btn",
                 label = "Load Saved Analysis", width = "80%")
  }
  delete_saved_btn_input <- function() {
    actionButton(inputId = "delete_saved_btn",
                 label = "Delete Saved Analysis", width = "80%")
  }
  button_input_1 <- function(active_tab) {
    if(active_tab == "saved_analysis") {
      buttons <- list(renderUI({load_saved_btn_input()}),
                      renderUI({delete_saved_btn_input()}))
    } else if(active_tab == "view_data") {
      buttons <- list(renderUI({run_btn_input()}))
    } else {
      buttons <- list(renderUI({run_btn_input()}),
                      renderUI({save_btn_input()}))
    }
    return(buttons)
  }
  output_area <- function(active_tab) {
    sections <- list(renderUI({uiOutput("message_to_user_01")}),
                     renderUI({uiOutput("message_to_user_02")}),
                     renderUI({textOutput("outlier_report_1")}),
                     renderUI({DT::DTOutput("outlier_report_table")}))
    if(active_tab %in% c("desc_stats", "freq_table", "regression",
                         "iv_dv_table", "pivot_table", "view_data")) {
      sections <- c(sections, list(renderUI({DT::DTOutput("table_1")})))
    }
    if(active_tab %in% c("histogram", "scatterplot")) {
      sections <- c(sections, list(renderUI({plotOutput("plot_1")})))
    }
    if(active_tab %in% c("raincloud", "histogram_by_group")) {
      sections <- c(sections, list(
        renderUI({plotOutput("plot_1", height = plot_1_height)}),
        renderUI({DT::DTOutput("table_2")}),
        renderUI({DT::DTOutput("table_3")})))
    }
    return(sections)
  }
  names_of_output_sections <- c(
    "message_to_user_01", "message_to_user_02",
    "outlier_report_1", "outlier_report_table",
    "plot_1", "table_1", "table_2"
  )
  outputs_to_clear_on_sidebar_menu_click <- c(
    paste0("dynamic_input_", seq_len(number_of_dynamic_ui)),
    paste0("dynamic_btn_", seq_len(number_of_dynamic_buttons)),
    paste0("dynamic_output_section_",
           seq_len(number_of_dynamic_output_sections)),
    names_of_output_sections)
  inputs_to_save <- function(analysis) {
    if(analysis %in% c("desc_stats", "freq_table", "histogram")) {
      inputs_to_save <- "var"
    }
    if(analysis %in% c("scatterplot", "regression")) {
      inputs_to_save <- c("iv", "dv")
    }
    if(analysis %in% c("raincloud", "histogram_by_group")) {
      inputs_to_save <- c("iv", "dv", "iv_order")
    }
    if(analysis %in% c("iv_dv_table")) {
      inputs_to_save <- c("iv", "dv", "include_totals", "function_name")
    }
    if(analysis %in% c("pivot_table")) {
      inputs_to_save <- c("row_vars", "col_vars", "cell_var", "function_name")
    }
    inputs_to_save <- c(
      inputs_to_save, "sidebar_menu", "sigfig",
      "vars_for_outliers", "names_of_filter_vars")
    return(inputs_to_save)
  }
  # header, sidebar, and body
  header <- dashboardHeader(title = "M", uiOutput("logoutbtn"))
  sidebar <- dashboardSidebar(uiOutput("sidebar_01"))
  body <- dashboardBody(
    shinyjs::useShinyjs(),
    uiOutput("body"),
    fluidRow(
      column(
        width = 4,
        lapply(seq_len(number_of_dynamic_ui), function(i) {
          uiOutput(paste0("dynamic_input_", i))}),
        lapply(seq_len(number_of_dynamic_buttons), function(i) {
          column(6, uiOutput(paste0("dynamic_btn_", i)))}),
        lapply(seq_len(number_of_static_ui), function(i) {
          uiOutput(paste0("static_input_", i))}),
        lapply(seq_len(number_of_max_filter_vars), function(i) {
          uiOutput(paste0("filter_var_", i, "_checkboxes"))})),
      # lapply(1:50, function(i) textOutput(paste0("testing", i)))),
      column(
        width = 8,
        lapply(seq_len(number_of_dynamic_output_sections), function(i) {
          uiOutput(paste0("dynamic_output_section_", i))})),
      tags$style(type='text/css',
                 ".selectize-dropdown-content {max-height: 500px; }
               .content-wrapper, .right-side {background-color: #FFFFFF;}")))
  # ui
  ui <- dashboardPage(header, sidebar, body, skin = "blue")
  # server
  server <- function(input, output, session) {
    login = FALSE
    USER <- reactiveValues(login = login)
    # observe login
    observe({
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            if(length(which(credentials$username_id==Username))==1) {
              pasmatch  <- credentials["passod"][
                which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              if(pasverify) {
                USER$login <- TRUE
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE,
                                time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(
                  id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE,
                              time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(
                id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
        }
      }
    })
    # observe logout
    output$logoutbtn <- renderUI({
      req(USER$login)
      tags$li(a(icon("fa fa-sign-out"), "Logout",
                href="javascript:window.location.reload(true)"),
              class = "dropdown",
              style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    # sidebar ui
    output$sidebar_01 <- renderUI({
      if(USER$login == TRUE ){
        sidebarMenu(
          id = "sidebar_menu",
          lapply(1:nrow(sidebar_menu_dt),
                 function(i) {
                   menuItem(sidebar_menu_dt$analysis_type_label[i],
                            tabName = sidebar_menu_dt$analysis_type[i],
                            icon = icon(sidebar_menu_dt$analysis_icon[i]))}))
      }
    })
    # body ui
    output$body <- renderUI({
      if (!USER$login) loginpage
    })
    # reactive values
    reactive_values <- reactiveValues()
    reactive_dt <- reactiveValues()
    USER <- reactiveValues(login = F)
    # default reactive dt
    reactive_dt$saved_analysis <- import_data(
      ezr_saved_analysis_csv_file_name, open_file_if_missing = F)
    reactive_dt$run_analysis <- import_data(
      ezr_analysis_history_csv_file_name, open_file_if_missing = F)
    reactive_dt$loaded_inputs <- NULL
    # static input uis
    observeEvent(USER$login, {
      req(USER$login)
      lapply(seq_along(static_input_uis), function(i) {
        output[[paste0("static_input_", i)]] <- static_input_uis[[i]]
      })
    })
    # analysis input uis
    observe({
      input$sidebar_menu
      input$delete_saved_btn
      req(input$sidebar_menu)
      # clear dynamic ui
      lapply(outputs_to_clear_on_sidebar_menu_click, function(x) {
        output[[x]] <- {}
      })
      # add input ui for given analysis
      analysis_input_uis <- analysis_input_1(
        input$sidebar_menu, reactive_dt$saved_analysis)
      lapply(seq_along(analysis_input_uis), function(i) {
        output[[paste0("dynamic_input_", i)]] <- analysis_input_uis[[i]]
      })
      # add buttons for given analysis
      button_uis <- button_input_1(input$sidebar_menu)
      lapply(seq_along(button_uis), function(i) {
        output[[paste0("dynamic_btn_", i)]] <- button_uis[[i]]
      })
      # add output sections for given analysis
      output_sections <- output_area(input$sidebar_menu)
      lapply(seq_along(output_sections), function(i) {
        output[[paste0("dynamic_output_section_", i)]] <-
          output_sections[[i]]
      })
    })
    # observer for iv order
    observe({
      input$iv
      # fill in values
      req(input$sidebar_menu)
      if(input$sidebar_menu %in% c("raincloud", "histogram_by_group")) {
        if(!is.null(input$iv)) {
          choices <- if(input$iv == "") "" else
            sort(unique(data_for_shiny[[input$iv]]))
          iv_order_to_update_to <- if(length(choices) > 20) NULL else choices
          updateSelectizeInput(
            session, inputId = "iv_order",
            choices = choices,
            selected = iv_order_to_update_to)}}
    })
    # observer for filter
    observe({
      filter_vars <- input$names_of_filter_vars
      lapply(seq_len(number_of_max_filter_vars), function(i) {
        if(is.null(filter_vars)) {
          output[[paste0("filter_var_", i, "_checkboxes")]] <- {}
        } else {
          output[[paste0("filter_var_", i, "_checkboxes")]] <-
            if(is.na(filter_vars[i])) {} else {
              choices <- sort(unique(
                data_for_shiny[[filter_vars[i]]]), na.last = F)
              renderUI({checkboxGroupInput(
                inputId = paste0("filter_var_", i),
                label = paste0(filter_vars[i], ":"),
                choices = choices, inline = T)})}}})})
    # observer for newly loaded inputs
    observe({
      input$sidebar_menu
      input$iv
      input$names_of_filter_vars
      dt01 <- reactive_dt$loaded_inputs
      unique_input_types <- setdiff(
        unique(dt01[["input_type"]]), "sidebar_menu")
      lapply(setdiff(unique_input_types, filter_var_ids), function(x) {
        updateSelectizeInput(
          session, inputId = x,
          selected = dt01[input_type == x][["input_value"]])
      })
      filter_var_ids_to_load <- intersect(
        unique(dt01[["input_type"]]), filter_var_ids)
      lapply(filter_var_ids_to_load, function(x) {
        updateCheckboxGroupInput(
          session, inputId = x,
          selected = dt01[input_type == x][["input_value"]])
      })
      # isolate({reactive_dt$loaded_inputs <- NULL})
    })
    # load saved analysis
    observeEvent(input$load_saved_btn, {
      req(input$saved_analysis_choices)
      # get id of analysis to load on "load saved" button
      id_of_analysis_to_load <-
        gsub("(^\\d+) - .*", "\\1",
             input$saved_analysis_choices[1]) # use only the first
      # load the saved analysis
      dt01 <- reactive_dt$saved_analysis[id == id_of_analysis_to_load]
      # switch to the tab
      sidebar_menu_item_to_switch_to <-
        dt01[input_type == "sidebar_menu"][["input_value"]]
      updateTabItems(
        session, inputId = "sidebar_menu",
        selected = sidebar_menu_item_to_switch_to)
      # reactive dt for loaded inputs
      reactive_dt$loaded_inputs <- dt01
    })
    # reset loaded input values
    load_saved_input_values <- eventReactive(
      c(input$sidebar_menu, input$iv, input$names_of_filter_vars,
        reactive_dt$loaded_inputs), {
          reactive_values$ready_to_load_input_values <- F
          dt01 <- reactive_dt$loaded_inputs
          unique_input_types <- setdiff(
            unique(dt01[["input_type"]]), "sidebar_menu")
          lapply(setdiff(unique_input_types, filter_var_ids), function(x) {
            updateSelectizeInput(
              session, inputId = x,
              selected = dt01[input_type == x][["input_value"]])
          })
          filter_var_ids_to_load <- intersect(
            unique(dt01[["input_type"]]), filter_var_ids)
          lapply(filter_var_ids_to_load, function(x) {
            updateCheckboxGroupInput(
              session, inputId = x,
              selected = dt01[input_type == x][["input_value"]])
          })
          reactive_values$ready_to_load_input_values <- T
        }, ignoreInit = T)
    # debounce for resetting
    load_saved_input_values_debounced <-
      debounce(load_saved_input_values,
               debounce_ms_for_resetting_loaded_inputs)
    observeEvent(load_saved_input_values_debounced(),{
      req(reactive_values$ready_to_load_input_values == T)
      reactive_dt$loaded_inputs <- NULL
    })
    # update input on load saved button click
    observeEvent(input$delete_saved_btn, {
      req(input$sidebar_menu == "saved_analysis")
      # load the saved analysis csv
      ezr_saved_analysis <- reactive_dt$saved_analysis
      # get id of the saved analysis
      id_of_saved_analysis <-
        gsub("(^\\d+) - .*", "\\1", input$saved_analysis_choices)
      # get the dt for saved analysis
      ezr_saved_analysis <-
        ezr_saved_analysis[!id %in% id_of_saved_analysis]
      if(nrow(ezr_saved_analysis) == 0) {
        ezr_saved_analysis <-
          data.table(
            id = numeric(),
            time = character(),
            ip = character(),
            input_type = character(),
            input_value = character())
      } else {
        # update id numbers
        ezr_saved_analysis[, id := match(id, unique(id))]
      }
      # save to csv
      output_to_csv(
        ezr_saved_analysis,
        gsub(".csv", "", ezr_saved_analysis_csv_file_name),
        timestamp = F)
      # update the reactive dt
      reactive_dt$saved_analysis <- ezr_saved_analysis
    })
    # run analysis
    observeEvent(input$run_btn, {
      # get active tab
      active_tab <- input$sidebar_menu
      # get data
      dt01 <- data_for_shiny
      # remove outliers
      if(!is.null(input$vars_for_outliers)) {
        for(i in seq_along(input$vars_for_outliers)) {
          dt01 <- dt01[
            dt01[[input$vars_for_outliers[i]]] %in%
              remove_outliers(dt01[[input$vars_for_outliers[i]]]), ]
        }
      }
      # filter data
      if(length(input$names_of_filter_vars) > 0) {
        for(i in seq_along(input$names_of_filter_vars)) {
          dt01 <- dt01[dt01[[input$names_of_filter_vars[i]]] %in% gsub(
            "^$", NA, input[[paste0("filter_var_", i)]]), ]}}
      # if iv_order was used as a filter
      if(!is.null(input$iv_order)) {
        dt01 <- dt01[dt01[[input$iv]] %in% input$iv_order, ]
      }
      # descriptive stats
      if(active_tab == "desc_stats") {
        output$table_1 <- DT::renderDataTable({
          descriptive_stats_for_a_variable(
            dt01[[input$var]], num_of_sigfigs = input$sigfig)})}
      # frequency table
      if(active_tab == "freq_table") {
        output$table_1 <- DT::renderDataTable({
          tabulate_vector(
            dt01[[input$var]], num_of_sigfigs = input$sigfig)})}
      # histogram
      if(active_tab == "histogram") {
        output$plot_1 <- renderPlot({
          hist(dt01[[input$var]],
               main = paste0("Histogram of ", input$var),
               xlab = input$var,
               breaks = "FD")
        }, width = 800, height = plot_1_height)}
      # scatterplot
      if(active_tab == "scatterplot") {
        output$plot_1 <- renderPlot({
          withProgress(
            message = "Generating the plot...",
            scatterplot_v2(
              data = dt01,
              name_of_x_var = input$iv,
              name_of_y_var = input$dv,
              annotate_stats = T))
        }, width = 800, height = plot_1_height)}
      # raincloud or histogram by group
      if(active_tab %in% c("raincloud", "histogram_by_group")) {
        if(length(unique(dt01[[input$iv]])) > 20) {
          output$message_to_user_01 <- renderText({
            paste0("The IV has more than 20 levels. ",
                   "The server cannot handle this operation.")
          })
        } else {
          output$message_to_user_01 <- renderText({
            "Generating and uploading the plot..."})
          output$plot_1 <- renderPlot({withProgress(
            message = "Generating the plot...",
            if(active_tab == "raincloud") {
              raincloud_by_group(
                data = dt01,
                name_of_dv = input$dv,
                name_of_iv = input$iv,
                order_of_groups_top_to_bot = input$iv_order)
            } else if(active_tab == "histogram_by_group") {
              histogram_by_group(
                data = dt01,
                name_of_dv = input$dv,
                name_of_iv = input$iv,
                order_of_groups_top_to_bot = input$iv_order)
            })}, width = 800, height = plot_1_height)
          output$table_2 <- DT::renderDataTable({
            table_with_iv_and_dv(
              data = dt01,
              dv_name = input$dv,
              iv_name = input$iv,
              order_of_groups_top_to_bot = input$iv_order,
              num_of_sigfigs = input$sigfig)})
          output$table_3 <- DT::renderDataTable({
            post_hoc_test(data = dt01,
                          iv_name = input$iv,
                          dv_name = input$dv,
                          num_of_sigfigs_for_means_and_d = input$sigfig)})
          output$message_to_user_01 <- renderText({
            t_or_f_test(data = dt01, iv_name = input$iv,
                        dv_name = input$dv)})
        }
      }
      # regression
      if(active_tab == "regression") {
        output$table_1 <- DT::renderDataTable({
          regression_table_for_shiny(
            data = dt01,
            dv_name = input$dv,
            iv_names = input$iv,
            num_of_sigfigs = input$sigfig)})}
      # iv dv table
      if(active_tab == "iv_dv_table") {
        # limit by number of unique values
        number_of_rows <-
          prod(sapply(input$iv, function(x) {
            length(unique(dt01[[x]]))}))
        if(number_of_rows > 100) {
          output$message_to_user_01 <- renderText({
            paste0("There will be more than 100 rows in the table. ",
                   "The server cannot handle this operation yet.")})
          return()}
        include_totals_t_or_f <-
          if(input$include_totals == "Include Totals") T else F
        output$table_1 <- DT::renderDataTable({
          iv_dv_table(
            data = dt01,
            row_var_names = input$iv,
            col_var_names = input$dv,
            include_totals = include_totals_t_or_f,
            cell_function_name = input$function_name,
            num_of_sigfigs = input$sigfig)})}
      # pivot_table
      if(active_tab == "pivot_table") {
        # limit by number of unique values
        number_of_rows <- prod(sapply(input$row_vars, function(x) {
          length(unique(dt01[[x]])) + 1}))
        number_of_columns <- prod(sapply(input$col_vars, function(x) {
          length(unique(dt01[[x]])) + 1}))
        if(number_of_rows > 100) {
          output$message_to_user_01 <- renderText({
            paste0("There will be more than 100 rows in the table. ",
                   "The server cannot handle this operation yet.")})
        } else if(number_of_columns > 50) {
          output$message_to_user_01 <- renderText({
            paste0("There will be more than 50 columns in the table. ",
                   "The server cannot handle this operation yet.")})
        }
        output$table_1 <- DT::renderDataTable({
          pivot_table_for_shiny(
            data = dt01,
            row_var_names = input$row_vars,
            col_var_names = input$col_vars,
            cell_var_name = input$cell_var,
            cell_function_name = input$function_name,
            num_of_sigfigs = input$sigfig
          )
        })}
      # view_data
      if(active_tab == "view_data") {
        output$table_1 <- DT::renderDataTable({dt01})}

      # record action
      dt11 <- reactive_dt$run_analysis
      # save inputs
      input_names <- inputs_to_save(input$sidebar_menu)
      if(length(input$names_of_filter_vars)) {
        input_names <- c(
          input_names,
          paste0("filter_var_", seq_along(input$names_of_filter_vars)))
      }
      input_values <- lapply(seq_along(input_names), function(x) {
        input[[input_names[x]]]
      })
      # new entry
      new_input_type <-
        unlist(rep(input_names, lengths(input_values)))
      new_input_value <-
        unlist(input_values)
      # id, time, ip, etc
      id <- c(dt11$id,
              rep(max(c(dt11$id, 0)) + 1,
                  length(new_input_value)))
      time <- c(dt11$time,
                rep(format(Sys.time(), "%b %d %Y %H%M%S"),
                    length(new_input_value)))
      ip <- c(dt11$ip,
              rep(session$request$REMOTE_ADDR,
                  length(new_input_value)))
      new_input_type_all <-
        c(dt11$input_type, new_input_type)
      new_input_value_all <-
        c(dt11$input_value, new_input_value)
      # put it together and export to csv
      run_analysis_dt <- data.table(
        id, time, ip,
        input_type = new_input_type_all,
        input_value = new_input_value_all)
      # save to csv
      output_to_csv(
        run_analysis_dt,
        gsub(".csv", "", ezr_analysis_history_csv_file_name),
        timestamp = F)
      # save again
      reactive_dt$run_analysis <- run_analysis_dt
    })
    # save analysis
    observeEvent(input$save_btn, {
      # ezr_saved_analysis <-
      #   import_data(ezr_saved_analysis_csv_file_name,
      #               open_file_if_missing = F)
      dt01 <- reactive_dt$saved_analysis
      # save inputs
      input_names <- inputs_to_save(input$sidebar_menu)
      if(length(input$names_of_filter_vars)) {
        input_names <- c(
          input_names,
          paste0("filter_var_", seq_along(input$names_of_filter_vars)))
      }
      input_values <- lapply(seq_along(input_names), function(x) {
        input[[input_names[x]]]
      })
      # new entry
      new_input_type <-
        unlist(rep(input_names, lengths(input_values)))
      new_input_value <-
        unlist(input_values)
      # message to user
      output$message_to_user_01 <- renderText({
        "The following analysis was saved:\n"
      })
      output$message_to_user_02 <- renderUI({
        DT::dataTableOutput("saved_analysis_summary_dt")
      })
      output$saved_analysis_summary_dt <- DT::renderDataTable({
        data.table(input_type = new_input_type,
                   input_value = new_input_value)
      }, options = list(pageLength = 100))
      # id, time, ip, etc
      id <- c(dt01$id,
              rep(max(c(dt01$id, 0)) + 1,
                  length(new_input_value)))
      time <- c(dt01$time,
                rep(format(Sys.time(), "%b %d %Y %H%M%S"),
                    length(new_input_value)))
      ip <- c(dt01$ip,
              rep(session$request$REMOTE_ADDR,
                  length(new_input_value)))
      new_input_type_all <-
        c(dt01$input_type, new_input_type)
      new_input_value_all <-
        c(dt01$input_value, new_input_value)
      # put it together and export to csv
      saved_analysis_dt <- data.table(
        id, time, ip,
        input_type = new_input_type_all,
        input_value = new_input_value_all)
      # save to csv
      output_to_csv(
        saved_analysis_dt,
        gsub(".csv", "", ezr_saved_analysis_csv_file_name),
        timestamp = F)
      # save again
      reactive_dt$saved_analysis <- saved_analysis_dt
    })
    # test_values <- list(
    #   renderPrint(c("reactive_values$inputs_to_load:", reactive_values$inputs_to_load)),
    #   renderPrint(c("reactive_values$need_to_load_main_input_values:", reactive_values$need_to_load_main_input_values)),
    #   renderPrint(c("reactive_values$need_to_load_filter_vars:", reactive_values$need_to_load_filter_vars)),
    #   renderPrint(c("reactive_values$need_to_load_iv_order:", reactive_values$need_to_load_iv_order)),
    #   renderPrint(c("sidebar_menu:", input$sidebar_menu)),
    #   renderPrint(c("var:", input$var)),
    #   renderPrint(c("iv:", input$iv)),
    #   renderPrint(c("dv:", input$dv)),
    #   renderPrint(c("iv_order:", input$iv_order)),
    #   renderPrint(c("include_totals:", input$include_totals)),
    #   renderPrint(c("function_name:", input$function_name)),
    #   renderPrint(c("row_vars:", input$row_vars)),
    #   renderPrint(c("col_vars:", input$col_vars)),
    #   renderPrint(c("cell_var:", input$cell_var)),
    #   renderPrint(c("saved_analysis_choices:", input$saved_analysis_choices)),
    #   renderPrint(c("vars_for_outliers:", input$vars_for_outliers)),
    #   renderPrint(c("sigfig:", input$sigfig)),
    #   renderPrint(c("names_of_filter_vars:", input$names_of_filter_vars)),
    #   renderPrint(c("reactive_values$loaded_filter_vars:", reactive_values$loaded_filter_vars)),
    #   renderPrint(c("run_btn:", input$run_btn)),
    #   renderPrint(c("save_btn:", input$save_btn)),
    #   renderPrint(c("load_saved_btn:", input$load_saved_btn)),
    #   renderPrint(c("delete_saved_btn:", input$delete_saved_btn)),
    #   renderPrint(c("reac dt saved analy row:", sort(unique(reactive_dt$saved_analysis$id)))),
    #   renderPrint(c("input$saved_analysis_choices[1]:", input$saved_analysis_choices[1])),
    #   renderPrint(c("reactive_dt$loaded_inputs:", reactive_dt$loaded_inputs)),
    #   renderPrint(c("reactive_values$loaded_iv_order :", reactive_values$loaded_iv_order)),
    #   renderPrint(c("reactive_values$filter_var_checkboxes_ready:", reactive_values$filter_var_checkboxes_ready)),
    #   renderPrint(c("reactive_values$ready_to_load_input_ui:", reactive_values$ready_to_load_input_ui)),
    #   renderPrint(c("reactive_values$ready_to_load_input_values:", reactive_values$ready_to_load_input_values)),
    #   renderPrint(c("reactive_values$filter_var_ids_to_load:", reactive_values$filter_var_ids_to_load)),
    #   renderPrint(c("reactive_values$filter_var_1_values_to_load:", reactive_values$filter_var_1_values_to_load)),
    #   renderPrint(c("reactive_values$analysis_option_ui_ready:", reactive_values$analysis_option_ui_ready)),
    #   renderPrint(c("reactive_values$input_ui_main_ready:", reactive_values$input_ui_main_ready)),
    #   renderPrint(c("reactive_values$iv_order_ui_ready:", reactive_values$iv_order_ui_ready)),
    #   renderPrint(c("reactive_values$loaded_iv_order:", reactive_values$loaded_iv_order)),
    #   renderPrint(c("reactive_values$filter_var_checkboxes_ready:", reactive_values$filter_var_checkboxes_ready)),
    #   renderPrint(c("reactive_values$ready_to_load_input_ui:", reactive_values$ready_to_load_input_ui)),
    #   renderPrint(c("reactive_values$ready_to_load_input_values:", reactive_values$ready_to_load_input_values)),
    #   renderPrint(c("reactive_values$all_input_values_loaded:", reactive_values$all_input_values_loaded)),
    #   renderPrint(c(paste0("input$filter_var_", 1, ":"), input[[paste0("filter_var_", 1)]])),
    #   renderPrint(c(paste0("input$filter_var_", 2, ":"), input[[paste0("filter_var_", 2)]])),
    #   renderPrint(c(paste0("input$filter_var_", 3, ":"), input[[paste0("filter_var_", 3)]]))
    # )
    # lapply(seq_along(test_values), function(i) {
    #   output[[paste0("testing", i)]] <- test_values[[i]]
    # })
  }
  # create the app to run
  shiny_app <- list(ui = ui, server = server)
  if(.Platform$OS.type == "unix") {
    if(missing(unix_port_number)) {
      stop("Please enter the port number for a unix OS.")
    }
    runApp(shiny_app, port = unix_port_number,
           launch.browser = F, host = "0.0.0.0")
  } else {
    runApp(shiny_app, port = pc_port_number,
           launch.browser = T, host = "0.0.0.0")
  }
}
# shiny_analysis_tool_v2(d001, unix_port_number = 3801)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
