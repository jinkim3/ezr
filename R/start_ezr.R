#' Start ezr
#'
#' Starts ezr on the local machine
#'
#' @param data_for_ezr a data object (a data frame or a data.table)
#' @param sigfig number of significant digits to round to
#' @param select_list_max maximum number of variable names to display
#' for dropdown menus
#' @param ezr_saved_analysis_file_name name of the .csv file on which
#' saved analysis will be recorded (default = "ezr_saved_analysis.csv")
#' @param ezr_run_analysis_file_name name of the .csv file on which
#' all conducted analyses will be recorded
#' (default = "ezr_run_analysis.csv")
#' @return There will be no output from this function. Rather, the ezr
#' program will open on a new tab or window of the local machine's
#' web browser
#' @examples
#' if (interactive()) {start_ezr(data = mtcars)}
#' @export
#' @import data.table ggplot2 shiny
start_ezr <- function(
  data_for_ezr = NULL,
  sigfig = 3,
  select_list_max = 100000,
  ezr_saved_analysis_file_name = "ezr_saved_analysis.csv",
  ezr_run_analysis_file_name = "ezr_run_analysis.csv"
) {
  # create sidebar menu analysis type
  analysis_type_label <-
    c("Saved Analysis",
      "Descriptive Stats",
      "Frequency Table",
      "Histogram (base R function)",
      "Scatter plot",
      # "Raincloud plots + Table",
      "Histogram by Group")
      # "Multiple Regression",
      # "IV / DV Table",
      # "Pivot Table",
      # "View Data")
  analysis_type <-
    c("saved_analysis",
      "desc_stats",
      "freq_table",
      "histogram",
      "scatterplot",
      # "raincloud",
      "histogram_by_group")
      # "regression",
      # "iv_dv_table",
      # "pivot_table",
      # "view_data")
  analysis_icon <-
    c("bookmark", # saved analysis
      "list", # desc stats
      "sort-amount-down", # freq table
      "chart-bar", # histogram
      "braille", # scatterplot
      # "cloud", # raincloud
      "sliders-h" # histogram by group
      # "registered", # regression
      # "table", # iv dv table
      # "th", # pivot table
      # "file-alt" # view data
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

  # create the saved analysis csv if it doesn't exist
  if (!file.exists(ezr_saved_analysis_file_name)) {
    # initialize the csv for saving analysis
    ezr_saved_analysis <-
      data.table(
        id = numeric(),
        time = character(),
        input_type = character(),
        input_value = character())
    fwrite(
      x = ezr_saved_analysis,
      file = ezr_saved_analysis_file_name)
    message(paste0("The following file was created: ",
                  ezr_saved_analysis_file_name))
  }

  # create the run analysis csv if it doesn't exist
  if(!file.exists(ezr_run_analysis_file_name)) {
    # initialize the csv for saving analysis
    shiny_run_analysis <-
      data.table(
        id = numeric(),
        time = character(),
        ip = character(),
        input_type = character(),
        input_value = character())
    fwrite(
      x = shiny_run_analysis,
      file = ezr_run_analysis_file_name)
    message(paste0("The following file was created: ",
                  ezr_run_analysis_file_name))
  }

  # import saved analysis csv for the first time
  ezr_saved_analysis <-
    fread(ezr_saved_analysis_file_name)

  # import run analysis csv for the first time
  shiny_run_analysis <-
    fread(ezr_run_analysis_file_name)

  # open saved analysis csv
  ezr_saved_analysis <-
    fread(ezr_saved_analysis_file_name)

  # open run analysis csv
  shiny_run_analysis <-
    fread(ezr_run_analysis_file_name)

  # var names
  var_names <- c("", names(data_for_ezr))

  # functions to be put inside shiny
  one_var_input <- function() {
    selectizeInput(
      inputId = "var",
      label = "Select a variable:",
      choices = var_names,
      selected = NULL,
      multiple = F,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  iv_input <- function(multiple = T) {
    selectizeInput(
      inputId = "iv",
      label = "Select IV(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  dv_input <- function(multiple = T) {
    selectizeInput(
      inputId = "dv",
      label = "Select DV(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  iv_order_input <- function(iv_order, backup_iv_order) {
    selectizeInput(
      inputId = "iv_order",
      label = "Order for values in IV (Top to Bottom):",
      choices = "",
      selected = NULL,
      multiple = T,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  include_totals_input <- function() {
    selectizeInput(
      inputId = "include_totals",
      label = "Include or exclude Totals?",
      choices = c("Include Totals", "Exclude Totals"),
      selected = NULL,
      multiple = F,
      options = list(maxOptions = select_list_max),
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
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  row_var_input <- function(multiple = T) {
    selectizeInput(
      inputId = "row_vars",
      label = "Select row variable(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  col_var_input <- function(multiple = T) {
    selectizeInput(
      inputId = "col_vars",
      label = "Select column variable(s):",
      choices = var_names,
      selected = NULL,
      multiple = multiple,
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  cell_var_input <- function() {
    selectizeInput(
      inputId = "cell_var",
      label = "Select a cell variable:",
      choices = var_names,
      selected = NULL,
      multiple = F,
      options = list(maxOptions = select_list_max),
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
          dt01[get("id") == saved_analysis_ids[i]]
        saved_analysis_temp_1 <-
          specific_saved_analysis_dt[
            get("input_type") == "sidebar_menu"][["input_value"]]
        saved_analysis_temp_2_var_type <-
          unique(specific_saved_analysis_dt[
            get("input_type") != "sidebar_menu"][["input_type"]])
        saved_analysis_temp_3_string <-
          paste0(
            sapply(saved_analysis_temp_2_var_type, function(x) {
              paste0(
                x, ": ", paste0(
                  specific_saved_analysis_dt[
                    get("input_type") == x][["input_value"]],
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
      options = list(maxOptions = select_list_max),
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
      options = list(maxOptions = select_list_max),
      width = "100%")
  }
  sigfig_input <- function() {
    numericInput(
      inputId = "sigfig",
      label = "Number of significant figures to display:",
      value = sigfig,
      min = 0, max = 20, step = 1)
  }
  filter_vars_input_1 <- function() {
    selectizeInput(
      inputId = "names_of_filter_vars",
      label = "Choose filtering variables:",
      choices = var_names,
      multiple = T,
      options = list(maxOptions = select_list_max),
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
  header <- shinydashboard::dashboardHeader(title = "ezr v0.1.1")
  sidebar <- shinydashboard::dashboardSidebar(uiOutput("sidebar_01"))
  body <- shinydashboard::dashboardBody(
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
  ui <- shinydashboard::dashboardPage(
    header, sidebar, body, skin = "blue")
  # server
  server <- function(input, output, session) {
    # sidebar ui
    output$sidebar_01 <- renderUI({
      shinydashboard::sidebarMenu(
          id = "sidebar_menu",
          lapply(1:nrow(sidebar_menu_dt),
                 function(i) {
                   shinydashboard::menuItem(sidebar_menu_dt$analysis_type_label[i],
                            tabName = sidebar_menu_dt$analysis_type[i],
                            icon = icon(sidebar_menu_dt$analysis_icon[i]))}))
      })
    # reactive values
    reactive_values <- reactiveValues()
    reactive_dt <- reactiveValues()
    # default reactive dt
    reactive_dt$saved_analysis <- fread(
      ezr_saved_analysis_file_name)
    reactive_dt$run_analysis <- fread(
      ezr_run_analysis_file_name)
    reactive_dt$loaded_inputs <- NULL
    # static input uis
    lapply(seq_along(static_input_uis), function(i) {
      output[[paste0("static_input_", i)]] <- static_input_uis[[i]]
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
            sort(unique(data_for_ezr[[input$iv]]))
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
                data_for_ezr[[filter_vars[i]]]), na.last = F)
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
          selected = dt01[get("input_type") == x][["input_value"]])
      })
      filter_var_ids_to_load <- intersect(
        unique(dt01[["input_type"]]), filter_var_ids)
      lapply(filter_var_ids_to_load, function(x) {
        updateCheckboxGroupInput(
          session, inputId = x,
          selected = dt01[get("input_type") == x][["input_value"]])
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
      dt01 <- reactive_dt$saved_analysis[get("id") == id_of_analysis_to_load]
      # switch to the tab
      sidebar_menu_item_to_switch_to <-
        dt01[get("input_type") == "sidebar_menu"][["input_value"]]
      shinydashboard::updateTabItems(
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
              selected = dt01[get("input_type") == x][["input_value"]])
          })
          filter_var_ids_to_load <- intersect(
            unique(dt01[["input_type"]]), filter_var_ids)
          lapply(filter_var_ids_to_load, function(x) {
            updateCheckboxGroupInput(
              session, inputId = x,
              selected = dt01[get("input_type") == x][["input_value"]])
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
        ezr_saved_analysis[!get("id") %in% id_of_saved_analysis]
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
        ezr_saved_analysis[
          , get("id") := match(get("id"), unique(get("id")))]
      }
      # save to csv
      fwrite(
        x = ezr_saved_analysis,
        file = ezr_saved_analysis_file_name)
      # update the reactive dt
      reactive_dt$saved_analysis <- ezr_saved_analysis
    })
    # run analysis
    observeEvent(input$run_btn, {
      # get active tab
      active_tab <- input$sidebar_menu
      # get data
      dt01 <- data_for_ezr
      # remove outliers
      if(!is.null(input$vars_for_outliers)) {
        for(i in seq_along(input$vars_for_outliers)) {
          dt01 <- dt01[
            dt01[[input$vars_for_outliers[i]]] %in%
              dt01[[input$vars_for_outliers[i]]][
                !dt01[[input$vars_for_outliers[i]]] %in%
                  grDevices::boxplot.stats(
                    dt01[[input$vars_for_outliers[i]]])$out], ]
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
          data.table(
            statistic = names(desc_stats(dt01[[input$var]])),
            value = signif(
              desc_stats(dt01[[input$var]]), input$sigfig))
          })}
      # frequency table
      if(active_tab == "freq_table") {
        output$table_1 <- DT::renderDataTable({
          tabulate_vector(
            dt01[[input$var]], sigfigs = input$sigfig)})}
      # histogram
      if(active_tab == "histogram") {
        output$plot_1 <- renderPlot({
          graphics::hist(dt01[[input$var]],
               main = paste0("Histogram of ", input$var),
               xlab = input$var,
               breaks = "FD")
        }, width = 800, height = plot_1_height)}
      # scatterplot
      if(active_tab == "scatterplot") {
        output$plot_1 <- renderPlot({
          withProgress(
            message = "Generating the plot...",
            scatterplot(
              data = dt01,
              x_var_name = input$iv,
              y_var_name = input$dv,
              annotate_stats = TRUE))
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
            # if(active_tab == "raincloud") {
            #   raincloud_by_group(
            #     data = dt01,
            #     name_of_dv = input$dv,
            #     name_of_iv = input$iv,
            #     order_of_groups_top_to_bot = input$iv_order)
            # } else
              if(active_tab == "histogram_by_group") {
              histogram_by_group(
                data = dt01,
                iv_name = input$iv,
                dv_name = input$dv,
                order_of_groups_top_to_bot = input$iv_order)
            })}, width = 800, height = plot_1_height)
          # output$table_2 <- DT::renderDataTable({
          #   table_with_iv_and_dv(
          #     data = dt01,
          #     dv_name = input$dv,
          #     iv_name = input$iv,
          #     order_of_groups_top_to_bot = input$iv_order,
          #     sigfig = input$sigfig)})
          # output$table_3 <- DT::renderDataTable({
          #   post_hoc_test(data = dt01,
          #                 iv_name = input$iv,
          #                 dv_name = input$dv,
          #                 sigfig_for_means_and_d = input$sigfig)})
          # output$message_to_user_01 <- renderText({
          #   t_or_f_test(data = dt01, iv_name = input$iv,
          #               dv_name = input$dv)})
        }
      }
      # # regression
      # if(active_tab == "regression") {
      #   output$table_1 <- DT::renderDataTable({
      #     regression_table_for_shiny(
      #       data = dt01,
      #       dv_name = input$dv,
      #       iv_names = input$iv,
      #       sigfig = input$sigfig)})}
      # # iv dv table
      # if(active_tab == "iv_dv_table") {
      #   # limit by number of unique values
      #   number_of_rows <-
      #     prod(sapply(input$iv, function(x) {
      #       length(unique(dt01[[x]]))}))
      #   if(number_of_rows > 100) {
      #     output$message_to_user_01 <- renderText({
      #       paste0("There will be more than 100 rows in the table. ",
      #              "The server cannot handle this operation yet.")})
      #     return()}
      #   include_totals_t_or_f <-
      #     if(input$include_totals == "Include Totals") T else F
      #   output$table_1 <- DT::renderDataTable({
      #     iv_dv_table(
      #       data = dt01,
      #       row_var_names = input$iv,
      #       col_var_names = input$dv,
      #       include_totals = include_totals_t_or_f,
      #       cell_function_name = input$function_name,
      #       sigfig = input$sigfig)})}
      # # pivot_table
      # if(active_tab == "pivot_table") {
      #   # limit by number of unique values
      #   number_of_rows <- prod(sapply(input$row_vars, function(x) {
      #     length(unique(dt01[[x]])) + 1}))
      #   number_of_columns <- prod(sapply(input$col_vars, function(x) {
      #     length(unique(dt01[[x]])) + 1}))
      #   if(number_of_rows > 100) {
      #     output$message_to_user_01 <- renderText({
      #       paste0("There will be more than 100 rows in the table. ",
      #              "The server cannot handle this operation yet.")})
      #   } else if(number_of_columns > 50) {
      #     output$message_to_user_01 <- renderText({
      #       paste0("There will be more than 50 columns in the table. ",
      #              "The server cannot handle this operation yet.")})
      #   }
      #   output$table_1 <- DT::renderDataTable({
      #     pivot_table_for_shiny(
      #       data = dt01,
      #       row_var_names = input$row_vars,
      #       col_var_names = input$col_vars,
      #       cell_var_name = input$cell_var,
      #       cell_function_name = input$function_name,
      #       sigfig = input$sigfig
      #     )
      #   })}
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
      # id, time, etc
      id <- c(dt11$id,
              rep(max(c(dt11$id, 0)) + 1,
                  length(new_input_value)))
      time <- c(dt11$time,
                rep(format(Sys.time(), "%b %d %Y %H%M%S"),
                    length(new_input_value)))
      new_input_type_all <-
        c(dt11$input_type, new_input_type)
      new_input_value_all <-
        c(dt11$input_value, new_input_value)
      # put it together and export to csv
      run_analysis_dt <- data.table(
        id, time,
        input_type = new_input_type_all,
        input_value = new_input_value_all)
      # save to csv
      fwrite(
        x = run_analysis_dt,
        file = ezr_run_analysis_file_name)
      # save again
      reactive_dt$run_analysis <- run_analysis_dt
    })
    # save analysis
    observeEvent(input$save_btn, {
      # ezr_saved_analysis <-
      #   import_data(ezr_saved_analysis_file_name,
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
      new_input_type_all <-
        c(dt01$input_type, new_input_type)
      new_input_value_all <-
        c(dt01$input_value, new_input_value)
      # put it together and export to csv
      saved_analysis_dt <- data.table(
        id, time,
        input_type = new_input_type_all,
        input_value = new_input_value_all)
      # save to csv
      fwrite(
        x = saved_analysis_dt,
        file = ezr_saved_analysis_file_name)
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
  runApp(shiny_app, launch.browser = TRUE)
}
