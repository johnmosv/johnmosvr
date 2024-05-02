#' table_dt: Create nice looking datatable with sane defaults
#'
#' @param data Table to present
#' @param col_names New column names to be used in table
#' @param first_colname A character vector with new colnames
#' @param title_row_names Should first row be formatted?
#' @param title_col_names Should colnames be formatted?
#' @param row_groups Should first columns be grouped?
#' @param alignment Alignment of cells c("dt-center", "dt-right", "dt-left")
#' @param align_targets Numeric vector containing columns positions to
# ` apply alignment to
#' @param page_length Number of rows per page
#' @param class Classes to apply. Defaults to "compact stripe"
#' @param row_callback Row callback to apply.
# ` Default to changing the color of every other row.
#' @param width Width as proportion of parent div. Based on ncol bby default
#' @param ... Additional variables to pass to datatable::DT
#'
#' @returns An html table
#'
#' @export
table_dt <- function(data, col_names = NULL, first_colname = NULL,
                     title_row_names = TRUE, title_col_names = TRUE,
                     row_groups = FALSE, alignment = "dt-center", align_targets = NULL, # nolint
                     page_length = 20, class = NULL, row_callback = NULL, width = NULL, ...) { # nolint

  if (!knitr::is_html_output()) {
    return(print(tibble::as_tibble(data), n = Inf))
  }
  data <- dplyr::ungroup(data)
  dom_settings <- "tB"

  if (is.null(col_names)) {
    col_names <- colnames(data)
  }

  if (title_col_names) {
    col_names <- stringr::str_to_title(stringr::str_replace_all(col_names, "_", " ")) # nolint
  }

  if (row_groups) {
    if (is.factor(data[[1]])) data[[1]] <- as.character(data[[1]])
    data[[1]] <- paste(data[[1]])
    data[duplicated(data[, 1]), 1] <- " "
  }

  if (!is.null(first_colname)) col_names[1] <- first_colname

  if (is.null(align_targets)) {
    align_targets <- 1:(ncol(data) - 1)
  }

  if (is.null(class)) {
    class <- "compact stripe"
  }

  if (page_length < nrow(data)) {
    dom_settings <- "tBflp"
  }

  if (is.null(width)) {
    n_col <- ncol(data)
    width <- dplyr::case_when(
      n_col >= 10 ~ 100,
      n_col >= 6 ~ 90,
      n_col >= 4 ~ 80,
      n_col >= 3 ~ 70,
      n_col <= 2 ~ 60,
      TRUE ~ 100
    )
    width <- paste0(width, "%")
  }

  if (title_row_names) data <- dplyr::mutate_at(data, 1, ~ stringr::str_to_title(stringr::str_replace_all(., "_", " ")))

  if (is.null(row_callback)) {
    row_callback <- htmlwidgets::JS(
      "function(row, data, num, index){",
      "  var $row = $(row);",
      "  if($row.hasClass('even')){",
      "    $row.css('background-color', 'white');",
      "    $row.hover(function(){",
      "      $(this).css('background-color', '#F6F8FA');",
      "     }, function(){",
      "      $(this).css('background-color', 'white');",
      "     }",
      "    );",
      "  }else{",
      "    $row.css('background-color', '#F6F8FA');",
      "    $row.hover(function(){",
      "      $(this).css('background-color', 'gray');",
      "     }, function(){",
      "      $(this).css('background-color', '#F6F8FA');",
      "     }",
      "    );",
      "  }",
      "}"
    )
  }

  return_table <- DT::datatable(
    data,
    rownames = FALSE,
    colnames = col_names,
    class = class,
    extensions = "Buttons",
    escape = FALSE,
    editable = TRUE,
    fillContainer = FALSE,
    width = width,
    options = list(
      dom = dom_settings,
      pageLength = page_length,
      columnDefs = list(list(className = alignment, targets = align_targets)),
      scrollX = TRUE,
      scrollCollapse = TRUE,
      buttons = list(list(
        extend = "collection",
        buttons = c("csv", "excel", "pdf"),
        text = "Download"
      )),
      rowCallback = row_callback,
      headerCallback <- htmlwidgets::JS(
        "function(thead, data, start, end, display){",
        "   $('thead').css({'background-color': 'white'});",
        "   $('thead').css({'font-weight': 'bold'});",
        # "   $('thead').css({'padding': '10px'});", # not working. need to target thead.th.tr
        "}"
      ),
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        #            "$('table.dataTable.no-footer').css('border-bottom', 'none');",
        "$('table.dataTable.no-footer').css('border-top', '2px solid black');",
        "$('table.dataTable.no-footer').css('border-bottom', '2px solid black');",
        "}"
      )
    ),
    ...
  )
  return_table <- DT::formatStyle(return_table, columns = 1, color = "black", fontWeight = "bold") # nolint

  if ("percent" %in% colnames(data)) {
    return_table <- DT::formatPercentage(return_table, "percent")
  }
  if ("valid_percent" %in% colnames(data)) {
    return_table <- DT::formatPercentage(return_table, "valid_percent")
  }

  return(return_table)
}
#' colorize_span
#'
#' @param text Text
#' @param color Color of the output. Hex or named.
#'
#' @returns Returns an span tag with a color or Latex colored text
#'
#' @export
colorize_span <- function(text, color = "red") {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, text)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, text)
  } else {
    text
  }
}
#' tabyl_dt: Call table_dt after running janitor::tabyl() of data
#'
#' @param data data.frame to be tabyled
#' @param var1 First var to tabulate over
#' @param var2 Second, optional, var to tabulate over
#' @param ... Additional arguments passed to table_dt
#'
#' @returns Tabyl output or table_dt output
#'
#' @export
tabyl_dt <- function(data, var1, var2 = NULL, ...) {
  res <- janitor::tabyl(data, {{ var1 }})
  if (!is.null(rlang::expr({{ var2 }}))) {
    res <- janitor::tabyl(data, {{ var1 }}, {{ var2 }})
  }
  if (knitr::is_html_output()) {
    return_table <- table_dt(res, , title_row_names = FALSE, title_col_names = FALSE, ...) # nolint

    percent_cols <- c("percent", "valid_percent")
    percent_cols_present <- percent_cols %in% colnames(res)
    if (any(percent_cols_present)) {
      percent_cols <- percent_cols[percent_cols_present]
      return_table <- return_table %>% DT::formatPercentage(columns = "percent") # nolint
    }
    return(return_table)
  }
  return(DT::datatable(res))
}
