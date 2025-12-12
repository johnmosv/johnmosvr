#' table_dt: Create nice looking datatable with sane defaults
#'
#' @param data Table to present
#' @param caption Table caption
#' @param col_names New column names to be used in table
#' @param first_colname A character vector with new colnames
#' @param title_row_names Should first row be formatted?
#' @param title_col_names Should colnames be formatted?
#' @param row_groups Should first columns be grouped?
#' @param alignment Alignment of cells c("dt-center", "dt-right", "dt-left")
#' @param align_targets Numeric vector containing columns positions to
# ` apply alignment to
#' @param digits Number of digits to round numeric values to, default: NUL
#' @param page_length Number of rows per page
#' @param class Classes to apply. Defaults to "compact stripe"
#' @param row_callback Row callback to apply.
#' @param use_default_row_callback, FALSE
# ` Default to changing the color of every other row.
#' @param width Width as proportion of parent div. Based on ncol bby default
#' @param force_html, force html output, default = FALSE
#' @param ... Additional variables to pass to datatable::DT
#'
#' @returns An html table
#'
#' @export
table_dt <- function(data, caption = NULL, col_names = NULL, first_colname = NULL,
                     title_row_names = FALSE, title_col_names = FALSE,
                     row_groups = FALSE, alignment = "dt-center", align_targets = NULL, # nolint
                     digits = NULL,
                     page_length = 20, class = NULL, row_callback = NULL,
                     use_default_row_callback = FALSE,
                     width = NULL, force_html = FALSE, ...) { # nolint


  if ("TableOne" %in% class(data)) {
    data <- convert_tableone(data)
  }
  if (
    !knitr::is_html_output() && !knitr::is_latex_output() &&
      !force_html
  ) {
    return(print(tibble::as_tibble(data), n = 1000))
  }
  if (knitr::is_latex_output()) {
    return(knitr::kable(data, caption = caption, format = "latex"))
  }
  data <- dplyr::ungroup(data)
  dom_settings <- "ftB"

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
    dom_settings <- "ftlpB"
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

  use_row_callback <- ifelse(!is.null(row_callback) | use_default_row_callback, TRUE, FALSE)

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

  if (!use_row_callback) {
    row_callback <- NULL
  }

  # Format caption to appear at top and left-aligned
  if (!is.null(caption)) {
    caption <- htmltools::tags$caption(style = "caption-side: top; text-align: left;", caption)
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
    caption = caption,
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
      headerCallback = htmlwidgets::JS(
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
        "$('.dt-buttons').css({'font-size': '0.8em', 'padding': '5px'});",
        "$('.dt-button').css({'font-size': '0.8em', 'padding': '3px 8px'});",
        "}"
      )
    ),
    ...
  )
  return_table <- DT::formatStyle(return_table, columns = 1, color = "black", fontWeight = "bold") # nolint

  if (!is.null(digits)) {
    # Only round numeric columns that contain non-integer values
    num_cols <- sapply(data, function(x) {
      if (!is.numeric(x)) return(FALSE)
      # Check if any values are not whole numbers (ignoring NAs)
      any(!is.na(x) & (x != floor(x)))
    })
    for (col in which(num_cols)) {
      return_table <- DT::formatRound(return_table, col, digits = digits)
    }
  }

  if ("percent" %in% colnames(data)) {
    return_table <- DT::formatPercentage(return_table, "percent")
  }
  if ("valid_percent" %in% colnames(data)) {
    return_table <- DT::formatPercentage(return_table, "valid_percent")
  }

  return(return_table)
}
