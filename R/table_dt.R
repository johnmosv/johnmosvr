
table_dt <- function(data, col_names = NULL, first_colname = NULL,
                     title_row_names = TRUE, title_col_names = TRUE,
                     row_groups = FALSE, alignment = "dt-center", align_targets = NULL,
                     page_length = 20, class = NULL, row_callback = NULL, width = NULL, ...) {
  data <- dplyr::ungroup(data)
  dom_settings <- "tB"

  if (is.null(col_names)) {
    col_names <- colnames(data)
  }

  if (title_col_names) {
    col_names <- stringr::str_to_title(stringr::str_replace_all(col_names, "_", " "))
  }


  if (row_groups) {
    if (is.factor(data[[1]])) data[[1]] <- as.character(data[[1]])
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
  return_table <- DT::formatStyle(return_table, columns = 1, color = "black", fontWeight = "bold")

  if ("percent" %in% colnames(data)) {
    return_table <- DT::formatPercentage(return_table, "percent")
  }
  if ("valid_percent" %in% colnames(data)) {
    return_table <- DT::formatPercentage(return_table, "valid_percent")
  }

  return(return_table)
}
