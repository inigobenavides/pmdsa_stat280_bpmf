# visualisations.R
# Compilation of functions that generates visuals

vis_matrix <- function(data, row_col, column_col, 
                       value_col, label = TRUE) {
  #' Produces geom tile for matrix
  #' @param data: dataframe or matrix in tidy format [row, column, value]
  #' @param row_col: string of column name depicting row index
  #' @param column_col: string of column name depicting column index
  #' @param value_col: string of column name depicting value
  #' @param label: if true, puts label in place
  
  data %>%
    as.data.frame() %>% 
    ggplot() +
    geom_tile(
      mapping = aes_string(y = row_col, 
                           x = column_col, 
                           fill = value_col)
    ) +
    theme_minimal() +
    scale_y_reverse(breaks = 1:max(data[[row_col]])) +
    scale_x_continuous(breaks = 1:max(data[[column_col]]), position = "top") +
    scale_fill_gradient2(
      low = "#70ff75",
      high = "#91c5ff",
      midpoint = 0,
      mid = "#ffffff"
    ) -> plot
  
  if (label) {
    plot +
      geom_text(
        mapping = aes_string(y = row_col, 
                             x = column_col, 
                             label = glue("round({value_col},2)")
                  )
      ) -> plot
  }
  
  return(plot)
}
