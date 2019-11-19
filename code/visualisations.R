# visualisations.R
# Compilation of functions that generates visuals

vis_matrix <- function(matrix, row_col, column_col, 
                       value_col = NULL, label = TRUE) {
  #' Produces geom tile for matrix
  #' @param matrix: matrix in tidy format [row, column, value]
  #' @param row_col: string of column name depicting row index
  #' @param column_col: string of column name depicting column index
  #' @param value_col: string of column name depicting value
  #' @param label: if true, puts label in place
  
  matrix %>% 
    as.data.frame() %>% 
    ggplot() +
    geom_tile(
      mapping = aes_string(y = row_col, 
                           x = column_col, 
                           fill = value_col)
    ) +
    theme_minimal() +
    scale_y_reverse() +
    scale_x_continuous(position = "top") -> plot
  
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
