library(readr)
control_df <- read_csv("controls.csv",  col_types =cols(
  control_id = col_character(),
  desc = col_character(),
  min = col_integer(),
  max = col_double(),
  value = col_double()
))

get_slider_input_from_row <- function(row) {

  
  sliderInput(row$control_id, row$desc,
              min = row$min, max = row$max, value = row$value)
              
  
}

get_input_sliders <- function() {
  
  purrrlyr::by_row(control_df, get_slider_input_from_row)$.out
  
}




