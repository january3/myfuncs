#' Show a zenity dialog "Calculation complete"
#'
#' Show a zenity dialog "Calculation complete"
#'
#' Show a zenity dialog "Calculation complete"
#'@param text Text to show in the dialog
#'@export

ding <- function( text= "Calculation complete" ) {
  system( paste0( 'zenity --info --text="', text, '"' ) )
}
