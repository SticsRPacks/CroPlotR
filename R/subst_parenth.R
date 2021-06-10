#' @title  Substitute "(xxx)" by *_xxx"
#' @description Substitute "(xxx)" by *_xxx" in case the user give names with parenthesis
#' while they are transformed by R in _
#' @param var_names Vector of strings to transform
#'
#' @return Vector of transformed strings
#'
#'
#' @examples
#' var_names <- c("var1","var2(n)")
#' CroPlotR:::subst_parenth(var_names)
#'
#' @keywords internal
#'
subst_parenth <- function(var_names) {
  .= NULL
    gsub("\\(","_",var_names) %>%
    gsub("\\)","",.)
}
