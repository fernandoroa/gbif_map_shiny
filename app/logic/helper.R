#' @export
capitalize_first <- function(string) {
  sub("([[:alpha:]]){1}(.*)", "\\U\\1\\L\\2", string, perl = TRUE)
}
