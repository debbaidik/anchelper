#' Hello World
#'
#' A simple greeting function to verify the package is working.
#'
#' @param name Character string. Name to greet. Defaults to "world".
#' @return A greeting string.
#' @export
#' @examples
#' hello()
#' hello("R user")
hello <- function(name = "world") {
  paste0("Hello, ", name, "! anchelper is ready.")
}
