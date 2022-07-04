#' A function that converts an SBGN-ML text file into a network object
#'
#' @param file A text file with extension .xml containing the markdown output
#'             of

convertSBGNdiagram <- function(file) {
  text <- read_xml(file)
  lang <- attr(as_list(xml_children(text))[[1]],"language")
  nodes <- xml_children(xml_children(text))

}

#buildEquation

#plantSimulator
