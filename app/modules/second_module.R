box::use(
  shiny[...],
  ./left_panel[...]
)
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      title = "Second Module", collapsible = FALSE, class = "background",
      solidHeader = TRUE, status = "success",
      height = "400px",
      fluidRow(
        column(6, numericInput(ns("input_2"), "module2input", 9, 1, max = 10)),
        column(6, textOutput(ns("output_2")))
      )
    )
  )
}

#' @export
server <- function(id, first_vars) {
  moduleServer(id, function(input, output, session) {
    output$output_2 <- renderText({
      "placeholder"
    })
  })
}
