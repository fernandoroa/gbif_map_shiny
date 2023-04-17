box::use(
  shiny[...],
  ./modules/left_panel,
  ./modules/second_module
)

ui <- function(req) {
  fluidPage(
    class = "container",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css")
    ),
    wellPanel(
      class = "main background", id = "central_panel",
      splitLayout(
        left_panel$ui("first"),
        second_module$ui("second")
      )
    )
  )
}

server <- function(input, output, session) {
  first_vars <- left_panel$server("first")
  second_module$server("second", first_vars)
}

shinyApp(ui, server)
