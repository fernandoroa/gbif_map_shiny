box::use(
  shiny[...],
)

box::use(
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
      div(class = "wrapper",
        div(class = "left_panel",
          left_panel$ui("first"),
        ),
        div(class = "right_panel",
          second_module$ui("second")
        )
      )
    )
  )
}

server <- function(input, output, session) {
  first_vars <- left_panel$server("first")
  second_module$server("second", first_vars)
}

shinyApp(ui, server)
