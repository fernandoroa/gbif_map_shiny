box::use(
  shiny[...],
  shinyjs[useShinyjs]
)

box::use(
  ./modules/left_panel,
  ./modules/right_panel
)

ui <- function(req) {
  fluidPage(
    useShinyjs(),
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
          right_panel$ui("second")
        )
      )
    )
  )
}

server <- function(input, output, session) {
  first_vars <- left_panel$server("first")
  right_panel$server("second", first_vars)
}

shinyApp(ui, server)
