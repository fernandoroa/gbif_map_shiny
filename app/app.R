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
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css"),
      tags$link(rel = "stylesheet", type = "text/css",
        href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.3/font/bootstrap-icons.css")
    ),
    div(class = "main",
      div(class = "wrapper",
        tags$section(class = "left_panel",
          left_panel$ui("first"),
        ),
        tags$section(class = "right_panel",
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
