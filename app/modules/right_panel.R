box::use(
  shiny[...],
  leaflet[leafletOutput, renderLeaflet]
)

box::use(
  ./left_panel[...],
  ../logic/helper[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      title = "Second Module", collapsible = FALSE, class = "background",
      solidHeader = TRUE, status = "success",
      height = "400px",
      leafletOutput(ns("map1"))
    )
  )
}

#' @export
server <- function(id, left_vars) {
  moduleServer(id, function(input, output, session) {

    output$map1 <- renderLeaflet({
      selected_taxon <- left_vars()$taxonID()
      validate(
        need(
          try(length(selected_taxon) > 0),
          FALSE
        )
      )
      make_map_from_selection(selected_taxon)
    })
  })
}
