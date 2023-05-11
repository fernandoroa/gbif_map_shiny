box::use(
  shiny[...],
  leaflet[leafletOutput, renderLeaflet],
  shinyjs[onclick, runjs],
  glue[glue]
)

box::use(
  ./left_panel[...],
  ../logic/helper[...],
  ../objects/objects[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
      div(class = "right_panel_wrapper",
        tags$section(class = "vertical_left_bar",
          id = "vertical_left_bar_id",
          div(
            class = "toggle_arrow",
            icon("caret-right", "fa-3x")
          ),
          div(
            HTML("<i class=\"bi-globe-americas big_icon\"></i>")
          )
        ),
        tags$section(class = "map_container",
          tagList(
            leafletOutput(ns("map1"))
          )
        ),
        tags$section(class = "vertical_right_bar",
          id = "vertical_right_bar_id",
          div(
            class = "toggle_arrow",
            icon("caret-left", "fa-3x")
          ),
          div(
            HTML("<i class=\"bi-file-easel big_icon\"></i>")
          )
        )
      )
}

#' @export
server <- function(id, left_vars) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    onclick("vertical_right_bar_id",
      runjs(
        paste0(
        "$('.right_panel_wrapper').css('grid-template-columns', 'minmax(auto, auto) minmax(auto, calc(100% - 35px)) 0');",
        "$('#vertical_right_bar_id').css('display', 'none');",
        "$('#vertical_left_bar_id').css('display', 'grid');",
        "$(function() { Shiny.setInputValue(\"", glue(ns("selected_show")), "\", \"image\"); });",
        "$(window).resize();"
        )
      ),
      asis = TRUE
    )

    onclick("vertical_left_bar_id",
      runjs(paste0(
        "$('.right_panel_wrapper').css('grid-template-columns', '0 minmax(auto, calc(100% - 35px)) minmax(auto, auto)');",
        "$('#vertical_right_bar_id').css('display', 'grid');",
        "$('#vertical_left_bar_id').css('display', 'none');",
        "$(function() { Shiny.setInputValue(\"", glue(ns("selected_show")), "\", \"map\"); });",
        "$(window).resize();"
      )),
      asis = TRUE
    )

    output$map1b <- output$map1 <- renderLeaflet({
      selected_taxon <- left_vars()$taxonID()
      validate(
        need(
          try(length(selected_taxon) > 0),
          FALSE
        )
      )
      make_map_from_selection(selected_taxon)
    })

    observeEvent(input$selected_show,{
      if (input$selected_show == "map") {
        removeUI(
          selector = "#second-map2"
        )
        insertUI(
          selector = ".map_container",
          where = "afterBegin",
          ui = leafletOutput(ns("map1b"))
        )
      } else {
        removeUI(
          selector = "#second-map1"
        )
        removeUI(
          selector = "#second-map1b"
        )
        insertUI(
          selector = ".map_container",
          where = "afterBegin",
          ui = plotOutput(ns("map2"))
        )
      }
    })

    output$map2 <- renderPlot({
      plot(1:10)
    })
  })
}
