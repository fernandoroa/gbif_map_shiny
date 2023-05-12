box::use(
  shiny[...],
  leaflet[leafletOutput, renderLeaflet],
  shinyjs[onclick, runjs],
  glue[glue],
  dplyr[filter, pull]
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

    output$multimedia_id <- renderUI({
      selected_taxon <- left_vars()$taxonID()
      validate(
        need(
          try(length(selected_taxon) > 0),
          FALSE
        )
      )

      image_files <- multimedia_filtered |> filter(taxonID %in% selected_taxon) |> pull(identifier)
      img_found <- ""
      if (length(image_files) > 0) {
        image_file <- image_files |> sample(1)
      } else {
        image_file <- "no-image-icon-23494.png"
        img_found <- "No "
      }
      tagList(
        div(`data-title` = image_file, id = "img_container",
          tags$img(
            src = image_file, alt = glue("{img_found}image for taxon {selected_taxon}")
          )
        ),
        tags$script(type = "text/javascript", "tooltip_toggle()")
      )
    })

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

    observeEvent(input$selected_show,{
      if (input$selected_show == "map") {
        removeUI(
          selector = glue("#{ns(\"multimedia_id\")}")
        )
        insertUI(
          selector = ".map_container",
          where = "afterBegin",
          ui = leafletOutput(ns("map1b"))
        )
      } else {
        removeUI(
          selector = glue("#{ns(\"map1\")}")
        )
        removeUI(
          selector = glue("#{ns(\"map1b\")}")
        )
        insertUI(
          selector = ".map_container",
          where = "afterBegin",
          ui = uiOutput(ns("multimedia_id"))
        )
      }
    })


  })
}
