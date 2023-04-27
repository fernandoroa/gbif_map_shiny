box::use(
  shiny[
    NS,
    selectizeInput,
    updateSelectizeInput,
    moduleServer,
    observe,
    reactive,
    req,
    reactiveVal,
    div
  ],
  dplyr[pull, filter],
  glue[glue],
  shinyjs[hide, show, hidden, toggle]
)

box::use(
  ./objects[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  hidden(
    div(id = ns("synonym_container_id"),
      selectizeInput(ns("synonym"), "Select Synonym", choices = NULL,
        selected = NULL
      )
    )
  )
}

#' @export
server <- function(id, input_manual_rank_id, list_inputs) {
  moduleServer(id, function(input, output, session) {

    rv_rank_selected <- reactiveVal("kingdom_input")

    observe({

      rank_selected <- paste0(input_manual_rank_id(), "_input")
      rv_rank_selected(rank_selected)

      if (rank_selected == "kingdom_input") {
        names <- list_inputs[["kingdom_input"]]()
      } else {
        names <- list_inputs[[rank_selected]]()$scientificName
      }

      toggle("synonym_container_id", condition = length(names) > 1)

      updateSelectizeInput(session,
        "synonym",
        glue("Synonym ({length(names)})"),
        names,
        selected = names[1]
      )

    })

    return(
      list(
        synonym = reactive(input[["synonym"]]),
        taxonID = reactive({
          req(input[["synonym"]])
          if (rv_rank_selected() == "kingdom_input") {
            kingdom_data |> filter(scientificName %in% input[["synonym"]]) |> pull(taxonID)
          } else {
            list_inputs[[rv_rank_selected()]]() |>
              filter(scientificName %in% input[["synonym"]]) |>
              pull(taxonID)
          }
        }),
        rank_string = reactive({
          req(input[["synonym"]])
          if (rv_rank_selected() == "kingdom_input") {
            kingdom_data |> filter(scientificName %in% input[["synonym"]]) |> pull(taxonRank)
          } else {
            list_inputs[[rv_rank_selected()]]() |>
              filter(scientificName %in% input[["synonym"]]) |>
              pull(taxonRank)
          }
        })
      )
    )
  })
}
