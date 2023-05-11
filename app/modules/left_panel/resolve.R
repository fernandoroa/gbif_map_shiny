box::use(
  shiny[
    NS,
    selectizeInput,
    updateSelectizeInput,
    moduleServer,
    observeEvent,
    reactive,
    req,
    reactiveVal,
    div,
    debounce,
    validate,
    need
  ],
  dplyr[pull, filter],
  glue[glue],
  shinyjs[hide, show, hidden, toggle]
)

box::use(
  ../../objects/objects[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  hidden(
    div(
      id = ns("synonym_container_id"),
      selectizeInput(ns("synonym"), "Select Synonym",
        choices = NULL,
        selected = NULL,
        options = list(create = TRUE)
      )
    )
  )
}

#' @export
server <- function(
    id,
    input_manual_rank_id,
    infrageneric_input,
    genus_input,
    family_input,
    order_input,
    class_input,
    phylum_input,
    kingdom_input) {
  moduleServer(id, function(input, output, session) {
    rv_rank_selected <- reactiveVal("kingdom_input")

    observeEvent(
      c(
        input_manual_rank_id(),
        infrageneric_input(),
        genus_input(),
        family_input(),
        order_input(),
        class_input(),
        phylum_input(),
        kingdom_input()
      ),
      {
        rank_selected <- paste0(input_manual_rank_id(), "_input")
        rv_rank_selected(rank_selected)

        if (rank_selected == "kingdom_input") {
          names_ <- get(rank_selected)()
        } else {
          names_ <- get(rank_selected)()$scientificName
        }

        toggle("synonym_container_id", condition = length(names_) > 1)

        updateSelectizeInput(session,
          "synonym",
          glue("Synonym ({length(names_)})"),
          names_,
          selected = names_[1],
          server = TRUE
        )
      }
    )

    input_synonym <- reactive({
      input[["synonym"]]
    })

    input_synonym_d <- input_synonym |> debounce(1000)

    return(
      list(
        synonym = reactive(input_synonym_d()),
        taxonID = reactive({
          if (rv_rank_selected() == "kingdom_input") {
            kingdom_data |>
              filter(scientificName %in% input_synonym_d()) |>
              pull(taxonID)
          } else {
            validate(
              need(
                try(get(rv_rank_selected())() |> filter(scientificName %in% input_synonym_d()) |> NROW() > 0),
                FALSE
              )
            )
            get(rv_rank_selected())() |>
              filter(scientificName %in% input_synonym_d()) |>
              pull(taxonID)
          }
        }),
        rank_string = reactive({
          req(input_synonym_d())

          if (rv_rank_selected() == "kingdom_input") {
            kingdom_data |>
              filter(scientificName %in% input_synonym_d()) |>
              pull(taxonRank)
          } else {
            validate(
              need(
                try(get(rv_rank_selected())() |> filter(scientificName %in% input_synonym_d()) |> NROW() > 0),
                FALSE
              )
            )
            get(rv_rank_selected())() |>
              filter(scientificName %in% input_synonym_d()) |>
              pull(taxonRank)
          }
        })
      )
    )
  })
}
