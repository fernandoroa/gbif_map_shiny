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
    debounce,
    validate,
    need
  ],
  dplyr[`%>%`],
  stats[setNames]
)

box::use(
  ./objects[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  selectizeInput(ns("manual_rank_id"), "Rank Selection", ranks,
    selected = "phylum"
  )
}

#' @export
server <- function(
    id,
    infrageneric_input,
    genus_input,
    family_input,
    order_input,
    class_input,
    phylum_input) {
  moduleServer(id, function(input, output, session) {

    observeEvent(
      c(
        infrageneric_input(),
        genus_input(),
        family_input(),
        order_input(),
        class_input(),
        phylum_input()
      ),
      {
        infrageneric_input <- infrageneric_input()
        genus_input <- genus_input()
        family_input <- family_input()
        order_input <- order_input()
        class_input <- class_input()
        phylum_input <- phylum_input()

        list_inputs <- lapply(ls(pattern = "_input$"), function(x) get(x, envir = environment())) |>
          setNames(ls(pattern = "_input$"))

        names_on_list <- setdiff(names(list_inputs), "kingdom_input")
        categories_to_remove <- sapply(names_on_list, function(x) list_inputs[[x]]$taxonID) |>
          is.na() |>
          which() |>
          names() %>%
          {
            gsub("_input", "", .)
          }

        ranks_filtered <- setdiff(ranks, categories_to_remove)

        if (is.null(session$userData$input_manual_rank_id()())) {
          selected <- "phylum"
        } else {
          selected <- session$userData$input_manual_rank_id()()
        }
        updateSelectizeInput(session,
          "manual_rank_id",
          "Rank Selection",
          ranks_filtered,
          selected = selected
        )
      }
    )

    input_manual_rank_id <- reactive({
      input$manual_rank_id
    })

    input_manual_rank_id_d <- input_manual_rank_id |> debounce(500)

    return(reactive(input_manual_rank_id_d()))
  })
}
