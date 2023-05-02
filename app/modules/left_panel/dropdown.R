box::use(
  shiny[tags, NS, selectizeInput, updateSelectizeInput, moduleServer, observeEvent, reactive, req, reactiveVal],
  dplyr[pull, filter],
  glue[glue],
  stats[setNames]
)

box::use(
  ../../logic/helper[capitalize_first],
)

#' @export
ui <- function(id, category) {
  ns <- NS(id)
  category_id <- paste0(category, "_id")
  category_name <- capitalize_first(category)
  selectizeInput(ns(category_id), category_name, choices = NULL)
}

#' @export
server <- function(id,
  category_children,
  list_children_per_parent,
  input_ = NULL,
  column = "scientificName",
  parent_column = NULL
  ) {
    moduleServer(id, function(input, output, session) {

      ns <- session$ns
      category_children_id <- paste0(category_children, "_id")
      category_children_name <- capitalize_first(category_children)

      rv_df_childrens <- reactiveVal(data.frame())
      rv_selec <- reactiveVal(NULL)

      observeEvent(input_(), {
        if (column == "phylum") {
          rv_selec(input_())
        } else {
          rv_selec(input_() |> pull(parent_column) |> unique())
        }
        childrens <- NULL
        req(rv_selec() != "")

        req(!is.null(list_children_per_parent[[rv_selec()]]))

        childrens <- list_children_per_parent[[rv_selec()]] |>
          pull(column) |>
          unique() |>
          sort()

        childrens <- childrens |> setNames(gsub("_", " ", childrens))

        updateSelectizeInput(session,
          category_children_id,
          glue("{category_children_name} ({length(childrens)})"),
          childrens,
          selected = childrens[1],
          server = TRUE
        )
      })

      return(
          reactive({
            req(rv_selec() != "")
            req(input[[category_children_id]] != "")
            list_children_per_parent[[rv_selec()]] |> filter(.data[[column]] == input[[category_children_id]])
          })
      )
    })
}
