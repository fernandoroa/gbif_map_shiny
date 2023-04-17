box::use(
  shiny[tags, NS, selectizeInput, updateSelectizeInput, moduleServer, observeEvent, reactive, req, reactiveVal],
  dplyr[pull]
)

box::use(
  ../../logic/helper[capitalize_first],
)

#' @export
ui <- function(id, category, choices = NULL, selected = NULL) {
  ns <- NS(id)
  category_id <- paste0(category, "_id")
  category_name <- capitalize_first(category)
  tags$section(selectizeInput(ns(category_id), category_name, choices,
    selected = selected
  ), options = list(dropdownParent = I("body")))
}

#' @export
server <- function(id, category_children, list_children_per_parent, input_ = NULL, column = "scientificName", return_ = "single") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    category_children_id <- paste0(category_children, "_id")
    category_children_name <- capitalize_first(category_children)

    rv_df_childrens <- reactiveVal(data.frame())

    observeEvent(input_(), {
      req(input_() != "")

      childrens <- list_children_per_parent[[input_()]] |> pull(column)
      rv_df_childrens(list_children_per_parent[[input_()]])

      updateSelectizeInput(session, category_children_id,
        category_children_name,
        childrens,
        selected = childrens[1]
      )
    })

    if (return_ == "single") {
      return(reactive(input[[category_children_id]]))
    } else {
      return(reactive(rv_df_childrens()))
    }
  })
}
