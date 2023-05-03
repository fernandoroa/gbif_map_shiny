box::use(
  shiny[...],
  imola[...],
  dplyr[pull, filter, `%>%`],
  stats[setNames]
)
box::use(
  ./objects[...],
  ./dropdown,
  ./resolve,
  ./select_rank
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      title = "Select a taxon", collapsible = FALSE, class = "left_background",
      solidHeader = TRUE, status = "primary",
      gridPanel(
        areas = c(
          "kingdom kingdom phylum phylum class class",
          "order order family family genus genus",
          "infrageneric infrageneric infrageneric infrageneric manual_rank manual_rank",
          "resolve resolve resolve selection selection selection"
        ),
        columns = "1fr 1fr 1fr 1fr 1fr 1fr",
        gap = "1em",
        kingdom = selectizeInput(ns("kingdom_id"), "Kingdom", names(phylums_per_kingdom),
          selected = "Plantae"
        ),
        phylum = dropdown$ui(ns("phylum_mod_id"), category = "phylum"),
        class = dropdown$ui(ns("class_mod_id"), category = "class"),
        order = dropdown$ui(ns("order_mod_id"), category = "order"),
        family = dropdown$ui(ns("family_mod_id"), category = "family"),
        genus = dropdown$ui(ns("genus_mod_id"), category = "genus"),
        infrageneric = dropdown$ui(ns("infragenus_mod_id"), category = "infrageneric"),
        selection = htmlOutput(ns("selection_box")),
        manual_rank = select_rank$ui(ns("select_rank_mod_id")),
        resolve = resolve$ui(ns("resolve_mod_id"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    session$userData$input_manual_rank_id <- reactiveVal(NULL)

    kingdom_input <- reactive(input$kingdom_id)

    phylum_input <- dropdown$server("phylum_mod_id",
      category_children = "phylum",
      list_children_per_parent = phylums_per_kingdom, kingdom_input, column = "phylum"
    )

    class_input <- dropdown$server("class_mod_id",
      category_children = "class",
      list_children_per_parent = classes_per_phylum, phylum_input, column = "class", parent_column = "phylum"
    )

    order_input <- dropdown$server("order_mod_id",
      category_children = "order",
      list_children_per_parent = orders_per_class, class_input, column = "order", parent_column = "class"
    )

    family_input <- dropdown$server("family_mod_id",
      category_children = "family",
      list_children_per_parent = families_per_order, order_input, column = "family", parent_column = "order"
    )

    genus_input <- dropdown$server("genus_mod_id",
      category_children = "genus",
      list_children_per_parent = genera_per_family, family_input, column = "genus", parent_column = "family"
    )

    infrageneric_input <- dropdown$server("infragenus_mod_id",
      category_children = "infrageneric",
      list_children_per_parent = infragenus_per_genus, genus_input, parent_column = "genus"
    )

    list_inputs <- lapply(ls(pattern = "_input$"), function(x) get(x, envir = environment())) |>
      setNames(ls(pattern = "_input$"))

    input_manual_rank_id <- select_rank$server(
      "select_rank_mod_id",
      infrageneric_input,
      genus_input,
      family_input,
      order_input,
      class_input,
      phylum_input
    )
    session$userData$input_manual_rank_id(input_manual_rank_id)

    vars_resolve_mod <- resolve$server(
      "resolve_mod_id",
      input_manual_rank_id,
      infrageneric_input,
      genus_input,
      family_input,
      order_input,
      class_input,
      phylum_input,
      kingdom_input
    )

    output$selection_box <- renderUI({
      req(input_manual_rank_id())
      div(
        class = "text_box",
        HTML(paste(
          paste("Taxon selected:", vars_resolve_mod$synonym()),
          paste("Rank:", vars_resolve_mod$rank_string()),
          paste("TaxonID:", vars_resolve_mod$taxonID()),
          sep = "<br>"
        ))
      )
    })

    return(reactive(vars_resolve_mod))
  })
}
