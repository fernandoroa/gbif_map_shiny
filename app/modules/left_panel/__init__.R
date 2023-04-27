box::use(
  shiny[...],
  imola[...],
  dplyr[pull, filter, `%>%`]
)
box::use(
  ./objects[...],
  ./dropdown
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
          "kingdom phylum class",
          "order family genus",
          "infrageneric infrageneric manual_rank",
          "selection selection selection "
        ),
        columns = "1fr 1fr 1fr",
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
        selection = htmlOutput(ns("output_1")),
        manual_rank = selectizeInput(ns("manual_rank_id"), "Rank Selection", ranks,
          selected = "infrageneric"
        ),
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    kingdom_input <- reactive(input$kingdom_id)

    input_manual_rank_id <- reactive(input$manual_rank_id)

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

    output$selection_box <- renderUI({
      req(input$manual_rank_id)
      div(class = "text_box",
        HTML(paste(
              paste("Taxon selected:", vars_resolve_mod$synonym()),
              paste("Rank:", vars_resolve_mod$rank_string()),
              paste("TaxonID:", vars_resolve_mod$taxonID()),
              sep = "<br>"
            )
        )
      )
    })

    return(reactive(input$kingdom_id))
  })
}

#' @export
run_sqrt <- function(number) {
  sqrt(number)
}
