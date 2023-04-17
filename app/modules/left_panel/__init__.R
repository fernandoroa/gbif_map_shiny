box::use(
  shiny[...],
  imola[...],
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
      title = "Select a taxon", collapsible = FALSE, class = "background",
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
        kingdom = tags$section(selectizeInput(ns("kingdom_id"), "Kingdom", names(phylums_per_kingdom),
          selected = "Plantae"
        ), options = list(dropdownParent = I("body"))),
        phylum = dropdown$ui(ns("phylum_mod_id"), category = "phylum", choices = NULL, selected = NULL),
        class = dropdown$ui(ns("class_mod_id"), category = "class", choices = NULL, selected = NULL),
        order = dropdown$ui(ns("order_mod_id"), category = "order", choices = NULL, selected = NULL),
        family = dropdown$ui(ns("family_mod_id"), category = "family", choices = NULL, selected = NULL),
        genus = dropdown$ui(ns("genus_mod_id"), category = "genus", choices = NULL, selected = NULL),
        infrageneric = dropdown$ui(ns("infragenus_mod_id"), category = "infrageneric", choices = NULL, selected = NULL),
        selection = tags$section(textOutput(ns("output_1"))),
        manual_rank = selectizeInput(ns("manual_rank_id"), "Manual Rank Selection", ranks,
          selected = "infrageneric"
        ), options = list(dropdownParent = I("body"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$output_1 <- renderText({
      rank_selected <- paste0(input$manual_rank_id, "_input")
      if (rank_selected == "infrageneric_input") {
        selection <- paste(infrageneric_input()$scientificName, "Rank:", infrageneric_input()$taxonRank)
      } else {
        selection <- get(paste0(input$manual_rank_id, "_input"))()
      }
      paste("Taxon selected:", selection)
    })

    kingdom_input <- reactive(input$kingdom_id)

    phylum_input <- dropdown$server("phylum_mod_id",
      category_children = "phylum",
      list_children_per_parent = phylums_per_kingdom, kingdom_input
    )

    class_input <- dropdown$server("class_mod_id",
      category_children = "class",
      list_children_per_parent = classes_per_phylum, phylum_input
    )

    order_input <- dropdown$server("order_mod_id",
      category_children = "order",
      list_children_per_parent = orders_per_class, class_input
    )

    family_input <- dropdown$server("family_mod_id",
      category_children = "family",
      list_children_per_parent = families_per_order, order_input
    )

    genus_input <- dropdown$server("genus_mod_id",
      category_children = "genus",
      list_children_per_parent = genera_per_family, family_input
    )

    infrageneric_input <- dropdown$server("infragenus_mod_id",
      category_children = "infrageneric",
      list_children_per_parent = infragenus_per_genus, genus_input, return_ = "df"
    )

    return(reactive(input$kingdom_id))
  })
}

#' @export
run_sqrt <- function(number) {
  sqrt(number)
}
