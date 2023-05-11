#' @export
kingdom_data <- readRDS("rds/kingdom_data.rds")

#' @export
phylums_per_kingdom <- readRDS("rds/phylums_per_kingdom.rds")

#' @export
classes_per_phylum <- readRDS("rds/classes_per_phylum.rds")

#' @export
orders_per_class <- readRDS("rds/orders_per_class.rds")

#' @export
families_per_order <- readRDS("rds/families_per_order.rds")

#' @export
genera_per_family <- readRDS("rds/genera_per_family.rds")

#' @export
infragenus_per_genus <- readRDS("rds/infrage_per_genus.rds")

# infragenus_ranks <-  c("species", "variety", "subspecies", "form")
manual_ranks <- c("kingdom", "phylum", "class", "order", "family", "genus")
# not used "unranked"

#' @export
ranks <- c(manual_ranks, "infrageneric")

#' @export
ranks_except_k <- setdiff(ranks, "kingdom")

#' @export
multimedia_filtered <- readRDS("rds/multimedia_filtered.rds")
