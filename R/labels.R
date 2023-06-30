# step 00 reference data
# brasil_state_centers <- data.frame(     x   =  -81.49496,   y = 36.42112,  county_name = "Ashe",   fips = "37009")


# step 1
#' Title
#'
#' @param data
#' @param scales
#' @param keep_state
#'
#' @return
#' @export
#'
#' @examples
#' ggnc::nc_flat |>
#'   dplyr::rename(fips = FIPS) |>
#'   dplyr::rename(label = NAME) |>
#'   compute_panel_county_centers()
compute_panel_state_centers <- function(data,
                                         scales,
                                         keep_state = NULL){

  brasil_state_centers_filtered <- brasil_state_centers

  if(!is.null(keep_state)){
    keep_state %>% tolower() -> keep_state

    brasil_state_centers_filtered %>%
      dplyr::filter(.data$state %>%
                      tolower() %in%
                      keep_state) ->
      brasil_state_centers_filtered}

  data %>%
    dplyr::inner_join(brasil_state_centers_filtered) %>%
    dplyr::select(x, y, label)

}




# step 2 proto
StatStatecenters <- ggplot2::ggproto(
  `_class` = "StatStatecenters",
  `_inherit` = ggplot2::Stat,
  # required_aes = c("label"), # for some reason this breaks things... why?
  compute_panel = compute_panel_state_centers
)

#' Title
#'
#' @param mapping
#' @param data
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' brasil_flat %>%
#' ggplot() +
#' aes(state = state) +
#' geom_sf_statebrasil() +
#' aes(fill = region, label = state) +
#'
#'  geom_label_brasil_state(lineheight = .7,
#'  size = 2, check_overlap= TRUE,
#'  color = "oldlace")
geom_label_brasil_state <- function(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatStatecenters,  # proto object from Step 2
    geom = ggplot2::GeomText,  # inherit other behavior
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

