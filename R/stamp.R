#' Title
#'
#' @param data
#' @param scales
#' @param county
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' #nc_flat |> rename(fips = FIPS) |> compute_state_brasil() |> head() |> str()
#' #nc_flat |> rename(fips = FIPS) |> compute_state_brasil(keep_state = "Ashe")
compute_state_brasil_stamp <- function(data, scales, keep_state = NULL){

  reference_filtered <- reference_full
  #
  if(!is.null(keep_state)){

    keep_state %>% tolower() -> keep_state

    reference_filtered %>%
      dplyr::filter(.data$state %>%
                      tolower() %in%
                      keep_state) ->
      reference_filtered

  }

  reference_filtered %>%
    dplyr::select("state", "state_abb","state_code",
                  "geometry",
                  "xmin", "xmax",
                  "ymin", "ymax") ->
    reference_filtered


  reference_filtered %>%
    dplyr::mutate(group = -1)

}


StatCountybrasilstamp <- ggplot2::ggproto(`_class` = "StatCountybrasilstamp",
                               `_inherit` = ggplot2::Stat,
                               compute_panel = compute_state_brasil_stamp,
                               default_aes = ggplot2::aes(geometry =
                                                            ggplot2::after_stat(geometry)))



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
#' library(ggplot2)
#' ggplot() +
#' stamp_sf_statebrasil()
stamp_sf_statebrasil <- function(
                                 mapping = NULL,
                                 data = cars,
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 crs = "NAD27", #WGS84, NAD83
                                 ...
                                 ) {

                                 c(ggplot2::layer_sf(
                                   stat = StatCountybrasilstamp,  # proto object from step 2
                                   geom = ggplot2::GeomSf,  # inherit other behavior
                                   data = data,
                                   mapping = mapping,
                                   position = position,
                                   show.legend = show.legend,
                                   inherit.aes = inherit.aes,
                                   params = rlang::list2(na.rm = na.rm, ...)),
                                   coord_sf(crs = crs,
                                            # default_crs = sf::st_crs(crs),
                                            # datum = sf::st_crs(crs),
                                            default = TRUE)
                                 )

}






