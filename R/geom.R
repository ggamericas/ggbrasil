################# Compute panel function ###########

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
#' library(dplyr)
#' #brasil_flat |> rename(fips = FIPS) |> compute_state_br() |> head()
#' #brasil_flat |> rename(fips = FIPS) |> compute_state_br(keep_state = "Ashe")
compute_state_br <- function(data, scales, keep_state = NULL){

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

  # to prevent overjoining
  reference_filtered %>%
    dplyr::select("state", "state_abb","state_code",  "geometry", "xmin",
                  "xmax", "ymin", "ymax") ->
    reference_filtered


  data %>%
    dplyr::inner_join(reference_filtered) %>% # , by = join_by(fips)
    dplyr::mutate(group = -1)
    # sf::st_as_sf() %>%
    # sf::st_transform(crs = 5070)

}


###### Specify ggproto ###############

StatStatebrasil <- ggplot2::ggproto(`_class` = "StatStatebrasil",
                               `_inherit` = ggplot2::Stat,
                               compute_panel = compute_state_br,
                               default_aes = ggplot2::aes(geometry =
                                                            ggplot2::after_stat(geometry)))



########### geom function, inherits from sf ##################

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
#' brasil_flat %>%
#' ggplot() +
#' aes(state = state) +
#' geom_sf_statebrasil(linewidth = .05) +
#' aes(fill = region) +
#' scale_fill_viridis_d()
geom_sf_statebrasil <- function(
                                 mapping = NULL,
                                 data = NULL,
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 crs = "NAD27", # "NAD27", 5070, "WGS84", "NAD83", 4326 , 3857
                                 ...
                                 ) {

                                 c(ggplot2::layer_sf(
                                   stat = StatStatebrasil,  # proto object from step 2
                                   geom = ggplot2::GeomSf,  # inherit other behavior
                                   data = data,
                                   mapping = mapping,
                                   position = position,
                                   show.legend = show.legend,
                                   inherit.aes = inherit.aes,
                                   params = rlang::list2(na.rm = na.rm, ...)),
                                   coord_sf(crs = crs,
                                            default_crs = sf::st_crs(crs),
                                            datum = crs,
                                            default = TRUE)
                                 )

}



