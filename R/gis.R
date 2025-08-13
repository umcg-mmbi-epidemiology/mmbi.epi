# ===================================================================== #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at University Medical Center Groningen (UMCG),             #
#  department of Medical Microbiology & Infection Prevention (MMBI),    #
#  unit Epidemiology & Data Science:                                    #
#  https://github.com/umcg-mmbi-epidemiology                            #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

#' GIS Data Sets
#'
#' These data sets are inherited from the `certegis` package.
#' @seealso
#' * [geo_gemeenten][certegis::geo_gemeenten]
#' * [geo_ggdregios][certegis::geo_ggdregios]
#' * [geo_nuts3][certegis::geo_nuts3]
#' * [geo_postcodes2][certegis::geo_postcodes2]
#' * [geo_postcodes3][certegis::geo_postcodes3]
#' * [geo_postcodes4][certegis::geo_postcodes4]
#' * [geo_postcodes6][certegis::geo_postcodes6]
#' * [geo_provincies][certegis::geo_provincies]
#' * [postcodes][certegis::postcodes]
#' * [postcodes4_afstanden][certegis::postcodes4_afstanden]
#' @format NULL
#' @rdname gis-datasets
#' @name gis-datasets
#' @export
#' @examples
#' geo_provincies
#'
#' geo_provincies |>
#'   plot2(title = "Inwoners in Nederlandse provincies")
#'
#' geo_provincies |>
#'   plot2(colour_fill = "umcgoranje")
#'
#' geo_gemeenten |>
#'   filter_geolocation(provincie == "Groningen") |>
#'   plot2(category = inwoners / oppervlakte_km2,
#'         category.title = "Inwoners per km^2",
#'         title = "Populatiedichtheid in Groningse gemeenten")
#'
#' geo_gemeenten |>
#'   filter_geolocation(provincie == "Groningen") |>
#'   plot2(category = inwoners / oppervlakte_km2,
#'         category.title = "Inwoners per km^2",
#'         title = "Populatiedichtheid in Groningse gemeenten",
#'         subtitle = "Met postcode-4 als overlay",
#'         datalabels = FALSE,
#'         colour = "umcgblauw",
#'         linewidth = 1) |>
#'   add_sf(geo_postcodes4 |> filter_geolocation(provincie == "Groningen"),
#'          colour = "umcgoranje",
#'          colour_fill = NA,
#'          linewidth = 0.25)
geo_gemeenten <- certegis::geo_gemeenten

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_ggdregios <- certegis::geo_ggdregios

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_nuts3 <- certegis::geo_nuts3

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_postcodes2 <- certegis::geo_postcodes2

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_postcodes3 <- certegis::geo_postcodes3

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_postcodes4 <- certegis::geo_postcodes4

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_postcodes6 <- certegis::geo_postcodes6

#' @rdname gis-datasets
#' @format NULL
#' @export
geo_provincies <- certegis::geo_provincies

#' @rdname gis-datasets
#' @format NULL
#' @export
postcodes <- certegis::postcodes

#' @rdname gis-datasets
#' @format NULL
#' @export
postcodes4_afstanden <- certegis::postcodes4_afstanden


# These are re-exports; they will link to the original package and help file --------

#' @importFrom certegis add_map
#' @export
certegis::add_map

#' @importFrom certegis as.sf
#' @export
certegis::as.sf

#' @importFrom certegis cases_within_radius
#' @export
certegis::cases_within_radius

#' @importFrom certegis convert_to_metre_CRS28992
#' @export
certegis::convert_to_metre_CRS28992

#' @importFrom certegis degrees_to_sf
#' @export
certegis::degrees_to_sf

#' @importFrom certegis filter_geolocation
#' @export
certegis::filter_geolocation

#' @importFrom certegis filter_sf
#' @export
certegis::filter_sf

#' @importFrom certegis geocode
#' @export
certegis::geocode

#' @importFrom certegis is.sf
#' @export
certegis::is.sf

#' @importFrom certegis latitude
#' @export
certegis::latitude

#' @importFrom certegis longitude
#' @export
certegis::longitude

#' @importFrom certegis reverse_geocode
#' @export
certegis::reverse_geocode
