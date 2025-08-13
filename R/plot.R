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

#' UMCG Scale Colours
#'
#' These are `ggplot2` helpers to add organisational colours to `ggplot` objects.
#' @param n Number of colours required.
#' @param ... arguments passed on to [ggplot2::scale_colour_manual()] or [ggplot2::scale_fill_manual()]
#' @rdname scale_colour_umcg
#' @importFrom plot2 get_colour
#' @export
#' @examples
#' library(ggplot2)
#'
#' mtcars |>
#'   ggplot(aes(x = hp,
#'              y = mpg,
#'              colour = as.factor(vs)),
#'              fill = as.factor(vs)) +
#'   geom_point(size = 3) +
#'   scale_colour_umcg(n = 3)
#'
#' mtcars |>
#'   ggplot(aes(x = hp,
#'              y = mpg,
#'              colour = as.factor(vs),
#'              fill = as.factor(vs))) +
#'   geom_point() +
#'   geom_smooth(method="lm") +
#'   scale_colour_umcg(n = 2) +
#'   scale_fill_umcg(n = 2) +
#'   labs(title = "Non-UMCG-styled plot")
#'
#' # With plot2(), this all goes automatically with less code
#' mtcars |>
#'   plot2(hp, mpg, as.factor(vs),
#'         title = "UMCG-styled plot",
#'         subtitle = "(with the right font too; 'Outfit' from Google Fonts)") |>
#'   add_smooth(method = "lm")
scale_colour_umcg <- function(..., n) {
  ggplot2::scale_colour_manual(values = get_colour("umcg", length = n), ...)
}

#' @rdname scale_colour_umcg
#' @export
scale_fill_umcg <- function(..., n) {
  ggplot2::scale_fill_manual(values = get_colour("umcg", length = n), ...)
}


# These are re-exports; they will link to the original package and help file --------

#' @importFrom plot2 plot2
#' @export
plot2::plot2

#' @importFrom plot2 add_type
#' @export
plot2::add_type

#' @importFrom plot2 add_line
#' @export
plot2::add_line

#' @importFrom plot2 add_smooth
#' @export
plot2::add_smooth

#' @importFrom plot2 add_sf
#' @export
plot2::add_sf

#' @importFrom plot2 get_colour
#' @export
plot2::get_colour

