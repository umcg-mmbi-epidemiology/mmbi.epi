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

#' UMCG Plot Style
#'
#' These are `ggplot2` helpers to add organisational colours to `ggplot` objects.
#' @param n Number of colours required.
#' @param ... arguments passed on to [ggplot2::scale_colour_manual()] or [ggplot2::scale_fill_manual()]
#' @rdname plot_style_umcg
#' @importFrom plot2 get_colour
#' @export
#' @examples
#' library(ggplot2)
#'
#' p <- mtcars |>
#'   ggplot(aes(x = hp,
#'              y = mpg,
#'              colour = as.factor(vs)),
#'              fill = as.factor(vs)) +
#'   geom_point(size = 3) +
#'   geom_smooth()
#' p
#'
#' p +
#'   scale_colour_umcg(n = 2) +
#'   scale_fill_umcg(n = 2) +
#'   labs(title = "UMCG colours")
#'
#' # With plot2(), this all goes automatically with less code
#' mtcars |>
#'   plot2(x = hp,
#'         y = mpg,
#'         category = as.factor(vs),
#'         smooth = TRUE,
#'         title = "Fully UMCG-styled plot",
#'         subtitle = "(with the right font too; 'Outfit' from Google Fonts)")
scale_colour_umcg <- function(..., n) {
  ggplot2::scale_colour_manual(values = get_colour("umcg", length = n), ...)
}

#' @rdname plot_style_umcg
#' @export
scale_fill_umcg <- function(..., n) {
  ggplot2::scale_fill_manual(values = get_colour("umcg", length = n), ...)
}

#' @rdname plot_style_umcg
#' @export
register_umcg_plot_style <- function() {
  # register UMCG colours to plot2
  suppressMessages(register_colour(
    umcgblauw        = "#003183",
    umcgdonkerblauw  = "#003183",
    umcglichtblauw   = "#eef7fb",
    umcgoranje       = "#FF7D00",
    umcgdonkeroranje = "#FF7D00",
    umcglichtoranje  = "#fff2e4",
    umcgteal         = "#007A8A", # from here on non-brand, UMCG only provides 2 colours with good intensity :(
    umcggroen        = "#2E8540",
    umcgpaars        = "#5B3FA8",
    umcggrijs        = "#3A3A3A",
    umcggoud         = "#B38F00",
    umcg             = c("umcgblauw","umcgoranje","umcgteal",
                         "umcggroen","umcgpaars","umcggrijs","umcggoud")))
  options(plot2.colour = "umcg",
          plot2.colour_font_secondary = "umcgdonkerblauw",
          plot2.colour_sf_fill = c("umcglichtblauw", "umcgdonkerblauw"),
          plot2.font = "Outfit") # 'Outfit', a Google Font, is the official UMCG font
}

#' @rdname plot_style_umcg
#' @export
unregister_umcg_plot_style <- function() {
  suppressMessages(unregister_colour("^umcg"))
  options(plot2.colour = NULL,
          plot2.colour_font_secondary = NULL,
          plot2.colour_sf_fill = NULL,
          plot2.font = NULL)
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

