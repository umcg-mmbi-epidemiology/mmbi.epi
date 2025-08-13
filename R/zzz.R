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

#' @importFrom plot2 register_colour
.onLoad <- function(...) {
  # load sf namespace on load, so that:
  # - `certegis` geographic data sets will print correctly
  # - `certegis` GIS functions can be used in this package
  # - `certegis` GIS functions can be used in other packages (`vctrs` pkg will otherwise complain)
  requireNamespace("sf", quietly = TRUE)

  # register UMCG colours to plot2
  suppressMessages(register_colour(
    umcgdonkerblauw = "#003183",
    umcgoranje      = "#FF7D00",
    umcgteal        = "#007A8A", # from here on non-brand, UMCG only provides 2 colours with good intensity :(
    umcggroen       = "#2E8540",
    umcgpaars       = "#5B3FA8",
    umcggrijs       = "#3A3A3A",
    umcggoud        = "#B38F00",
    umcg            = c("umcgdonkerblauw","umcgoranje","umcgteal",
                        "umcggroen","umcgpaars","umcggrijs","umcggoud")))
  options(plot2.colour = "umcg",
          plot2.colour_font_secondary = "umcgdonkerblauw",
          plot2.font = "Outfit")
}

#' @importFrom plot2 unregister_colour
.onUnload <- function(...) {
  suppressMessages(unregister_colour("^umcg"))
  options(plot2.colour = NULL,
          plot2.colour_font_secondary = NULL,
          plot2.font = NULL)
}
