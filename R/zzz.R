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

globalVariables(c("ORD_RECEIPTTIME"))

#' @importFrom plot2 register_colour
.onLoad <- function(...) {
  # load sf namespace on load, so that:
  # - `certegis` geographic data sets will print correctly
  # - `certegis` GIS functions can be used in this package
  # - `certegis` GIS functions can be used in other packages (`vctrs` pkg will otherwise complain)
  requireNamespace("sf", quietly = TRUE)

  register_umcg_plot_style()
}

#' @importFrom plot2 unregister_colour
.onUnload <- function(...) {
  unregister_umcg_plot_style()
}
