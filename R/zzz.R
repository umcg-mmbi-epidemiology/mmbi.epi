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

# these are mentioned as objects, putting them in globalVariables() prevents a note in R CMD CHECK
globalVariables(c("ab_col",
                  "AB_NAME",
                  "ABRS_AGARDIFFUSIONRAWVALUE",
                  "ABRS_ETESTRAWVALUE",
                  "ABRS_MICRAWVALUE",
                  "ABRS_RISRAWVALUE",
                  "ABRS_RISREPORTVALUE",
                  "age_group",
                  "CHCL_FREETEXTALLOWED",
                  "CHCL_NAME",
                  "dbname",
                  "DEPT_NAME",
                  "DIM_NAME",
                  "ENCT_PERSON",
                  "free_text",
                  "HCPR_BIRTHDATE",
                  "HCPR_FIRSTNAME",
                  "HCPR_LASTNAME",
                  "HCPR_MNEMONIC",
                  "HCPR_SEX",
                  "HCPR_TITLE",
                  "MAT_COMMENT",
                  "MAT_MNEMONIC",
                  "MAT_SAMPLINGCODE",
                  "MAT_SHORTNAME",
                  "MORG_MNEMONIC",
                  "MORG_NAME",
                  "MORG_SHORTNAME",
                  "ORD_RECEIPTTIME",
                  "PROP_ID",
                  "PROP_MNEMONIC",
                  "PROP_SHORTNAME",
                  "RSLT_RAWVALUE",
                  "servername",
                  "SPEC_MNEMONIC",
                  "SPEC_NAME",
                  "SPMN_INTERNALID",
                  "SPMN_SAMPLINGTIME",
                  "STAY_ENDTIME",
                  "STAY_STARTTIME",
                  "STN_MNEMONIC",
                  "U##ORD_INTERNALID",
                  "UNIT_NAME",
                  "username",
                  "WARD_MNEMONIC",
                  "WARD_NAME"))

.onLoad <- function(...) {
  # load sf namespace on load, so that:
  # - `certegis` geographic data sets will print correctly
  # - `certegis` GIS functions can be used in this package
  # - `certegis` GIS functions can be used in other packages (`vctrs` pkg will otherwise complain)
  requireNamespace("sf", quietly = TRUE)

  register_umcg_plot_style()
}

.onUnload <- function(...) {
  unregister_umcg_plot_style()
}
