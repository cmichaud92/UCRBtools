
#########################################################
#                        life_stage                     #
#########################################################

# Classifies Smallmouth, Walleye and Northern Pike into UCRB life stage classifications
# Use piscivore = TRUE to divide adult class into adult and adult_pic (piscivore)

life_stage_nf <- function(.data, specvar, lenvar, ...) {
    .data %>%
        dplyr::mutate(ls = dplyr::case_when({{specvar}} == "CS" &
                                                {{lenvar}} <= 90 ~ "YOY",
                                            {{specvar}} == "CS" &
                                                {{lenvar}} >= 90 & {{lenvar}} < 400 ~ "JUV",
                                            {{specvar}} == "CS" &
                                                {{lenvar}} >= 400 ~ "ADULT",
                                            {{specvar}} %in% c("RZ", "FM", "BH", "FR", "FB", "BHRZ", "SU") &
                                                {{lenvar}} < 100 ~ "YOY",
                                            {{specvar}} %in% c("RZ", "FM", "BH", "FR", "FB", "BHRZ", "SU") &
                                                {{lenvar}} >= 100 & {{lenvar}} < 400 ~ "JUV",
                                            {{specvar}} %in% c("RZ", "FM", "BH", "FR", "FB", "BHRZ", "SU") &
                                                {{lenvar}} <= 400 ~ "ADULT",
                                            {{specvar}} %in% c("BT", "HB", "RT", "CH") &
                                                {{lenvar}} < 100 ~ "YOY",
                                            {{specvar}} %in% c("BT", "HB", "RT", "CH") &
                                                {{lenvar}} >= 100 & {{lenvar}} <= 250 ~ "JUV",
                                            {{specvar}} %in% c("BT", "HB", "RT", "CH") &
                                                {{lenvar}} > 250 ~ "ADULT"))
}


life_stage <- function(.data, specvar, lenvar, piscivore = FALSE, ...) {

    if (piscivore == TRUE) {
        .data %>%
            dplyr::mutate(ls = dplyr::case_when(specvar == "SM" &
                                      lenvar < 100 ~ "SUB_JUV",
                                  specvar == "SM" &
                                      lenvar >= 100 & lenvar < 200 ~ "JUV",
                                  specvar == "SM" &
                                      lenvar >= 200 & lenvar < 325 ~ "ADULT",
                                  specvar == "SM" &
                                      lenvar >=325 ~ "ADULT_PIC",
                                  specvar == "NP" &
                                      lenvar < 300 ~ "JUV",
                                  specvar == "NP" &
                                      lenvar >= 300 & lenvar < 450 ~ "ADULT",
                                  specvar == "NP" &
                                      lenvar >= 450 ~ "ADULT_PIC",
                                  specvar == "WE" &
                                      lenvar < 300 ~ "JUV",
                                  specvar == "WE" &
                                      lenvar >= 300 & lenvar < 375 ~ "ADULT",
                                  specvar == "WE" &
                                      lenvar >= 375 ~ "ADULT_PIC"))

    } else {
        .data %>%
            dplyr::mutate(ls = dplyr::case_when(specvar == "SM" &
                                      lenvar < 100 ~ "SUB-JUV",
                                  specvar == "SM" &
                                      lenvar >= 100 & lenvar < 200 ~ "JUV",
                                  specvar == "SM" &
                                      lenvar >= 200 ~ "ADULT",
                                  specvar == "NP" &
                                      lenvar < 300 ~ "JUV",
                                  specvar == "NP" &
                                      lenvar >= 300 ~ "ADULT",
                                  specvar == "WE" &
                                      lenvar < 300 ~ "JUV",
                                  specvar == "WE" &
                                      lenvar >= 300 ~ "ADULT"))
    }
}


#---------------------------------
# is_piscivore()
#---------------------------------

# Creates logical variable identifying fish of piscivore size (TRUE)
is_piscivore <- function(.data, specvar, lenvar, ...) {

    .data %>%
        dplyr::mutate(piscivore = as.logical(dplyr::case_when(specvar == "SM" &
                                                    lenvar >=325 ~ "TRUE",
                                                specvar == "NP" &
                                                    lenvar >= 450 ~ "TRUE",
                                                specvar == "WE" &
                                                    lenvar >= 375 ~ "TRUE",
                                                specvar == "LG" &
                                                    lenvar >= 325 ~ "TRUE",
                                                specvar == "SB" &
                                                    lenvar >= 350 ~ "TRUE",
                                                specvar == "CC" &
                                                    lenvar >= 400 ~ "TRUE",
                                                specvar == "BU" &
                                                    lenvar >= 450 ~ "TRUE",
                                                TRUE ~ "FALSE")))
}

