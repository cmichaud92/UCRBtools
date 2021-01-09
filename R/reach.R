#' @title Reach designation
#'
#' @description
#'
#' Appends reach codes to data based on "rmi"
#'
#' @author Christopher Michaud
#' @export

reach <- function(.data, RiverCode, rmi) {

    .data %>%
        dplyr::mutate(cd_rch = dplyr::case_when(RiverCode == "GR" &
                                     rmi < 129.5 ~ "LGR",
                                 RiverCode == "GR" &
                                     between(rmi, 129.5, 207.1) ~ "DESO",
                                 RiverCode == "GR" &
                                     between(rmi, 207.1, 321.3) ~ "MGR",
                                 RiverCode == "GR" &
                                     between(rmi, 321.3, 412.7) ~ "UGR",
                                 RiverCode == "CO" &
                                     between(rmi, 0, 111.8) ~ "LCO",
                                 RiverCode == "CO" &
                                     between(rmi, 111.8, 129.3) ~ "WW",
                                 RiverCode == "CO" &
                                     between(rmi, 129.3, 154.6) ~ "RUHT",
                                 RiverCode == "CO" &
                                     between(rmi, 154.6, 189.8) ~ "GRVA",
                                 RiverCode == "CO" &
                                     rmi > 189.8 ~ "UCO",
                                 RiverCode == "CO" &
                                     rmi < -0 ~ "CAT",
                                 RiverCode == "DO" &
                                     between(rmi, 0, 20) ~ "LDO",
                                 RiverCode == "YA" &
                                     rmi < 46.9 ~ "LYA",
                                 RiverCode == "YA" &
                                     between(rmi, 46.9, 189.2) ~ "MYA",
                                 RiverCode == "YA" &
                                     rmi > 189.2 ~ "UYA",
                                 RiverCode == "WH" &
                                     rmi < 71.7 ~ "LWH",
                                 RiverCode == "WH" &
                                     between(rmi, 71.7, 104.5) ~ "MWH",
                                 RiverCode == "WH" &
                                     rmi > 104.5 ~ "UWH",
                                 RiverCode == "DU" &
                                     rmi < 16.7 ~ "LDU",
                                 RiverCode == "DU" &
                                     rmi >= 16.7 ~ "UDU",
                                 RiverCode == "PR" &
                                     rmi < 26.7 ~ "LPR",
                                 RiverCode == "PR" &
                                     between(rmi, 26.7, 91.7) ~ "MPR",
                                 RiverCode == "PR" &
                                     rmi > 91.7 ~ "UPR",
                                 RiverCode == "SR" &
                                     rmi < 45.8 ~ "LSR",
                                 RiverCode == "SR" &
                                     rmi >= 45.8 ~ "USR",
                                 RiverCode == "GU" &
                                     rmi < 30 ~ "LGU",
                                 RiverCode == "GU" &
                                     rmi >= 30 ~ "MGU",
                                 RiverCode == "SJ" &
                                     rmi < 4.5 ~ "LLSJ",
                                 RiverCode == "SJ" &
                                     between(rmi, 4.5, 61.2) ~ "LSJ",
                                 RiverCode == "SJ" &
                                     between(rmi, 61.2, 108) ~ "MSJ",
                                 RiverCode == "SJ" &
                                     between(rmi, 108, 170.5) ~ "USJ",
                                 RiverCode == "SJ" &
                                     rmi >= 170.5 ~ "UUSJ"))
}



