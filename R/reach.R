#' @title Reach designation
#'
#' @description
#'
#' Appends reach codes to data based on "rmi"
#'
#' @author Christopher Michaud
#' @export

rch <- function(.data, cd_rvr = cd_rvr, rmi = rmi_nhd) {

    .data %>%
        dplyr::mutate(cd_rch = dplyr::case_when({{cd_rvr}} == "GR" &
                                     {{rmi}} < 129.5 ~ "LGR",
                                 {{cd_rvr}} == "GR" &
                                     between({{rmi}}, 129.5, 207.1) ~ "DESO",
                                 {{cd_rvr}} == "GR" &
                                     between({{rmi}}, 207.1, 321.3) ~ "MGR",
                                 {{cd_rvr}} == "GR" &
                                     between({{rmi}}, 321.3, 412.7) ~ "UGR",
                                 {{cd_rvr}} == "CO" &
                                     between({{rmi}}, 0, 111.8) ~ "LCO",
                                 {{cd_rvr}} == "CO" &
                                     between({{rmi}}, 111.8, 129.3) ~ "WW",
                                 {{cd_rvr}} == "CO" &
                                     between({{rmi}}, 129.3, 154.6) ~ "RUHT",
                                 {{cd_rvr}} == "CO" &
                                     between({{rmi}}, 154.6, 189.8) ~ "GRVA",
                                 {{cd_rvr}} == "CO" &
                                     {{rmi}} > 189.8 ~ "UCO",
                                 {{cd_rvr}} == "CO" &
                                     {{rmi}} < -0 ~ "CAT",
                                 {{cd_rvr}} == "DO" &
                                     between({{rmi}}, 0, 20) ~ "LDO",
                                 {{cd_rvr}} == "YA" &
                                     {{rmi}} < 46.9 ~ "LYA",
                                 {{cd_rvr}} == "YA" &
                                     between({{rmi}}, 46.9, 189.2) ~ "MYA",
                                 {{cd_rvr}} == "YA" &
                                     {{rmi}} > 189.2 ~ "UYA",
                                 {{cd_rvr}} == "WH" &
                                     {{rmi}} < 71.7 ~ "LWH",
                                 {{cd_rvr}} == "WH" &
                                     between({{rmi}}, 71.7, 104.5) ~ "MWH",
                                 {{cd_rvr}} == "WH" &
                                     {{rmi}} > 104.5 ~ "UWH",
                                 {{cd_rvr}} == "DU" &
                                     {{rmi}} < 16.7 ~ "LDU",
                                 {{cd_rvr}} == "DU" &
                                     {{rmi}} >= 16.7 ~ "UDU",
                                 {{cd_rvr}} == "PR" &
                                     {{rmi}} < 26.7 ~ "LPR",
                                 {{cd_rvr}} == "PR" &
                                     between({{rmi}}, 26.7, 91.7) ~ "MPR",
                                 {{cd_rvr}} == "PR" &
                                     {{rmi}} > 91.7 ~ "UPR",
                                 {{cd_rvr}} == "SR" &
                                     {{rmi}} < 45.8 ~ "LSR",
                                 {{cd_rvr}} == "SR" &
                                     {{rmi}} >= 45.8 ~ "USR",
                                 {{cd_rvr}} == "GU" &
                                     {{rmi}} < 30 ~ "LGU",
                                 {{cd_rvr}} == "GU" &
                                     {{rmi}} >= 30 ~ "MGU",
                                 {{cd_rvr}} == "SJ" &
                                     {{rmi}} < 4.5 ~ "LLSJ",
                                 {{cd_rvr}} == "SJ" &
                                     between({{rmi}}, 4.5, 61.2) ~ "LSJ",
                                 {{cd_rvr}} == "SJ" &
                                     between({{rmi}}, 61.2, 108) ~ "MSJ",
                                 {{cd_rvr}} == "SJ" &
                                     between({{rmi}}, 108, 170.5) ~ "USJ",
                                 {{cd_rvr}} == "SJ" &
                                     {{rmi}} >= 170.5 ~ "UUSJ"))
}



