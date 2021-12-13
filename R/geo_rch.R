#' @title Geomorphic reach designation
#'
#' @description
#'
#' Appends geo-reach codes to data based on "rmi_nhd"
#'
#' @author Christopher Michaud
#' @export

geo_rch <- function(.data, cd_rvr = cd_rvr, rmi = rmi_nhd) {

    .data %>%
        dplyr::mutate(cd_georch = dplyr::case_when({{cd_rvr}} == "GR" &
                                                       {{rmi}} < 19.8 ~ "STW",
                                                   {{cd_rvr}} == "GR" &
                                                        between({{rmi}}, 19.8, 34.2) ~ "TWP",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 34.3, 96.6) ~ "LAB",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 96.7, 129.5) ~ "TDV",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 129.6, 157.9) ~ "GRA",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 158.0, 190.6) ~ "DES",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 190.7, 240.0) ~ "LMGR",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 240.1, 260.0) ~ "OUR",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 260.1, 300.0) ~ "UMGR",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 300.1, 321.3) ~ "JEN",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 321.4, 328.6) ~ "SPM",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 328.7, 335.9) ~ "IRP",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 336, 346.3) ~ "WPC",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi}}, 346.4, 364.8) ~ "LOD",
                                                   {{cd_rvr}} == "GR" &
                                                       {{rmi}} > 364.8 ~ "UGR",
                                                   {{cd_rvr}} == "CO" &
                                                       {{rmi}} < 47.9 ~ "MEA",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi}}, 47.9, 75.0) ~ "MOB",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi}}, 75.1, 95.9) ~ "DAL",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi}}, 96.0, 118.0) ~ "CIS",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi}}, 118.1, 154.6) ~ "WWRH",
                                                   # {{cd_rvr}} == "CO" &
                                                   #     between({{rmi}}, 129.3, 154.6) ~ "RUHT",
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



