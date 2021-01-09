#' @title Geomorphic reach designation
#'
#' @description
#'
#' Appends geo-reach codes to data based on "rmi_nhd"
#'
#' @author Christopher Michaud
#' @export

geo_rch <- function(.data, cd_rvr = cd_rvr, rmi_nhd = rmi_nhd) {

    .data %>%
        dplyr::mutate(cd_georch = dplyr::case_when({{cd_rvr}} == "GR" &
                                                       {{rmi_nhd}} < 19.8 ~ "STW",
                                                   {{cd_rvr}} == "GR" &
                                                        between({{rmi_nhd}}, 19.8, 34.2) ~ "TWP",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 34.3, 96.6) ~ "LAB",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 96.7, 129.5) ~ "TDV",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 129.6, 157.9) ~ "GRA",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 158.0, 190.6) ~ "DES",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 190.7, 321.3) ~ "MGR",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 321.4, 328.6) ~ "SPM",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 328.7, 335.9) ~ "IRP",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 336, 346.3) ~ "WPC",
                                                   {{cd_rvr}} == "GR" &
                                                       between({{rmi_nhd}}, 346.4, 364.8) ~ "LOD",
                                                   {{cd_rvr}} == "GR" &
                                                       {{rmi_nhd}} > 364.8 ~ "UGR",
                                                   {{cd_rvr}} == "CO" &
                                                       {{rmi_nhd}} < 47.8 ~ "MEA",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi_nhd}}, 47.9, 75.0) ~ "MOB",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi_nhd}}, 75.1, 95.9) ~ "DAL",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi_nhd}}, 96.0, 118.0) ~ "CIS",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi_nhd}}, 118.1, 129.3) ~ "MOB",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi_nhd}}, 129.3, 154.6) ~ "RUHT",
                                                   {{cd_rvr}} == "CO" &
                                                       between({{rmi_nhd}}, 154.6, 189.8) ~ "GRVA",
                                                   {{cd_rvr}} == "CO" &
                                                       {{rmi_nhd}} > 189.8 ~ "UCO",
                                                   {{cd_rvr}} == "CO" &
                                                       {{rmi_nhd}} < -0 ~ "CAT",
                                                   {{cd_rvr}} == "DO" &
                                                       between({{rmi_nhd}}, 0, 20) ~ "LDO",
                                                   {{cd_rvr}} == "YA" &
                                                       {{rmi_nhd}} < 46.9 ~ "LYA",
                                                   {{cd_rvr}} == "YA" &
                                                       between({{rmi_nhd}}, 46.9, 189.2) ~ "MYA",
                                                   {{cd_rvr}} == "YA" &
                                                       {{rmi_nhd}} > 189.2 ~ "UYA",
                                                   {{cd_rvr}} == "WH" &
                                                       {{rmi_nhd}} < 71.7 ~ "LWH",
                                                   {{cd_rvr}} == "WH" &
                                                       between({{rmi_nhd}}, 71.7, 104.5) ~ "MWH",
                                                   {{cd_rvr}} == "WH" &
                                                       {{rmi_nhd}} > 104.5 ~ "UWH",
                                                   {{cd_rvr}} == "DU" &
                                                       {{rmi_nhd}} < 16.7 ~ "LDU",
                                                   {{cd_rvr}} == "DU" &
                                                       {{rmi_nhd}} >= 16.7 ~ "UDU",
                                                   {{cd_rvr}} == "PR" &
                                                       {{rmi_nhd}} < 26.7 ~ "LPR",
                                                   {{cd_rvr}} == "PR" &
                                                       between({{rmi_nhd}}, 26.7, 91.7) ~ "MPR",
                                                   {{cd_rvr}} == "PR" &
                                                       {{rmi_nhd}} > 91.7 ~ "UPR",
                                                   {{cd_rvr}} == "SR" &
                                                       {{rmi_nhd}} < 45.8 ~ "LSR",
                                                   {{cd_rvr}} == "SR" &
                                                       {{rmi_nhd}} >= 45.8 ~ "USR",
                                                   {{cd_rvr}} == "GU" &
                                                       {{rmi_nhd}} < 30 ~ "LGU",
                                                   {{cd_rvr}} == "GU" &
                                                       {{rmi_nhd}} >= 30 ~ "MGU",
                                                   {{cd_rvr}} == "SJ" &
                                                       {{rmi_nhd}} < 4.5 ~ "LLSJ",
                                                   {{cd_rvr}} == "SJ" &
                                                       between({{rmi_nhd}}, 4.5, 61.2) ~ "LSJ",
                                                   {{cd_rvr}} == "SJ" &
                                                       between({{rmi_nhd}}, 61.2, 108) ~ "MSJ",
                                                   {{cd_rvr}} == "SJ" &
                                                       between({{rmi_nhd}}, 108, 170.5) ~ "USJ",
                                                   {{cd_rvr}} == "SJ" &
                                                       {{rmi_nhd}} >= 170.5 ~ "UUSJ"))
}



