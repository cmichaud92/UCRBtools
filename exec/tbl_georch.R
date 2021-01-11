
library(tidyverse)
library(UCRBtools)
rch <- read_csv("~/reach-table.csv") %>%
    rename(cd_georch = cd_rch)

rch2 <- tribble(
    ~cd_georch, ~cd_rvr, ~fct_rvr, ~fct_georch, ~nhd_up, ~nhd_dn, ~nm_up, ~nm_dn,
    "CIS", "CO", "Colorado River", "Cisco", 118.0, 95.9, "above Big Hole", "Dewey",
    "DAL", "CO", "Colorado River", "Daily", 95.9, 75.0, "Dewey", "Takeout Beach",
    "MOB", "CO", "Colorado River", "Moab", 75.0, 47.8, "Takeout Beach", "Potash",
    "MEA", "CO", "Colorado River", "Meander Canyon", 47.8, 0, "Potash", "Green River Confluence",
    "TDV", "GR", "Green River", "Tusher Diversion", 129.5, 96.6, "Tusher Diversion", "White Wash",
    "LAB", "GR", "Green River", "Labyrinth Canyon", 96.6, 34.2, "White Wash", "Millard Canyon",
    "TPK", "GR", "Green River", "Tower Park", 34.2, 19.8, "Millard Canyon", "Deadhorse Canyon",
    "STW", "GR", "Green River", "Stillwater Canyon", 19.6, 0, "Deadhorse Canyon", "Colorado River Confluence",
    "DES", "GR", "Green River", "Desolation Canyon", 190.6, 157.9, "Jack Creek", "Wire Fence Canyon",
    "GRA", "GR", "Green River", "Gray Canyon", 157.9, 129.5, "Wire Fence Canyon", "Tusher Diversion",
    "LOD", "GR", "Green River", "Lodor Canyon", 364.8, 346.3, "Gates of Lodor", "Yampa River Confluence",
    "WPC", "GR", "Green River", "Whirlpool Canyon", 346.3, 335.9, "Yampa River Confluence", "Cove Camp",
    "IRP", "GR", "Green River", "Island and Rainbow Parks", 335.9, 328.6, "Cove Camp", "Above Moonshine Rapid",
    "SPM", "GR", "Green River", "Split Mountain Canyon", 328.6, 321.3, "Above Moonshine Rapid", "Split Mountain Boat Ramp"
)

tbl_georch <- rch %>%
    select(-id_rch) %>%
    bind_rows(rch2) %>%
    mutate(fct_rvr = as_factor(fct_rvr),
           fct_rvr = fct_relevel(fct_rvr, "Colorado River", "Dolores River", "Gunnison River",
                                 "Green River", "San Rafael River", "Price River",
                                 "Duchesne River", "White River", "Yampa River", "San Juan River",
                                 "Lake Powell")) %>%
    arrange(fct_rvr, nhd_up) %>%
    mutate(idx = row_number(),
           fct_rch = as_factor(fct_rch),
           fct_rch = fct_reorder(fct_rch, idx),
           .keep = "unused") %>%
    arrange(fct_rvr, desc(nhd_up)) %>%
    mutate(id_georch = row_number()) %>%
    select(starts_with(c("id_", "cd_", "fct_")), everything()) %>%
    select(-idx)

write_csv(tbl_georch, "~/geoReach.csv")
usethis::use_data(tbl_georch)

devtools::install_github("cmichaud92/UCRBtools")


levels(rch3$fct_rvr)
levels(rch3$fct_rch)
levels(tbl_rch$fct_rch)


#    arrange(id_rch) %>%
select(starts_with(c("id_", "cd_", "fct_")), everything()) %>%
    select(-idx)

arrange(cd_rvr, desc(nhd_up))

# correct col names
tbl_georch <- tbl_georch %>%
    rename(fct_georch = fct_rch)
