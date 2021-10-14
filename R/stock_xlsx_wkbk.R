
#############################################################
#                    FWS-RAND excel workbook                #
#                       STReaMS submission                  #
#############################################################

# Function to Format an xlsx woorkbook for FWS-RAND's
# STReaMS submission for stocking data

# !!!!! Once this code is complete and debugged move to UCRBtools !!!!!

# Author/maintainer: C. Michaud
# email: christopher_michaud@fws.gov
# Created on 10/14/2021



stocking_xlsx_wkbk <- function(data = data, output_path = "./output/") {

  # Set Default options (only work on cols where data is present)
  options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")            # Sets default date format
  options("openxlsx.dateFormat" = "yyyy-mm-dd")
  options("openxlsx.numFmt" = "0.0")                                    # Sets default numeric to 1 decimal place

  # Set style formats for specific formatting requirements
  n0 <- openxlsx::createStyle(numFmt = "0")                                        # Creates a style object for interger
  n1<- openxlsx::createStyle(numFmt = "0.0")                                       # One decimal place
  n4 <- openxlsx::createStyle(numFmt = "0.0000")                                   # Four decimal places approximates "float"
  t <- openxlsx::createStyle(numFmt = "@")                                         # Text formatting
  dt <- openxlsx::createStyle(numFmt = "DATE")                                     # Date

  # stocking datasheet
  st_n0 <- c(5, 10, 11, 13, 19, 25)
  st_n1 <- c(9, 15:18)
  st_n4 <- c(26, 27)
  st_t <- c(1:5, 7, 8, 12, 14, 20:24, 28:30)
  st_dt <- c(6)
  # stocking event datasheet
  ev_n0 <- c(6, 13, 20)
  ev_n1 <- c(8, 12, 16:19)
  ev_n4 <- c(14, 15)
  ev_t <- c(1, 3:5, 7, 9:11, 21, 22)
  ev_dt <- c(2)


  # Create a blank workbook
  B <- openxlsx::createWorkbook()

  # Add worksheets and data
  openxlsx::addWorksheet(wb = B, sheetName = "stocking datasheet")                # Adds a blank sheet and names it
  openxlsx::writeData(wb = B, sheet = 1, x = data$stock)                            # Writes data to the blank sheet
  # Add column formatting
  openxlsx::addStyle(B, 1, style = n0,                                             # Associates styles to specific cols
                     rows = 2:(nrow(data$stock)+1),
                     cols = st_n0,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 1, style = n1,
                     rows = 2:(nrow(data$stock)+1),
                     cols = st_n1,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 1, style = n4,
                     rows = 2:(nrow(data$stock)+1),
                     cols = st_n4,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 1, style = t,
                     rows = 2:(nrow(data$stock)+1),
                     cols = st_t,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 1, style = dt,
                     rows = 2:(nrow(data$stock)+1),
                     cols = st_dt,
                     gridExpand = TRUE)

  # Add worksheets and data
  openxlsx::addWorksheet(wb = B, sheetName = "stocking event datasheet")                # Adds a blank sheet and names it
  openxlsx::writeData(wb = B, sheet = 2, x = data$event)                            # Writes data to the blank sheet
  # Add column formatting
  openxlsx::addStyle(B, 2, style = n0,                                             # Associates styles to specific cols
                     rows = 2:(nrow(data$event)+1),
                     cols = ev_n0,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 2, style = n1,
                     rows = 2:(nrow(data$event)+1),
                     cols = ev_n1,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 2, style = n4,
                     rows = 2:(nrow(data$event)+1),
                     cols = ev_n4,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 2, style = t,
                     rows = 2:(nrow(data$event)+1),
                     cols = ev_t,
                     gridExpand = TRUE)
  openxlsx::addStyle(B, 2, style = dt,
                     rows = 2:(nrow(data$event)+1),
                     cols = ev_dt,
                     gridExpand = TRUE)

  openxlsx::saveWorkbook(wb = B,
                         file = file.path("./output/", paste0(paste("STReaMS_stocking_fmt",
                                                                    unique(data$stock$`STOCK YEAR`),
                                                                    unique(data$stock$AGENCY),
                                                                    sep = "_"),
                                                              ".xlsx")),
                         overwrite = TRUE)
}


