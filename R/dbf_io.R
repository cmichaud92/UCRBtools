
#######################################################################################################
#                                             dbf_io()                                                #
#######################################################################################################

# Generalized function intended to import .dbf files from a DataPlus application.
# Function then returns a list of named tibbles (data frames)
# If compiling multiple efforts, need to grepl resulting list for tibble name
# as there may be multiple identically named tibbles in the output list

dbf_io <- function(file_path_in) {

  files <- list.files(path = file_path_in, pattern = '.dbf$', full.names = TRUE)

  dat_name <- list()

  dat_name <- as.list(stringr::str_extract(files, "(?<=\\+).*(?=\\.dbf)")) # Creates list-element names from file names


  data <- purrr::map(files, foreign::read.dbf, as.is = TRUE) # Read all dbf files, strings as characters (as.is)

  names(data) <- dat_name                                    # Set list-element names

  data <- purrr::map(data, tibble::as_tibble)                        # Converts df to tibbles

  data <- purrr::compact(data)                               # Removes all empty list elements

  data
}



