#' Read the raw data into memory
#'
#' This function takes the raw csv files exported by the imaging software and loads it into memory.
#' It is important that this is the csv file directly exported by the program and not modified in
#' any way.  It detectes the correct section of the csv file to import by looking for the
#' '========>Calibration<========' flag.  It will import from this flag to either the end of the
#' file or the next flag, which ever comes first.  It also cleans up the raw data, removing
#' extraneous columns and extracting the well name and multiple fields of view if present.
#'
#' @param path File path of the csv file
#' @param flag Name of the section to import. Default is 'Calibration'.
#' @param convert_time Conversion factor for the time column.  Default is \eqn{1000 * 60} (converts
#'   milliseconds to minutes). NULL means no conversion will be performed.
#'
#' @return dataframe
#'
#' @import dplyr
#'
#' @export

read_data = function(path, flag = "Calibration", convert_time = 1000 * 60) {
  raw = readr::read_lines(path)
  startidx = which(stringr::str_detect(raw, flag)) + 1
  markers = which(stringr::str_detect(raw,"=========="))
  markers = markers[markers > startidx]
  endidx = length(raw)
  if (min(markers) < endidx) endidx = min(markers) - 1
  if (length(endidx) != 0) raw = raw[startidx:endidx]
  raw = paste0(raw, collapse = "\n")
  col_types = "c____c_____n_n_"
  col_names = c("Section", "Label", "Time", "Intensity")
  raw = readr::read_csv(raw, skip = 3, col_names = col_names, col_types = col_types)
  if (stringr::str_detect(raw$Section[1], "fld")) {
    message("Detected multiple fields")
    raw = raw %>%
      tidyr::extract(Section, c("Row", "Col", "Field" ), "([A-Z]{1}) - ([0-9]+) \\(fld ([12]{1}) - .*\\)") %>%
      tidyr::unite(Well, Row, Col, sep = "") %>% unite(Well, Well, Field, sep = "_")
  } else {
    message("No fields detected")
    raw = raw %>%
      tidyr::extract(Section, c("Row", "Col" ), "([A-Z]{1}) - ([0-9]+)") %>%
      tidyr::unite(Well, Row, Col, sep = "")
  }
  if (!is.na(convert_time)) {
    raw = raw %>% mutate(Time = Time / convert_time)
  }
  return(raw)
}
