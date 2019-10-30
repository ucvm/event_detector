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
#' @param section Name of the section to import. Default is 'cy5'.
#' @param convert_time Conversion factor for the time column.  Default is \eqn{1000 * 60} (converts
#'   milliseconds to minutes). NA means no conversion will be performed.
#' @param channel The channel to use for intensity measurements. Default is 'Ch 3'
#'
#' @return dataframe
#'
#' @import dplyr
#' @import stringr
#'
#' @export

read_data = function(path, section = "cy5", convert_time = 1000 * 60, channel = "Ch 3") {

  # load it all up into a single column dataframe
  df = vroom::vroom(path, col_names = FALSE)

  # find our sections and mark the starts and ends
  markers = df$X1[str_detect(df$X1,"==========")]
  sections = str_match(markers, "=+>(.*)<==+")[,2]
  starts = match(markers, df$X1)
  ends = c(starts[2:3] - 1, nrow(df))

  # split into sections and read each section in properly (comma-delimited)
  section_dfs = purrr::map2(starts, ends, ~df[.x:.y,]) %>%
    purrr::set_names(sections)
  sp = purrr::map(section_dfs, read_section)

  # keep the section, channel, and columns we need and clean up the horrid names
  cols_needed = c("section", "target", str_c(c("label","dens_levels", "time"),
                                             janitor::make_clean_names(channel), sep = "_"))
  final = sp[[section]]
  final = final[, colnames(final) %in% cols_needed]

  # one more name clean up
  colnames(final) = final_names(colnames(final))

  # clean up the row-column column and extract field if present
  final = clean_row_columns(final)

  # finall convert the time column if requested
  if (!is.na(convert_time) && is.numeric(convert_time)) {
    final = final %>% mutate(Time = Time / convert_time)
  }


  return(final)
}


read_section = function(ds) {
  # this function reads in a single section and cleans up the mess that is the column names
  data = str_c(ds$X1[-c(1:3)], collapse = "\n")
  cols = str_split(ds[3,], ",")[[1]]
  chans = str_split(ds[2,], ",")[[1]]

  # clean up names and add channel info
  cols = str_c(cols, chans, sep = "_")
  cols = janitor::make_clean_names(stringi::stri_trans_general(cols, "latin-ascii"))

  return(
    vroom::vroom(data, delim = ",", col_names = cols)
  )
}


clean_row_columns = function(df) {

  if (stringr::str_detect(df$Section[1], "fld")) {
    message("Detected multiple fields")
    df = df %>%
      tidyr::extract(Section, c("Row", "Col", "Field" ), "([A-Z]{1}) - ([0-9]+) \\(fld ([123]{1}) - .*\\)") %>%
      tidyr::unite(Well, Row, Col, sep = "") %>% tidyr::unite(Well, Well, Field, sep = "_")
  } else {
    message("No fields detected")
    df = df %>%
      tidyr::extract(Section, c("Row", "Col" ), "([A-Z]{1}) - ([0-9]+)") %>%
      tidyr::unite(Well, Row, Col, sep = "")
  }

  return(df)
}


final_names = function(x) {

  x = str_remove(x, "ch_[23]")
  x[str_detect(x, "dens_levels")] = "Intensity"
  x = str_to_sentence(str_remove_all(x, "_"))
  return(x)

}


#' Scan file
#'
#' Scan raw file for channels and sections
#'
#' @param path file path of the csv file
#'
#' @return list Returns a named list with two entries, channels and sections
#' @export
#'
#' @examples
scan_file = function(path) {

  raw = readr::read_lines(path)

  # get sections
  sections = raw[str_detect(raw, "=========")]
  sections = str_match(sections, "=+>(.*)<==+")[,2]

  channels = paste(raw[str_detect(raw, "Ch")], collapse = "")
  channels = str_split(channels, ",")[[1]]
  channels = unique(channels[channels != ""])

  return(list(channels = channels, sections = sections))

}
