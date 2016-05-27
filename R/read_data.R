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
#' @param section Name of the section to import. Default is 'Calibration'.
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

read_data = function(path, section = "Calibration", convert_time = 1000 * 60, channel = "Ch 3") {

  raw = readr::read_lines(path)

  # mark the start of the section of interest and stop if we can't find it
  startidx = which(str_detect(raw, section)) + 1
  if (length(startidx) == 0) stop("Couldn't find section named ", section)


  markers = which(str_detect(raw,"==========")) # find all the other markers
  markers = markers[markers > startidx]  # get rid of ones above our start
  endidx = length(raw) # set the end index

  # if we have a marker before the end then this is our new end
  if (min(markers) < endidx) endidx = min(markers) - 1
  if (length(endidx) != 0) raw = raw[startidx:endidx] # get the lines in our section

  # identify the columns we want
  columns = raw[str_detect(raw, "Section")]
  channels = raw[str_detect(raw, "Ch")]
  channels = channels %>% str_split(",") %>% "[["(1)
  columns = columns %>% str_split(",") %>% "[["(1)

  uniq_channels = unique(channels[channels != ""])

  if (!channel %in% uniq_channels) {
    stop("Couldn't find channel named ", channel, " Should be one of ", paste(uniq_channels, collapse = ", "))
  }

  col_types = list()

  # here we create the column type vector using readr's character syntax
  for (i in 1:length(columns)) {
    if (columns[i] %in%  c("Section", "Label")) {
      col_types[i] = "c"
    } else if (columns[i] == "Dens - Levels" && channels[i] == channel) {
      col_types[i] = "n"
    } else if (columns[i] == "Time") {
      col_types[i] = "n"
    } else {
      col_types[i] = "_"
    }
  }

  col_types = paste(col_types, collapse = "")
  #col_names = c("Section", "Label", "Time", "Intensity")

  # parse the data
  raw = paste0(raw, collapse = "\n")
  #raw = readr::read_csv(raw, skip = 2, col_names = col_names, col_types = col_types)
  raw = readr::read_csv(raw, skip = 1, col_types = col_types) %>%
    dplyr::rename(Intensity = `Dens - Levels`)


  if (stringr::str_detect(raw$Section[1], "fld")) {
    message("Detected multiple fields")
    raw = raw %>%
      tidyr::extract(Section, c("Row", "Col", "Field" ), "([A-Z]{1}) - ([0-9]+) \\(fld ([12]{1}) - .*\\)") %>%
      tidyr::unite(Well, Row, Col, sep = "") %>% tidyr::unite(Well, Well, Field, sep = "_")
  } else {
    message("No fields detected")
    raw = raw %>%
      tidyr::extract(Section, c("Row", "Col" ), "([A-Z]{1}) - ([0-9]+)") %>%
      tidyr::unite(Well, Row, Col, sep = "")
  }
  if (!is.na(convert_time) && is.numeric(convert_time)) {
    raw = raw %>% mutate(Time = Time / convert_time)
  }
  return(raw)
}
