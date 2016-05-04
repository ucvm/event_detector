

read_data = function(path) {
  raw = read_lines(path)
  startidx = which(str_detect(raw, "Calibration")) + 1
  markers = which(str_detect(raw,"=========="))
  markers = markers[markers > startidx]
  endidx = length(raw)
  if (min(markers) < endidx) endidx = min(markers) - 1
  if (length(endidx) != 0) raw = raw[startidx:endidx]
  raw = paste0(raw, collapse = "\n")
  col_types = "c____c_____n_n_"
  col_names = c("Section", "Label", "Time", "Intensity")
  raw = read_csv(raw, skip = 3, col_names = col_names, col_types = col_types)
  if (str_detect(raw$Section[1], "fld")) {
    message("Detected multiple fields")
    raw = raw %>%
      tidyr::extract(Section, c("Row", "Col", "Field" ), "([A-Z]{1}) - ([0-9]+) \\(fld ([12]{1}) - .*\\)") %>%
      unite(Well, Row, Col, sep = "") %>% unite(Well, Well, Field, sep = "_")
  } else {
    message("No fields detected")
    raw = raw %>%
      tidyr::extract(Section, c("Row", "Col" ), "([A-Z]{1}) - ([0-9]+)") %>%
      unite(Well, Row, Col, sep = "")
  }
  raw %>%
    mutate(Time = Time / (1000*60))
}
