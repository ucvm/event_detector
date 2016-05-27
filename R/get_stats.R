#' Get well stats
#'
#' Get stats about peaks present in each well
#'
#' @param data intensity data
#' @param peaks peak data from \code{\link{detect_peaks}}
#'
#' @return a data.frame with the data summarized by well.  Contains columns for the number of
#'   tracks, number of events and stats on the peak width and number of peaks.
#' @export
#'
get_stats = function(data, peaks) {
  num_tracks = data %>%
    ungroup() %>% group_by(Well)  %>%
    distinct(Label) %>% count(Well) %>%
    rename(`Number of tracks` = n)

  num_events = peaks %>%
    ungroup() %>% group_by(Well) %>%
    distinct(Label) %>% count(Well) %>%
    rename(`Number of events` = n)

  peak_width = peaks %>% ungroup() %>% group_by(Well) %>%
    summarise(`Mean peak width` = mean(Width),
              `Max peak width` = max(Width),
              `Min peak width` = min(Width),
              `Sd peak width` = sd(Width),
              `Median peak width` = median(Width))

  peaks %>% ungroup() %>% group_by(Well) %>% count(Well, Label) %>%
    summarise(`Mean Peaks` = mean(n),
              `Max Peaks` = max(n),
              `Min Peaks` = min(n),
              `Std deviation` = sd(n),
              `Median Peaks` = median(n)) %>%
    left_join(num_events) %>%
    left_join(num_tracks) %>%
    left_join(peak_width) %>%
    arrange(`Median Peaks`)
}
