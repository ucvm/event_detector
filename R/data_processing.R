#' Filter by tracks
#'
#' Filter data by the number of tracks or time points.  Used to get rid of incomplete tracks.
#'
#' @param data
#' @param track_size_cutoff
#'
#' @return
#' @export
#'
filter_by_tracks = function(data, track_size_cutoff){

  track_sizes = data %>% group_by(Well, Label) %>% tally()
  track_size_filtered = track_sizes %>% filter(n > track_size_cutoff)
  data_filtered = inner_join(data, track_size_filtered)

  return(data_filtered)

}


#' Detect peaks
#'
#' The main function to detect the peaks in the data.  Best results with baseline corrected data.
#' Uses the \code{\link[pracma]{findpeaks}} function.
#'
#' @param data dataframe Usually from \code{\link{baseline_correct}}. Must contain columns called
#'   "Intensity" and "Time".
#' @param ... Other parameters passed to findpeaks
#'
#' @return dataframe with information about each peak
#' @export
#'
detect_peaks = function(data, ...) {
  spectra = data %>%
    dplyr::select(Intensity, Time) %>%
    tidyr::spread(Time, Intensity)  %>% "["(1,) %>% as.matrix()

  spectra = spectra[1,]
  peak_idx = pracma::findpeaks(spectra, ...)
  if (class(peak_idx) == "numeric") {
    peak_idx = matrix(peak_idx, ncol = 4)
  }

  peak_df = data.frame(
    Time = as.numeric(names(spectra)[peak_idx[,2]]),
    Intensity = as.numeric(peak_idx[,1]),
    Start = as.numeric(names(spectra)[peak_idx[,3]]),
    End = as.numeric(names(spectra)[peak_idx[,4]])
  )
  peak_df = peak_df %>% mutate(Width = End - Start)
  return(peak_df)
}


#' Baseline correct
#'
#' Baseline correct the raw intensity data.  Uses \code{\link[baseline]{baseline}} with the "irls"
#' method to correct the baseline.
#'
#' @param data dataframe Usually from \code{\link{read_data}}. Must contain columns called
#'   "Intensity" and "Time".
#'
#' @return dataframe with corrected intensities
#' @export
#'
baseline_correct = function(data) {
  spectra = data %>%
    dplyr::select(Intensity, Time) %>%
    tidyr::spread(Time, Intensity)  %>% "["(1,) %>% as.matrix()

  bc = baseline::baseline(spectra, method = "irls")
  corrected = baseline::getCorrected(bc)[1,]
  return(data.frame(Time = as.numeric(names(corrected)), Intensity = corrected))
}
