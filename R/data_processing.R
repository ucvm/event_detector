filter_by_tracks = function(data, track_size_cutoff){

  track_sizes = data %>% group_by(Well, Label) %>% tally()
  track_size_filtered = track_sizes %>% filter(n > track_size_cutoff)
  data_filtered = inner_join(data, track_size_filtered)

  return(data_filtered)

}


detect_peaks = function(data, ...) {
  spectra = data %>%
    dplyr::select(Intensity, Time) %>%
    spread(Time, Intensity)  %>% "["(1,) %>% as.matrix()

  spectra = spectra[1,]
  peak_idx = findpeaks(spectra, ...)
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


baseline_correct = function(data) {
  spectra = data %>%
    dplyr::select(Intensity, Time) %>%
    spread(Time, Intensity)  %>% "["(1,) %>% as.matrix()

  bc = baseline(spectra, method = "irls")
  corrected = getCorrected(bc)[1,]
  return(data.frame(Time = as.numeric(names(corrected)), Intensity = corrected))
}
