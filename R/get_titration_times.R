# get_titration_times.R
# Port of getTitrationTimes.py behavior into the oroboros package.

is_cleaning_protocol <- function(protocol_name) {
  is.character(protocol_name) && grepl("cleaning", protocol_name, ignore.case = TRUE)
}

is_background_protocol <- function(protocol_name) {
  is.character(protocol_name) && grepl("background", protocol_name, ignore.case = TRUE)
}

is_calibration_protocol <- function(protocol_name) {
  is.character(protocol_name) && grepl("calibration", protocol_name, ignore.case = TRUE)
}

decode_float64 <- function(value) {
  if (is.null(value)) return(numeric(0))
  if (is.numeric(value)) return(as.numeric(value))
  if (is.raw(value)) {
    if (length(value) %% 8 != 0) {
      warning("Raw value length is not multiple of 8; cannot decode as float64")
      return(numeric(0))
    }
    return(readBin(value, what = "double", size = 8, n = length(value) / 8, endian = "little"))
  }
  if (is.vector(value) && is.raw(value[[1]])) {
    bytes <- do.call(c, value)
    if (length(bytes) %% 8 != 0) return(numeric(0))
    return(readBin(bytes, what = "double", size = 8, n = length(bytes) / 8, endian = "little"))
  }
  if (is.list(value) && all(vapply(value, is.numeric, logical(1)))) {
    return(as.numeric(value))
  }
  if (is.list(value) && all(vapply(value, is.raw, logical(1)))) {
    bytes <- do.call(c, value)
    if (length(bytes) %% 8 != 0) return(numeric(0))
    return(readBin(bytes, what = "double", size = 8, n = length(bytes) / 8, endian = "little"))
  }
  value
}

#' Build protocol definition map for chamber events
#'
#' @param chamber_data List parsed from a DLD8 file chamber block.
#' @return Named list with explicit event definitions and a 3U fallback.
build_protocol_definition_map <- function(chamber_data) {
  defs <- list(`__3U__` = NA_character_)
  steps <- safe_get(chamber_data, c("protocol", "protocolSteps"), default = list())
  if (!is.list(steps) || length(steps) == 0) return(defs)

  for (step in steps) {
    if (!is.list(step)) next
    event_name <- safe_get(step, c("event", "eventName", "value"), default = "")
    definition <- safe_get(step, c("event", "definition", "value"), default = "")
    event_name <- as.character(event_name)
    definition <- as.character(definition)

    if (nzchar(event_name) && nzchar(definition) && is.null(defs[[event_name]])) {
      defs[[event_name]] <- definition
    }

    if ((is.na(defs[["__3U__"]]) || !nzchar(defs[["__3U__"]])) &&
        nzchar(event_name) && startsWith(event_name, "3U") && nzchar(definition)) {
      defs[["__3U__"]] <- definition
    }
  }

  defs
}

#' Extract titration events and time series for one chamber
#'
#' @param chamber_id Character or factor; "chamber_a" or "chamber_b".
#' @param dld8_json Parsed DLD8 content as list.
#' @param only_protocol_events Logical; keep only protocol events when TRUE.
#' @param remove_bg_cal_protocols Logical; drop background/calibration protocol runs when TRUE.
#' @return List with operator, protocolSubtype, a0, b0, events, timeSeries, o2raw, o2Series, o2Flux, o2Slope.
get_titration_times_from_chamber <- function(chamber_id,
                                             dld8_json,
                                             only_protocol_events = FALSE,
                                             remove_bg_cal_protocols = TRUE) {
  run_data <- get_latest_run_data(dld8_json)
  if (!is.list(run_data) || length(run_data) == 0) return(NULL)

  chamber_key <- get_chamber_key_name(chamber_id)
  chamber_data <- safe_get(run_data, c(chamber_key), default = list())
  if (!is.list(chamber_data)) chamber_data <- list()

  protocol_subtype <- safe_get(chamber_data, c("protocol", "protocolSubtype", "value"), default = "")
  protocol_subtype <- as.character(protocol_subtype)

  if (is_cleaning_protocol(protocol_subtype)) {
    return(NULL)
  }

  if (isTRUE(remove_bg_cal_protocols) &&
      (is_background_protocol(protocol_subtype) || is_calibration_protocol(protocol_subtype))) {
    return(NULL)
  }

  operator <- safe_get(run_data, c("userInfo", "userName", "value"), default = NA_character_)
  a0 <- safe_get(chamber_data, c("oxygenBackgroundCorrection", "a0", "value"), default = NA_real_)
  b0 <- safe_get(chamber_data, c("oxygenBackgroundCorrection", "b0", "value"), default = NA_real_)

  measurement_data <- safe_get(run_data, c("measurement", chamber_key), default = list())
  time_series <- decode_float64(safe_get(measurement_data, c("Time"), default = list()))
  o2_raw <- decode_float64(safe_get(measurement_data, c("O2"), default = list()))

  analyses_data <- safe_get(chamber_data, c("analyses", "analyses_0", "data"), default = list())
  o2_series <- decode_float64(safe_get(analyses_data, c("O2"), default = list()))
  o2_flux <- decode_float64(safe_get(analyses_data, c("O2Flux"), default = list()))
  o2_slope <- decode_float64(safe_get(analyses_data, c("O2Slope"), default = list()))

  photobiology <- decode_float64(safe_get(analyses_data, c("PhotoBiology"), default = NULL))

  events_data <- safe_get(chamber_data, c("events", "events"), default = list())
  protocol_definition_map <- build_protocol_definition_map(chamber_data)

  events <- list()
  if (is.list(events_data) && length(events_data) > 0) {
    for (event_item in events_data) {
      if (!is.list(event_item)) next
      protocolStepIdx <- safe_get(event_item, c("protocolStepIdx", "value"), default = -1)
      if (isTRUE(only_protocol_events) && identical(protocolStepIdx, -1)) next

      event_name <- safe_get(event_item, c("eventName", "value"), default = NA_character_)
      explicit_definition <- safe_get(event_item, c("definition", "value"), default = "")
      definition <- as.character(explicit_definition)
      if (!nzchar(definition)) {
        if (!is.null(event_name) && !is.na(event_name)) {
          fallback <- protocol_definition_map[[as.character(event_name)]]
          if (is.null(fallback) || !nzchar(as.character(fallback))) {
            if (startsWith(as.character(event_name), "3U")) {
              fallback <- protocol_definition_map[["__3U__"]]
            }
          }
          if (!is.null(fallback) && nzchar(as.character(fallback))) {
            definition <- as.character(fallback)
          }
        }
      }

      events <- c(events, list(list(
        eventName = as.character(event_name),
        definition = if (nzchar(definition)) definition else NA_character_,
        time = safe_get(event_item, c("time", "value"), default = NA_real_),
        isMultiEvent = safe_get(event_item, c("isMultiEvent", "value"), default = NA),
        protocolStepIdx = protocolStepIdx
      )))
    }
  }

  result <- list(
    operator = operator,
    protocolSubtype = protocol_subtype,
    a0 = a0,
    b0 = b0,
    timeSeries = time_series,
    o2raw = o2_raw,
    o2Series = o2_series,
    o2Flux = o2_flux,
    o2Slope = o2_slope,
    events = events
  )
  if (!is.null(photobiology)) {
    result$PhotoBiology <- photobiology
  }
  result
}

extract_events_and_raw_data <- function(project,
                                            include_non_experimental = FALSE,
                                            only_protocol_events = FALSE,
                                            remove_bg_cal_protocols = TRUE) {
  if (!is.list(project) || !inherits(project, "dld8_project")) {
    stop("project must be a dld8_project object")
  }

  rel_files <- project$dld8_files
  if (isTRUE(include_non_experimental)) {
    rel_files <- c(rel_files, project$excluded_dld8_files)
  }

  if (length(rel_files) == 0) {
    return(list(events = data.frame(), time_series = data.frame()))
  }

  events_rows <- list()
  time_series_rows <- list()

  for (rel in rel_files) {
    dld8_json <- get_dld8(project, rel)
    if (is.null(dld8_json)) next

    for (chamber in get_project_chambers(project, rel, include_non_experimental = include_non_experimental)) {
      chamber_data <- get_titration_times_from_chamber(chamber, dld8_json,
                                                       only_protocol_events = only_protocol_events,
                                                       remove_bg_cal_protocols = remove_bg_cal_protocols)
      if (is.null(chamber_data)) next

      md <- get_sample_metadata_from_json(chamber, dld8_json)
      sample_amount <- safe_get(md, c("sampleQuantity", "amount"), default = NA_real_)
      chamber_volume <- md$chamberVolume %||% NA_real_
      sample_concentration <- if (!is.na(chamber_volume) && chamber_volume > 0 && !is.na(sample_amount)) {
        sample_amount / chamber_volume
      } else {
        NA_real_
      }

      base_meta <- list(
        rel_path = rel,
        filename = basename(rel),
        chamber = chamber,
        chamberVolume = chamber_volume,
        sampleAmount = sample_amount,
        sampleConcentration = sample_concentration,
        baselineMarker = NA_character_,
        referenceMarker = NA_character_,
        baselineStep = NA_integer_,
        referenceStep = NA_integer_,
        sampleType = md$metaData$sampleType %||% NA_character_,
        cohort = md$metaData$cohort %||% NA_character_,
        sampleCode = md$metaData$sampleCode %||% NA_character_,
        sampleNumber = md$metaData$sampleNumber %||% NA_character_,
        subsampleNumber = md$metaData$subsampleNumber %||% NA_character_,
        protocol = md$protocol %||% NA_character_,
        operator = chamber_data$operator,
        protocolSubtype = chamber_data$protocolSubtype,
        a0 = chamber_data$a0,
        b0 = chamber_data$b0
      )

      events <- chamber_data$events
      if (is.null(events) || length(events) == 0) {
        events_rows <- c(events_rows, list(as.data.frame(c(base_meta, list(
          eventName = NA_character_,
          definition = NA_character_,
          time = NA_real_,
          isMultiEvent = NA,
          protocolStepIdx = NA_real_
        )), stringsAsFactors = FALSE)))
      } else {
        for (ev in events) {
          events_rows <- c(events_rows, list(as.data.frame(c(base_meta, list(
            eventName = ev$eventName %||% NA_character_,
            definition = ev$definition %||% NA_character_,
            time = ev$time %||% NA_real_,
            isMultiEvent = ev$isMultiEvent %||% NA,
            protocolStepIdx = ev$protocolStepIdx %||% NA_real_
          )), stringsAsFactors = FALSE)))
        }
      }

      if (length(chamber_data$timeSeries) > 0) {
        ts_row <- c(base_meta, list(
          timeSeries = I(list(chamber_data$timeSeries)),
          o2raw = I(list(chamber_data$o2raw)),
          o2Series = I(list(chamber_data$o2Series)),
          o2Flux = I(list(chamber_data$o2Flux)),
          o2Slope = I(list(chamber_data$o2Slope))
        ))
        if (!is.null(chamber_data$PhotoBiology)) {
          ts_row <- c(ts_row, list(PhotoBiology = I(list(chamber_data$PhotoBiology))))
        }
        time_series_rows <- c(time_series_rows, list(as.data.frame(ts_row, stringsAsFactors = FALSE)))
      }
    }
  }

  events_df <- if (length(events_rows) == 0) data.frame() else do.call(rbind, events_rows)
  time_series_df <- if (length(time_series_rows) == 0) data.frame() else do.call(rbind, time_series_rows)

  list(events = events_df, time_series = time_series_df)
}

#' Extract event data from project
#'
#' @inheritParams extract_events_and_raw_data
#' @return Data frame of events.
#' @export
extract_events <- function(project,
                           include_non_experimental = FALSE,
                           only_protocol_events = FALSE,
                           remove_bg_cal_protocols = TRUE) {
  extract_events_and_raw_data(project,
                              include_non_experimental = include_non_experimental,
                              only_protocol_events = only_protocol_events,
                              remove_bg_cal_protocols = remove_bg_cal_protocols)$events
}

#' Extract raw time series data from project
#'
#' @inheritParams extract_events_and_raw_data
#' @return Data frame of raw time series rows.
#' @export
extract_raw_data <- function(project,
                             include_non_experimental = FALSE,
                             only_protocol_events = FALSE,
                             remove_bg_cal_protocols = TRUE) {
  extract_events_and_raw_data(project,
                              include_non_experimental = include_non_experimental,
                              only_protocol_events = only_protocol_events,
                              remove_bg_cal_protocols = remove_bg_cal_protocols)$time_series
}

#' DEPRECATED for compatibility
#' @export
extract_titration_times <- function(...) {
  .Deprecated("extract_events")
  extract_events(...)
}

#' Write titration times tables to CSV files
#'
#' @param titration_data List from `extract_titration_times()`.
#' @param intervals_path Path for intervals CSV.
#' @param rawo2_path Path for raw O2 time series CSV.
#' @export
write_titration_times <- function(titration_data,
                                  intervals_path,
                                  rawo2_path) {
  if (is.null(titration_data$events) || nrow(titration_data$events) == 0) {
    warning("No intervals data to write")
  } else {
    utils::write.csv(titration_data$events, intervals_path, row.names = FALSE)
  }

  if (is.null(titration_data$time_series) || nrow(titration_data$time_series) == 0) {
    warning("No time_series data to write")
  } else {
    # Encode list columns as JSON-like strings for compatibility with the Python behavior.
    ts_df <- titration_data$time_series
    ts_df$timeSeries <- vapply(ts_df$timeSeries, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), "")
    ts_df$o2raw <- vapply(ts_df$o2raw, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), "")
    ts_df$o2Series <- vapply(ts_df$o2Series, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), "")
    ts_df$o2Flux <- vapply(ts_df$o2Flux, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), "")
    ts_df$o2Slope <- vapply(ts_df$o2Slope, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), "")
    if ("PhotoBiology" %in% names(ts_df)) {
      ts_df$PhotoBiology <- vapply(ts_df$PhotoBiology, function(x) jsonlite::toJSON(x, auto_unbox = TRUE), "")
    }
    utils::write.csv(ts_df, rawo2_path, row.names = FALSE)
  }
}
