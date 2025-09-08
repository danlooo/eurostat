#' @title Get Asynchronous Eurostat SDMX Data by ID
#'
#' @description
#' Polls Eurostat's async API for the status of a submitted request and downloads the data when available.
#'
#' @inheritParams get_eurostat_sdmx
#' @param id Character. Async request ID from initial SDMX call.
#' @param wait Integer. Seconds between status checks.
#' @param max_wait Integer. Max time to wait in seconds.
#' @importFrom data.table %chin%
#'
#' @return A data.frame containing the SDMX-CSV data.
#'
#' @export
get_eurostat_async <- function(
    id,
    time_format = "date",
    type = "code",
    lang = "en",
    use.data.table,
    agency = "Eurostat",
    keepFlags = FALSE,
    legacy.data.output = FALSE,
    wait = 1,
    max_wait = 60,
    compressed = TRUE,
    verbose = TRUE
) {
  stopifnot(nzchar(id))
  lang <- check_lang(lang)
  agency <- tolower(agency)
  api_base_uri <- build_api_base_uri(agency)
  agencyID <- build_agencyID(agency)

  stopifnot(!is.null(api_base_uri))

  status_url <- paste0(api_base_uri, "/1.0/async/status/", id)
  data_url <- paste0(api_base_uri, "/1.0/async/data/", id,
                     "?format=SDMX-CSV",
                     if (compressed) "&compressed=true" else "&compressed=false")

  wait_times <- seq(wait, max_wait, by = wait)

  status_check <- function(t) {
    Sys.sleep(wait)
    res <- httr::GET(status_url)
    content <- httr::content(res, as = "parsed", encoding = "UTF-8")
    xml2::xml_text(xml2::xml_find_first(content, ".//status"))
  }

  statuses <- vapply(wait_times, status_check, FUN.VALUE = character(1))

  status_flags <- stats::setNames(seq_along(statuses), statuses)
  available_pos <- status_flags["AVAILABLE"]
  fail_pos <- which(statuses %in% c("EXPIRED", "ERROR", "UNKNOWN_REQUEST"))[1]

  outcome_index <- which(c(length(available_pos) > 0, length(fail_pos) > 0, TRUE))[1]
  outcome <- c("success", "fail", "timeout")[outcome_index]

  if (verbose) message(sprintf("Final status after %d seconds: %s", wait_times[length(statuses)], outcome))

  handlers <- list(
    "success" = function() {
      tfile <- tempfile()
      on.exit(unlink(tfile), add = TRUE)

      # Enhanced download with retries and HTTP checks
      download_attempt <- function(url, destfile, retries = 3, delay = 5) {
        for (i in seq_len(retries)) {
          res <- httr::GET(url)
          if (httr::status_code(res) == 200) {
            writeBin(httr::content(res, "raw"), destfile)
            return(TRUE)
          }
          if (i < retries) {
            if (verbose) message("Download failed (HTTP ", httr::status_code(res), "). Retrying...")
            Sys.sleep(delay)
          }
        }
        stop("Download failed after ", retries, " attempts. Last HTTP status: ", httr::status_code(res))
      }

      tryCatch(
        download_attempt(data_url, tfile),
        error = function(e) stop("Data download failed: ", e$message)
      )

      # Handle compressed/uncompressed content
      if (compressed) {
        is_zip <- identical(readBin(tfile, what = "raw", n = 2), charToRaw("PK"))
        if (is_zip) {
          zip_list <- unzip(tfile, list = TRUE)
          if (nrow(zip_list) == 0) stop("Empty ZIP archive received")
          csv_file <- unzip(tfile, exdir = tempdir())[1]
        } else {
          if (verbose) message("Expected ZIP archive, but received CSV instead — continuing anyway.")
          csv_file <- tfile
        }
      } else {
        csv_file <- tfile
      }
      on.exit(unlink(csv_file), add = TRUE)

      # Read data with data.table
      dat <- data.table::fread(csv_file, colClasses = "character")
      data.table::setnames(dat, toupper(names(dat)))

      # Cleanup flags
      if (!keepFlags && "OBS_FLAG" %chin% names(dat)) {
        dat[, OBS_FLAG := NULL]
      }

      # Convert time format
      #dat[, TIME_PERIOD := convert_time_col(TIME_PERIOD, time_format)]
      if ("TIME_PERIOD" %in% names(dat)) {
        dat[, TIME_PERIOD := convert_time_col(TIME_PERIOD, time_format)]
      } else {
        warning("'TIME_PERIOD' column not found in the downloaded data.")
      }

      # Convert values to numeric
      if ("OBS_VALUE" %chin% names(dat)) {
        dat[, OBS_VALUE := as.numeric(OBS_VALUE)]
      }

      # Apply labels if requested
      if (type == "label") {
        dat <- label_eurostat_sdmx(dat, agency = agency, id = id, lang = lang)
      }

      # Convert to legacy format if needed
      if (legacy.data.output) {
        dat <- legacy_data_format(dat)
      }

      return(dat)
    },
    "fail" = function() {
      stop("Eurostat async request failed. Final status: ", statuses[fail_pos])
    },
    "timeout" = function() {
      stop("Eurostat async request timed out after ", max_wait, " seconds.")
    }
  )

  handlers[[outcome]]()
}
