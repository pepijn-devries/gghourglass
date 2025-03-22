#' Observations from a bat detector
#' 
#' A dataset containing detections of audio call sequences from
#' bats. It is a small subset of the data published by Lagerveld et al. (2023)
#' 
#' @references Lagerveld, S., Wilkes, T., van Puijenbroek, M.E.B., Noort, B.C.A., Geelhoed,
#' S.C.V. Acoustic monitoring reveals spatiotemporal occurrence of Nathusius’ pipistrelle
#' at the southern North Sea during autumn migration. Environ Monit Assess 195, 1016 (2023)
#' <doi:10.1007/s10661-023-11590-2>
#' @docType data
#' @name bats
#' @format A `data.frame` with 1,049 rows and 2 columns:
#' 
#'   * `RECDATETIME`: datetime of the recorded bat call sequence
#'   * `SPECDESCSCI`: scientific species name
#'   
#' It also contains an attribute named `monitoring` which is a `data.frame`
#' containing monitoring periods at which the bat detector was active. Each
#' row is a monitoring period, and it holds the following columns:
#' 
#'   * `start`: start datetime of the monitoring period
#'   * `end`: end datetime of the monitoring period
#'   * `time_on`: time of day at which the detector is activated
#'      during the monitoring period
#'   * `time_off`: time of day at which the detector is deactivated
#'      during the monitoring period
#'   * `longitude` and `latitude`: coordinates of the detector's location
#'   * `altitude`: altitude in meters above sea level of the detector.
#' @examples
#' data("bats")
NULL