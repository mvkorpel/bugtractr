#' Get Change History of Bug
#'
#' Returns the change history of a single bug. Throughout the life of a bug
#' report, its status may get changed from UNCONFIRMED to NEW or ASSIGNED, and
#' finally to CLOSED. New people may be added to the \acronym{CC} list of people
#' subscribed to notifications about the bug. These and other changes are
#' recorded, and the changes can be inspected with this function. By default,
#' the R bug tracker is queried.
#'
#' If several properties (fields) of the bug were changed at once, the result
#' will show duplicated values in the \code{"when"} and \code{"who"} columns.
#'
#' @param bug_id \acronym{ID} or alias of one bug (numeric or \code{character}).
#' @param new_since Discard changes older than this. A \code{character} string
#'   with date in format \verb{YYYY-MM-DD} or date and time as
#'   \verb{YYYY-MM-DDTHH24:MI:SSZ}. The latter is year-month-date, letter
#'   \code{"T"}, elements of time (hours, minutes and seconds) separated by
#'   \code{:}, and a final letter \code{"Z"} to mark that \acronym{UTC} time is
#'   being used. The standard \link[=DateTimeClasses]{date-time classes} are
#'   also supported. Numeric values are accepted and interpreted as number of
#'   days relative to the current date. The default \code{NULL} means no limit.
#' @param base_url \acronym{URL} to Bugzilla \acronym{REST} \acronym{API}
#'   (bug history). Defaults to R Bugzilla.
#' @importFrom httr RETRY accept_json content stop_for_status
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms
#' @export
#' @return a \code{\link{tibble}}, with rows representing historical changes to
#'   the bug (in chronological order) and the following columns describing the
#'   details of the change:
#'   \item{when}{Date and time of change (\code{\link{POSIXct}}).}
#'   \item{who}{Email address of person who made the change (\code{character}).}
#'   \item{field_name}{Field that was changed (\code{character}).}
#'   \item{removed}{Previous value of the field (\code{character}).}
#'   \item{added}{New value of the field (\code{character}).}
#'   \item{attachment_id}{\acronym{ID} of attachment that was changed
#'     (\code{integer}, \code{NA_integer_} if the change was not about an
#'     attachment).}
#' @examples
#' \dontrun{
#' hist1 <- bug_history(1)
#' }
bug_history <- function(bug_id, new_since = NULL,
                        base_url = "https://bugs.r-project.org/bugzilla3/rest/bug/") {
    stopifnot(is.numeric(bug_id) || is.character(bug_id), !is.na(bug_id),
              length(bug_id) == 1L)
    stopifnot(is.character(base_url), length(base_url) == 1L,
              !is.na(base_url), nzchar(base_url))
    process_date <- function(date_arg) {
        stopifnot(length(date_arg) == 1L, !is.na(date_arg))
        if (inherits(date_arg, "Date")) {
            format(date_arg, format = "%Y-%m-%d")
        } else if (inherits(date_arg, "POSIXt")) {
            format(date_arg, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")
        } else if (is.numeric(date_arg)) {
            format(Sys.Date() + date_arg, format = "%Y-%m-%d")
        } else if (is.character(date_arg)) {
            stopifnot(nzchar(date_arg))
            date_arg
        } else {
            stop("type of date argument is not supported")
        }
    }
    have_since <- !is.null(new_since)
    if (have_since) {
        new_since2 <- process_date(new_since)
    }
    bug_url <- paste0(base_url, bug_id, "/history",
                      if (have_since) paste0("?new_since=", new_since2))
    bug_get <- stop_for_status(RETRY("GET", bug_url, accept_json(),
                                     terminate_on = .terminate_on))
    bug_content <- content(bug_get)
    bug_history <- bug_content[["bugs"]][[1]][["history"]]
    changes <- lapply(bug_history, `[[`, "changes")
    if (length(changes) == 0L) {
        return(tibble(when = as.POSIXct("2000-01-01")[FALSE],
                      who = character(0),
                      field_name = character(0),
                      removed = character(0),
                      added = character(0),
                      attachment_id = integer(0)))
    }
    n_changes <- vapply(changes, length, 0L)
    when <- vapply(bug_history, `[[`, "", "when")
    who <- vapply(bug_history, `[[`, "", "who")
    changes2 <- do.call(c, changes)
    id <- lapply(changes2, `[[`, "attachment_id")
    tibble(when = ymd_hms(unlist(mapply(rep.int, when, n_changes,
                                        SIMPLIFY = FALSE, USE.NAMES = FALSE))),
           who = unlist(mapply(rep.int, who, n_changes,
                               SIMPLIFY = FALSE, USE.NAMES = FALSE)),
           field_name = vapply(changes2, `[[`, "", "field_name"),
           removed = vapply(changes2, `[[`, "", "removed"),
           added = vapply(changes2, `[[`, "", "added"),
           attachment_id = unlist(ifelse(vapply(id, is.null, FALSE),
                                         NA_integer_, id)))
}
