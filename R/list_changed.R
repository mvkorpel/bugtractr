#' List Bugzilla Reports Opened or Closed in a Given Period
#'
#' Specify the type of status changes you are interested in and the limits of a
#' time interval during which the status change must have occurred. The function
#' will return a list of bugs matching these criteria. By default, the R bug
#' tracker is queried.
#'
#' The \code{type} option specifies what kind of changes will be detected:
#' \describe{
#' \item{\code{"opened"}}{Bug report was created during the time frame
#'   (\code{from} \ldots \code{to}) and is still open.}
#' \item{\code{"opened_confirmed"}}{Like \code{"opened"}, and the bug is
#'   confirmed.}
#' \item{\code{"closed"}}{The last status change of the bug occurred during the
#'   time frame, and the bug is still CLOSED.}
#' \item{\code{"closed_fixed"}}{Like \code{"closed"}, and the resolution of the
#'   bug is FIXED.}
#' }
#' @param type Type of changes to list (\code{character}). See \sQuote{Details}.
#' @param from Starting point of search interval. Look for changes that occurred
#'   at this moment or after it. Must be \code{character}, numeric, or a value
#'   belonging to one of the \link[=DateTimeClasses]{date-time classes}. See the
#'   description of \code{new_since} in \code{\link{bug_history}} for details.
#' @param to Like \code{from}, but end point of time interval.
#' @param exclude_components Don't show bugs with the component field matching
#'   one of the given \code{character} strings.
#' @param list_url Optional \acronym{URL} to use for the \code{base_url} option
#'   of \code{\link{list_bugs}}. Defaults to using the R Bugzilla.
#' @param info_url Optional \acronym{URL} for \code{base_url} in
#'   \code{\link{bug_info}} and \code{\link{bug_history}}. If not explicitly
#'   set, the value of this option will be derived from a user-specified
#'   \code{list_url}. Defaults to using the R Bugzilla.
#' @importFrom stringr str_detect str_replace
#' @export
#' @return A list of bugs matching the criteria. The format is the same as used
#'   by \code{\link{list_bugs}}.
#' @examples
#' \dontrun{
#' fixed_jan_2017 <- list_changed("closed_fixed",
#'                                from = "2017-01-01", to = "2017-02-01")
#' }
list_changed <- function(type = c("opened", "opened_confirmed", "closed",
                                  "closed_fixed"),
                         from = to - 120 - 7 * 24 * 3600,
                         to = Sys.time() + 60,
                         exclude_components = "Wishlist",
                         list_url = NULL,
                         info_url = NULL) {
    process_date <- function(date_arg) {
        stopifnot(length(date_arg) == 1L, !is.na(date_arg))
        if (is.character(date_arg) &&
            str_detect(date_arg,
                       paste0("[0-9]+-[0-9]{1,2}-[0-9]{1,2}",
                              "T[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}",
                              "(?:\\.[0-9]*)?Z"))) {
            as.POSIXct(strptime(date_arg,
                                format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"))
        } else if (is.numeric(date_arg)) {
            as.POSIXct(Sys.Date() + date_arg, tz = "GMT")
        } else {
            as.POSIXct(date_arg, tz = "GMT")
        }
    }
    to2 <- process_date(to)
    from2 <- process_date(from)
    type2 <- match.arg(type)
    do_exclude <- length(exclude_components) > 0L
    if (do_exclude) {
        stopifnot(is.character(exclude_components), !is.na(exclude_components))
    }
    if (str_detect(type2, "^opened")) {
        bug_cat <- "all"
        cat_all <- TRUE
    } else {
        bug_cat <- "closed"
        cat_all <- FALSE
    }
    list_args <- list(category = bug_cat, changed_from = as.Date(from2),
                      changed_to = as.Date(to2) + 1)
    if (!is.null(list_url)) {
        list_args <- c(list_args, alist(base_url = list_url))
    }
    bug_list <- do.call(list_bugs, list_args)
    if (nrow(bug_list) == 0L) {
        return(bug_list)
    }
    bug_ids <- bug_list[["id"]]
    info_args <- alist(bug_ids = bug_ids)
    if (!is.null(info_url)) {
        info_url2 <- info_url
    } else if (!is.null(list_url) && str_detect(list_url, "/buglist.cgi$")) {
        info_url2 <- str_replace(list_url, "/buglist.cgi$", "/rest/bug/")
    } else {
        info_url2 <- NULL
    }
    if (!is.null(info_url2)) {
        info_args <- c(info_args, list(base_url = info_url2))
    }
    info <- do.call(bug_info, info_args)
    if (cat_all) {
        created <- info[["created"]]
        keep <- created >= from2 & created <= to2 &
            info[["is_open"]]
        if (type2 == "opened_confirmed") {
            keep <- keep & info[["is_confirmed"]]
        }
    } else {
        changed <- info[["changed"]]
        keep <- changed >= from2 & changed <= to2 &
            !info[["is_open"]]
        if (type2 == "closed_fixed") {
            keep <- keep & info[["resolution"]] == "FIXED"
        }
        hist_args <- alist(new_since = from2)
        if (!is.null(info_url2)) {
            hist_args <- c(hist_args, list(base_url = info_url2))
        }
        for (k in which(keep)) {
            hist_args[["bug_id"]] <- bug_ids[k]
            bug_hist <- do.call(bug_history, hist_args)
            status_change <- bug_hist[["field_name"]] == "status"
            keep[k] <- any(status_change & bug_hist[["when"]] <= to2) &&
                !any(status_change & bug_hist[["when"]] > to)
        }
    }
    if (do_exclude) {
        keep <- keep & !(info[["component"]] %in% exclude_components)
    }
    bug_list[keep, ]
}
