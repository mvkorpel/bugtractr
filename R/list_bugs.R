#' List Bugs in a Bugzilla Database
#'
#' Shows the \acronym{ID}s, short descriptions (summaries), and other
#' information about bugs in a Bugzilla database. The bugs can be filtered by
#' status and date of last change. By default, the R bug tracker is queried.
#'
#' The \code{category} option accepts a number of choices. The default
#' (\code{"open"}) returns bugs that have one of the following as their status:
#' NEW, ASSIGNED, REOPENED, UNCONFIRMED. The other choices are \code{"closed"}
#' which covers bugs with the CLOSED status, \code{"other"} for bugs with a
#' WISH, RESOLVED, or VERIFIED status, and \code{"all"} for listing bugs with
#' any status.
#'
#' To limit the results by date and time of last change, use \code{changed_from}
#' and \code{changed_to}. These options accept a date in the standard
#' \verb{YYYY-MM-DD} (year, month, day) notation or a relative date-time, for
#' example \code{"-2d"} for \dQuote{two days ago} including the time of day. The
#' following abbreviations can be used in relative date-times: \code{"y"} for
#' year, \code{"m"} for month, \code{"w"} for week, \code{"d"} for day and
#' \code{"h"} for hour. Numeric values may also be used, and they are
#' interpreted as days relative to the current date. The standard
#' \link[=DateTimeClasses]{date-time classes} are also supported, but only the
#' date part will be utilized. Note that the option to use numeric and date-time
#' types is a convenience feature provided by this function. Thus, they are
#' eventually transmitted to the server as \verb{YYYY-MM-DD}, which apparently
#' refers to the beginning of the day. See \sQuote{Examples}.
#'
#' The \code{sorting} argument has two choices which sort according to multiple
#' properties of each bug: \code{"importance"} uses \code{"priority"} and
#' \code{"severity"} (in this order), whereas with \code{"assignee"} the sort
#' order is primarily based on the person to whom the bug is assigned to, but
#' also on the bug status, priority and \acronym{ID} (in this order).
#'
#' This function extracts information from an \acronym{HTML} document
#' presenting the bug list. Therefore some results are presented in an
#' abbreviated form. More detailed information about selected bugs can
#' be obtained with \code{\link{bug_info}} and
#' \code{\link{bug_history}}.
#'
#' @param category A \code{character} string: what kind of bugs should be
#'   listed? See \sQuote{Details}.
#' @param limit A non-negative integer limiting the number of bugs to show.
#'   Zero means no limit. The default is 500. There have been issues with
#'   large numbers, including the no-limit setting.
#' @param status An optional \code{character} vector. If set, overrides
#'   \code{category} and defines the exact values for accepted bug status.
#' @param changed_from Optional \code{character} string, date-time object of
#'   numeric value to limit the time frame of bugs shown, based on the change
#'   date and time of the bug. This is the start of the search interval.
#'   \code{NULL} (the default) means no limit. See \sQuote{Details}.
#' @param changed_to Like \code{changed_from}, but this is the other endpoint.
#'   Defaults to the present (no limit).
#' @param product An optional \code{character} string which limits the bug list
#'   to a single product.
#' @param component Like \code{product}, but only show a single component.
#' @param sorting A \code{character} string stating the desired sorting of the
#'   results. The default \code{"changed"} orders according to the date and time
#'   of last change, while \code{"id"} uses the numerical order of the bug
#'   \acronym{ID}s. Also \code{"priority"} and \code{"severity"} are simple
#'   choices which sort according to one property of each bug. See
#'   \sQuote{Details} for the other choices.
#' @param reverse A \code{logical} flag: reverse the order of results? The
#'   default for \code{sorting = "changed"} is \code{TRUE} (most recently
#'   changed first), otherwise \code{FALSE}.
#' @param base_url \acronym{URL} of a Bugzilla bug list page. Defaults to R
#'   Bugzilla.
#' @importFrom tibble lst tibble
#' @importFrom httr RETRY accept content stop_for_status
#' @importFrom xml2 xml_find_all xml_text xml_attr url_absolute url_escape
#' @importFrom stringr str_trim str_replace str_replace_all fixed
#' @importFrom lubridate ymd_hms
#' @export
#' @return a \code{\link{tibble}}, with rows representing bugs and the following
#'   columns representing their properties:
#'   \item{id}{Unique bug \code{ID} (\code{integer}).}
#'   \item{summary}{Short description of bug (\code{character}).}
#'   \item{link}{\code{URL} to detailed bug report web page (\code{character}).}
#'   \item{product}{Product where the bug occurs (\code{character}).}
#'   \item{component}{Which part of the product is affected? (\code{character})}
#'   \item{assignee}{To whom is the bug assigned? (\code{character})}
#'   \item{status}{Abbreviated status of bug (\code{character}):
#'     \code{"UNCO"}(NFIRMED), \code{"NEW"}, \code{"CLOS"}(ED), \ldots}
#'   \item{resolution}{Together with \code{"status"}, defines the current state
#'     of the bug: \code{"FIXE"}(D), \code{"WONT"}(FIX), \code{"WORK"}(SFORME),
#'     \code{"INVA"}(LID), \ldots}
#'   \item{changed}{(Date and) time of last change (\code{character}). The
#'     format may be irregular.}
#' @examples
#' \dontrun{
#' ## Several ways to get open bugs without time limit
#' bugs1a <- list_bugs() # changed_to = "Now"
#' bugs1b <- list_bugs(changed_to = 1)
#' bugs1c <- list_bugs(changed_to = Sys.Date() + 1)
#' bugs1d <- list_bugs(changed_to = as.character(Sys.Date() + 1))
#'
#' ## List bugs in reverse order.
#' ## Note that reverse = TRUE is default for sorting = "changed", which
#' ## means most recent first. So, reverse = FALSE is oldest first.
#' bugs2 <- list_bugs(reverse = FALSE)
#' identical(bugs1a$id, rev(bugs2$id)) # TRUE if bug database did not change
#'
#' ## Get all bugs changed during the last week, sorted according to priority
#' bugs3 <- list_bugs(category = "all", changed_from = "-1w",
#'                    sorting = "priority")
#'
#' ## Override predefined bug categories to look for ASSIGNED bugs
#' bugs4 <- list_bugs(status = "ASSIGNED")
#' }
list_bugs <- function(category = c("open", "closed", "other", "all"),
                      limit = 500,
                      status = NULL, changed_from = NULL, changed_to = "Now",
                      product = NULL, component = NULL,
                      sorting = c("changed", "id", "importance", "assignee",
                                  "priority", "severity"),
                      reverse = match.arg(sorting) == "changed",
                      base_url = "https://bugs.r-project.org/bugzilla3/buglist.cgi") {
    sorting2 <- match.arg(sorting)
    stopifnot(is.character(base_url), length(base_url) == 1L,
              !is.na(base_url), nzchar(base_url))
    stopifnot(is.logical(reverse), length(reverse) == 1L, !is.na(reverse))
    process_date <- function(date_arg) {
        stopifnot(length(date_arg) == 1L, !is.na(date_arg))
        if (inherits(date_arg, "Date")) {
            format(date_arg, format = "%Y-%m-%d")
        } else if (inherits(date_arg, "POSIXt")) {
            format(date_arg, format = "%Y-%m-%d", tz = "GMT")
        } else if (is.numeric(date_arg)) {
            format(Sys.Date() + date_arg, format = "%Y-%m-%d")
        } else if (is.character(date_arg)) {
            stopifnot(nzchar(date_arg))
            date_arg
        } else {
            stop("type of date argument is not supported")
        }
    }
    have_from <- !is.null(changed_from)
    if (have_from) {
        changed_from2 <- process_date(changed_from)
    } else {
        changed_from2 <- NULL
    }
    have_product <- !is.null(product)
    if (have_product) {
        stopifnot(is.character(product), length(product) == 1L,
                  !is.na(product))
    }
    have_component <- !is.null(component)
    if (have_component) {
        stopifnot(is.character(component), length(component) == 1L,
                  !is.na(component))
    }
    changed_to2 <- process_date(changed_to)
    if (is.null(status)) {
        category2 <- match.arg(category)
        k_status <- list(open = c("NEW", "ASSIGNED", "REOPENED", "UNCONFIRMED"),
                         closed = "CLOSED",
                         other = c("WISH", "RESOLVED", "VERIFIED"))
        k_status[["all"]] <-
            c(k_status[["open"]], k_status[["closed"]], k_status[["other"]])
        status2 <- k_status[[category2]]
    } else {
        stopifnot(is.character(status), length(status) > 0L,
                  !is.na(status), nzchar(status))
        status2 <- status
    }
    stopifnot(is.numeric(limit), length(limit) == 1L, is.finite(limit),
              limit >= 0, round(limit) == limit)
    k_sorting <- c(changed = "changeddate",
                   id = "bug_id",
                   importance = "priority%2Cbug_severity",
                   assignee = "assigned_to%2Cbug_status%2Cpriority%2Cbug_id",
                   priority = "priority",
                   severity = "bug_severity")
    ## When difficult to achieve exactly reverse order of results, do it
    ## manually
    k_reverse_manually <- c("importance", "priority", "severity")
    reverse_manually <- sorting2 %in% k_reverse_manually
    sorting3 <- k_sorting[[sorting2]]
    if (reverse && !reverse_manually) {
        sorting3 <-
            paste0(str_replace_all(sorting3, fixed("%2C"), "%20DESC%2C"),
                   "%20DESC")
    }
    bug_url <-
        c(base_url, "?",
          paste0("bug_status=", status2, collapse = "&"),
          if (have_from) paste0("&chfieldfrom=", changed_from2),
          if (have_product) paste0("&product=", url_escape(product)),
          if (have_component) paste0("&component=", url_escape(component)),
          "&chfieldto=", changed_to2,
          "&order=", sorting3,
          sprintf("&limit=%d", limit))
    bug_url <- paste0(bug_url, collapse = "")
    bug_get <- stop_for_status(RETRY("GET", bug_url, accept("text/html"),
                                     terminate_on = .terminate_on))
    bug_content <- content(bug_get)
    bug_summary_a <-
        xml_find_all(bug_content,
                     "//td[contains(@class, 'bz_short_desc_column')]//a")
    bug_summaries <- str_trim(xml_text(bug_summary_a))
    bug_hrefs <- xml_attr(bug_summary_a, "href")
    bug_ids <- as.integer(str_replace(bug_hrefs, ".*id=", ""))
    get_td_texts <- function(td_class, document = bug_content) {
        nodes <- xml_find_all(document,
                              sprintf("//td[contains(@class, 'bz_%s_column')]",
                                      td_class))
        str_trim(xml_text(nodes))
    }
    result <- tibble(id = bug_ids, summary = bug_summaries,
                     link = url_absolute(bug_hrefs, base_url),
                     product = get_td_texts("product"),
                     component = get_td_texts("component"),
                     assignee = get_td_texts("assigned_to"),
                     status = get_td_texts("bug_status"),
                     resolution = get_td_texts("resolution"),
                     changed = get_td_texts("changeddate"))
    if (!reverse || !reverse_manually) {
        result
    } else {
        n_rows <- nrow(result)
        result[seq.int(from = n_rows, by = -1L, length.out = n_rows), ]
    }
}
