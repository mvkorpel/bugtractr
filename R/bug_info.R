#' Get Information about Selected Bugs
#'
#' Returns more detailed bug information than \code{\link{list_bugs}}. By
#' default, the R bug tracker is queried.
#'
#' @param bug_ids One or more numeric (or coercible to numeric) bug
#'   \acronym{ID}s
#' @param base_url \acronym{URL} to Bugzilla \acronym{REST} \acronym{API}
#'   (get bug). Defaults to R Bugzilla.
#' @importFrom httr RETRY accept_json content stop_for_status
#' @importFrom tibble tibble
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_trim
#' @export
#' @return a \code{\link{tibble}}, with rows representing bugs (in the order
#'   specified by the \code{bug_ids} argument) and the following columns
#'   representing their properties:
#'   \item{id}{Unique bug \code{ID} (\code{integer}).}
#'   \item{summary}{Short description of bug (\code{character}).}
#'   \item{creator}{Who created the bug report? (name followed by email in angle
#'     brackets, \code{character})}
#'   \item{is_open}{Is the bug open? (\code{logical})}
#'   \item{product}{Product where the bug occurs (\code{character}).}
#'   \item{component}{Which part of the product is affected? (\code{character})}
#'   \item{assignee}{To whom is the bug assigned? (name followed by email in
#'     angle brackets, \code{character})}
#'   \item{status}{Status of bug (\code{character}): \code{"UNCONFIRMED"},
#'     \code{"NEW"}, \code{"CLOSED"}, \ldots}
#'   \item{is_confirmed}{Has the bug been confirmed? (\code{logical})}
#'   \item{resolution}{Together with \code{"status"}, defines the current state
#'     of the bug: \code{"FIXED"}, \code{"WONTFIX"}, \code{"WORKSFORME"},
#'     \code{"INVALID"}, \ldots}
#'   \item{created}{Date and time of creation (\code{\link{POSIXct}}).}
#'   \item{changed}{Date and time of last change (\code{\link{POSIXct}}).}
#'   \item{alias}{Any aliases the bug may have, separated by \code{";"}
#'     (\code{character}).}
#'   \item{version}{Version of product related to bug (\code{character}).}
#'   \item{os}{Operating system where the bug occurs (\code{character}).}
#'   \item{hardware}{Type of hardware exhibiting the bug (\code{character}).}
#'   \item{priority}{Priority of the bug (\code{character}). Typically
#'     \code{"P1"} means the highest priority and \code{"P5"} the lowest
#'     priority.}
#'   \item{severity}{Severity of the bug (\code{character}): \code{"Blocker"},
#'     \code{"Critical"}, \code{"Major"}, \code{"Normal"}, \code{"Minor"},
#'     \code{"Trivial"}, \code{"Enhancement"}.}
#'   \item{url}{\acronym{URL} related to bug (\code{character}, possibly
#'     empty \code{""}).}
#'   \item{depends_on}{A list of bugs (numeric \acronym{ID} as \code{character})
#'     which have to be solved before this one. Uses \code{";"} as separator.
#'     Possibly empty \code{""}.}
#'   \item{blocks}{A list of bugs which cannot be solved before this one.
#'     Formatted like \code{"depends_on"}.}
#'   \item{cc}{List of interested parties subscribed to updates about the bug
#'     report. A \code{character} string, with people separated by \code{";"}.
#'     Each person is shown as a name followed by an email address in angle
#'     brackets. Possibly empty \code{""}.}
#'   \item{see_also}{List of \acronym{URL}s pointing to related bug reports.
#'     Uses the space \code{" "} as separator. Possibly empty \code{""}.}
#'   \item{dupe_of}{If the bug report is a duplicate of a previous issue, this
#'     contains the bug \acronym{ID} of the earlier report as a \code{character}
#'     string. Otherwise an empty string: \code{""}.}
#' @examples
#' \dontrun{
#' info1 <- bug_info(1)
#' }
bug_info <- function(bug_ids,
                     base_url = "https://bugs.r-project.org/bugzilla3/rest/bug/") {
    bug_ids2 <- as.numeric(bug_ids)
    stopifnot(is.finite(bug_ids2))
    n_bugs <- length(bug_ids2)
    stopifnot(n_bugs > 0L)
    bug_ids2 <- sprintf("%.0f", bug_ids)
    stopifnot(is.character(base_url), length(base_url) == 1L,
              !is.na(base_url), nzchar(base_url))
    base_url2 <- paste0(base_url, "?")

    ## All bug database fields we are interested in
    include_fields <-
        c("id", "summary", "creator", "creator_detail", "is_open", "product",
          "component", "assigned_to", "assigned_to_detail", "status",
          "is_confirmed", "resolution", "creation_time", "last_change_time",
          "alias", "version", "op_sys", "platform", "priority", "severity",
          "url", "depends_on", "blocks", "cc", "cc_detail", "see_also",
          "dupe_of")

    ## Grouping of fields by type of information
    char_fields <-
        c("summary", "product", "component", "status", "resolution",
          "creation_time", "last_change_time", "version", "op_sys", "platform",
          "priority", "severity", "url") # , "creator", "assigned_to")
    int_fields <- "id"
    int_null_fields <- "dupe_of"
    lgcl_fields <- c("is_open", "is_confirmed")
    structured_list_fields <- c("creator_detail", "assigned_to_detail")
    plain_list_fields <-
        c("alias", "depends_on", "blocks", "cc_detail", "see_also") # , "cc")

    ## It seems creator|assigned_to|cc is needed for the corresponding
    ## _detail field. Creation and last change times may be missing, in which
    ## case errors may follow.
    get_sets <- list(setdiff(include_fields,
                             c("creation_time", "last_change_time",
                               "creator", "creator_detail",
                               "assigned_to", "assigned_to_detail",
                               "cc", "cc_detail")),
                     "creation_time",
                     "last_change_time",
                     c("creator", "creator_detail"),
                     c("assigned_to", "assigned_to_detail"),
                     c("cc", "cc_detail"))
    n_get_sets <- length(get_sets)
    n_fields <- vapply(get_sets, length, 0L)

    get_dummy_value <- function(field_name) {
        if (field_name %in% c(char_fields, plain_list_fields,
                              structured_list_fields)) {
            NA_character_
        } else if (field_name %in% int_fields) {
            NA_integer_
        } else if (field_name %in% int_null_fields) {
            ## Response from bug database may be integer or NULL.
            ## Convert integer to string, NULL to empty string,
            ## use NA_character_ for error.
            NA_character_
        } else if (field_name %in% lgcl_fields) {
            NA
        } else {
            ## Will not be used in the final return value
            0L
        }
    }
    ## Like paste0, but keeps a lone NA_character_ (does not convert to "NA")
    paste1 <- function(..., collapse = NULL) {
        dots <- list(...)
        if (length(dots) == 1L && identical(dots[[1L]], NA_character_)) {
            NA_character_
        } else {
            paste0(..., collapse = collapse)
        }
    }

    bug_url <-
        paste0(base_url2,
               paste0("ids=", bug_ids2, collapse = "&"),
               "&",
               paste0("include_fields=", include_fields, collapse = "&"))
    bug_get <- stop_for_status(RETRY("GET", bug_url, accept_json()))
    bug_content <- content(bug_get)

    if (isTRUE(bug_content[["error"]])) {
        bugs <- vector(mode = "list", length = n_bugs)
        if (n_bugs > 1L) {
            failed <- logical(n_bugs)
            for (k in seq_len(n_bugs)) {
                bug_url2 <-
                    paste0(base_url2,
                           "ids=", bug_ids2[k], "&",
                           paste0("include_fields=", include_fields,
                                  collapse = "&"))
                bug_get2 <- stop_for_status(RETRY("GET", bug_url2,
                                                  accept_json()))
                bug_content2 <- content(bug_get2)
                if (isTRUE(bug_content2[["error"]])) {
                    failed[k] <- TRUE
                } else {
                    bugs[k] <- bug_content2[["bugs"]]
                }
            }
        } else {
            failed <- TRUE
        }
        for (k in which(failed)) {
            bug_k <- vector(mode = "list", length = length(include_fields))
            names(bug_k) <- include_fields
            for (l in seq_len(n_get_sets)) {
                set_l <- get_sets[[l]]
                bug_url3 <-
                    paste0(base_url2,
                           "ids=", bug_ids2[k], "&",
                           paste0("include_fields=", set_l, collapse = "&"))
                bug_get3 <- stop_for_status(RETRY("GET", bug_url3,
                                                  accept_json()))
                bug_content3 <- content(bug_get3)
                if (isTRUE(bug_content3[["error"]])) {
                    if (n_fields[l] > 2L) {
                        failed2 <- logical(n_fields[l])
                        for (m in seq_len(n_fields[l])) {
                            bug_url4 <-
                                paste0(base_url2,
                                       "ids=", bug_ids2[k],
                                       "&include_fields=", set_l[m])
                            bug_get4 <-
                                stop_for_status(RETRY("GET", bug_url4,
                                                      accept_json()))
                            bug_content4 <- content(bug_get4)
                            if (isTRUE(bug_content4[["error"]])) {
                                failed2[m] <- TRUE
                            } else {
                                bug_k[set_l[m]] <- bug_content4[["bugs"]]
                            }
                        }
                    } else {
                        failed2 <- rep.int(TRUE, n_fields[l])
                    }
                    for (m in which(failed2)) {
                        field_m <- set_l[m]
                        bug_k[[field_m]] <- get_dummy_value(field_m)
                    }
                } else {
                    fields_k <- bug_content3[["bugs"]][[1L]]
                    bug_k[names(fields_k)] <- fields_k
                }

            }
            bugs[[k]] <- bug_k
        }
    } else {
        bugs <- bug_content[["bugs"]]
    }

    ## Functions for reformatting information from the bug database
    cc_func <- function (x) {
        if (is.character(x)) {
            x
        } else {
            paste0(str_trim(paste(vapply(x, `[[`, "", "real_name"),
                                  vapply(x, `[[`, "", "email"), sep = " <"),
                            side = "left"),
                   if (length(x) > 0L) ">", collapse = ";")
        }
    }
    creator_func <- function (x) {
        if (is.character(x)) {
            x
        } else {
            str_trim(paste0(x[["real_name"]], " <", x[["email"]], ">"),
                     side = "left")
        }
    }
    dupe_func <- function (x) {
        y <- x[["dupe_of"]]
        if (is.null(y)) {
            ""
        } else {
            as.character(y)
        }
    }

    tibble(id = vapply(bugs, `[[`, 0L, "id"),
           summary = vapply(bugs, `[[`, "", "summary"),
           creator = vapply(lapply(bugs, `[[`, "creator_detail"),
                            creator_func, ""),
           is_open = vapply(bugs, `[[`, FALSE, "is_open"),
           product = vapply(bugs, `[[`, "", "product"),
           component = vapply(bugs, `[[`, "", "component"),
           assignee = vapply(lapply(bugs, `[[`, "assigned_to_detail"),
                             creator_func, ""),
           status = vapply(bugs, `[[`, "", "status"),
           is_confirmed = vapply(bugs, `[[`, FALSE, "is_confirmed"),
           resolution = vapply(bugs, `[[`, "", "resolution"),
           created = ymd_hms(vapply(bugs, `[[`, "", "creation_time")),
           changed = ymd_hms(vapply(bugs, `[[`, "", "last_change_time")),
           alias = vapply(lapply(bugs, `[[`, "alias"),
                          paste1, "", collapse = ";"),
           version = vapply(bugs, `[[`, "", "version"),
           os = vapply(bugs, `[[`, "", "op_sys"),
           hardware = vapply(bugs, `[[`, "", "platform"),
           priority = vapply(bugs, `[[`, "", "priority"),
           severity = vapply(bugs, `[[`, "", "severity"),
           url = vapply(bugs, `[[`, "", "url"),
           depends_on = vapply(lapply(bugs, `[[`, "depends_on"),
                               paste1, "", collapse = ";"),
           blocks = vapply(lapply(bugs, `[[`, "blocks"),
                           paste1, "", collapse = ";"),
           cc = vapply(lapply(bugs, `[[`, "cc_detail"), cc_func, ""),
           see_also = vapply(lapply(bugs, `[[`, "see_also"),
                             paste1, "", collapse = " "),
           dupe_of = vapply(bugs, dupe_func, ""))
}
