#' @importFrom httr RETRY accept_json content stop_for_status
attachment_worker <- function(bug_id, attachment_id = NULL,
                              exclude_fields = character(0),
                              base_url = "https://bugs.r-project.org/bugzilla3/rest/bug/") {
    if (!is.null(attachment_id)) {
        att_id <- TRUE
        stopifnot(is.numeric(attachment_id) || is.character(attachment_id),
                  !is.na(attachment_id), length(attachment_id) == 1L)
    } else {
        att_id <- FALSE
        stopifnot(is.numeric(bug_id) || is.character(bug_id), !is.na(bug_id),
                  length(bug_id) == 1L)
    }
    stopifnot(is.character(base_url), length(base_url) == 1L,
              !is.na(base_url), nzchar(base_url))
    bug_url <- paste0(base_url, if (!att_id) paste0(bug_id, "/"),
                      "attachment",
                      if (att_id) paste0("/", attachment_id),
                      if (length(exclude_fields) > 0L)
                          paste0("?", paste0("exclude_fields=", exclude_fields,
                                             collapse = "&")))
    bug_get <- stop_for_status(RETRY("GET", bug_url, accept_json(),
                                     terminate_on = .terminate_on))
    bug_content <- content(bug_get)
    if (att_id) {
        atts <- bug_content[["attachments"]]
        if (length(atts) == 0L) {
            stop("No attachments returned. Wrong attachment ID?")
        }
        list(atts[[1L]])
    } else {
        bug_content[["bugs"]][[1L]]
    }
}

#' Get Attachment Metadata
#'
#' Returns the metadata related to the attachments of a single bug or any
#' single attachment selected by its \acronym{ID}. By default, the R bug
#' tracker is queried.
#'
#' @param bug_id \acronym{ID} of one bug (numeric or \code{character}).
#' @param attachment_id Optional \acronym{ID} of one attachment (numeric or
#'   \code{character}). If given, \code{bug_id} is ignored.
#' @param obsolete Return information about attachments marked as obsolete?
#'   A \code{logical} flag. Default is \code{FALSE}, i.e., drop obsolete
#'   attachments. Ignored when \code{attachment_id} is given.
#' @param patch_only Only return information about files marked as a patch?
#'   A \code{logical} flag. Default is \code{FALSE}, i.e., include all kinds of
#'   attachments. Ignored when \code{attachment_id} is given.
#' @param base_url \acronym{URL} to Bugzilla \acronym{REST} \acronym{API}
#'   (bug history). Defaults to R Bugzilla.
#' @seealso \code{\link{attachment_flags}}
#' @importFrom lubridate ymd_hms
#' @importFrom tibble tibble
#' @export
#' @return a \code{\link{tibble}}, with rows representing attachments
#'   and the following columns representing their properties:
#'   \item{bug_id}{Bug \code{ID} (\code{integer}).}
#'   \item{id}{Attachment \code{ID} (\code{integer}).}
#'   \item{summary}{Short description of attachment (\code{character}).}
#'   \item{file_name}{Filename of attachment (\code{character}).}
#'   \item{size}{Size of attachment in bytes (\code{integer}).}
#'   \item{content_type}{\acronym{MIME} type of attachment (\code{character}).}
#'   \item{creator}{Who created it? (\code{character})}
#'   \item{creation_time}{Date and time of creation (\code{\link{POSIXct}}).}
#'   \item{last_change_time}{Date and time of last change (\code{\link{POSIXct}}).}
#'   \item{is_patch}{Is it a patch? (\code{logical})}
#'   \item{is_obsolete}{Is it obsolete? (\code{logical})}
#'   \item{is_private}{Is it private? (\code{logical})}
#'   \item{any_flags}{Does the attachment have any flags? (\code{logical})}
#' @examples
#' \dontrun{
#' att17398b <- attachment_info(17398)
#' att2326a <- attachment_info(attachment_id = 2326)
#' identical(att17398b[att17398b$id == 2326, ], att2326a)
#' }
attachment_info <- function(bug_id, attachment_id = NULL, obsolete = FALSE,
                            patch_only = FALSE,
                            base_url = "https://bugs.r-project.org/bugzilla3/rest/bug/") {
    atts <- attachment_worker(bug_id, attachment_id, "data", base_url)
    ans <-
        tibble(bug_id = vapply(atts, `[[`, 0L, "bug_id"),
               id = vapply(atts, `[[`, 0L, "id"),
               summary = vapply(atts, `[[`, "", "summary"),
               file_name = vapply(atts, `[[`, "", "file_name"),
               size = vapply(atts, `[[`, 0L, "size"),
               content_type = vapply(atts, `[[`, "", "content_type"),
               creator = vapply(atts, `[[`, "", "creator"),
               creation_time = ymd_hms(vapply(atts, `[[`, "", "creation_time")),
               last_change_time = ymd_hms(vapply(atts, `[[`, "",
                                                 "last_change_time")),
               is_patch = as.logical(vapply(atts, `[[`, 0L, "is_patch")),
               is_obsolete = as.logical(vapply(atts, `[[`, 0L, "is_obsolete")),
               is_private = as.logical(vapply(atts, `[[`, 0L, "is_private")),
               any_flags = lengths(lapply(atts, `[[`, "flags")) > 0L)
    if (is.null(attachment_id)) {
        if (!isTRUE(obsolete)) {
            ans <- ans[!ans[["is_obsolete"]], ]
        }
        if (isTRUE(patch_only)) {
            ans <- ans[ans[["is_patch"]], ]
        }
    }
    ans
}

#' Get Flags in Attachment Metadata
#'
#' Returns the flags embedded in the metadata related to a single attachment
#' of a bug report. By default, the R bug tracker is queried.
#'
#' @param attachment_id \acronym{ID} of one attachment (numeric or
#'   \code{character}).
#' @param base_url \acronym{URL} to Bugzilla \acronym{REST} \acronym{API}
#'   (bug history). Defaults to R Bugzilla.
#' @seealso \code{\link{attachment_info}}
#' @importFrom lubridate ymd_hms
#' @importFrom tibble tibble
#' @export
#' @return a \code{\link{tibble}}, with rows representing flags
#'   and the following columns representing their properties:
#'   \item{id}{\code{ID} of flag (\code{integer}).}
#'   \item{name}{Name of flag (\code{character}).}
#'   \item{type_id}{Type \code{ID} (\code{integer}).}
#'   \item{creation_date}{Date and time of creation (\code{\link{POSIXct}}).}
#'   \item{modification_date}{Date and time of last modification
#'     (\code{\link{POSIXct}}).}
#'   \item{status}{Status of flag (\code{character}).}
#'   \item{setter}{Who created or last modified the flag? (\code{character})}
#'   \item{requestee}{The account requested to handle the flag
#'     (\code{character}). Empty if not set.}
#' @examples
#' \dontrun{
#' freezilla <- "https://bugs.freebsd.org/bugzilla/rest/bug/"
#' flags_freebsd <- attachment_flags(183342, base_url = freezilla)
#' print(flags_freebsd)
#' }
attachment_flags <- function(attachment_id,
                             base_url = "https://bugs.r-project.org/bugzilla3/rest/bug/") {
    exclude <- c("summary", "creator", "data", "size", "content_type",
                 "is_private", "creation_time", "bug_id", "id",
                 "is_obsolete", "is_patch", "last_change_time", "file_name")
    atts <- attachment_worker(NULL, attachment_id, exclude, base_url)
    flags <- atts[[1L]][["flags"]]
    requestee_func <- function (x) {
        y <- x[["requestee"]]
        if (is.null(y)) {
            ""
        } else {
            y
        }
    }
    tibble(id = vapply(flags, `[[`, 0L, "id"),
           name = vapply(flags, `[[`, "", "name"),
           type_id = vapply(flags, `[[`, 0L, "type_id"),
           creation_date = ymd_hms(vapply(flags, `[[`, "", "creation_date")),
           modification_date = ymd_hms(vapply(flags, `[[`, "",
                                              "modification_date")),
           status = vapply(flags, `[[`, "", "status"),
           setter = vapply(flags, `[[`, "", "setter"),
           requestee = vapply(flags, requestee_func, ""))
}

#' Get Files Attached to Bug Reports
#'
#' Gets the attachments of a single bug or any single attachment selected by
#' its \acronym{ID}. By default, the R bug tracker is queried.
#'
#' @param bug_id \acronym{ID} of one bug (numeric or \code{character}).
#' @param attachment_id Optional \acronym{ID} of one attachment (numeric or
#'   \code{character}). If given, \code{bug_id} is ignored.
#' @param obsolete Return information about attachments marked as obsolete?
#'   A \code{logical} flag. Default is \code{FALSE}, i.e., drop obsolete
#'   attachments. Ignored when \code{attachment_id} is given.
#' @param patch_only Only return information about files marked as a patch?
#'   A \code{logical} flag. Default is \code{FALSE}, i.e., include all kinds of
#'   attachments. Ignored when \code{attachment_id} is given.
#' @param output_dir file system directory where attachments will be saved
#'   (\code{character} string). If necessary, this function will create one
#'   directory level.
#' @param output_file name of file where the attachment will be saved
#'   (\code{character} string). Only takes effect if \code{attachment_id} is
#'   used instead of \code{bug_id}.
#' @param base_url \acronym{URL} to Bugzilla \acronym{REST} \acronym{API}
#'   (bug history). Defaults to R Bugzilla.
#' @importFrom base64enc base64decode
#' @export
#' @return If \code{output_dir} or \code{output_file} is specified, the function
#'   writes to the file system and invisibly returns the \code{output_dir} or
#'   \code{output_file} parameter (the one that is actually used). Otherwise
#'   returns a \code{\link{list}} containing the attachments as either
#'   \code{\link{raw}} vectors or \code{character} strings, depending on the
#'   file type. The \code{list} has the following vector attributes with one
#'   item corresponding to each attachment:
#'   \item{"file_name"}{Name of file (\code{character}).}
#'   \item{"is_patch"}{Is the file marked as a patch? (\code{logical})}
#'   \item{"content_type"}{\acronym{MIME} type of file (\code{character}).}
#' @examples
#' \dontrun{
#' # Get attachments related to a bug
#' data17398b <- attachment_data(17398)
#'
#' # Get a single attachment
#' data2328a <- attachment_data(attachment_id = 2328)
#'
#' # Save files to a temporary directory
#' dir17148 <- attachment_data(17148, output_dir = tempfile(pattern = "dir"))
#' unlink(dir17148, recursive = TRUE)
#'
#' # With and without obsolete files (different code paths used)
#' data16895b2 <- attachment_data(16895, obsolete = TRUE)
#' data16895b <- attachment_data(16895)
#' all(data16895b %in% data16895b2)
#' }
attachment_data <- function(bug_id, attachment_id = NULL, obsolete = FALSE,
                            patch_only = FALSE,
                            output_dir = NULL, output_file = NULL,
                            base_url = "https://bugs.r-project.org/bugzilla3/rest/bug/") {
    att_id <- !is.null(attachment_id)
    exclude <- c("bug_id", "id", "summary", "creator", "flags", "is_private",
                 "creation_time","last_change_time", "is_obsolete")
    if (!att_id && (!isTRUE(obsolete) || isTRUE(patch_only))) {
        ids <- attachment_info(bug_id, obsolete = obsolete,
                               patch_only = patch_only, base_url = base_url)$id
        atts <- lapply(ids, attachment_worker, bug_id = NULL,
                       exclude_fields = exclude, base_url = base_url)
        atts <- do.call(rbind, atts)
    } else {
        atts <- attachment_worker(bug_id, attachment_id, exclude, base_url)
    }
    out_dir <- !is.null(output_dir)
    out_file <- !is.null(output_file)
    raw_output <- (att_id && !out_file && !out_dir) || (!att_id && !out_dir)
    if (!raw_output) {
        if (!att_id && out_file) {
            message("getting all attachments of a bug => 'output_file' is ignored")
            out_file <- FALSE
        }
        if (att_id && out_dir && out_file) {
            message("'output_file' given and getting one attachment => 'output_dir' is ignored")
            out_dir <- FALSE
        }
        if (out_dir) {
            stopifnot(is.character(output_dir), length(output_dir) == 1L,
                      dir.exists(output_dir) || dir.exists(dirname(output_dir)))
        }
        if (out_file) {
            stopifnot(is.character(output_file), length(output_file) == 1L)
        }
    }
    if (raw_output && out_file) {
        warning("'output_file' ignored when getting all attachments => returning raw vector")
    }
    dat <- lapply(vapply(atts, `[[`, "", "data"), base64decode)
    size <- vapply(atts, `[[`, 0L, "size")
    dat_ok <- size == lengths(dat)
    if (any(!dat_ok)) {
        warning("attachment(s) malformed, setting to NULL")
        dat[!dat_ok] <- list(NULL)
    }
    file_name <- vapply(atts, `[[`, "", "file_name")
    if (raw_output) {
        content_type <- vapply(atts, `[[`, "", "content_type")
        is_patch <- as.logical(vapply(atts, `[[`, 0L, "is_patch"))
        is_text <- dat_ok & is_patch
        do_grep <- dat_ok & !is_text
        if (any(do_grep)) {
            text_apps <-
                c("mbox", "x-csh", "x-sh", "x-shellscript", "ecmascript",
                  "javascript", "(?:.+[+])?(?:json|xml)", "typescript")
            is_text[do_grep] <-
                grepl(sprintf("^(?:text/|(?:application/(?:%s)$))",
                              paste0(text_apps, collapse = "|")),
                      content_type[do_grep])
        }
        for (k in which(is_text)) {
            dat[[k]] <- rawToChar(dat[[k]])
        }
        structure(dat, file_name = file_name, is_patch = is_patch,
                  content_type = content_type)
    } else if (att_id && dat_ok[[1L]] && out_file) {
        message(gettextf("Writing to file \"%s\"", output_file), domain = NA)
        writeBin(dat[[1L]], output_file)
        invisible(output_file)
    } else {
        n_ok <- sum(dat_ok)
        if (n_ok > 0L) {
            if (!dir.exists(output_dir)) {
                message(gettextf("Creating directory \"%s\"",
                                 output_dir), domain = NA)
                dir.create(output_dir)
                if (!dir.exists(output_dir)) {
                    stop("Failed to create output directory")
                }
            }
            message(sprintf(ngettext(n_ok, "Writing to file %s in directory %s",
                                     "Writing to files %s in directory %s"),
                            paste0('"',file_name[dat_ok],'"', collapse=", "),
                            paste0('"', output_dir, '"')),
                    domain = NA)
        }
        for (k in which(dat_ok)) {
            writeBin(dat[[k]], file.path(output_dir, file_name[[k]]))
        }
        invisible(output_dir)
    }
}
