# =============================================================================
# dimensions.R  --  rink sizes (IIHF, NHL) and a validated way to derive your own.
#
# Every drawing and binning function takes one of these lists as `dimensions`.
# All geometry is in METRES with the origin at the bottom-left corner of the
# boards (x along the length, y across the width); shift the origin with
# rink_dimensions() to match a data source that centres its coordinates.
# =============================================================================

#' Rink dimensions
#'
#' Named lists describing a rink's markings, in metres, with the origin at the
#' bottom-left corner of the boards: `x` runs along the length of the rink (left
#' end boards to right end boards) and `y` across its width. `IIHF` is the
#' standard 60 x 30 m international rink, `NHL` the 200 x 85 ft North American
#' rink converted to metres. Use [rink_dimensions()] to derive a variant.
#'
#' @format A named list. Geometry fields are in metres: `length`, `width`,
#'   `blue_line` and `end_line` (distance from the end boards), `faceoff_dot_x` /
#'   `faceoff_dot_y` (end-zone dot offsets from the corner), `nz_faceoff_dot`
#'   (neutral-zone dot offset from the blue line), `faceoff_circle` (radius), the
#'   hash-mark, crease and goal measures, and `corner_border_start_x` /
#'   `corner_border_start_y` (corner radii). `origin_x` / `origin_y` place the
#'   corner of the boards in data coordinates, `lines_small` / `lines_big` are the
#'   ggplot2 line widths for thin and thick markings, and `type` is `"IIHF"` or
#'   `"NHL"`.
#' @seealso [rink_dimensions()], [annotate_rink()], [rink_geometry()]
#' @keywords rink, dimensions
#' @examples
#' IIHF$length
#' NHL$length
#' @name rink_dims
NULL

#' @rdname rink_dims
#' @export
IIHF <- list(
  type = "IIHF",
  origin_x = 0,
  origin_y = 0,
  width = 30,
  length = 60,
  blue_line = 22.5,
  faceoff_circle = 4.5,
  faceoff_dot_x = 10,
  faceoff_dot_y = 8,
  nz_faceoff_dot = 1.5,
  faceoff_dot_size = 0.3,
  faceoff_dot_size_center = 0.15,
  lines_big = 2,
  lines_small = 1.25,
  end_line = 4,
  inner_hash_x = 0.6,
  inner_hash_y = 0.225,
  inner_hash_length_hor = 0.9,
  inner_hash_length_vert = 1.2,
  outer_hash_x = 0.08,
  outer_hash_y = 0.85,
  outer_hash_length = 0.6,
  goal_depth = 1,
  goal_width = 1.9,
  crease_width = 2.44,
  crease_height = 1.37,
  crease_ring_height = 0.46,
  corner_border_start_x = 8,
  corner_border_start_y = 7,
  center_spot_free_space = 0.5,
  faceoff_dot_white_radius = 0.2
)

#' @rdname rink_dims
#' @export
NHL <- list(
  type = "NHL",
  origin_x = 0,
  origin_y = 0,
  width = 85 * 0.3048,
  length = 200 * 0.3048,
  blue_line = 75 * 0.3048,
  faceoff_circle = 15 * 0.3048,
  faceoff_dot_x = 31 * 0.3048,
  faceoff_dot_y = 20.5 * 0.3048,
  nz_faceoff_dot = 5 * 0.3048,
  faceoff_dot_size = 0.3,
  faceoff_dot_size_center = 0.15,
  lines_big = 2,
  lines_small = 1.25,
  end_line = 11 * 0.3048,
  inner_hash_x = 25 / 12 * 0.3048,
  inner_hash_y = 10 / 12 * 0.3048,
  inner_hash_length_hor = 35 / 12 * 0.3048,
  inner_hash_length_vert = 47 / 12 * 0.3048,
  outer_hash_x = 14 / 12 * 0.3048,
  outer_hash_y = 69 / 12 * 0.3048,
  outer_hash_length = 2 * 0.3048,
  goal_depth = 40 / 12 * 0.3048,
  goal_width = 74.375 / 12 * 0.3048,
  crease_width = 8 * 0.3048,
  crease_height = 4.5 * 0.3048,
  crease_ring_height = 1.5 * 0.3048,
  corner_border_start_x = 27 * 0.3048,
  corner_border_start_y = 24 * 0.3048,
  center_spot_free_space = 2 * 0.3048,
  faceoff_dot_white_radius = 10 / 12 * 0.3048
)

#' Build or modify a set of rink dimensions
#'
#' Starts from an existing set of [rink dimensions][rink_dims] and overrides single
#' fields, checking that each override is a known field and a single finite number.
#' Typical uses: matching your data's coordinate frame (e.g. `origin_x = -30,
#' origin_y = -15` for centre-ice-origin IIHF data) or drawing a non-standard rink.
#' @param base The dimensions to start from, e.g. [IIHF] or [NHL].
#' @param ... Named fields to override, e.g. `width = 26`.
#' @return A named list of rink dimensions, usable wherever gghockey takes
#'   `dimensions`.
#' @seealso [rink_dims]
#' @keywords rink, dimensions
#' @examples
#' # IIHF rink with the origin at centre ice
#' centred <- rink_dimensions(IIHF, origin_x = -30, origin_y = -15)
#'
#' # A narrower 60 x 26 m rink
#' narrow <- rink_dimensions(IIHF, width = 26)
#' @export
rink_dimensions <- function(base = IIHF, ...) {
  .check_dimensions(base)
  overrides <- list(...)
  if (length(overrides) == 0) return(base)

  nms <- names(overrides)
  if (is.null(nms) || any(nms == ""))
    stop("all overrides in `...` must be named, e.g. `width = 26`", call. = FALSE)
  unknown <- setdiff(nms, names(base))
  if (length(unknown))
    stop(sprintf("Unknown rink dimension field(s): %s -- see ?rink_dims for the fields",
                 paste(unknown, collapse = ", ")), call. = FALSE)
  # `type` is the one character field; everything else is geometry or a line width.
  bad <- nms[!vapply(overrides, .is_number, logical(1)) & nms != "type"]
  if (length(bad))
    stop(sprintf("%s must be a single finite number", paste(bad, collapse = ", ")),
         call. = FALSE)

  out <- utils::modifyList(base, overrides)
  .check_dimensions(out)   # an override can still break the layout (e.g. blue line past centre)
  out
}

.is_number <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)

# Readable argument checks in place of stopifnot() (whose "x > 0 is not TRUE" doesn't
# say which argument or what's allowed). `ok` is only evaluated once `x` is known to
# be a number, so a range test like `x > 0` never runs on NULL / a string.
.check_number <- function(x, arg, ok = TRUE, what = "a single finite number") {
  if (!.is_number(x) || !isTRUE(ok))
    stop(sprintf("`%s` must be %s, got: %s", arg, what,
                 if (is.null(x)) "NULL" else paste(utils::head(format(x), 3), collapse = ", ")),
         call. = FALSE)
  invisible(x)
}

# Fail EARLY, with a readable message, on anything that isn't a full set of rink
# dimensions or whose markings can't fit on the rink -- otherwise the error surfaces
# deep inside a ggplot build as a cryptic NA / polygon complaint.
.check_dimensions <- function(dimensions, arg = "dimensions") {
  required <- setdiff(names(IIHF), "type")
  missing <- setdiff(required, names(dimensions))
  if (!is.list(dimensions) || length(missing))
    stop(sprintf("`%s` must be a set of rink dimensions, such as `IIHF` or `NHL`%s", arg,
                 if (length(missing)) paste0(" (missing: ", paste(missing, collapse = ", "), ")")
                 else ""), call. = FALSE)
  d <- dimensions
  ok <- d$length > 0 && d$width > 0 &&
    d$end_line < d$blue_line && d$blue_line < d$length / 2 &&
    d$corner_border_start_x <= d$length / 2 && d$corner_border_start_y <= d$width / 2
  if (!isTRUE(ok))
    stop(sprintf(paste0("`%s` is not a valid rink: need positive length/width, ",
                        "end_line < blue_line < length / 2 and corner radii within half the rink"),
                 arg), call. = FALSE)
  invisible(dimensions)
}
