#' Team colour palettes
#'
#' `team_palette()` builds a palette from your own team colours (gghockey ships
#' none) and `scale_colour_team()` / `scale_fill_team()` apply it, matching
#' teams by name. Each team has a primary and, optionally, a secondary colour;
#' a missing secondary falls back to the primary.
#'
#' @param team Team names (or ids), as they appear in your data.
#' @param primary Primary colours, one per team: any colour R understands
#'   (hex codes like `"#C8102E"` or names like `"navy"`).
#' @param secondary Optional secondary colours, one per team (`NA` to fall back
#'   to the primary).
#' @param palette A palette from `team_palette()`.
#' @param which `"primary"` or `"secondary"` colours.
#' @param ... Passed to [ggplot2::scale_colour_manual()] /
#'   [ggplot2::scale_fill_manual()], e.g. `name` or `na.value`.
#'
#' @return `team_palette()` returns a `gghockey_palette` data frame with
#'   columns `team`, `primary` and `secondary`. The scales return ggplot2
#'   scales.
#' @examples
#' pal <- team_palette(
#'   team = c("Lakeside", "Northfield"),
#'   primary = c("#0B3D91", "#C8102E"),
#'   secondary = c("#F2A900", NA)
#' )
#' pal
#'
#' library(ggplot2)
#' ggplot(shots, aes(x, y, colour = team)) +
#'   annotate_rink() +
#'   geom_point(alpha = 0.5) +
#'   scale_colour_team(pal)
#'
#' # Turn a named vector into a palette
#' cols <- c(Lakeside = "#0B3D91", Northfield = "#C8102E")
#' team_palette(names(cols), cols)
#' @export
team_palette <- function(team, primary, secondary = NULL) {
  team <- as.character(team)
  primary <- unname(as.character(primary))
  if (length(team) == 0 || length(team) != length(primary)) {
    rlang::abort("`team` and `primary` must be non-empty and the same length.")
  }
  if (anyNA(team) || anyDuplicated(team)) {
    rlang::abort(c(
      "`team` must be unique and not `NA`.",
      x = if (anyDuplicated(team)) paste("Duplicated:", paste(unique(team[duplicated(team)]), collapse = ", "))
    ))
  }
  if (is.null(secondary)) secondary <- rep(NA_character_, length(team))
  secondary <- unname(as.character(secondary))
  if (length(secondary) != length(team)) {
    rlang::abort("`secondary` must be the same length as `team`.")
  }
  check_colours(primary, team, "primary", allow_na = FALSE)
  check_colours(secondary, team, "secondary", allow_na = TRUE)

  structure(
    data.frame(team = team, primary = primary,
               secondary = ifelse(is.na(secondary), primary, secondary),
               stringsAsFactors = FALSE),
    class = c("gghockey_palette", "data.frame")
  )
}

check_colours <- function(cols, team, arg, allow_na) {
  bad <- vapply(cols, function(cl) {
    if (is.na(cl)) return(!allow_na)
    inherits(tryCatch(grDevices::col2rgb(cl), error = identity), "error")
  }, logical(1))
  if (any(bad)) {
    rlang::abort(c(
      paste0("Invalid `", arg, "` colour(s)."),
      x = paste0(team[bad], ": \"", cols[bad], "\"", collapse = ", ")
    ))
  }
}

#' @rdname team_palette
#' @export
scale_colour_team <- function(palette, which = c("primary", "secondary"), ...) {
  ggplot2::scale_colour_manual(values = team_values(palette, which), ...)
}

#' @rdname team_palette
#' @export
scale_color_team <- scale_colour_team

#' @rdname team_palette
#' @export
scale_fill_team <- function(palette, which = c("primary", "secondary"), ...) {
  ggplot2::scale_fill_manual(values = team_values(palette, which), ...)
}

team_values <- function(palette, which) {
  if (!inherits(palette, "gghockey_palette")) {
    rlang::abort("`palette` must be built with `team_palette()`.")
  }
  which <- rlang::arg_match(which, c("primary", "secondary"))
  stats::setNames(palette[[which]], palette$team)
}

#' @export
print.gghockey_palette <- function(x, ...) {
  cat("<gghockey_palette> ", nrow(x), " team", if (nrow(x) != 1) "s", "\n", sep = "")
  print(as.data.frame(unclass(x), stringsAsFactors = FALSE), row.names = FALSE)
  invisible(x)
}
