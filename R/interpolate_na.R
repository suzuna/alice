#' interpolate NA, only at the "head" and "tail" of a data.frame.
#' @param data a data.frame
#' @param cols <tidy-select> a column or columns
#' @param fn a interpolate function
#' @param ... passing to fn
#' @return a data.frame
#' @examples
#' \dontrun{
#'   df <- data.frame(aa=c(NA,NA,1,2,NA),ab=c(NA,2,3,NA,4),ac=c(1,2,NA,3,NA))
#'   interpolate_na(df,aa,zoo::na.approx)
#'   interpolate_na(df,c(aa,ab),zoo::na.approx)
#'   interpolate_na(df,everything(),zoo::na.spline)
#' }
#' @export
interpolate_na <- function(data,cols,fn,...) {
  fn <- rlang::as_function(fn)
  data <- data %>%
    dplyr::mutate(dplyr::across(!!!rlang::enquos(cols),~{
      idx_former <- max(cumsum(cumsum(is.na(.x))==1:length(.x)))
      rev_vec <- rev(.x)
      idx_latter_from_tail <- max(cumsum(cumsum(is.na(rev_vec))==1:length(rev_vec)))
      idx_latter <- length(rev_vec)-idx_latter_from_tail+1
      interpolated <- fn(.x[(idx_former+1):(idx_latter-1)],...)
      c(rep(NA,idx_former),interpolated,rep(NA,idx_latter_from_tail))
    }))
  return(data)
}
