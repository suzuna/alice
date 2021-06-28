#' fill NA which is "head" of a data.frame to other value.
#' @param data a data.frame
#' @param cols <tidy-select> a column or columns
#' @param value value which NA are replaced with
#' @return a data.frame
#' @examples
#' \dontrun{
#'   df <- data.frame(aa=c(NA,NA,1,2,NA),ab=c(NA,2,3,NA,4),ac=c(1,2,NA,3,NA))
#'   fill_head_na(df,aa,0)
#'   fill_head_na(df,c(aa,ab),0)
#'   fill_head_na(df,everything(),0)
#' }
#' @export
fill_head_na <- function(data,cols,value) {
  data <- data %>%
    dplyr::mutate(dplyr::across(!!!rlang::enquos(cols),
        ~dplyr::if_else(cumsum(is.na(.x))==dplyr::row_number(),value,.x)))
  return(data)
}
