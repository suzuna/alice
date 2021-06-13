#' Create a data.frame which have a "hyperlinked" column for knitr::kable and DT::datatable.
#' @param data: a data.frame.
#' @param name: <tidy-select> a name of a column which have text you want to hyperlinked.
#' @param link: <tidy-select> a name of a column which have hyperlink (URL).
#' @param new_col: <tidy-select> a name of a column, "name" with "link", which you want to create.
#' @param format: "kable" or "DT".
#' @param DT_newtab: logical. (needs when format = "DT") If format = "DT", you can select whether opening a new tab when "new_col" is clicked.
#' @return a data.frame which has a knitr::kable-styled or DT::datatable-styled hyperlink column.
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   manufacturer = c("Toyota", "Nissan", "Honda"),
#'   url = c("https://toyota.jp/","http://www.nissan.co.jp/","https://www.honda.co.jp/")
#' )
#' create_hyperlinked_dataframe(df,manufacturer,url,url_hyperlinked,"kable")
#' create_hyperlinked_dataframe(df,manufacturer,url,url_hyperlinked,"DT",TRUE)
#' }
#' @export
create_hyperlinked_dataframe <- function(data,name,link,new_col,format,DT_newtab=TRUE){
  name_enquoed <- rlang::enquo(name)
  link_enquoed <- rlang::enquo(link)
  new_col_enquoed <- rlang::enquo(new_col)

  if (format=="kable"){
    data <- data %>%
      dplyr::mutate(!!new_col_enquoed:=purrr::map2(!!name_enquoed,!!link_enquoed,function(x,y){
        stringr::str_glue("[{x}]({y})")
      }))
  } else if (format=="DT" & DT_newtab){
    data <- data %>%
      dplyr::mutate(!!new_col_enquoed:=purrr::map2(!!name_enquoed,!!link_enquoed,function(x,y){
        stringr::str_glue("<a href='{y}' target='_blank' rel='noopener noreferrer'>{x}</a>")
      }))
  } else if (format=="DT" & !DT_newtab){
    data <- data %>%
      dplyr::mutate(!!new_col_enquoed:=purrr::map2(!!name_enquoed,!!link_enquoed,function(x,y){
        stringr::str_glue("<a href='{y}'>{x}</a>")
      }))
  }

  return(data)
}
