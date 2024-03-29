#' Extract substrings from a character vector used by bytes.
#' @param str input vector
#' @param start numeric
#' @param end numeric
#' @param bytes_type "Shift-JIS" or "Unicode"
#' "Shift-JIS": Every characters are counted by a rule of Shift-JIS, that is,
#' ASCII characters are counted as 1 byte and non-ASCII characters are counted as 2 bytes.
#' "Unicode": Every characters are counted by a rule of Unicode, that is,
#' ASCII characters are counted as 1 bytes and non-ASCII characters are counted as 2 bytes to 4 bytes.
#' @return a character of character vector
#' @examples
#' \dontrun{
#'   str_sub_bytes("abcdefg",3,6,"Unicode")
#' }
#' @export
str_sub_bytes <- function(str,start,end,bytes_type=c("Shift-JIS","Unicode")) {
  bytes_type <- match.arg(bytes_type)
  if (rlang::is_missing(start)) {
    start <- 1
  }

  chr <- str %>%
    stringr::str_split("")
  chr %>%
    map_chr(~{
      if (bytes_type=="Shift-JIS") {
        is_ascii <- stringi::stri_enc_isascii(.x)
        bytes <- as.numeric(!is_ascii)+1
      } else if (bytes_type=="Unicode") {
        bytes <- stringi::stri_numbytes(.x)
      }
      bytes_cumsum <- cumsum(bytes)
      names(bytes_cumsum) <- .x

      if (rlang::is_missing(end)) {
        end <- last(bytes_cumsum)
      }

      bytes_cumsum[bytes_cumsum>=start & bytes_cumsum<=end] %>%
        names() %>%
        str_flatten(collapse="")
    })
}
