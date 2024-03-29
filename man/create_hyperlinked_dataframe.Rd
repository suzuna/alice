% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_hyperlinked_dataframe.R
\name{create_hyperlinked_dataframe}
\alias{create_hyperlinked_dataframe}
\title{Create a data.frame which have a "hyperlinked" column for knitr::kable and DT::datatable.}
\usage{
create_hyperlinked_dataframe(
  data,
  name,
  link,
  new_col,
  format,
  DT_newtab = TRUE
)
}
\arguments{
\item{data:}{a data.frame.}

\item{name:}{<tidy-select> a name of a column which have text you want to hyperlinked.}

\item{link:}{<tidy-select> a name of a column which have hyperlink (URL).}

\item{new_col:}{<tidy-select> a name of a column, "name" with "link", which you want to create.}

\item{format:}{"kable" or "DT".}

\item{DT_newtab:}{logical. (needs when format = "DT") If format = "DT", you can select whether opening a new tab when "new_col" is clicked.}
}
\value{
a data.frame which has a knitr::kable-styled or DT::datatable-styled hyperlink column.
}
\description{
Create a data.frame which have a "hyperlinked" column for knitr::kable and DT::datatable.
}
\examples{
\dontrun{
df <- data.frame(
  manufacturer = c("Toyota", "Nissan", "Honda"),
  url = c("https://toyota.jp/","http://www.nissan.co.jp/","https://www.honda.co.jp/")
)
create_hyperlinked_dataframe(df,manufacturer,url,url_hyperlinked,"kable")
create_hyperlinked_dataframe(df,manufacturer,url,url_hyperlinked,"DT",TRUE)
}
}
