% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_bindings.R
\name{set_distinct_by}
\alias{set_distinct_by}
\title{Set counts to be distinct by some grouping variable.}
\usage{
set_distinct_by(e, distinct_by)
}
\arguments{
\item{e}{A \code{count_layer/shift_layer} object}

\item{distinct_by}{Variable(s) to get the distinct data.}
}
\value{
The layer object with
}
\description{
In some situations, count summaries may want to see distinct counts by a
variable like subject. For example, the number of subjects in a population
who had a particular adverse event. \code{set_distinct_by} allows you to set
the by variables used to determine a distinct count.
}
\details{
When a \code{distinct_by} value is set, distinct counts will be used by
default. If you wish to combine distinct and not distinct counts, you can
choose which to display in your \code{f_str()} objects using \code{n},
\code{pct}, \code{distinct_n}, and \code{distinct_pct}. Additionally, denominators
may be presented using \code{total} and \code{distinct_total}
}
\examples{
#Load in pipe
library(magrittr)

tplyr_table(mtcars, gear) \%>\%
  add_layer(
    group_count(cyl) \%>\%
      set_distinct_by(carb)
  ) \%>\%
  build()
}
