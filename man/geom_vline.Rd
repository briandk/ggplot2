\name{geom_vline}
\alias{geom_vline}
\title{Line, vertical.}

\description{
  Line, vertical.
}

\details{
  This geom allows you to annotate the plot with vertical
  lines (see \code{\link{geom_hline}} and
  \code{\link{geom_abline}} for other types of lines.

  There are two ways to use it.  You can either specify the
  intercept of the line in the call to the geom, in which
  case the line will be in the same position in every
  panel.  Alternatively, you can supply a different
  intercept for each panel using a data.frame.  See the
  examples for the differences.
}
\seealso{\code{\link{geom_hline}} for horizontal lines,
\code{\link{geom_abline}} for lines defined by a slope and intercept,
\code{\link{geom_segment}} for a more general approach"}
\examples{# Fixed lines
p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + geom_vline(xintercept = 5)
p + geom_vline(xintercept = 1:5)
p + geom_vline(xintercept = 1:5, colour="green")

last_plot() + coord_equal()
last_plot() + coord_flip()

p2 <- p + aes(colour = factor(cyl))
p2 + geom_vline(xintercept = 15)}