\name{CEPATheme}
\alias{CEPATheme}
\title{CEPA Theme}
\usage{
CEPATheme()
}
\description{
Creates the CEPA theme for use with ggplot2.

CEPA theme colours are:

#002776 = CEPA dark blue

#5A85D7 = CEPA light blue

#F7403A = CEPA fire

#FFA02F = CEPA orange

#F3CF45 = CEPA yellow

#BED600 = CEPA lime

#7765A0 = CEPA grape

#009AA6 = CEPA teal

#65cfe9 = CEPA sky

#c9cac8 = CEPA clay

}
\examples{
library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Width, y = Petal.Length, color = Species)) +
geom_point() + CEPATheme()

}
