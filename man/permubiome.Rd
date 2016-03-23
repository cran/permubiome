\name{permubiome}
\alias{permubiome}
\alias{Permutation test for microbiome data}
\docType{package}
\title{
\packageTitle{permubiome}
}
\description{
\packageDescription{permubiome}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{permubiome}
\packageIndices{permubiome}
The permubiome R package was created to perform permutation-based non-parametric analysis on microbiome data for biomaker discovery aims. This test executes thousands of comparisons in pairwise manner, after random shuffling of data into the different groups of study. Previous to the permutation test itself, data can be normalized according to different methods proposed to handle microbiome data ("proportions, "anders", and "paulson"). The median-based differences between groups resulting from the multiple simulations are fitted to normal distribution with the aim to calculate their significance. A multiple testing correction (fdr) is finally applied to extract the differentially presented features between groups of your dataset.
}
\author{
\packageAuthor{permubiome}

Maintainer: \packageMaintainer{permubiome}
}
\references{
Benitez-Paez A. & Sanz Y. (2016). Permubiome: an R package to perform permutation based test for biomarker discovery in microbiome analyses. In press.
}

\keyword{ permubiome }
\examples{
get.data()
normalize(numz = 0.5, method = 1)
permutation(nperm = 1000, write.output = TRUE)
plots()
}
