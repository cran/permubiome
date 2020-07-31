\name{size.effect}
\alias{size.effect}
\title{
Executing estimation statistics based on bootstrap-coupled approach
}
\description{
Assessing the size effect on selected microbiome features found to be differentially abundant between classes. This analysis is based on the Data Analysis using Bootstrap-Coupled Estimation (dabestr) R package and gives you the option to create Gardner-Altman estimation plots individually all features found to be differentially presented in your dataset.
}
\usage{
size.effect(category = "", replicates = 5000, n.seed = 12345, 
paired = FALSE, plot.file = "tiff", id.pairs = NULL)
}
\arguments{
  \item{category}{
Name of the microbiome feature, which differential abundance between classes will be further explored. 
}
  \item{replicates}{
The number of bootstrap resamples that have to be generated. Integer, default 5000. 
}
  \item{n.seed}{
This specifies the seed used to set the random number generator. Setting a seed ensures that the bootstrap confidence intervals for the same data will remain stable over separate runs/calls of this function. Integer, default 12345. 
}
  \item{paired}{
If TRUE, the two groups are treated as paired samples, please add an extra column (id.pairs) to parse identity of the datapoint. Default FALSE, the control_group group is treated as pre-intervention and the test_group group is considered post-intervention. 
}
  \item{plot.file}{
Extension for plot graphics (ps, pdf, jpeg, tiff, png, bmp). Default "tiff".
}
  \item{id.pairs}{
Column name for information to parse identity of the datapoint in case of paired data.
}
}
\details{
Be careful to type the "category" correctly to be analyzed in order to that matches with the table contained information.
}
\references{
Benitez-Paez A. 2020. Permubiome: an R package to perform permutation based test for biomarker discovery in microbiome analyses. [https://cran.r-project.org].
Benitez-Paez A, et al. mSystems. 2020;5:e00857-19. doi: 10.1128/mSystems.00857-19.
}
\author{
Alfonso Benitez-Paez
}
\examples{
## The function is currently defined as
function (category = "", replicates = 5000, n.seed = 12345, 
    paired = FALSE, plot.file = "tiff", id.pairs = NULL)
{
    Class <- NULL
    ref <- NULL
    loadNamespace("dabestr")
    loadNamespace("rlang")
    load("permubiome.RData")
    df_norm <- df_norm
    if (paired == TRUE) {
        print(paste("You declared paired data, be sure to include the correct -id.column- argument 
	to parse the identity of the datapoint!"))
    }
    classes <- levels(df_norm$Class)
    if (REFERENCE == "") {
        REFERENCE <- classes[1]
    }
    else if (REFERENCE == classes[2]) {
        classes[2] <- classes[1]
        classes[1] <- REFERENCE
    }
    prepare.stats <- dabest(df_norm, Class, category, paired = paired, 
        idx = c(classes[1], classes[2]), id.column = id.pairs)
    prepare.stats$y<-quo_set_expr(prepare.stats$y, as.symbol(category))
    print(prepare.stats)
    if (category == "") {
        category <- colnames(df_norm[3])
        print(paste("As you declared no categories, the very first one of your dataset will be 
	processed!"))
    }
    estimation.stats<-median_diff(prepare.stats, ci = 95, reps = replicates, 
        seed = n.seed)
    e_plot <- plot(estimation.stats, group.summaries = "median_quartiles", 
        palette = "Set1", rawplot.ylabel = paste(category, "normalized reads", 
            sep = " "), tick.fontsize = 12, axes.title.fontsize = 18)
    tiff(filename = paste(category, "estimation", plot.file, sep = "."), 
        width = 650, height = 600, res = 100, units = "px")
    e_plot
    dev.off()
    print(e_plot)
    save(df, df_norm, REFERENCE, classes, file = "permubiome.RData")
  }
}
\keyword{ plotting }
\keyword{ estimation statistics }
\keyword{ bootstrap }
