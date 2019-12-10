\name{size.effect}
\alias{size.effect}
\title{
Executing estimation statistics based on bootstrap-coupled approach
}
\description{
Assessing the size effect on selected microbiome features found to be differentially abundant between classes. This analysis is based on the Data Analysis using Bootstrap-Coupled Estimation (dabestr) R package and gives you the option to create Gardner-Altman estimation plots individually all features found to be differentially presented in your dataset.
}
\usage{
size.effect(category = "", replicates = 5000, n.seed = 12345, paired = F, 
plot.file = "tiff")
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
If TRUE, the two groups are treated as paired samples. The control_group group is treated as pre-intervention and the test_group group is considered post-intervention. Default FALSE.
}
  \item{plot.file}{
Extension for plot graphics (ps, pdf, jpeg, tiff, png, bmp). Default "tiff".
}
}
\details{
Be careful to type the "category" correctly to be analyzed in order to that matches with the table contained information.
}
\references{
Benitez-Paez A. 2019. Permubiome: an R package to perform permutation based test for biomarker discovery in microbiome analyses. [https://cran.r-project.org]
}
\author{
Alfonso Benitez-Paez
}
\examples{
## The function is currently defined as
function (category = "", replicates = 5000, n.seed = 12345, paired = F, 
    plot.file = "tiff") 
{
    Class <- NULL
    ref <- NULL
    loadNamespace("dabestr")
    load("permubiome.RData")
    df_norm <- df_norm
    if (category == "") {
        category <- colnames(df_norm[3])
        print(paste("As you declared no categories, the very first one of your dataset 
        will be processed!"))
    }
    class_to_estimate <- df_norm[, c("Sample", "Class", category)]
    classes <- levels(class_to_estimate$Class)
    if (REFERENCE == "") {
        REFERENCE <- classes[1]
    }
    else if (REFERENCE == classes[2]) {
        classes[2] <- classes[1]
        classes[1] <- REFERENCE
    }
    names(class_to_estimate)[3] <- "category"
    class_to_estimate$ref <- factor(class_to_estimate$Class, 
        levels = (c(classes[1], classes[2])))
    estimation.stats <- dabest(class_to_estimate, ref, category, 
        paired = paired, ci = 95, reps = replicates, func = median, 
        seed = n.seed, idx = c(classes[1], classes[2]))
    print(estimation.stats)
    e_plot <- plot(estimation.stats, group.summaries = "median_quartiles", 
        palette = "Set1", rawplot.ylabel = paste(category, "normalized reads", 
            sep = " "), tick.fontsize = 14, axes.title.fontsize = 22)
    tiff(filename = paste(category, "estimation_plot.tiff", sep = "."), 
        width = 650, height = 600, res = 100, units = "px")
    e_plot
    dev.off()
    print(e_plot)
    save(df, df_norm, class_to_estimate, REFERENCE, classes, 
        file = "permubiome.RData")
  }
}
\keyword{ plotting }
\keyword{ estimation statistics }
\keyword{ bootstrap }
