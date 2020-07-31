\name{plots}
\alias{plots}
\title{
Plotting the features with differential abundance.
}
\description{
Option to plot individually all features found to be differentially presented in the classes of your dataset.
}
\usage{
plots()
}
\details{
When executed, the name of the feature as well as the different output options will be prompted.
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
function () 
{
    Class <- NULL
    non_zero <- NULL
    Occurring <- NULL
    prevalence <- NULL
    loadNamespace("ggplot2")
    load("permubiome.RData")
    df_norm <- df_norm
    category <- readline("Type the category you want to plot : ")
    if (category == "") {
        category <- colnames(df_norm[3])
        print(paste("As you declared no categories, the very first one of your 
        dataset is plotted!"))
    }
    df_to_plot <- df_norm[, c("Sample", "Class", category)]
    classes <- levels(df_to_plot$Class)
    if (REFERENCE == "") {
        REFERENCE <- classes[1]
    }
    else if (REFERENCE == classes[2]) {
        classes[2] <- classes[1]
        classes[1] <- REFERENCE
    }
    df_to_plot$ref <- factor(df_to_plot$Class, levels = (c(classes[1], 
        classes[2])))
    p1 <- (ggplot(df_to_plot, aes(df_to_plot$ref, df_to_plot[, 
        category], fill = df_to_plot$Class), environment = environment())) + 
        geom_boxplot(notch = F, outlier.colour = "blue", outlier.shape = 1, 
            outlier.size = 3) + geom_point(colour = "#000000", 
        size = 2.5, pch = 19) + scale_fill_manual(values = c("#E41A1C", 
        "#377EB8")) + ggtitle(category) + theme(plot.title = element_text(size = 24, 
        face = "bold")) + ylab("Normalized read proportion") + 
        xlab("Classes") + theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 16, face = "bold")) + 
        coord_flip() + guides(fill = FALSE) + theme(plot.margin = unit(c(0.25, 
        0.25, 0.25, 0.25), "cm"))
    non_zero <- as.data.frame((tapply(df_to_plot[[category]], 
        df_to_plot$ref, nnzero)))
    total <- as.data.frame((tapply(df_to_plot[[category]], df_to_plot$ref, 
        length)))
    prevalence_table <- data.frame(names = factor(c(classes[1], 
        classes[2]), levels = c(classes[1], classes[2])), Occurring = c(non_zero[, 
        1]), Subjects = c(total[, 1]))
    prevalence_table$prevalence <- (prevalence_table$Occurring/prevalence_table$Subjects) * 
        100
    p2 <- ggplot(prevalence_table, aes(x = names, y = prevalence, fill = names, 
        width = 0.75)) + geom_bar(stat = "identity", colour = "grey20") + 
        scale_fill_manual(values = c("#E41A1C", "#377EB8")) + 
        coord_flip() + guides(fill = FALSE) + ggtitle(paste("", 
        "", sep = " ")) + theme(plot.title = element_text(size = 24, 
        face = "bold")) + ylab("Prevalence (percentage)") + theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.text.x = element_text(size = 12), 
        axis.title = element_text(size = 16, face = "bold")) + 
        theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) + 
        ylim(0, 100)
    output <- readline("Do you want an output file (yes/no)? : ")
    if (substr(output, 1, 1) == "y") {
        extension <- readline("What extension do you prefer for the output plot 
        (ps, pdf, jpeg, tiff, png, bmp )? : ")
        tiff(filename = paste(category, extension, sep = "."), 
            width = 1000, height = 200, res = 100, units = "px")
        grid.arrange(p1, p2, ncol = 2)
        dev.off()
    }
    else {
        print(grid.arrange(p1, p2, ncol = 2))
    }
    save(df, df_norm, df_to_plot, REFERENCE, classes, file = "permubiome.RData")
  }
}
\keyword{ plotting }
\keyword{ boxplot }
