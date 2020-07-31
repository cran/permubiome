\name{optimize}
\alias{optimize}
\title{
Optimization for detection of features with larger variation between classes
}
\description{
This function is the previous step to the permutation test and it optimizes the detection of features differentially distributed between classes. The intra- and inter-classes distances are calculated with log-transformed data and then a ratio test is done to maximize the variation between classes. This pre-processing penalizes the microbiome features with a larger intra-class and lower inter-class variation, which would interfere with the statistical estimations to executed during the permutation test.
}
\usage{
optimize()
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
    load("permubiome.RData")
    df_norm <- df_norm
    REFERENCE <- REFERENCE
    classes <- levels(df_norm$Class)
    if (REFERENCE == "") {
        REFERENCE <- classes[1]
    }
    else if (REFERENCE == classes[2]) {
        classes[2] <- classes[1]
        classes[1] <- REFERENCE
    }
    df_norm$Class <- factor(df$Class, levels = (c(classes[1], 
        classes[2])))
    group1 <- subset(df_norm, Class == classes[1])
    group2 <- subset(df_norm, Class == classes[2])
    categories <- colnames(df_norm)
    distance_matrix <- matrix(, nrow = ncol(df_norm) - 2, ncol = 7, 
        byrow = T)
    colnames(distance_matrix) <- c("Category", paste("SumDist ", 
        "[", classes[1], "]", sep = ""), paste("SumDist ", "[", 
        classes[2], "]", sep = ""), "ClassDist", "RatioDist", 
        "Delta-Log", "Z-score")
    for (i in 3:(ncol(group1))) {
        mydata1 <- group1[, i]
        sumdist1 <- log10(sum(abs(apply(combn(mydata1, 2), 2, 
            diff))))
        distance_matrix[(i - 2), 1] <- categories[i]
        distance_matrix[(i - 2), 2] <- sumdist1
    }
    for (j in 3:(ncol(group2))) {
        mydata2 <- group2[, j]
        sumdist2 <- log10(sum(abs(apply(combn(mydata2, 2), 2, 
            diff))))
        distance_matrix[(j - 2), 3] <- sumdist2
    }
    classes_matrix <- matrix(, nrow = ncol(group1) - 2, ncol = (nrow(group1) * 
        nrow(group2)), byrow = T)
    rownames(classes_matrix) <- colnames(group1[3:ncol(group1)])
    features <- colnames(group1)
    for (k in 3:(ncol(group1))) {
        classdist <- vector()
        for (l in 1:nrow(group1[k])) {
            classdist_tmp <- as.list(abs(group2[k] - group1[l, 
                k]))
            classdist <- c(classdist, classdist_tmp[[features[k]]])
        }
        classes_matrix[(k - 2), ] <- classdist
    }
    inter.class.dist <- as.list(rowSums(classes_matrix))
    for (m in 3:(ncol(group1))) {
        distance_matrix[(m - 2), 4] <- log10(inter.class.dist[[features[m]]])
    }
    distance_matrix[, 5] <- as.numeric(distance_matrix[, 4])/((as.numeric(distance_matrix[, 
        3])/as.numeric(distance_matrix[, 2])))
    distance_matrix[, 6] <- abs(as.numeric(distance_matrix[, 
        5]) - as.numeric(distance_matrix[, 4]))
    distance_matrix[, 7] <- (as.numeric(distance_matrix[, 6]) - 
        mean(as.numeric(distance_matrix[, 6])))/sd(as.numeric(distance_matrix[, 
        6]))
    selected_features <- subset(distance_matrix, abs(as.numeric(distance_matrix[, 
        6])) > quantile(as.numeric(distance_matrix[,6]),0.96))
    save(df, df_norm, REFERENCE, classes, distance_matrix, selected_features, 
        file = "permubiome.RData")
  }
}
\keyword{ optimization }
\keyword{ distance }
\keyword{ ratio-test }
\keyword{ variability }
\keyword{ maximization }
