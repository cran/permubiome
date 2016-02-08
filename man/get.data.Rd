\name{get.data}
\alias{get.data}
\title{
Parsing the data file.
}
\description{
This function prompts for the file contained all the data needed to process. You only have to execute this function in the working directory where your file is stored properly formatted as requested.

The input file is a tab-delimited text matrix as follows:

\preformatted{
Sample	Class	feature(1)	feature(2)	feature(n)	...
sampleA	classX	counts(A1)	counts(A2)	counts(An)	...
sampleB	classY	counts(B1)	counts(B2)	counts(Bn)	...
sampleC	classX	counts(C1)	counts(C2)	counts(Cn)	...
sampleD	classY	counts(D1)	counts(D2)	counts(Dn)	...
}
}
\usage{
get.data()
}
\references{
Benitez-Paez A.	& Sanz Y. (2015). Permubiome: an R package to perform permutation based test for biomarker discovery in microbiome analyses. In press.
}
\author{
Alfonso Benitez-Paez
}
\examples{
## The function is currently defined as
function () 
{
    DATA <- readline("Type the name of your data set : ")
    if (DATA == "") {
        tb <- read.table(system.file("extdat", "DATA_1", package = "permubiome"), 
            header = T, sep = "\t")
        print(paste("As you declare no input file, the permubiome test data was loaded"))
    }
    else {
        tb <- read.table(DATA, header = T, sep = "\t")
    }
    df <- as.data.frame(tb)
    classes <- levels(df$Class)
    samples <- nrow(df)
    print(paste("Your data file contains:", samples, "samples"))
    print(paste("The classes in your data file are:", classes[1], 
        "and", classes[2]))
    print(paste("The number of different categories to compare are:", 
        (ncol(tb) - 2)))
    save(df, file = "permubiome.RData")
  }
}
\keyword{ input }
\keyword{ dataset }
