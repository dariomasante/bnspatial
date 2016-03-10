\name{importClasses}
\alias{importClasses}
\title{importClasses}
\usage{
importClasses(classFile)
}
\arguments{
\item{\code{classFile}}{character. A text file containing known nodes as inputs, formatted as follows: 
<nodename1>
<minimum (-Inf allowed)>, <intermediate boundaries>, <maximum(Inf allowed)>
<nodename2>
<minimum (-Inf allowed)>, <intermediate boundaries>, <maximum(Inf allowed)>
...}
}
\description{
This function returns a formatted classification of nodes classes from a formatted text file.
}
