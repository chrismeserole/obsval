\name{studentVote}
\alias{studentVote}
\docType{data}
\title{
    A survey dataset
}
\description{
    A survey of college students about support for Iraq war
}
\usage{data("studentVote")}
\format{
  A data frame with 1146 observations on the following 6 variables.
  \describe{
    \item{\code{partyid}}{a factor with levels \code{strong democrat} \code{weak democract} \code{independent democrat} \code{independent independent} \code{independent republican} \code{weak republican} \code{strong republican} \code{other party} \code{apolitical}}
    \item{\code{pstrngth}}{a factor with levels \code{other party} \code{independent or apolitical} \code{independent leaner} \code{weak partisan} \code{strong partisan}}
    \item{\code{female}}{a numeric vector}
    \item{\code{pid}}{a factor with levels \code{strong democrat} \code{weak democract} \code{independent democrat} \code{independent independent} \code{independent republican} \code{weak republican} \code{strong republican} \code{other party} \code{apolitical}}
    \item{\code{warsup}}{a factor with levels \code{strongly support} \code{somewhat support} \code{somewhat oppose} \code{strongly oppose}}
    \item{\code{binary_warsup}}{a factor with levels \code{support} \code{oppose}}

  }
}
\details{
%%  ~~ If necessary, more details than the __description._ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(studentVote)
## maybe str(studentVote) ; plot(studentVote) ...
}
\keyword{datasets}
