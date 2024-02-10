\name{random.cluster.C}
\alias{random.cluster.C}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Random cluster parameter
}
\description{
	This function generates a cluster parameter vector randomly. The upper
	limit of the number of clusters is 13.
}
\usage{
random.cluster.C(info.to.C)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{info.to.C}{
		The vector consisting of the number of variables, the number of
		clusters, a real number 1, and the minimum number of variables each
		cluster should have.
	}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	The vector which includes a cluster number where each variable is assigned.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
	Michio Yammaoto\cr
	\email{yamamoto.michio.hus@osaka-u.ac.jp}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% ## The function is currently defined as
% function (info.to.C) 
% {
%     .Call("random_cluster_C", as.numeric(as.double(info.to.C)))
%   }
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
