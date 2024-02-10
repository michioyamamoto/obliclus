\name{alpha.optim}
\alias{alpha.optim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Optimal value of alpha in Gradient Projection algorithm.
}
\description{
	%% ~~ A concise (1-5 lines) description of what the function does. ~~
	This function gives a value of alpha which guarantees monotone
	decrease of the objective function in "obliclus".
}
\usage{
	alpha.optim(A, T, G, cluster, info, maxit = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
		The loading matrix for rotation.
	}
  \item{T}{
		The current value of rotation matrix.
	}
  \item{G}{
		The gradient of the objective function at \code{T} on the set of
		oblique rotation matrices.
	}
  \item{cluster}{
		The vector of cluster parameters which indicate a cluster where each
		variable is assigned.
	}
  \item{info}{
		The list including an initial value of alpha.
	}
  \item{maxit}{
		%%     ~~Describe \code{maxit} here~~
		The limit of the iteration of partial step modification for the
		value of alpha.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
	The value of alpha which is calculated by the partial step
	modification described in Jennrich (2002).
}

\references{
%% ~put references to the literature/web site here ~
Jennrich, R.I. (2012). A simple general method for oblique rotation.
\emph{Psychometrika}, 67, 7-20.
}

\author{
%%  ~~who you are~~
Michio Yamamoto\cr
	\email{yamamoto.michio.hus@osaka-u.ac.jp}
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	%% ~~objects to See Also as \code{\link{help}}, ~~~
	\code{obliclus}
}

