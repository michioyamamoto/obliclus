\name{obliclus}
\alias{obliclus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	A cluster-based factor rotation
}
\description{
	This function conducts rotations of loading matrix based on cluster
	structure of variables. It can be used for two ways. One is to find
	simple and well-grouped structure simultaneously, and the other is to
	find simple structure based on the prior information on cluster
	structure. The objective function is optimized using the Gradient
	Projection (GP) algorithm and k-means algorithm alternately.
}
\usage{
obliclus(A, cluster = NULL, normalize = TRUE, N.random = 10,
         fixed = FALSE, N.cluster = NULL, ini.cluster = NULL,
         maxit = 1000, alpha = 1, method = "oblimin",
         geomin.par = 0.01, oblimin.index = 0, fit.cr = 1e-04)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
		The loading matrix for rotation.
	}
  \item{cluster}{
		The vector of cluster parameters which indicate a cluster where each
		variable is assigned.
	}
  \item{normalize}{
		If \code{"TRUE"}, Kaiser's normalization is applied.
	}
  \item{N.random}{
		The number of sets of random initial values of rotatin matrix \code{T}.
	}
  \item{fixed}{
		If \code{"TRUE"}, \code{obliclus} gives an optimal loading matrix
		based on cluster structure \code{cluster}.
	}
  \item{N.cluster}{
		The number of clusters. If this is null, the number of factors is
		used for the number of clusters.
	}
  \item{ini.cluster}{
		The vector of initial cluster parameters.
	}
  \item{maxit}{
		The upper limit of iteration of the algorithm.
	}
  \item{alpha}{
		The initial value of \code{alpha} which is a step size in the GP algorithm.
	}
  \item{method}{
		For "oblimin", \code{obliclus} conducts rotation whose objective
		function includes \code{oblimin} with \code{oblimin.index} and the
		measure of cluster similarity among variables. Similarily, for
		"geomin", \code{obliclus} conducts rotation whose objective functin
		includes \code{geomin} with \code{geomin.par} and the measure of
		cluster similarity.
	}
  \item{geomin.par}{
		The parameter for geomin rotation. The default is 0.01.
	}
  \item{oblimin.index}{
		The parameter for oblimin rotation. The default is 0, which imply
		the Quartimin rotation.
	}
  \item{fit.cr}{
		The criterion for convergence of algorithm, abs(criteria.new - criteria.old).
	}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
	\item{A}{Rotated loading matrix.}
	\item{T}{Estimated rotation matrix.}
	\item{E}{Estimated correlation matrix among factors.}
	\item{conv}{If \code{"TRUE"}, the algorithm converges.}
	\item{cluster}{Estimated cluster parameter.}
	\item{cr}{The value of objective function finally attained.}
}
\references{
	Yamamoto, M. and Jennrich, R. I. (2013). A cluster-based factor rotation. \emph{British Journal of Mathematical and Statistical Psychology}, 66: 488-502.	
}
\author{
	Michio Yamamoto\cr
	\email{yamamoto.michio.hus@osaka-u.ac.jp}
}
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
##loading matrix for rotation (Perfect simple structure)
##Find optimal T and U (cluster parameter) simultaneouly
A <- matrix(c(.8, 0, 0,
              .7, 0, 0,
               0,.8, 0,
               0,.7, 0,
               0, 0,.8,
               0, 0,.7), 6, 3, byrow=TRUE)

(out <- obliclus(A))


##loading matrix for rotation (More complex structure)
##Find optimal T based on the information on cluster structure
A <- matrix(c(   1,   0,   0,
                 0,   1,   0,
                 0,   0,   1,
               0.9, 0.6,-0.3,
              -0.3, 0.9, 0.6,
               0.6,-0.3, 0.9) ,6,3,byrow=TRUE)
cluster <- c(1,2,3,4,4,4)
(out <- obliclus(A, cluster=cluster, fixed=TRUE, N.cluster=4))
}
