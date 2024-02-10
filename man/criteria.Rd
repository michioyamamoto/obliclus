\name{criteria}
\alias{criteria}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	%%  ~~function to do ... ~~
	The value of the objective function which is minimized.
}
\description{
	The value of the objective function of Obliclus is calculated. In
	\code{criteria}, two different rotation criteria, Oblimin and Geomin,
	can be calculated in the objective function of Obliclus.
}
\usage{
criteria(A, T, cluster, info)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
		The loading matrix for rotation.
	}
  \item{T}{
		The current value of rotation matrix.
	}
  \item{cluster}{
		The vector of cluster parameters which indicate a cluster where each
		variable is assigned.
	}
  \item{info}{
		The list including \code{N.cluster} the number of
		clusters, \code{N.var} the number of observed variables,
		\code{method} a rotation method whose criterion is included in an
		objective function, and the value of an specific parameter to a
		rotation method, for example, the value of oblimin parameter for the
		method \code{oblimin}.
	}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
	The value of the objective function given \code{A}, \code{T}, and
	\code{cluster}.
}
\references{
	Yamamoto, M. and Jennrich, R. I. (2013). A cluster-based factor rotation. \emph{British Journal of Mathematical and Statistical Psychology}, 66: 488-502.	
}
\author{
	Michio Yamamoto\cr
	\email{yamamoto.michio.hus@osaka-u.ac.jp}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{obliclus}
}
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% ## The function is currently defined as
% function (A, T, cluster, info) 
% {
%     w <- info$oblimin.index
%     N.var <- dim(A)[1]
%     N.fac <- dim(A)[2]
%     N.cluster <- info$N.cluster
%     L <- A \%*\% solve(t(T))
%     cr.l <- cr.r <- 0
%     if (info$method == "oblimin") {
%         Q <- matrix(0, N.fac, N.fac)
%         for (i in 1:N.fac) {
%             for (j in 1:N.fac) {
%                 Q[i, j] <- sum(L[, i]^2 * L[, j]^2) - w/N.var * 
%                   sum(L[, i]^2) * sum(L[, j]^2)
%             }
%         }
%         diag(Q) <- 0
%         cr.l <- sum(Q)
%     }
%     else if (info$method == "geomin") {
%         k <- ncol(A)
%         p <- nrow(A)
%         delta <- info$geomin.par
%         L2 <- L^2 + delta
%         pro <- exp(rowSums(log(L2))/k)
%         cr.l <- sum(pro)
%     }
%     L2.mean <- matrix(0, N.cluster, N.fac)
%     for (j in 1:N.var) L2.mean[cluster[j], ] <- L2.mean[cluster[j], 
%         ] + L[j, ]^2
%     N.clust.in <- numeric(N.cluster)
%     for (k in 1:N.cluster) N.clust.in[k] <- length(which(cluster == 
%         k))
%     L2.mean <- L2.mean/N.clust.in
%     for (j in 1:N.var) cr.r <- cr.r + sum((L[j, ]^2 - L2.mean[cluster[j], 
%         ])^2)
%     out <- (cr.l + cr.r)/4
%     return(out)
%   }
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
