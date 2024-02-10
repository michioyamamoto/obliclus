\name{analy.deriv}
\alias{analy.deriv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	%%  ~~function to do ... ~~
	Gradient of an objective function at a rotation matrix on the set of
	rotation matrices.
}
\description{
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
	This function gives the gradient of an objective function at a
	rotation matrix \code{T} on the set of rotation matrices.
}
\usage{
analy.deriv(A, T, cluster, info)
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
		which indicate a cluster where each variable is assigned.
	}
	\item{info}{
		The list including \code{N.cluster} the number of
		clusters, \code{N.var} and the value of an specific parameter to a
		rotation method, for example, the value of oblimin parameter for the
		method \code{oblimin}.
	}
}

% \details{
% 	%%  ~~ If necessary, more details than the description above ~~
% }

\value{
	The gradient matrix is returned.
}
%\references{
%% ~put references to the literature/web site here ~
%}
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
% \examples{
% ##---- Should be DIRECTLY executable !! ----
% ##-- ==>  Define data, use random,
% ##--	or do  help(data=index)  for the standard data sets.

% ## The function is currently defined as
% function (A, T, cluster, info) 
% {
%     alpha <- info$alpha
%     L <- A \%*\% solve(t(T))
%     U <- matrix(0, info$N.var, info$N.cluster)
%     for (i in 1:info$N.var) {
%         U[i, cluster[i]] <- 1
%     }
%     L2 <- L * L
%     one.vec <- rep(1, len = info$N.fac)
%     if (info$method == "oblimin") {
%         Gq <- L * (L2 \%*\% one.vec \%*\% t(one.vec) - U \%*\% solve(t(U) \%*\% 
%             U) \%*\% t(U) \%*\% L2)
%     }
%     else if (info$method == "geomin") {
%         delta <- info$geomin.par
%         k <- ncol(L)
%         p <- nrow(L)
%         L2 <- L^2 + delta
%         pro <- exp(rowSums(log(L2))/k)
%         Gq = (2/k) * (L/L2) * matrix(rep(pro, k), p) + L * (L2 - 
%             U \%*\% solve(t(U) \%*\% U) \%*\% t(U) \%*\% L2)
%     }
%     return(-t(t(L) \%*\% Gq \%*\% solve(T)))
%   }
% }
% % Add one or more standard keywords, see file 'KEYWORDS' in the
% % R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
