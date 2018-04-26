#
# Metrics computations for IR effectiveness
#
# This package has routines for computing metrics in the "weighted precision"
# family, which includes e.g. precision (P), AP, RBP, DCG, and INST.
#
# (See DESCRIPTION for author, licence, etc.)
#

# We use the formulation of Moffat et al., CIKM 2013, and treat each metric as
# being defined by three vectors which in turn describe a user model.
# - "C" is the continuation vector. If a user is looking at rank i, C[[i]] is
# the chance they'll continue on to rank i+1. NB these are *conditional*
# probabilities.
# - "L" is the last vector; L[[i]] is the probability that rank i is the last
# one (deepest one) a user looks at.
# - "W" is the weight vector; W[[i]] can be interpreted as the probability of
# the user looking at rank i at any random moment. Gives the weight to apply to
# each rank. The final metric is always Wr , where r is a vector of relevance
# (or gain, more generally).
#
# Each of W, C, L can be derived given just one of them, and any one completely
# defines a weighted-precision metric. We use C as it's generally easy to
# describe/reason about.

#
# Convert a C (continue) vector to a W (weight) vector
#
#' @importFrom utils head tail
Cvec.to.Wvec <- function(Cvec) {
  C.prod <- c(1, cumprod(head(Cvec, -1)))
  W1 <- 1 / sum(C.prod)
  W.tail <- W1 * tail(C.prod, -1)
  c(W1, W.tail)
}

#'Generic weighted-precision metric for IR effectiveness.
#'
#'\code{weighted.precision} takes a metric as defined by a \code{C} (continue)
#'vector, a vector \code{doc.gain} of gains from documents (typically 0..1 for
#'each), and returns the value of the metric.
#'
#'This function can be used to calculate any number of metrics in the "weighted
#'precision" family, which includes precision (P@@k), AP, RBP, MRR, and INST. A
#'metric is entirely defined by a vector \code{C}, where each element
#'\code{C[i]} gives the probability of a user who is looking at rank \code{i}
#'continuing on to rank \code{i+1}.
#'
#'For details of this approach, see Moffat et al., "Users Versus Models: What
#'Observation Tells Us About Effectiveness Metrics", in Proc. Conf. Information
#'and Knowledge Management (CIKM) 2013.
#'
#'You may find the derived functions \code{\link{P}}, \code{\link{RBP}}, and
#'\code{\link{INST}} easier to use.
#'
#'@param C.fn A function which, given the arguments \code{R} and
#'  vector \code{i}, returns the \code{C} parameter defining the metric. See
#'  Moffat et al. for details (or see examples in this package such as
#'  \code{\link{C.fn.RBP}}).
#'
#'@param doc.gain A vector of gains from documents, typically each in the range
#'  0..1. Also sometimes known as \code{r} or the "relevance" vector.
#'
#'@param minN The minimum length of list (number of documents) for which the
#'  metric makes sense. \code{doc.gain} vectors shorter than this will be padded
#'  with 0.
#'
#'@export
weighted.precision <- function(C.fn, doc.gain, minN=1) {

  # length of list (of documents)
  N <- length(doc.gain)
  if (N < minN) {
    warning(paste0("Doc N (", N, ") is less than minimum expected (", minN, "). Padding with gain=0."))
    doc.gain <- c(doc.gain, rep(0, times=(minN-N)))
    N <- minN
  }

  # sanity
  if (any(doc.gain > 1)) {
    warning("Gains > 1, metric may not make sense.")
  }

  # get the C vector from cumulated relevance and rank...
  Cvec <- C.fn(R=doc.gain, i=1:N)
  # ...and that gives us the final discount (weight) curve...
  Wvec <- Cvec.to.Wvec(Cvec)
  # ...from which two vectors we get the final metric
  m <- list(metric=sum(Wvec*doc.gain),
            C=Cvec,
            W=Wvec,
            gain=doc.gain,
            cum.metric=cumsum(Wvec*doc.gain),
            i=1:length(Cvec))
  class(m) <- c("irmetric", "list")
  m
}

#
# Here are some handy C functions. Each takes two parameters: a vector of gain
# R and a rank i.
#

#' Continue functions for common IR metrics.
#'
#' These are helper functions which build the \code{C.fn} parameter for
#' \code{\link{weighted.precision}}.
#'
#' \code{weighted.precision} accepts not a \code{C} vector, but a function for
#' making one. This is because, for adaptive metrics, \code{C} will depend on
#' the gain (relevance) at each point down the ranked list and this relationship
#' can be arbitrarily complicated.
#'
#' \code{weighted.precision} calls the \code{C} function with two parameters:
#' \code{R} is a vector of relevance, or gain. \code{i} is the current depth in
#' the rank list, i.e. the length of R. \code{C} should be vectorised over both
#' arguments.
#'
#' The functions \code{C.fn.RBP} and \code{C.n.P} are parameterised in the same
#' way as the RBP and P metrics, by persistence \code{p} or cutoff \code{k}. The
#' function \code{C.fn.INST} is parameterised by \code{T}, the user's target
#' amount of relevance or gain.
#'
#' @param p "Persistence" parameter for RBP.
#' @param k Cutoff or depth, beyond which the metric is not evaluated.
#' @param T "Target" parameter for INST.
#'
#' @name continue.functions
NULL

#' @rdname continue.functions
#' @export
C.fn.RBP <- function(p) {
  # a fixed probability of continuing = p
  function(R, i) { rep(p, length(i)) }
}

#' @rdname continue.functions
#' @export
C.fn.P <- function(k) {
  # always continue past the first k-1, then stop
  function(R, i) { ifelse(i < k, 1, 0) }
}

#' @rdname continue.functions
#' @export
# INST is from Moffat et al., Australasian Document Computing Symposium 2015
C.fn.INST <- function(T) {
  # depends on rank and how much of what we expected has been seen so far
  function(R, i) {
    Ti <- T-cumsum(R)
    ((i+T+Ti-1) / (i+T+Ti)) ** 2
  }
}

#' @rdname continue.functions
#' @export
C.fn.RR <- function() {
  function(R, i) {
    # continue until we have a document with non-zero gain
    R[i]==0
  }
}

#' @rdname continue.functions
#' @export
C.fn.ERR <- function() {
  function(R, i) {
    1-R[i]
  }
}

#' @rdname continue.functions
#' @export
C.fn.SDCG <- function(k) {
  function(R, i) {
    ifelse(i < k, log2(i+1)/log2(i+2), 0)
  }
}

#' @rdname continue.functions
#' @importFrom utils tail
#' @export
# this formulation is from Moffat et al., CIKM'13
# TODO: check this!
C.fn.AP <- function() {
  function(R, i) {
    denominator <- sum(R/i) - prepend0(cumsum(R/i))
    numerator <- c(tail(denominator, -1), 0)
    ifelse(numerator>0, numerator/denominator, 0)
  }
}

#
# Calculate with a large tail of all zeros, then trim back to the same length as the input
#
pad.trim.metric <- function(C.fn, doc.gain, PADDING=1000) {
  doc.gain2 <- c(doc.gain, rep(0, PADDING))
  to.trim <- weighted.precision(C.fn, doc.gain2)

  # now trim back to size
  N <- length(doc.gain)
  for (field in names(to.trim)) {
    if (field != "metric") {
      to.trim[[field]] <- to.trim[[field]][1:N]
    }
  }
  to.trim$residual <- 1 - sum(to.trim$W)
  to.trim
}

#' IR effectiveness metrics in the "weighted precision" family.
#'
#' These common IR metrics take a vector of "gains", which represents the
#' quality (or relevance) of each item in a search engine's list of retrieved
#' documents, and return a measure of the effectiveness of that list.
#'
#' \code{RBP} is rank-biased precision, with persistence parameter p.
#'
#' \code{P} is precision at depth k.
#'
#' \code{RR} is reciprocal rank.
#'
#' \code{ERR} is expected (graded) reciprocal rank. (Chapelle et al., Proc
#' CIKM'09: https://doi.org/10.1145/1645953.1646033.)
#'
#' \code{SDCG} is (scaled) discounted cumulative gain at depth k, an
#' analogue of NDCG. (Moffat et al., TOIS 35(3):
#' https://doi.org/10.1145/3052768.)
#'
#' \code{AP} is average precision.
#'
#' \code{INST} is an adaptive and goal-sensitive metric which has different
#' behaviour depending whether the user is looking for few or many documents,
#' and depending on the quality of the list. (Moffat et al., TOIS 35(3):
#' https://doi.org/10.1145/3052768.)
#'
#' @param doc.gain A vector of gains from returned documents, typically (but not
#'   always) each in the range 0..1.
#' @param k For \code{P}, the cutoff rank.
#' @param p For \code{RBP}, the persistence parameter.
#' @param T For \code{INST}, the user's target (number of relevant documents, or
#'   total gain).
#'
#' @return All these metrics return a list with the following elements:
#' \describe{
#' \item{metric}{The metric value.}
#' \item{C}{The computed \code{C} vector: \code{C[[i]]} is the probability of
#' the user Continuing to read past rank \emph{i}.}
#' \item{W}{The computed vector of weights: the important placed on each rank
#' in the list.}
#' \item{gain}{The passed gains.}
#' \item{cum.metric}{The metric accumulated along the list.}
#' \item{i}{Ranks used (starting at 1).}
#' }
#'
#' For RBP and INST, there will be a further element \code{residual} which
#' is the total gain possible from unjudged documents: i.e., \code{metric+residual}
#' is an upper bound on effectiveness if all unjudged documents had gain=1.
#'
#' For details of this approach, see Moffat et al., "Users Versus Models: What
#'Observation Tells Us About Effectiveness Metrics", in Proc. Conf. Information
#'and Knowledge Management (CIKM) 2013.
#'
#' @name ir.metrics
#' @seealso \code{\link{IFT}}
NULL

#' @rdname ir.metrics
#' @examples
#' RBP(c(1, 1, 1, 0, 0), p=0.6)
#' # 0.8501041
#' @export
RBP <- function(doc.gain, p) { pad.trim.metric(C.fn.RBP(p), doc.gain) }

#' @rdname ir.metrics
#' @examples
#' P(c(1, 1, 1, 0, 0), k=1)
#' # 1
#' P(c(1, 1, 1, 0, 0), k=5)
#' # 0.6
#' @export
P <- function(doc.gain, k) { weighted.precision(C.fn.P(k), doc.gain, minN=k) }

#' @rdname ir.metrics
#' @examples
#' RR(c(0, 0, 0, 1))
#' # 0.25
#' @export
RR <- function(doc.gain) { weighted.precision(C.fn.RR(), doc.gain) }

#' @rdname ir.metrics
#' @examples
#' ERR(c(0, 1, 0))
#' # 0.5
#' ERR(c(0, .5, 0))
#' # 0.2
#' @export
ERR <- function(doc.gain) { weighted.precision(C.fn.ERR(), doc.gain) }

#' @rdname ir.metrics
#' @examples
#' SDCG(c(1, 0, 1, 0, 0), k=1)
#' # 1
#' SDCG(c(1, 0, 1, 0, 0), k=5)
#' # .5087403
#' @export
SDCG <- function(doc.gain, k) { weighted.precision(C.fn.SDCG(k), doc.gain, minN=k) }

#' @rdname ir.metrics
#' @examples
#' AP(c(1, 0, 1, 0, 0))
#' # 0.8333333
#' @export
AP <- function(doc.gain) { weighted.precision(C.fn.AP(), doc.gain) }

#' @rdname ir.metrics
#' @examples
#' INST(c(1, 1, 1, 0, 0), T=1)
#' # 0.9830949
#' @export
INST  <- function(doc.gain, T) { pad.trim.metric(C.fn.INST(T), doc.gain) }

#
# IFT needs special handling as it takes two vectors: gain (as above) but also
# cost.
#
C.fn.IFT <- function(A, target, rationality, b1=NA, intercept1=NA, b2=NA, intercept2=NA) {

  # we can specify either b or intercept -- they are equivalent
  if (missing(b1) || is.na(b1)) {
    if (missing(intercept1) || is.na(intercept1)) {
      stop("Need either b1 or intercept1")
    }
    # at rate=A, we should have C=1/(1+b); so solving gives b=(1/C)-1
    b1 <- (1/intercept1) - 1
  }

  if (missing(b2) || is.na(b2)) {
    if (missing(intercept2) || is.na(intercept2)) {
      b2 <- b1
    } else {
      b2 <- (1/intercept2) - 1
    }
  }

  # There are two parts to this. First, we consume something (a card or a document) and might be
  # satisfied; then, we if we're not satisfied, we choose whether or not to persist with the
  # next thing.
  # C1 is continuing past the first bit, C2 is continuing past the second.
  #
  # Both C1 and C2 depend on:
  # - A, the tolerated gain:cost ratio from the SERP.
  # - the gain:cost ratio so far.
  # So the "perfectly rational" thing to do is have C=1 if x>=A, and C=0 if x<A. With rationality=inf this should
  # (I hope) be a step function.
  # T is the target (how big is your stomach, how much do you want?), as for INST
  # Finally we have "intercept" which is the value we actually want for C when x=theta.

  function(R, k) {
    gain <- cumsum(R)   # total gain so far
    cost <- cumsum(k)   # total cost so far
    rate <- gain / cost # rate of gain so far

    # C1: have we had enough? (1-... as we want a continue function, not a stopping function)
    C1 <- 1 - (1 / (1 + b1*exp((-gain+target)*rationality)))

    # C2: are we tired of looking?
    C2 <- 1 / (1 + b2*exp((-rate+A)*rationality))

    list(C1=C1, C2=C2, C=C1*C2)
  }

}

#' IR effectiveness metric derived from information foraging theory.
#'
#' Note that unlike the other metrics in this package, \code{IFT} takes as
#' input both \emph{gain} and \emph{cost} vectors.
#'
#' For details, see Azzopardi et al., "Measuring the Utility of Search Engine Result
#' Pages", Proc. SIGIR 2018.
#'
#' \code{b1} and \code{b2} can be specified either directly (via the parameters of
#' the same name), or via intercepts: where the intercept is the probability of stopping
#' (C1) or continuing (C2) when the searcher is exactly at their target gain or
#' allowable rate. If neither \code{b2} nor \code{intercept2} is given, values are copied
#' from \code{b1}.
#'
#' @param doc.gain A vector of gains from returned documents, typically (but not
#'   always) each in the range 0..1.
#' @param doc.cost A vector of costs to examine SERP elements, typically (but not
#'   always) each in the range 0..1.
#' @param A The allowed (minumum) rate of gain.
#' @param target The target (maximum) amount of gain. Called $T$ in the description
#'   by Azzopardi et al.
#' @param rationality Controls the abruptness with which a searcher changes
#'   strategy. Called $R1$ and $R2$ in Azzopardi et al., but here we use the same
#'   value for both continue functions.
#' @param b1 Base rate: rate of stopping when the searcher has enough gain.
#' @param intercept1 Base rate: rate of stopping when the searcher has enough gain.
#' @param b2 Base rate: rate of stopping when the searcher is getting gain at the allowable rate.
#' @param intercept2 Base rate: rate of stopping when the searcher getting gain at the allowable rate.
#'
#' @return Compatible with other \link{ir.metrics} metrics, a list with the following elements:
#' \describe{
#' \item{metric}{The metric value.}
#' \item{C}{The computed \code{C} vector: \code{C[[i]]} is the probability of
#' the user Continuing to read past rank \emph{i}.}
#' \item{W}{The computed vector of weights: the important placed on each rank
#' in the list.}
#' \item{gain}{The passed gains.}
#' \item{cum.metric}{The metric accumulated along the list.}
#' \item{i}{Ranks used (starting at 1).}
#' \item{C}{The computed \code{C} vector: \code{C[[i]]} is the probability of
#' the user Continuing to read past rank \emph{i}.}
#' }
#'
#' IFT adds:\describe{
#' \item{C1}{The computed \code{C1} vector: \code{C1[[i]]} is the probability of
#' the user Continuing to read past rank \emph{i}, based on accumulated gain.}
#' \item{C2}{The computed \code{C2} vector: \code{C2[[i]]} is the probability of
#' the user Continuing to read past rank \emph{i}, based on rate of gain.}
#' \item{cost}{The passed costs.}
#' \item{rate}{The rate of gain at each rank.}
#' }
#'
#' @seealso \link{ir.metrics}
#' @export
IFT  <- function(doc.gain, doc.cost, A, target, rationality, b1=NA, intercept1=NA, b2=NA, intercept2=NA) {

  # length of list (of documents)
  N <- length(doc.gain)

  # sanity
  if (any(doc.gain > 1)) {
    warning("Gains > 1, metric may not make sense.")
  }
  if (any(doc.cost <= 0)) {
    warning("Costs <= 0, metric may not make sense.")
  }

  # get the C vector from cumulated relevance and cost...
  C.fn <- C.fn.IFT(A=A, target=target, rationality=rationality, b1=b1, intercept1=intercept1, b2=b2, intercept2=intercept2)
  Cvecs <- C.fn(R=doc.gain, k=doc.cost)
  # ...and that gives us the final discount (weight) curve...
  Wvec <- Cvec.to.Wvec(Cvecs$C)
  # ...from which two vectors we get the final metric
  m <- list(metric=sum(Wvec*doc.gain),
            C=Cvecs$C,
            W=Wvec,
            gain=doc.gain,
            cost=doc.cost,
            rate=cumsum(doc.gain)/cumsum(doc.cost),
            cum.metric=cumsum(Wvec*doc.gain),
            i=1:length(Cvecs),
            C1=Cvecs$C1,
            C2=Cvecs$C2)
  class(m) <- c("irmetric", "list")
  m
}

#
# Some things we can do with metrics (generic functions)
#
#' @export
print.irmetric <- function(x, ...) { print(x$metric, ...) }
