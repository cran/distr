############################# Utils

.mergegaps <- function(gaps, support){
 if(is.null(gaps)) return(NULL)
 mm <- rbind(cbind(gaps[ ,1],1),
             cbind(gaps[ ,2],2),
             cbind(support,3))
 mm <- mm[order(mm[,1]),]
 jj <- 0
 ein <- FALSE
 gaps.new <- mm
 for(j in 1:nrow(mm))
  {if (mm[j,2] == 1 & !ein){
       jj <- jj + 1
       ein <- TRUE
       gaps.new[jj,1] <- mm[j,1]
       }
   if (mm[j,2] == 3 &  ein){
       gaps.new[jj,2] <- mm[j,1]
       jj <- jj + 1
       gaps.new[jj,1] <- mm[j,1]
       }
   if (mm[j,2] == 2)       {
       ein <- FALSE
       gaps.new[jj,2] <- mm[j,1]
       }
   }
 ga <- gaps.new[1:jj,,drop = FALSE]
 if(jj>0){
    ln <- seq(jj)
    ga <- matrix(ga[ln[ga[,1] != ga[,2]], ], ncol = 2)
    }
 else ga <- NULL
 return(ga)

}

.mergegaps2 <- function(gaps1, gaps2){
 if(is.null(gaps1)) return(NULL)
 if(is.null(gaps2)) return(NULL)
 mm <- rbind(cbind(gaps1[ ,1],1), 
             cbind(gaps1[ ,2],2),
             cbind(gaps2[ ,1],3),
             cbind(gaps2[ ,2],4))
 mm <- mm[order(mm[ ,1]), ]
 jj <- 0
 state1 <- 0
 state2 <- 0
 gaps.new <- mm
 for(j in 1:nrow(mm))
  {if (mm[j,2] == 1 && state2 == 0){
        state1 <- 1
       }
   if (mm[j,2] == 1 && state2 == 1){
        jj <- jj + 1
        gaps.new[jj,1] <- mm[j,1]
        state1 <- 1
       }
   if (mm[j,2] == 2 && state2 == 1){
        gaps.new[jj,2] <- mm[j,1]
        state1 <- 0
       }
   if (mm[j,2] == 2 && state2 == 0){
        state1 <- 0
       }
   if (mm[j,2] == 3 && state1 == 0){
        state2 <- 1
       }
   if (mm[j,2] == 3 && state1 == 1){
        jj <- jj + 1
        gaps.new[jj,1] <- mm[j,1]
        state2 <- 1
       }
   if (mm[j,2] == 4 && state1 == 1){
        gaps.new[jj,2] <- mm[j,1]
        state2 <- 0
       }
   if (mm[j,2] == 4 && state1 == 0){
        state2 <- 0
       }
   }
 erg <- if (jj > 0) gaps.new[1:jj, ] else NULL
 return(erg)

}

.pmixfun <- function(mixDistr, mixCoeff){
  l <- length(mixCoeff)
  return(function(q, lower.tail = TRUE, log.p = FALSE ){
  p0 <- as.vector(matrix(unlist(lapply(mixDistr, function(x)
         do.call(x@p, list(q = q, lower.tail = lower.tail)))),
                 ncol = l, nrow = length(q)) %*% mixCoeff)
  if(log.p) p0 <- log(p0)
  return(p0)               
   })
}
.dmixfun <- function(mixDistr, mixCoeff, withStand = FALSE, supp = NULL){
  l <- length(mixCoeff)
  if(withStand) {su <- sum(as.vector(matrix(unlist(lapply(mixDistr, function(y)
                                               do.call(y@d, list(x = supp)))),
                                 ncol = l, nrow = length(supp)) %*% mixCoeff))
                }else su <- 1
 
  return(function(x, log = FALSE ){
         d0 <- 
         as.vector(matrix(unlist(lapply(mixDistr, function(y)
            do.call(y@d, list(x = x)))),
         ncol = l, nrow = length(x)) %*% mixCoeff)/su
         if(log) d0 <- log(d0)
         return(d0)
         })
}

.rmixfun <- function(mixDistr, mixCoeff){
  l <- length(mixCoeff)
  return(function(n){
  Un <- outer(sample(1:l, size = n, replace = TRUE, prob = mixCoeff),
                          1:l, function(x,y) x == y)
  mal <- lapply(mixDistr, function(x) x@r(n))
  ma <- matrix(unlist(mal), ncol = l, nrow = n)
  return(rowSums(Un*ma))})

}


.qmixfun <- function(mixDistr, mixCoeff, Cont = TRUE, pnew){
  l <- length(mixCoeff)
  loup <- .loupmixfun(mixDistr)

  n <- getdistrOption("DefaultNrGridPoints")
  up <- min(loup$qu,1000)#getdistrOption("LARGE")
  lo <- max(loup$ql,-1000)#getdistrOption("LARGE")
  h <- (up-lo)/n

  xseq <- seq(from = lo, to = up, by = h)

  px.l <- pnew(xseq, lower.tail = TRUE)
  px.u <- pnew(xseq, lower.tail = FALSE)

  return(.makeQNew(xseq, px.l, px.u, TRUE, loup$qL, loup$qU, Cont = Cont))
}


.loupmixfun <- function(mixDistr){
    qL0 <- as.vector(unlist(lapply(mixDistr, function(x)
                         do.call(x@q,list(p = 0, lower.tail = TRUE)))))
    qL1 <- as.vector(unlist(lapply(mixDistr, function(x)
                         do.call(x@q,list(p = getdistrOption("TruncQuantile"),
                                 lower.tail = TRUE)))))
    qL  <- min(qL0); ql <- min(qL1)

    qU0 <- as.vector(unlist(lapply(mixDistr, function(x)
                         do.call(x@q,list(p = 0, lower.tail = FALSE)))))
    qU1 <- as.vector(unlist(lapply(mixDistr, function(x)
                         do.call(x@q,list(p = getdistrOption("TruncQuantile"),
                                 lower.tail = FALSE)))))
    qU  <- max(qU0); qu <- max(qU1)
    return(list(qL = qL, ql = ql, qU = qU, qu = qu))
    }
