############################ plot #######################

setMethod("plot", signature(x = "AffLinUnivarLebDecDistribution", y = "missing"),
    function(x, width = 10, height = 5.5, withSweave = getdistrOption("withSweave"),
             xlim = NULL, ylim = NULL, ngrid = 1000,
             verticals = TRUE, do.points = TRUE,
             main = FALSE, inner = TRUE, sub = FALSE,
             bmar = par("mar")[1], tmar = par("mar")[3], ...,
             cex.main = par("cex.main"), cex.inner = 1.2,
             cex.sub = par("cex.sub"), col.points = par("col"),
             col.hor = par("col"), col.vert = par("col"),
             col.main = par("col.main"), col.inner = par("col.main"),
             col.sub = par("col.sub"),  cex.points = 2.0,
             pch.u = 21, pch.a = 16, mfColRow = TRUE){

      mc <- match.call(call = sys.call(sys.parent(1)), expand.dots = FALSE)[-1]
      mc$x <- NULL
      x <- as(x,"UnivarLebDecDistribution")
      mc <- c(list(x=x), mc)
      do.call(getMethod("plot",
              signature(x="UnivarLebDecDistribution",y="missing")), mc)
      return(invisible())
})

setMethod("plot", signature(x = "UnivarLebDecDistribution", y = "missing"),
    function(x, width = 10, height = 14.5, withSweave = getdistrOption("withSweave"),
             xlim = NULL, ylim = NULL, ngrid = 1000,
             verticals = TRUE, do.points = TRUE,
             main = FALSE, inner = TRUE, sub = FALSE,
             bmar = par("mar")[1], tmar = par("mar")[3], ...,
             cex.main = par("cex.main"), cex.inner = 0.9,
             cex.sub = par("cex.sub"), col.points = par("col"),
             col.hor = par("col"), col.vert = par("col"),
             col.main = par("col.main"), col.inner = par("col.main"),
             col.sub = par("col.sub"),  cex.points = 2.0,
             pch.u = 21, pch.a = 16, mfColRow = TRUE){


      mc <- match.call(call = sys.call(sys.parent(1)), expand.dots = FALSE)[-1]
      xc <- mc$x
      ### manipulating the ... - argument
      dots <- match.call(call = sys.call(sys.parent(1)),
                       expand.dots = FALSE)$"..."

      if(x@mixCoeff[1]==0){
         mc$x <- NULL
         x <- x@mixDistr[[2]]
         mc <- c(list(x=x), mc)
         do.call(getMethod("plot",signature(x = "DiscreteDistribution",
                                            y = "missing")), mc)
         return(invisible())
        }

      if(x@mixCoeff[1]==1){
         mc$x <- NULL
         x <- x@mixDistr[[1]]
         mc <- c(list(x=x), mc)
         do.call(getMethod("plot",signature(x = "AbscontDistribution",
                                            y = "missing")), mc)
         return(invisible())
        }



      dots.for.points <- dots[names(dots) %in% c("bg", "lwd", "lty")]
      if (length(dots.for.points) == 0 ) dots.for.points <- NULL

      dots.without.pch <- dots[! (names(dots) %in% c("pch",
                                  "main", "sub", "log"))]

     ###
     if(!is.logical(inner))
         if(!is.list(inner)||length(inner) != 2)
            stop("Argument 'inner' must either be 'logical' or a 'list' vector of length 2")

     cex <- if (hasArg(cex)) dots$cex else 1

     if (hasArg(cex) && missing(cex.points))
         cex.points <- 2.0 * cex

     if (hasArg(pch) && missing(pch.u))
          pch.u <- dots$pch
     if (hasArg(pch) && missing(pch.a))
          pch.a <- dots$pch

     if (hasArg(col) && missing(col.points))
         col.points <- dots$col
     if (hasArg(col) && missing(col.vert))
         col.vert <- dots$col
     if (hasArg(col) && missing(col.main))
        col.main <- dots$col
     if (hasArg(col) && missing(col.inner))
        col.inner <- dots$col
     if (hasArg(col) && missing(col.sub))
        col.sub <- dots$col

     if (!withSweave)
          devNew(width = width, height = height)
     omar <- par("mar")

     mainL <- FALSE
     subL <- FALSE
     lineT <- NA
     logpd <- logq <- ""

     slots <-  slotNames(param(x))
     slots <-  slots[slots != "name"]
     nrvalues <-  length(slots)
     if(nrvalues > 0){
           values <-  numeric(nrvalues)
       for(i in 1:nrvalues)
         values[i] <-  attributes(attributes(x)$param)[[slots[i]]]
       paramstring <-  paste(values, collapse = ", ")
       nparamstring <-  paste(slots, "=", values, collapse = ", ")
       qparamstring <- paste("(",paramstring,")",sep="")
     }
     else paramstring <- qparamstring <- nparamstring <- ""

     .mpresubs <- function(inx)
                    .presubs(inx, c("%C", "%D", "%N", "%P", "%Q", "%A"),
                          c(as.character(class(x)[1]),
                            as.character(date()),
                            nparamstring,
                            paramstring,
                            qparamstring,
                            as.character(deparse(xc))))

     if (hasArg(main)){
         mainL <- TRUE
         if (is.logical(main)){
             if (!main) mainL <-  FALSE
             else
                  main <- gettextf("Distribution Plot for %%A") ###
                          ### double  %% as % is special for gettextf
             }
         main <- .mpresubs(main)
         if (mainL) {
             if(missing(tmar))
                tmar <- 5
             if(missing(cex.inner))
                cex.inner <- .9
             lineT <- 0.6
             }
     }
     if (hasArg(sub)){
         subL <- TRUE
         if (is.logical(sub)){
             if (!sub) subL <-  FALSE
             else       sub <- gettextf("generated %%D")
                          ### double  %% as % is special for gettextf
         }
         sub <- .mpresubs(sub)
         if (subL)
             if (missing(bmar)) bmar <- 6
     }

     if(mfColRow){
        opar <- par("mfrow", mar = c(bmar,omar[2],tmar,omar[4]))
        layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5,6,6,7,7,8,8), byrow=TRUE, 
               nrow=3))
     }else 
        opar <- par(mar = c(bmar,omar[2],tmar,omar[4]))

     if(is.logical(inner)){
        inner.p <- if (inner)
                   .mpresubs(gettextf("CDF of %%C%%Q")) else ""
        inner.q <- if (inner)
                   .mpresubs(gettextf("Quantile function of %%C%%Q")) else ""
                          ### double  %% as % is special for gettextf
     }else{
        inner.p <- .mpresubs(inner[[1]])
        inner.q <- .mpresubs(inner[[2]])
     }


     lower <- min(getLow(x@mixDistr[[1]],
                      eps = getdistrOption("TruncQuantile")*2),
                  getLow(x@mixDistr[[2]]))
     upper <- max(getUp(x@mixDistr[[1]],
                      eps = getdistrOption("TruncQuantile")*2),
                  getUp(x@mixDistr[[2]]))

     ## ngrid  nr of gridpoints
     ## exactq two p-values are considered equal if difference is
     ## is less than 10^-exactq in abs. value

     dist <- upper - lower
     del <- getdistrOption("DistrResolution")
     supp <- support(x)

     if(hasArg(xlim))
     {  if(length(xlim)!=2) stop("Wrong length of Argument xlim");
           grid <- seq(xlim[1], xlim[2], length = ngrid)
           supp <- supp[(supp >= xlim[1]) & (supp <= xlim[2])]
     }else{grid <- seq(from = lower - 0.1 * dist, to = upper + 0.1 * dist,
                       length = ngrid)
           supp <- support(x)
     }

     grid <- unique(sort( c(supp, supp-del , grid )))
     pxg <- p(x)(grid)


     if(hasArg(ylim))
         {if(length(ylim)!=2)
             stop("Wrong length of Argument ylim")}

     else {ylim <- c(-0.05,1.05)}

     if(hasArg(log))
         {logpd <- dots$log
          logq <- gsub("u","y",gsub("y","x",gsub("x", "u", logpd)))
          if(length(grep("y",logpd))){
             ylim <- c(max(min(pxg[pxg>0]), ylim[1]),
                               ylim[2])
             }
          }

     if(!verticals){
         grid <- unique(sort( c(supp-del/2, grid )))
         grid[.isIn(grid,cbind(supp-del/2,supp-del/2))] <- NA
         pxg <- p(x)(grid)
     }else{
         xv <- as.vector(t(cbind(supp-del,supp,NA)))
         pxv <- p(x)(xv)
     }

     owarn <- getOption("warn"); options(warn = -1)
     do.call(plot, c(list(x = grid, pxg, type = "l",
          ylim = ylim, ylab = "p(q)", xlab = "q", log = logpd),
          dots.without.pch))
     options(warn = owarn)

     pxg.d <- p(x)(supp)
     pxg.d0 <- p(x)(supp-del)
     if(do.points){
        do.call(points, c(list(x = supp, y = pxg.d, pch = pch.a,
                  cex = cex.points, col = col.points), dots.for.points))
        do.call(points, c(list(x = supp-del, y = pxg.d0, pch = pch.u,
                  cex = cex.points, col = col.points), dots.for.points))
     }
     if(verticals){
         do.call(lines, c(list(x = xv, y = pxv, col = col.vert),
                 dots.without.pch))
     }

     title(main = inner.p, line = lineT, cex.main = cex.inner,
           col.main = col.inner)

     ### quantiles

     ### fix finite support bounds
     ixg  <-  grid>=max(q(x)(0),lower) & grid <= min(q(x)(1),upper)
     pxg  <-   pxg[ixg]
     grid <-  grid[ixg]
     if(is.finite(q(x)(0))) {grid <- c(q(x)(0),grid); pxg <- c(0,pxg)}
     if(is.finite(q(x)(1))) {grid <- c(grid,q(x)(1)); pxg <- c(pxg,1)}

#     ### fix constancy regions of p(x)
#     if(isOldVersion(x)) x <- conv2NewVersion(x)
#
#     if(length(pxv))
#         do.call(lines, c(list(x = pxv, y = xv), dots.without.pch))
     
     if(!is.null(gaps(x))){
        i.not.gap    <- !.isIn(grid,gaps(x))
        ndots <- nrow(gaps(x))
        pu1 <- p(x)(gaps(x)[,1])
        if (verticals){
             xu <- c(gaps(x)[,1],gaps(x)[,2], grid[i.not.gap])
             pu <- c(rep(pu1,2), pxg[i.not.gap])
        }else{
             xu <- c(gaps(x)[,1],rep(NA,ndots),gaps(x)[,2], grid[i.not.gap])
             pu <- c(rep(pu1,3), pxg[i.not.gap])
        }
        #
        o <- order(pu,xu)
        po <- pu[o]
        xo <- xu[o]
     }else{
        po <- pxg
        xo <- grid
     }

     owarn <- getOption("warn"); options(warn = -1)
     do.call(plot, c(list(x = po, xo, type = "n",
          xlim = ylim, ylim = xlim, ylab = "q(p)", xlab = "p",
          log = logq), dots.without.pch))
     options(warn = owarn)


     title(main = inner.q, line = lineT, cex.main = cex.inner,
           col.main = col.inner)

     owarn <- getOption("warn"); options(warn = -1)
     lines(po,xo, ...)
#    if (verticals && !is.null(gaps(x))){
#         do.call(lines, c(list(rep(pu1,2), c(gaps(x)[,1],gaps(x)[,2]),
#                 col = col.vert), dots.without.pch))
#     }
     options(warn = owarn)


     if (verticals && !is.null(gaps(x))){
             pu <- rep(pu1,3)
             xu <- c(gaps(x)[,1],gaps(x)[,2],rep(NA,ndots))
             o <- order(pu)
             do.call(lines, c(list(pu[o], xu[o],
                     col = col.vert), dots.without.pch))
     }
     if(!is.null(gaps(x)) && do.points){
        do.call(points, c(list(x = pu1, y = gaps(x)[,1], pch = pch.a,
                cex = cex.points, col = col.points), dots.for.points) )
        do.call(points, c(list(x = pu1, y = gaps(x)[,2], pch = pch.u,
                cex = cex.points, col = col.points), dots.for.points) )

     }

     if(do.points){
        if(is.finite(q(x)(0))) 
           do.call(points, c(list(x = 0, y = q(x)(0), pch = pch.u,
                cex = cex.points, col = col.points), dots.for.points) )
        if(is.finite(q(x)(1))) 
           do.call(points, c(list(x = 1, y = q(x)(1), pch = pch.a,
                cex = cex.points, col = col.points), dots.for.points) )
     }
     if (mainL)
         mtext(text = main, side = 3, cex = cex.main, adj = .5,
               outer = TRUE, padj = 1.4, col = col.main)

     if (subL)
         mtext(text = sub, side = 1, cex = cex.sub, adj = .5,
               outer = TRUE, line = -1.6, col = col.sub)
               
     mc.ac <- mc
     if(!is.logical(inner)) mc.ac$inner <- inner[3:5] 
     mc.ac$mfColRow <- FALSE
     mc.ac$main <- FALSE
     mc.ac$sub <- FALSE
     mc.ac$x <- NULL 
     mc.ac$withSweave <- TRUE 
     if(is.null(mc.ac$cex.inner))  mc.ac$cex.inner <- 0.9
     do.call(plot, c(list(acPart(x)),mc.ac))

     mc.di <- mc
     if(!is.logical(inner)) mc.di$inner <- inner[6:8] 
     mc.di$mfColRow <- FALSE
     mc.di$main <- FALSE
     mc.di$sub <- FALSE
     mc.di$x <- NULL
     mc.di$withSweave <- TRUE 
     if(is.null(mc.di$cex.inner))  mc.di$cex.inner <- 0.9
     do.call(plot, c(list(discretePart(x)),mc.di))
     
     par(opar)
   }
   )
