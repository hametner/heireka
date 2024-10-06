##################
# MODIFIED VERSION of predictrms from rms package (Frank Harrell)
##################
predict.blrm <-
        function(object, ...,
                 kint=NULL, ycut=NULL, zcppo=TRUE, Zmatrix=TRUE,
                 fun=NULL, funint=TRUE,
                 type=c("lp","fitted","fitted.ind","mean","x","data.frame",
                        "terms", "cterms", "ccterms", "adjto", "adjto.data.frame",
                        "model.frame"),
                 se.fit=FALSE, codes=FALSE,
                 posterior.summary=c('mean', 'median', 'all'), cint=0.95)
        {
                
                type              <- match.arg(type)
                posterior.summary <- match.arg(posterior.summary)
                
                if(se.fit) warning('se.fit does not apply to Bayesian models')
                if(posterior.summary == 'all' && ! missing(cint) && cint)
                        message('cint ignored when posterior.summary="all"')
                
                kintgiven <- length(kint) > 0
                iref <- object$interceptRef
                if(! kintgiven) kint <- iref
                
                ylevels   <- object$ylevels
                ycutgiven <- length(ycut) > 0
                if(kintgiven && ycutgiven) stop('may only specify one of kint, ycut')
                if(! ycutgiven) ycut <- ylevels[kint + 1]
                if(ycutgiven) {
                        if(type == 'lp' && length(ycut) > 1)
                                stop('ycut may only be a scalar for type="lp"')
                        kint <- if(all.is.numeric(ylevels)) which(ylevels == ycut[1]) - 1
                        else max((1 : length(ylevels))[ylevels <= ycut[1]]) - 1
                }
                
                pppo <- object$pppo
                if(pppo == 0) zcppo <- FALSE
                
                if(type == 'x' && (pppo == 0 || ! Zmatrix))
                        return(predictrms(object, ..., type='x'))
                
                if(type %in% c('data.frame', 'terms', 'cterms', 'ccterms',
                               'adjto', 'adjto.data.frame', 'model.frame'))
                        return(predictrms(object, ..., type=type))
                
                if(pppo > 0) {
                        cppo <- object$cppo
                        if(! length(cppo))
                                stop('only constrained partial PO models are implemented at present')
                }
                
                X     <- predictrms(object, ..., type='x')
                rnam  <- rownames(X)
                n     <- nrow(X)
                
                if(pppo > 0) {
                        Z  <- predictrms(object, ..., type='x', second=TRUE)
                        nz <- nrow(Z)
                        if(n != nz) stop('program logic error 4')
                        if(type == 'x' && zcppo) {
                                ly <- length(ycut)
                                if(nz == 1 && ly > 1) {
                                        X <- X[rep(1, ly),, drop=FALSE]
                                        Z <- Z[rep(1, ly),, drop=FALSE]
                                }
                                else
                                        if(ly %nin% c(1, nz))
                                                stop('ycut must be of length 1 or the number of requested predictions')
                                nz   <- nrow(Z)
                                ycut <- rep(ycut, length=nz)
                                for(i in 1 : nz) Z[i,] <- Z[i,] * cppo(ycut[i])
                        }
                        if(type == 'x') return(cbind(X, Z))
                }
                
                ns      <- object$non.slopes
                tauinfo <- object$tauInfo
                
                if(ns == 1 && length(fun) && funint)
                        stop('specifying fun= with funint=TRUE makes no sense with a binary response')
                
                draws   <- object$draws
                ndraws  <- nrow(draws)
                p       <- ncol(draws)
                cn      <- colnames(draws)
                ints    <- draws[, 1 : ns, drop=FALSE]
                betanam <- setdiff(cn, c(cn[1:ns], tauinfo$name))
                betas   <- draws[, betanam, drop=FALSE]
                if(pppo > 0) {
                        taunam <- tauinfo$name
                        taus   <- draws[, taunam, drop=FALSE]
                }
                
                postsum <- switch(posterior.summary, mean=mean, median=median,
                                  all=function(z) z)
                
                if(type == 'lp') {
                        ## If linear predictor is requested, only one intercept applies and
                        ## if the HPD interval is not requested, don't need to keep draws after
                        ## getting posterior summaries of parameters
                        ## Note that fun= that is not a function of all the intercepts can be
                        ## simply applied to linear predictor at reference intercept
                        if(length(ycut) %nin% c(1, n))
                                stop('ycut must be of length 1 or the number of requested predictions')
                        ycut <- rep(ycut, length=n)
                        
                        draws1int <- draws[, c(kint, (ns + 1) : p), drop=FALSE]
                        
                        if(posterior.summary != 'all' &&
                           (! length(fun) || ! funint) & ! cint) {
                                if(pppo > 0) {
                                        for(i in 1 : n) Z[i,] <- Z[i,] * cppo(ycut[i])
                                        X     <- cbind(X, Z)
                                }
                                cof   <- apply(draws1int, 2, postsum)
                                lp    <- matxv(X, cof)
                                if(length(fun)) lp <- fun(lp)
                                return(lp)
                        }
                        
                        ## Compute lp and for all posterior draws to create an ndraws x n matrix
                        if(! length(fun) || ! funint ) {
                                if(pppo > 0) {
                                        for(i in 1 : n) Z[i,] <- Z[i,] * cppo(ycut[i])
                                        X <- cbind(X, Z)
                                }
                                lp   <- t(matxv(X, draws1int, bmat=TRUE))
                                if(length(fun)) lp <- fun(lp)
                                if(posterior.summary == 'all') return(lp)
                                lpsum <- apply(lp, 2, postsum)
                                if(! cint) return(lpsum)
                                
                                ## When not 'all' the no-cint case was handled above.  Compute HPD
                                ## intervals for each prediction
                                hpd   <- apply(lp, 2, HPDint, prob=cint)
                                return(list(linear.predictors=lpsum, lower=hpd[1,], upper=hpd[2,]))
                        }
                        
                        ## What's left is a complex linear predictor request, i.e., for
                        ## a function that must use all the intercepts from each draw
                        
                        xb   <- t(matxv(X, cbind(ints[, iref], betas), bmat=TRUE))
                        ztau <- if(pppo > 0) t(matxv(Z, taus, bmat=TRUE))
                        r    <- matrix(NA, nrow=ndraws, ncol=n)
                        for(i in 1 : ndraws)
                                r[i, ] <- fun(xb[i, ], lptau=ztau[i, ], intercepts=ints[i, ], codes=codes)
                        if(posterior.summary == 'all') return(r)
                        rsum <- apply(r, 2, postsum)
                        if(! cint) return(rsum)
                        hpd  <- apply(r, 2, HPDint, prob=cint)
                        r <- list(linear.predictors=rsum, lower=hpd[1,], upper=hpd[2,])
                        return(r)
                }
                
                ## What's left is type=fitted, fitted.ind
                
                ## Get cumulative probability function used
                link    <- object$link
                cumprob <- rms::probabilityFamilies[[link]]$cumprob
                
                # if(ns == 1) return(cumprob(ints + betas %*% t(X)))  # binary logistic model
                if(ns == 1) {
                        lp <- betas %*% t(X)                     # Compute the linear predictor
                        lp <- sweep(lp, 1, ints[,1], '+')        # Add intercept to each row
                        return(cumprob(lp))                      # Apply the cumulative probability function
                }
                cnam  <- cn[1:ns]
                # First intercept corresponds to second distinct Y value
                if(pppo > 0) cppos <- cppo(ylevels[-1])
                
                ynam <- paste(object$yname, "=", ylevels, sep="")
                PP   <- array(NA, dim=c(ndraws, n, ns),
                              dimnames=list(NULL, rnam, cnam))
                PPeq <- array(NA, dim=c(ndraws, n, ns + 1),
                              dimnames=list(NULL, rnam, ynam))
                
                for(i in 1 : ndraws) {
                        inti    <- ints[i, ]               # alphas for ith draw
                        betai   <- betas[i,, drop=FALSE]
                        xb      <- X %*% t(betai)          # n x 1
                        if(pppo > 0) {
                                taui <- taus[i,, drop=FALSE]
                                zt   <- Z %*% t(taui)            # n x 1
                        }
                        for(j in 1 : n) {
                                ep <- inti + xb[j]    # 1 x ns
                                if(pppo > 0) ep <- ep + cppos * zt[j]
                                ep <- cumprob(ep)
                                PP[i, j, ] <- ep
                                if(type == 'fitted.ind') PPeq[i, j, ] <- c(1., ep) - c(ep, 0.)
                        }
                }
                
                if(posterior.summary == 'all')
                        return(switch(type,
                                      fitted     = PP,
                                      fitted.ind = PPeq))
                
                h <- function(x) {
                        s  <- postsum(x)
                        ci <- HPDint(x, cint)
                        r  <- c(s, ci)
                        names(r)[1] <- posterior.summary
                        r
                }
                
                ## Function to summarize a 3-d array and transform the results
                ## to a data frame with variables x (row names of predictions), y level,
                ## mean/median, Lower, Upper
                ## Input is ndraws x # predicton requests x # y categories (less 1 if cum.p.)
                
                s3 <- function(x) {
                        yl <- if(type == 'fitted') cnam else ynam
                        d <- expand.grid(y = yl, x = rnam, stat=NA, Lower=NA, Upper=NA,
                                         stringsAsFactors=FALSE)
                        for(i in 1 : nrow(d)) {
                                u <- h(x[, d$x[i], d$y[i]])
                                d$stat[i] <- u[1]
                                d$Lower[i] <- u['Lower']
                                d$Upper[i] <- u['Upper']
                        }
                        names(d)[3] <- upFirst(posterior.summary)
                        d
                }
                
                ## Similar for a 2-d array
                #s2 <- function(x) {
                #  d <- expand.grid(x = rnam, stat=NA, Lower=NA, Upper=NA,
                #                   stringsAsFactors=FALSE)
                #  for(i in 1 : nrow(d)) {
                #    u <- h(x[, d$x[i]])
                #    d$stat[i]  <- u[1]
                #    d$Lower[i] <- u['Lower']
                #    d$Upper[i] <- u['Upper']
                #  }
                #  names(d)[2] <- Hmisc::upFirst(posterior.summary)
                #  d
                #}
                
                r <- switch(type,
                            fitted     =  structure(s3(PP),   draws=PP),
                            fitted.ind =  structure(s3(PPeq), draws=PPeq))
                
                class(r) <- c('predict.blrm', class(r))
                r
        }

##' Print Predictions for [blrm()]
##'
##' Prints the summary portion of the results of `predict.blrm`
##' @param x result from `predict.blrm`
##' @param digits number of digits to round numeric results
##' @param ... ignored
##' @author Frank Harrell
##' @export
print.predict.blrm <- function(x, digits=3, ...) {
        numvar <- sapply(x, is.numeric)
        numvar <- names(numvar)[numvar]
        for(j in numvar) x[, j] <- round(x[, j], digits)
        print.data.frame(x)
}

##################
# MODIFIED VERSION of predictrms from rms package (Frank Harrell)
##################
##newdata=data frame, vector,  matrix, or list.  All but first assume data
##need coding, e.g. categorical variables are given as integers
##variable missing for all obs -> use adjust-to value in limits
##(means (parms) for matrx)

predictrms <-
        function(fit, newdata=NULL,
                 type=c("lp", "x", "data.frame", "terms", "cterms", "ccterms",
                        "adjto", "adjto.data.frame", "model.frame"),
                 se.fit=FALSE, conf.int=FALSE,
                 conf.type=c('mean', 'individual', 'simultaneous'),
                 kint=NULL, na.action=na.keep, expand.na=TRUE,
                 center.terms=type=="terms", ref.zero=FALSE,
                 posterior.summary=c('mean', 'median', 'mode'), second=FALSE, ...)
        {
                type              <- match.arg(type)
                conf.type         <- match.arg(conf.type)
                posterior.summary <- match.arg(posterior.summary)
                
                # Prevents structure(NULL, ...) below (now deprecated)
                nulll <- function(z) if(is.null(z)) list() else z
                
                if(second && type %nin% c('lp', 'x', 'adjto', 'adjto.data.frame'))
                        stop('type not implemented when second=TRUE')
                
                draws <- fit$draws
                bayes <- length(draws) > 0
                if(bayes) param <- fit$param
                if(bayes && conf.type == 'simultaneous')
                        stop('conf.type simultaneous not supported for Bayesian models')
                if(bayes && se.fit) {
                        warning('se.fit ignored for Bayesian models')
                        se.fit <- FALSE
                }
                if(second || conf.type == 'simultaneous') {
                        ## require(multcomp)
                        if(missing(newdata) || ! length(newdata))
                                stop('newdata must be given if conf.type="simultaneous" or second=TRUE')
                }
                
                at        <- if(second) fit$zDesign else fit$Design
                assume    <- at$assume.code
                Limval    <- Getlim(at, allow.null=TRUE, need.all=FALSE)
                Values    <- Limval$values
                non.ia    <- assume != 9L
                non.strat <- assume != 8L
                f         <- sum(non.ia)
                nstrata   <- sum(assume == 8L)
                somex     <- any(non.strat)
                rnam      <- NULL
                cox <- inherits(fit, "cph")
                naa <- fit$na.action
                if(! expand.na)
                        naresid <- function(a,b) b #don't really call naresid if drop NAs
                
                parms   <- at$parms
                name    <- at$name
                coeff   <- if(bayes) rmsb::getParamCoef(fit, posterior.summary,
                                                        what=if(second) 'taus' else 'betas')
                # else fit$coefficients
                nrp     <- num.intercepts(fit)
                nrpcoef <- num.intercepts(fit, 'coef')
                if(! length(kint)) kint <- fit$interceptRef  # orm, lrm, blrm otherwise NULL
                
                int.pres <- nrp > 0L
                
                assign <- if(second) fit$zassign else fit$assign
                nama <- names(assign)[1L]
                asso <- 1*(nama=="Intercept" | nama=="(Intercept)")
                
                Center <- if(cox) fit$center else 0.
                
                oldopts <- options(contrasts=c(factor="contr.treatment",
                                               ordered="contr.treatment"),   # was "contr.poly"
                                   Design.attr=at)
                
                ## In SV4 options(two lists) causes problems
                on.exit({options(contrasts=oldopts$contrasts)
                        options(Design.attr=NULL)})
                
                # OLD
                # Formula without response variable and any offsets:
                formulano <- if(second){fit$zsformula}
                else {removeFormulaTerms(fit$sformula, which='offset',
                                           delete.response=TRUE)}
                # OLD END
                
                # CHANGED TO
                # if (second) {
                #         if (!is.null(fit$zsformula)) {
                #                 formulano <- fit$zsformula
                #         } else if (!is.null(fit$zformula)) {
                #                 formulano <- fit$zformula  # Use zformula if zsformula is NULL
                #         } else {
                #                 stop("No formula available for second=TRUE")
                #         }
                # } else {
                #         formulano <- removeFormulaTerms(fit$sformula, which='offset',
                #                                         delete.response=TRUE)
                # }
                # ENDCHANGE
                
                offset <- 0; offpres <- FALSE
                ## offset is ignored for prediction (offset set to zero)
                ## if(! missing(newdata) && length(newdata)) {
                ##    offset <- model.offset(model.frame(removeFormulaTerms(fit$sformula,
                ##                   delete.response=TRUE), newdata,
                ##                   na.action=na.action, ...))
                ## offpres <- length(offset) > 0
                ## if(! offpres) offset <- 0
                ## }
                
                #Terms  <- delete.response(terms(formula(fit), specials='strat'))
                Terms  <- terms(formulano, specials='strat')
                
                attr(Terms, "response")  <- 0L
                attr(Terms, "intercept") <- 1L
                ## Need intercept whenever design matrix is generated to get
                ## current list of dummy variables for factor variables
                stra      <- attr(Terms, "specials")$strat
                
                Terms.ns       <- if(length(stra))      Terms[-stra] else Terms
                
                if(conf.int) {
                        vconstant <- 0.
                        if(conf.type=='individual') {
                                vconstant <- fit$stats['Sigma']^2
                                if(is.na(vconstant))
                                        stop('conf.type="individual" requires that fit be from ols')
                        }
                        zcrit <- if(length(idf <- fit$df.residual))
                                qt((1. + conf.int) / 2., idf) else qnorm((1. + conf.int) / 2.)
                }
                
                ## Form design matrix for adjust-to values
                ## Result of Adjto() is a model matrix with no intercept(s)
                Adjto <- function(type) {
                        adjto <- list()
                        ii <- 0L
                        for(i in (1L : length(assume))[non.ia]) {
                                ii <- ii + 1L
                                xi <- Getlimi(name[i], Limval, need.all=TRUE)[2L]
                                if(assume[i] %in% c(5L, 8L))
                                        xi <- factor(xi, parms[[name[i]]])
                                else
                                        if(assume[i] == 7L) xi <- scored(xi, name=name[i])
                                else
                                        if(assume[i] == 10L)
                                                xi <- I(matrix(parms[[name[i]]], nrow=1)) #matrx col medians
                                adjto[[ii]] <- xi
                        }
                        names(adjto) <- name[non.ia]
                        attr(adjto, "row.names") <- "1"
                        class(adjto) <- "data.frame"
                        if(type == "adjto.data.frame") return(adjto)
                        adjto <- model.frame(Terms, adjto)
                        adjto <- model.matrix(Terms.ns, adjto)[, -1, drop=FALSE]
                        if(type == 'adjto') {
                                k <- (nrpcoef + 1L) : length(coeff)
                                nck <- names(coeff)[k]
                                if(is.matrix(adjto))
                                        dimnames(adjto) <- list(dimnames(adjto)[[1L]], nck)
                                else names(adjto) <- nck
                        }
                        adjto
                }
                
                adjto <- NULL
                
                if(type %nin% c('adjto', 'adjto.data.frame')) {
                        X <- NULL
                        if(missing(newdata) || ! length(newdata)) {
                                flp <- fit$linear.predictors
                                if(type == "lp" && length(flp)) {
                                        LP  <- naresid(naa, flp)
                                        if(int.pres) {
                                                lpkint <- attr(flp, 'intercepts')
                                                if(! length(lpkint)) lpkint <- 1L
                                                if(length(kint) && kint != lpkint)
                                                {
                                                        LP <- LP - coeff[lpkint] + coeff[kint]
                                                }
                                        }
                                        if(length(stra <- fit$strata))
                                                attr(LP, "strata") <- naresid(naa, stra)
                                        if(! se.fit && ! conf.int) return(LP)
                                        else
                                                if(length(fit$se.fit)) {
                                                        if(nrp > 1L)
                                                                warning("se.fit is retrieved from the fit but it corresponded to kint")
                                                        retlist <- list(linear.predictors=LP)
                                                        if(se.fit) retlist$se.fit <- naresid(naa, fit$se.fit)
                                                        if(conf.int) {
                                                                plminus <- zcrit * sqrt(retlist$se.fit^2 + vconstant)
                                                                retlist$lower <- LP - plminus
                                                                retlist$upper <- LP + plminus
                                                        }
                                                        return(retlist)
                                                }
                                }   # end type='lp' with linear.predictors stored in fit
                                else
                                        if(type == "x") return(structure(nulll(naresid(naa, fit$x)),
                                                                         strata=if(length(stra <- fit$strata))
                                                                                 naresid(naa, stra) else NULL))
                                X <- fit[['x']]
                                rnam <- dimnames(X)[[1]]
                                if(! length(X))
                                        stop("newdata not given and fit did not store x")
                        }   # end no newdata
                        if(! length(X)) {
                                if(! is.data.frame(newdata)) {
                                        if(is.list(newdata)) {
                                                ## When second=TRUE the formula may not contain all the variables
                                                ## in newdata
                                                loc <- name[assume != 9L]
                                                if(length(names(newdata))) newdata <- newdata[loc]
                                                ## loc <- if(! length(names(newdata))) 1L : f else name[assume != 9L]
                                                new <- matrix(double(1L),
                                                              nrow=length(newdata[[1L]]),
                                                              ncol=length(newdata))
                                                for(j in 1L : ncol(new)) new[, j] <- newdata[[loc[j]]]
                                                newdata <- new
                                        }
                                        if(! is.matrix(newdata)) newdata <- matrix(newdata, ncol=f)
                                        if(ncol(newdata) != f)
                                                stop("# columns in newdata not= # factors in design")
                                        X  <- list()
                                        k  <- 0L
                                        ii <- 0L
                                        for(i in (1L : length(assume))[non.ia]) {
                                                ii <- ii + 1L
                                                xi <- newdata[, ii]
                                                as <- assume[i]
                                                allna <- all(is.na(xi))
                                                if(as == 5L | as == 8L) {
                                                        xi <- as.integer(xi)
                                                        levels(xi) <- parms[[name[i]]]
                                                        class(xi) <- "factor"
                                                }
                                                else if(as == 7L) xi <- scored(xi, name=name[i])
                                                else if(as == 10L) {
                                                        if(i == 1) ifact <- 1L
                                                        else ifact <- 1L + sum(assume[1L : (i - 1L)] != 8L)
                                                        ##	Accounts for assign not being output for strata factors
                                                        ncols <- length(assign[[ifact + asso]])
                                                        if(allna) {
                                                                xi <- matrix(double(1L),
                                                                             nrow=length(xi), ncol=ncols)
                                                                for(j in 1L : ncol(xi)) xi[, j] <- parms[[name[i]]][j]
                                                                xi <- I(xi)
                                                        }
                                                        else xi <- I(matrix(xi, nrow=length(xi), ncol=ncols))
                                                }
                                                
                                                ##	Duplicate single value for all parts of matrix
                                                k <- k + 1L
                                                X[[k]] <- xi
                                        }
                                        names(X) <- name[non.ia]
                                        attr(X, "row.names") <- as.character(1L : nrow(newdata))
                                        class(X) <- "data.frame"
                                        newdata <- X
                                        ## Note: data.frame() converts matrix variables to individual variables
                                        if(type == "data.frame") return(newdata)
                                }  # end !is.data.frame(newdata)
                                else {
                                        ## Need to convert any factors to have all levels in original fit
                                        ## Otherwise, wrong dummy variables will be generated by model.matrix
                                        
                                        nm <- names(newdata)
                                        for(i in 1L : ncol(newdata)) {
                                                j <- match(nm[i], name)
                                                if(! is.na(j)) {
                                                        asj <- assume[j]
                                                        w   <- newdata[, i]
                                                        V   <- NULL
                                                        if(asj %in% c(5L, 7L, 8L) | 
                                                           (name[j] %in% names(Values) &&
                                                            asj != 11 && length(V <- Values[[name[j]]]) &&
                                                            is.character(V))) {
                                                                if(length(Pa <- parms[[name[j]]])) V <- Pa
                                                                newdata[,i] <- factor(w, V)
                                                                ## Handles user specifying numeric values without quotes, that
                                                                ## are levels
                                                                ww <- is.na(newdata[,i]) & ! is.na(unclass(w))
                                                                if(any(ww)) 	{
                                                                        cat("Error in predictrms: Values in", names(newdata)[i],
                                                                            "not in", V, ":\n")
                                                                        print(as.character(w[ww]), quote=FALSE); stop()
                                                                }
                                                        }
                                                }
                                        }
                                }  # is.data.frame(newdata)
                                
                                X <- model.frame(Terms, newdata, na.action=na.action, ...)
                                if(type == "model.frame") return(X)
                                naa  <- attr(X, "na.action")
                                rnam <- row.names(X)
                                
                                strata <- list()
                                nst <- 0
                                ii <- 0
                                for(i in 1L : ncol(X)) {
                                        ii <- ii + 1L
                                        xi <- X[[i]]
                                        asi <- attr(xi, "assume.code")
                                        as <- assume[ii]
                                        if(! length(asi) && as == 7L) {
                                                attr(X[,i], "contrasts") <- 
                                                        attr(scored(xi, name=name[ii]), "contrasts")
                                                if(length(xi) == 1L) warning("a bug in model.matrix can produce incorrect results\nwhen only one observation is being predicted for an ordered variable")
                                        }
                                        
                                        if(as == 8L) {
                                                nst <- nst + 1L
                                                ste <- paste(name[ii], parms[[name[ii]]], sep='=')
                                                strata[[nst]] <- factor(ste[X[,i]], ste)
                                        }
                                }
                                X <- if(! somex) NULL
                                else model.matrix(Terms.ns, X)[, -1L, drop=FALSE]
                                
                                if(nstrata > 0L) {
                                        names(strata) <- paste("S", 1L : nstrata, sep="")
                                        strata <- interaction(as.data.frame(strata), drop=FALSE)
                                }
                        }   # end !length(X)
                        else strata <- attr(X, "strata")
                }   # if(end adj.to adj.to.data.frame)
                if(somex && ! bayes) {
                        cov <- vcov(fit, regcoef.only=TRUE, intercepts=kint)
                        covnoint <- if(nrp == 0) cov
                        else 
                                vcov(fit, regcoef.only=TRUE, intercepts='none')
                }
                
                if(type %in% c('adjto.data.frame', 'adjto')) return(Adjto(type))
                
                if(type=="x") return(
                        structure(nulll(naresid(naa, X)),
                                  strata=if(nstrata > 0)  naresid(naa, strata) else NULL,
                                  na.action=if(expand.na) NULL else naa)
                )
                
                
                if(type == "lp") {
                        if(somex) {
                                xb <- matxv(X, coeff, kint=kint) - Center + offset
                                names(xb) <- rnam
                                if(bayes && conf.int) {
                                        xB <- matxv(X, draws, kint=kint, bmat=TRUE)
                                        xB <- apply(xB, 1, rmsb::HPDint, prob=conf.int)
                                        lower <- xB[1, ]
                                        upper <- xB[2, ]
                                }
                        }
                        else {
                                xb <- if(offpres) offset else numeric(0)
                                if(nstrata > 0) attr(xb, 'strata') <- naresid(naa, strata)
                                return(structure(if(se.fit)
                                        list(linear.predictors=xb,
                                             se.fit=rep(NA, length(xb))) else
                                                     xb,
                                        na.action=if(expand.na) NULL else naa))
                        }
                        xb <- naresid(naa, xb)
                        if(nstrata > 0) attr(xb, "strata") <- naresid(naa, strata)
                        ycenter <- if(ref.zero && somex) {
                                if(! length(adjto)) adjto <- Adjto(type)
                                matxv(adjto, coeff, kint=kint) - Center
                        } else 0.
                        
                        if(ref.zero || ((se.fit || conf.int) && somex)) {
                                dx <- dim(X)
                                n <- dx[1L]; p <- dx[2L]
                                if(cox && ! ref.zero) X <- X - rep(fit$means, rep.int(n, p))
                                if(ref.zero) {
                                        if(! length(adjto)) adjto <- Adjto(type)
                                        X <- X - rep(adjto, rep.int(n, p))
                                }
                                if(! bayes) {
                                        se <- drop(if(ref.zero || nrp == 0L)
                                                sqrt(((X %*% covnoint) * X) %*% rep(1L, ncol(X)))
                                                else {
                                                        Xx <- cbind(Intercept=1., X)
                                                        sqrt(((Xx %*% cov) * Xx) %*% rep(1L, ncol(Xx)))
                                                })
                                        names(se) <- rnam
                                        sef <- naresid(naa, se)
                                }
                                ww <- if(conf.int || se.fit) {
                                        if(se.fit)
                                                list(linear.predictors = xb - ycenter, se.fit = sef) else
                                                        list(linear.predictors = xb - ycenter)
                                }
                                else
                                        xb - ycenter
                                if(bayes) {lower <- lower - ycenter; upper <- upper - ycenter}
                                retlist <- structure(nulll(ww), 
                                                     na.action=if(expand.na) NULL else naa)
                                if(conf.int) {
                                        if(conf.type == 'simultaneous') {
                                                num.intercepts.not.in.X <- length(coeff) - ncol(X)
                                                u <- confint(multcomp::glht(fit,
                                                                            if(num.intercepts.not.in.X == 0L) X else Xx,
                                                                            df=if(length(idf)) idf else 0L),
                                                             level=conf.int)$confint
                                                retlist$lower <- u[,'lwr']
                                                retlist$upper <- u[,'upr']
                                        } else {
                                                if(bayes) {
                                                        retlist$lower <- lower
                                                        retlist$upper <- upper
                                                }
                                                else {
                                                        plminus <- zcrit*sqrt(sef^2 + vconstant)
                                                        retlist$lower <- xb - plminus - ycenter
                                                        retlist$upper <- xb + plminus - ycenter
                                                }
                                        }
                                }
                                return(retlist)
                        }
                        else return(structure(xb - ycenter, na.action=if(expand.na)NULL else naa))
                }   ## end if type='lp'
                
                if(type %in% c("terms", "cterms", "ccterms")) {
                        if(! somex)
                                stop('type="terms" may not be given unless covariables present')
                        
                        usevar <- if(type=="terms") non.strat else rep(TRUE, length(assume))
                        fitted <- array(0, c(nrow(X), sum(usevar)),
                                        list(rnam, name[usevar]))
                        if(se.fit) se <- fitted
                        if(center.terms) {
                                if(! length(adjto)) adjto <- Adjto(type)
                                if(ncol(adjto) != ncol(X)) {
                                        if(dimnames(adjto)[[2L]][1L] %in% c('Intercept','(Intercept)') &&
                                           dimnames(X)[[2L]][1L]    %nin% c('Intercept','(Intercept)'))
                                                adjto <- adjto[, -1L, drop=FALSE]
                                        if(ncol(adjto) != ncol(X)) stop('program logic error')
                                }
                                X <- sweep(X, 2L, adjto) # center columns
                        }
                        j <- 0L
                        for(i in (1L : length(assume))[usevar]) {
                                j <- j + 1L
                                if(assume[i] != 8L) { # non-strat factor; otherwise leave fitted=0
                                        k <- assign[[j + asso]]
                                        num.intercepts.not.in.X <- length(coeff) - ncol(X)
                                        ko <- k - num.intercepts.not.in.X
                                        fitted[, j] <- matxv(X[, ko, drop=FALSE], coeff[k])
                                        if(se.fit) se[,j] <-
                                                (((X[, ko, drop=FALSE]  %*% cov[k, k, drop=FALSE]) * 
                                                          X[, ko, drop=FALSE]) %*% rep(1., length(ko)))^.5
                                }
                        }
                        if(type == "cterms") {
                                ## Combine all related interation terms with main effect terms
                                w <- fitted[, non.ia, drop=FALSE]  # non-interaction terms
                                for(i in 1L : f) {
                                        ia <- interactions.containing(at, i)
                                        ## subscripts of interaction terms related to predictor i
                                        if(length(ia)) w[, i] <- rowSums(fitted[, c(i,ia), drop=FALSE])
                                }
                                fitted <- w
                        }
                        
                        if(type=='ccterms') {
                                z <- combineRelatedPredictors(at)
                                f <- length(z$names)
                                w <- matrix(NA, ncol=f, nrow=nrow(fitted))
                                colnames(w) <- sapply(z$names, paste, collapse=', ')
                                for(i in 1L : f)
                                        w[,i] <- rowSums(fitted[, z$namesia[[i]], drop=FALSE])
                                fitted <- w
                        }
                        
                        fitted <- structure(nulll(naresid(naa, fitted)),
                                            strata=if(nstrata==0) NULL else naresid(naa, strata))
                        
                        if(se.fit) {
                                return(structure(list(fitted=fitted, se.fit=naresid(naa,se)),
                                                 na.action=if(expand.na)NULL else naa)) 	}
                        else return(structure(fitted, na.action=if(expand.na)NULL else naa))
                }
        }   

Getlim <- function(at, allow.null=FALSE, need.all=TRUE) {
        nam    <- at$name[at$assume!="interaction"]
        limits <- at$limits
        values <- at$values
        
        XDATADIST <- .Options$datadist
        X <- lims <- vals <- NULL
        if(! is.null(XDATADIST)) {
                X <- if(inherits(XDATADIST, 'datadist')) XDATADIST
                else
                        if(exists(XDATADIST)) eval(as.name(XDATADIST))
                if(! is.null(X)) {
                        lims <- X$limits
                        if(is.null(lims)) stop(paste("options(datadist=",XDATADIST,
                                                     ") not created with datadist"))
                        vals <- X$values
                }
        }
        
        if((length(X) + length(limits)) == 0) {
                if(allow.null) {
                        lims <- list()
                        for(nn in nam) lims[[nn]] <- rep(NA,7)
                        lims <- structure(lims, class="data.frame", 
                                          row.names=c("Low:effect","Adjust to", "High:effect", "Low:prediction",
                                                      "High:prediction","Low","High"))
                        return(list(limits=lims, values=values))
                }
                stop("no datadist in effect now or during model fit")
        }
        
        na <- if(length(limits))
                sapply(limits, function(x) all(is.na(x))) else rep(TRUE, length(nam))
        if(length(lims) && any(na)) for(n in nam[na]) { #if() assumes NA stored in fit
                # for missing vars
                z <- limits[[n]]
                u <- if(match(n, names(lims), 0) > 0) lims[[n]] else NULL
                # This requires exact name match, not substring match
                if(is.null(u)) {
                        if(need.all) stop(paste("variable",n,
                                                "does not have limits defined in fit or with datadist"))
                        else limits[[n]] <- rep(NA,7)    # Added 28 Jul 94
                }
                else limits[[n]] <- u
        }
        limits <- structure(limits, class="data.frame", 
                            row.names=c("Low:effect","Adjust to", "High:effect", "Low:prediction",
                                        "High:prediction","Low","High"))
        
        if(length(vals)) values <- c(values, 
                                     vals[match(names(vals),nam,0)>0 & match(names(vals),names(values),0)==0]
        )   # add in values from datadist corresponding to vars in model
        # not already defined for model
        
        list(limits=limits, values=values)
}

#Function to return limits for an individual variable, given an object
#created by Getlim

Getlimi <- function(name, Limval, need.all=TRUE)
{
        lim <- if(match(name, names(Limval$limits), 0) > 0) 
                Limval$limits[[name]] else NULL
        if(is.null(Limval) || is.null(lim) || all(is.na(lim))) {
                if(need.all) stop(paste("no limits defined by datadist for variable",
                                        name))
                return(rep(NA,7))
        }
        lim
}

removeFormulaTerms <- function(form, which=NULL, delete.response=FALSE) {
        # drop.terms will not remove offsets.  Trick it by renaming offset() terms .off.
        if('offset' %in% which) {
                form <- format(form)
                which[which == 'offset'] <- '.off.'
                form <- as.formula(paste(gsub('offset(', '.off.(', form, fixed=TRUE), collapse=""))
        }
        
        te <- terms(form, specials=which)
        s  <- unlist(attr(te, 'specials'))  # LHS present -> 1 added to s
        ypresent <- attr(te, 'response')
        # drop.terms counts only RHS terms
        te <- drop.terms(te, s - ypresent, keep.response= ! delete.response)
        formula(te)   # don't allow other attributes to be there
}