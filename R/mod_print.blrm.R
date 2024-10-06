##' Print [blrm()] Results
##'
##' Prints main results from [blrm()] along with indexes and predictive accuracy and their highest posterior density intervals computed from `blrmStats`.
##' @param x object created by [blrm()]
##' @param dec number of digits to print to the right of the decimal
##' @param coefs specify `FALSE` to suppress printing parameter estimates, and in integer k to print only the first k
##' @param intercepts set to `FALSE` to suppress printing intercepts.   Default is to print them unless there are more than 9.
##' @param prob HPD interval probability for summary indexes
##' @param ns number of random samples of the posterior draws for use in computing HPD intervals for accuracy indexes
##' @param title title of output, constructed by default
##' @param ... passed to `prModFit`
##' @examples
##' \dontrun{
##'   f <- blrm(...)
##'   options(lang='html')   # default is lang='plain'; also can be latex
##'   f               # print using defaults
##'   print(f, posterior.summary='median')   # instead of post. means
##' }
##' @author Frank Harrell
##' @export
print.blrm_mod <- function(x, dec=4, coefs=TRUE, intercepts=x$non.slopes < 10,
                       prob=0.95, ns=400, title=NULL, ...) {
        latex <- Hmisc::prType() == 'latex'
        
        if(! length(title))
                title <- paste0('Bayesian',
                                if(length(x$cppo))        ' Constrained',
                                if(x$pppo > 0)            ' Partial',
                                if(length(x$ylevels) > 2) ' Proportional Odds Ordinal',
                                ' Logistic Model')
        iprior <- x$iprior
        if(! length(iprior)) iprior <- 0   ## backward compatibility
        iprior <- as.character(iprior)
        conc   <- x[['conc']]
        ascale <- x$ascale
        subtitle <-
                switch(iprior,
                       '0' = paste('Dirichlet Priors With Concentration Parameter',
                                   round(conc, 3), 'for Intercepts'),
                       '1' = 'Non-informative Priors for Intercepts',
                       '2' = paste('t-Distribution Priors With 3 d.f. and Scale Parameter',
                                   round(ascale, 2), 'for Intercepts')
                )
        
        z <- list()
        k <- 0
        
        if(length(x$freq) > 3 && length(x$freq) < 50) {
                k <- k + 1
                z[[k]] <- list(type='print', list(x$freq),
                               title='Frequencies of Responses')
        }
        if(length(x$na.action)) {
                k <- k + 1
                z[[k]] <- list(type=paste('naprint',class(x$na.action),sep='.'),
                               list(x$na.action))
        }
        qro <- function(x) {
                r <- round(c(median(x), HPDint(x, prob)), 4)
                paste0(r[1], ' [', r[2], ', ', r[3], ']')
        }
        ci  <- x$clusterInfo
        sigmasum <- NULL
        if(length(ci)) sigmasum <- qro(x$omega[, 'sigmag'])
        
        loo <- x$loo
        elpd_loo <- p_loo <- looic <- NULL
        if(length(loo)) {
                lo <- loo$estimates
                pm <- if(prType() == 'plain') '+/-' else
                        markupSpecs[[prType()]][['plminus']]
                nlo <- rownames(lo)
                lo <- paste0(round(lo[, 'Estimate'], 2), pm, round(lo[, 'SE'], 2))
                elpd_loo <- lo[1]; p_loo <- lo[2]; looic <- lo[3]
        }
        Ncens <- x$Ncens
        L     <- if(Ncens[1] > 0) paste0('L=', Ncens[1])
        R     <- if(Ncens[2] > 0) paste0('R=', Ncens[2])
        int   <- if(Ncens[3] > 0) paste0('I=', Ncens[3])
        ce    <- if(sum(Ncens) > 0) sum(Ncens)
        ced   <- if(sum(Ncens) > 0)
                paste0(paste(c(L, R, int), collapse=', '))
        misc <- rms::reListclean(Obs             = x$N,
                                 Censored        = ce,
                                 ' '             = ced,
                                 Draws           = nrow(x$draws),
                                 Chains          = x$chains,
                                 Imputations     = x$n.impute,
                                 p               = x$p,
                                 'Cluster on'    = ci$name,
                                 Clusters        = ci$n,
                                 'sigma gamma'   = sigmasum)
        
        if(length(x$freq) < 4) {
                names(x$freq) <- paste(if(latex)'~~' else ' ',
                                       names(x$freq), sep='')
                misc <- c(misc[1], x$freq, misc[-1])
        }
        a <- blrmStats(x, ns=ns)$text
        mixed <- rms::reListclean('LOO log L'  = elpd_loo,
                                  'LOO IC'      = looic,
                                  'Effective p' = p_loo,
                                  B             = a['B'])
        
        disc <- rms::reListclean(g       = a['g'],
                                 gp      = a['gp'],
                                 EV      = a['EV'],
                                 v       = a['v'],
                                 vp      = a['vp'])
        
        discr <- rms::reListclean(C       = a['C'],
                                  Dxy     = a['Dxy'])
        
        
        headings <- c('','Mixed Calibration/\nDiscrimination Indexes',
                      'Discrimination\nIndexes',
                      'Rank Discrim.\nIndexes')
        
        data <- list(misc, c(mixed, NA), c(disc, NA), c(discr, NA))
        k <- k + 1
        z[[k]] <- list(type='stats', list(headings=headings, data=data))
        
        if(coefs) {
                k <- k + 1
                z[[k]] <- list(type='coefmatrix',
                               list(bayes=print.rmsb(x, prob=prob, intercepts=intercepts,
                                                     pr=FALSE)))
        }
        
        footer <- if(length(x$notransX))
                paste('The following parameters remained separate (where not orthogonalized) during model fitting so that prior distributions could be focused explicitly on them:',
                      paste(x$notransX, collapse=', '))
        
        # mod
        df <- unlist(disc) %>% 
                as_tibble() %>% 
                mutate(`Discrimination Index` = names(unlist(disc)), .before = "value") %>%
                mutate(`Rank Discrimination` = c(names(unlist(discr)), "", "", "")) %>% 
                mutate(`value.` = c(unlist(discr), "", "", ""))
        
        
        rms::prModFit(x, title=title, z, digits=dec, coefs=coefs, footer=footer,
                      subtitle=subtitle, ...)
}


## General function to print model fit objects using latex, html, or regular
## print (the default)

prModFit <- function(x, title, w, digits=4, coefs=TRUE, footer=NULL,
                     lines.page=40, long=TRUE, needspace, subtitle=NULL, ...) {
        lang   <- prType()
        specs  <- markupSpecs[[lang]]
        transl <- switch(lang,
                         latex = latexTranslate,
                         html  = htmlTranslate,
                         plain = function(x) x)
        
        #  cca  <- htmlSpecial('combiningcircumflexaccent')
        nbsp <- htmlSpecial('nbsp')
        gt   <- transl('>')
        vbar <- transl('|')
        chi2 <- specs$chisq()
        beta <- htmlGreek('beta')
        
        R <- character(0)
        
        bverb <- function() {
                switch(lang,
                       html  = '<pre>',
                       latex = '\\begin{verbatim}',
                       plain = NULL)
        }
        
        everb <- function()
                switch(lang,
                       html  = '</pre>',
                       latex = '\\end{verbatim}',
                       plain = NULL)
        
        skipt  <- function(n=1) {
                if(n==0) return(character(0))
                if(n == 1) return('')
                specs$lineskip(n)
        }
        
        catl  <- function(x, skip=1, bold=FALSE, verb=FALSE, pre=0,
                          center=FALSE, indent=FALSE) {
                if(lang == 'latex') {
                        if(verb)
                                c('\\begin{verbatim}', skipt(pre),
                                  x,
                                  skipt(skip),
                                  '\\end{verbatim}')
                        else
                                c(skipt(pre),
                                  paste0(
                                          if(center) '\\centerline{'
                                          else if(!indent) '\\noindent ',
                                          if(bold) '\\textbf{',
                                          x,
                                          if(bold) '}',
                                          if(center) '}'),
                                  skipt(skip))
                } else if(lang == 'html') {
                        if(verb)
                                c('<pre>', skipt(pre),
                                  x,
                                  skipt(skip),
                                  '</pre>')
                        else
                                c(skipt(pre),
                                  paste0(if(center) '<div align=center>',
                                         if(bold) '<strong>',
                                         x,
                                         if(bold) '</strong>',
                                         if(center) '</div>'),
                                  skipt(skip))
                }
                else c(paste0(skipt(pre), x), skipt(skip))
        }
        latexVector <- function(x, ...)
                latexTabular(t(x), helvetica=FALSE, ...)
        
        if(length(x$fail) && x$fail) {
                return(catl('Model Did Not Converge.  No summary provided.',
                            bold=TRUE, pre=1, verb=TRUE))
        }
        
        R <- character(0)
        
        if(! missing(needspace) && lang == 'latex')
                R <- paste0('\\Needspace{', needspace, '}')
        
        lsub <- length(subtitle)
        if(title != '') R <- c(R, catl(title, pre=1, bold=TRUE,
                                       skip=1))
        ## was skip=if(lsub) 0 else 1
        if(lsub)
                for(i in lsub) R <- c(R, catl(subtitle[i], bold=FALSE))
        
        if(long) {
                R <- c(R, bverb(), deparse(x$call), everb(), '')
                ## dput(x$call) didn't work with rmarkdown because dput has no append=
        }
        
        for(z in w) {
                type <- z$type
                obj  <- z[[2]]
                titl <- z$title
                tex  <- z$tex
                if(! length(tex)) tex <- FALSE
                if(type == 'naprint.delete') {
                        if(lang == 'latex') {
                                type <- 'latex.naprint.delete'
                                tex <- TRUE
                        }
                        if(lang == 'html') type <- 'html.naprint.delete'
                }
                
                preskip <- z$preskip
                if(! length(preskip)) preskip <- 0
                if(! tex && length(titl)) R <- c(R, '', catl(titl, pre=preskip, skip=1))
                if(type == 'stats') {
                        R <- c(R, prStats(obj[[1]], obj[[2]], lang=lang))
                } else if(type == 'coefmatrix') {
                        if(coefs) {
                                pad <- function(x)
                                        switch(lang, 
                                               latex = paste0('~', x, '~'),
                                               html  = paste0(nbsp, x),
                                               plain  = x)
                                betan <- switch(lang,
                                                plain = 'Beta',
                                                html  = htmlGreek('beta'),
                                                latex = '$\\hat{\\beta}$')
                                
                                B   <- obj$bayes
                                if(length(B)) {
                                        U <- matrix('', nrow=nrow(B), ncol=ncol(B))
                                        for(i in 1:ncol(B)) {
                                                dig <- if(colnames(B)[i] == 'Symmetry') 2 else digits
                                                U[, i] <- pad(formatNP(B[, i], dig, lang=lang))
                                        }
                                        pn <- switch(lang, plain='Pr(Beta>0)',
                                                     html = paste0('Pr(', betan, transl('>'), '0)'),
                                                     latex = 'Pr$(\\beta>0)$')
                                        coltrans <- c(Mean     = paste('Mean', betan),
                                                      Median   = paste('Median', betan),
                                                      Mode     = paste('Mode', betan),
                                                      SE       = 'S.E.',
                                                      Lower    = 'Lower',
                                                      Upper    = 'Upper',
                                                      P        = pn,
                                                      Symmetry = 'Symmetry')
                                        colnames(U) <- coltrans[colnames(B)]
                                        rownames(U) <- rownames(B)
                                        betanames   <- rownames(B)
                                }
                                else  {
                                        errordf <- obj$errordf
                                        beta <- obj$coef
                                        betanames <- names(beta)
                                        se   <- obj$se
                                        Z    <- beta / se
                                        P    <- if(length(errordf)) 2 * (1 - pt(abs(Z), errordf))
                                        else
                                                1 - pchisq(Z ^ 2, 1)
                                        
                                        U    <- cbind('Coef' =
                                                              pad(formatNP(beta, digits, lang=lang)),
                                                      'S.E.' =
                                                              pad(formatNP(se,   digits, lang=lang)),
                                                      'Wald Z'  =
                                                              formatNP(Z,    2, lang=lang),
                                                      'Pr(>|Z|)' =
                                                              formatNP(P, 4, lang=lang, pvalue=TRUE))
                                        if(lang == 'latex')
                                                colnames(U) <- c('$\\hat{\\beta}$', 'S.E.', 'Wald $Z$',
                                                                 'Pr$(>|Z|)$')
                                        else
                                                if(lang == 'html')
                                                        colnames(U) <- c(htmlGreek('beta'),   # did have cca
                                                                         'S.E.', 'Wald <i>Z</i>',
                                                                         paste0('Pr(', gt, vbar, '<i>Z</i>', vbar, ')'))
                                        if(length(errordf))
                                                colnames(U)[3:4] <-
                                                switch(lang,
                                                       latex = c('$t$', 'Pr$(>|t|)$'),
                                                       html  = c('<i>t</i>', paste0('Pr(', gt, vbar, '<i>t</i>',
                                                                                    vbar, ')')),
                                                       plain = c('t',   'Pr(>|t|)') )
                                        
                                        rownames(U) <- betanames
                                        
                                        if(length(obj$aux)) {
                                                U <- cbind(U, formatNP(obj$aux, digits, lang=lang))
                                                colnames(U)[ncol(U)] <- obj$auxname
                                        }
                                }
                                if(lang %in% c('latex', 'html')) {
                                        R <- c(R, skipt(1))
                                        rownames(U) <- transl(betanames)
                                        
                                        if(is.numeric(coefs)) {
                                                U <- U[1:coefs,,drop=FALSE]
                                                U <- rbind(U, rep('', ncol(U)))
                                                rownames(U)[nrow(U)] <- if(lang == 'html') '&hellip;' else '\\dots'
                                        }
                                        ## Translate interaction symbol (*) to times symbol
                                        rownames(U) <- gsub('*', specs$times, rownames(U), fixed=TRUE)
                                        
                                        if(! missing(needspace) && lang == 'latex')
                                                R <- c(R, paste0('\\Needspace{', needspace, '}'))
                                        
                                        if(lang == 'latex') 
                                                R <- c(R,   # was capture.output(latex())
                                                       capture.output(latex(U, file='',
                                                                            first.hline.double=FALSE,
                                                                            table=FALSE, longtable=TRUE,
                                                                            lines.page=lines.page,
                                                                            col.just=rep('r',ncol(U)), rowlabel='',
                                                                            already.math.col.names=TRUE,
                                                                            append=TRUE)))
                                        else {
                                                al <- paste(rep('r', ncol(U)), collapse='')
                                                R <- c(R, as.character(
                                                        htmlTable::htmlTable(U,
                                                                             css.cell = 'min-width: 7em;',
                                                                             align=al, align.header=al,
                                                                             rowlabel='', escape.html=FALSE)))
                                        }
                                } else {
                                        if(is.numeric(coefs)) {
                                                U <- U[1:coefs,,drop=FALSE]
                                                U <- rbind(U, rep('', ncol(U)))
                                                rownames(U)[nrow(U)] <- '. . .'
                                        }
                                        R <- c(R, '', capture.output(print(U, quote=FALSE)), '')
                                }
                        }   ## end if(coefs)
                }     ## end coefmatrix
                else {
                        if(tex) {    ### ??? how does this apply to html?
                                R <- c(R, '\\begin{center}',
                                       if(length(titl)) c(titl, '\n'))
                        } else {
                                R <- c(R,  skipt(preskip))
                        }
                        R <- c(R,
                               if(type == 'html.naprint.delete')
                                       do.call(type, obj)
                               else
                                       if(type == 'latex.naprint.delete')
                                               capture.output(do.call(type,
                                                                      c(obj, list(file=''))))
                               else
                                       if(type == 'print')
                                               c(bverb(), capture.output(do.call(type, obj)), everb())
                               else
                                       do.call(type, obj),
                               ## unlike do.call, eval(call(...)) dispatches on class of ...
                               if(tex) '\\end{center}' else ''
                        )
                }
        }
        if(length(footer))
                R <- c(R, paste(specs$smallskip, transl(footer)))
        
        R <- paste0(R, '\n')
        switch(lang,
               html  = htmltools::HTML(R),
               latex = cat(R),
               plain = cat(R))
}




prStats <- function(labels, w, lang=c('plain', 'latex', 'html')) {
        
        lang  <- match.arg(lang)
        lorh  <- lang != 'plain'
        specs <- markupSpecs[[lang]]
        
        partial <- htmlSpecial('part')
        vbar    <- htmlTranslate('|')
        cca     <- htmlSpecial('combiningcircumflexaccent')
        beta    <- htmlGreek('beta')
        geq     <- htmlTranslate('>=')
        
        
        spaces <- function(n) if(n <= 0.5) '' else
                substring('                                                         ',
                          1, floor(n))
        ## strsplit returns character(0) for ""
        ssplit <- function(x) {
                x <- strsplit(x, split='\n')
                for(i in 1 : length(x)) if(! length(x[[i]])) x[[i]] <- ''
                x
        }
        trans <- switch(lang,
                        latex = latexTranslate,
                        html  = htmlTranslate,
                        plain = function(x) x )
        ## Find maximum width used for each column
        p <- length(labels)
        width <- numeric(p)
        for(i in 1:p) {
                labs <- ssplit(labels[i])[[1]]
                width[i] <- max(nchar(labs))
                u <- w[[i]]
                dig <- NA
                if(any(names(u)=='')) {
                        dig <- unlist(u[names(u) == ''])
                        u   <- u[names(u) != '']
                }
                lu  <- length(u)
                dig <- rep(dig, length=lu)
                fu  <- character(lu)
                for(j in 1 : length(u)) {
                        uj <- u[[j]]
                        nuj <- names(u)[j]
                        dg <- dig[j]
                        fu[j] <- if(nuj == 'Cluster on') specs$code(trans(uj))
                        else
                                if(nuj == 'max |deriv|')
                                        formatNP(signif(uj, 1), lang=lang)
                        else
                                if(is.na(dg)) format(uj)
                        else
                                if(dg < 0) formatNP(uj, -dg, pvalue=TRUE, lang=lang)
                        else
                                formatNP(uj, dg, lang=lang)
                }
                names(fu) <- names(u)
                w[[i]]    <- fu
                for(j in 1 : length(u))
                        width[i] <- max(width[i],
                                        1 + nchar(nuj) + nchar(fu[j]))
        }
        if(lorh) {
                maxl <- max(sapply(w, length))
                z <- matrix('', nrow=maxl, ncol=p)
                fil <- if(lang == 'latex') '~\\hfill ' else htmlSpecial('emsp')
                
                chisq <- specs$chisq()
                
                trans <- rbind(
                        'Dxy'        = c(latex = '$D_{xy}$',
                                         html  = '<i>D</i><sub>xy</sub>'),
                        'LR chi2'    = c(latex = paste0('LR ', chisq),
                                         html  = paste0('LR ', chisq)),
                        'Score chi2' = c(latex = paste0('Score ', chisq),
                                         html  = paste0('Score ', chisq)),
                        'Pr(> chi2)' = c(latex = 'Pr$(>\\chi^{2})$',
                                         html  = paste0('Pr(', htmlTranslate('>'), chisq, ')')),
                        'tau-a'      = c(latex = '$\\tau_{a}$',
                                         html  = paste0(htmlGreek('tau'), '<sub>a</sub>')),
                        'sigma gamma'= c(latex = '$\\sigma_{\\gamma}$',
                                         html  = '&sigma;<sub>&gamma;</sub>'),
                        'sigma w'    = c(latex = '$\\sigma_{w}$',
                                         html  = '&sigma;<sub>w</sub>'),
                        'gamma'      = c(latex = '$\\gamma$',
                                         html  = htmlGreek('gamma')),
                        'R2'         = c(latex = '$R^{2}$',
                                         html  = '<i>R</i><sup>2</sup>'),
                        'R2 adj'     = c(latex = '$R^{2}_{\\textrm{adj}}$',
                                         html  = paste0('<i>R</i>', specs$subsup('adj', '2'))),
                        'C'          = c(latex = '$C$',
                                         html  = '<i>C</i>'),
                        'g'          = c(latex = '$g$',
                                         html  = '<i>g</i>'),
                        'gp'         = c(latex = '$g_{p}$',
                                         html  = '<i>g</i><sub>p</sub>'),
                        'gr'         = c(latex = '$g_{r}$',
                                         html  = '<i>g</i><sub>r</sub>'),
                        'max |deriv|'   = c(latex = '$\\max|\\frac{\\partial\\log L}{\\partial \\beta}|$',
                                            html  = paste0('max ', vbar, partial,
                                                           'log <i>L</i>/', partial,
                                                           beta, vbar)),
                        'mean |Y-Yhat|' = c(latex = 'mean $|Y-\\hat{Y}|$',
                                            html  = paste0('mean ', vbar, '<i>Y - Y</i>',
                                                           cca, vbar)),
                        'Distinct Y'   = c(latex = 'Distinct $Y$',
                                           html  = 'Distinct <i>Y</i>'),
                        'Median Y'   = c(latex = '$Y_{0.5}$',
                                         html  = '<i>Y</i><sub>0.5</sub>'),
                        '|Pr(Y>=median)-0.5|'  =
                                c(latex = '$|\\overline{\\mathrm{Pr}(Y\\geq Y_{0.5})-\\frac{1}{2}}|$',
                                  html  = paste0('<span style="text-decoration: overline">', vbar,
                                                 'Pr(<i>Y</i> ', geq, ' median)-',
                                                 htmlSpecial('half'), vbar,
                                                 '</span>'))
                        
                )
                
                for(i in 1 : p) {
                        k <- names(w[[i]])
                        for(j in 1 : length(k)) {
                                u <- k[j]
                                k[j] <- if(u %in% rownames(trans)) trans[u, lang]
                                else if(grepl('R2\\(', u))   # handle R2(p,n) from R2Measures
                                        switch(lang,
                                               plain = u,
                                               latex = sub('R2\\((.*)\\)', '$R^{2}_{\\1}$', u),
                                               html  = sub('R2\\((.*)\\)',
                                                           paste0('<i>R</i>',
                                                                  specs$subsup('\\1', '2')),u))
                                else
                                        switch(lang,
                                               plain = u,
                                               latex = latexTranslate(u, greek=TRUE),
                                               html  = htmlTranslate (u, greek=TRUE) )
                        }
                        z[1 : length(k), i] <- paste0(k, fil, w[[i]])
                }
                
                al <- paste0('|', paste(rep('c|', p), collapse=''))
                if(lang == 'latex')
                        w <- latexTabular(z, headings=labels, align=al, halign=al,
                                          translate=FALSE, hline=2, center=TRUE)
                else {
                        labels <- gsub('\n', '<br>', labels)
                        w <- htmlTable::htmlTable(z,
                                                  header=labels,
                                                  css.cell = 'min-width: 9em;',
                                                  align=al, align.header=al,
                                                  escape.html=FALSE)
                        w <- htmltools::HTML(paste0(w, '\n'))
                }
                return(w)
        }
        z <- labs <- character(0)
        for(i in 1:p) {
                wid <- width[i]
                lab <- ssplit(labels[i])[[1]]
                for(j in 1:length(lab))
                        lab[j] <- paste0(spaces((wid - nchar(lab[j])) / 2), lab[j])
                labs <- c(labs, paste(lab, collapse='\n'))
                u   <- w[[i]]
                a <- ''
                for(i in 1:length(u))
                        a <- paste0(a, names(u)[i],
                                    spaces(wid - nchar(u[i]) - nchar(names(u[i]))),
                                    u[i],
                                    if(i < length(u)) '\n')
                z <- c(z, a)
        }
        res <- rbind(labs, z)
        rownames(res) <- NULL
        capture.output(print.char.matrix(res, vsep='', hsep='    ', csep='',
                                         top.border=FALSE, left.border=FALSE))
}




getParamCoef <- function(fit, posterior.summary=c('mean', 'median', 'mode'),
                         what=c('both', 'betas', 'taus')) {
        posterior.summary <- match.arg(posterior.summary)
        what              <- match.arg(what)
        param <- fit$param
        if(what == 'both') i <- TRUE
        else {
                pppo <- fit$pppo
                if(! length(pppo)) pppo <- 0
                if(pppo == 0 && what == 'taus')
                        stop('taus requested but model is not a partial prop. odds model')
                p <- ncol(param)
                i <- switch(what,
                            betas = 1 : (p - pppo),
                            taus  = (p - pppo + 1) : p)
        }
        if(posterior.summary == 'mode' && 'mode' %nin% rownames(param))
                stop('posterior mode not included in model fit')
        param[posterior.summary, i]
}



# mod from primt rmsb function / rmsb package
getRmsb <- function(x, prob=0.95, dec=2, intercepts=FALSE, pr=TRUE, ...) {
        nrp   <- num.intercepts(x)
        s     <- x$draws
        param <- t(x$param)
        if(! intercepts && nrp > 0) {
                s     <- s[,   -(1 : nrp),  drop=FALSE]
                param <- param[-(1 : nrp),, drop=FALSE]
        }
        means <- param[, 'mean']
        colnames(param) <- Hmisc::upFirst(colnames(param))
        
        se  <- sqrt(diag(var(s)))
        hpd <- apply(s, 2, HPDint, prob=prob)
        P   <- apply(s, 2, function(u) mean(u > 0))
        sym <- apply(s, 2, distSym)
        w <- cbind(param, SE=se, Lower=hpd[1,], Upper=hpd[2,], P, Symmetry=sym)
        rownames(w) <- names(means)
        if(! pr) return(w)
        # cat(nrow(s), 'draws from the posterior distribution\n\n')
        # mod
        df <- as.data.frame(round(w, dec)) %>% 
                select(Median, SE, Lower, Upper, P) 
        return(df)
        # round(w, dec)
}

# Test Kable - Visualization within Table
data.frame(
        Variable = rownames(df),
        Visualization = ""
) %>%
        kbl(booktabs = T) %>%
        kable_classic(full_width = FALSE) %>%
        column_spec(2, image = spec_pointrange(
                x = df[,1],
                xmin = df[,3],
                xmax = df[,4],
                vline = 1)
        )


appendChar <- function(c, var, m){
        l <- tibble(label = m$Design$label, var = m$Design$name)
        c <- paste0(var, " = ", l %>% filter(var==!!var) %>% select(label) %>% .[[1]], "; ")
        return(c)
}

getFootnote <- function(m){
        name <- m$Design$colnames %>% str_extract("^\\w*(?=\\W|$)") %>% unique()
        c <- ""
        c <- map_chr(name, ~appendChar(c, ., m))
        fn <- paste0("Variables: ", paste0(c, collapse=''))
        return(fn)
}

# getTableRsmb - Extracts defined information from RSMB-Model + print if desired
getTableRsmb <- function(m, print = TRUE, exp = TRUE) {
        tblcount <- 1
        # get Labels
        
        l <- tibble(label = m$Design$label, var = m$Design$name)
        
        m2 <- m # for exp = TRUE
        
        m <- getRmsb(m)
        if (print == FALSE) return(m)
        
        # print beta
        if (exp == FALSE) {
                m <- m %>% mutate(`Odds Ratio (95% CrI)` = "")
                var <- data.frame(var = rownames(m)) %>% 
                        mutate(extr = str_extract(var, ".*?(?=\\=|$)"))
                var <- left_join(var, l, by = c("extr" = "var"))
                
                
                
                kable(
                m, 
                caption = paste0(
                        "Table ",
                        tblcount,
                        ": Bayesian Proportional Odds Ordinal Logistic Model"
                ),
                booktabs = T,
                escape = F,
                col.names = linebreak(
                        c(
                                paste0("median $\\hat{\\beta}$"),
                                "standard error",
                                "95% Credible Interval Lower",
                                "95% Credible Interval Upper ",
                                "Posterior probability",
                                "vis"
                        )
                )) %>%
                kable_styling(full_width = TRUE) %>% 
                kable_classic(lightable_options = "striped") %>%
                column_spec(length(m) + 1, image = spec_pointrange(
                        x = m[, 1],
                        xmin = m[, 3],
                        xmax = m[, 4],
                        vline = 1
                )) %>%
                footnote(
                        c(
                                getFootnote(m2),
                                "marks: Quotation marks (') indicate non-linear terms (restricted cubic splines)",
                                ""
                        )
                )
        } else { 
                # print odds ratios 95% CrI
                s <- summary(m2)
                var <- data.frame(rownames(s), s %>% as_tibble() %>% select(Type)) %>% as_tibble() %>% filter(Type==1) %>% select(var = 1) %>% 
                                mutate(extr = str_extract(var, ".*?(?=\\s|$)"))
                var <- left_join(var, l, by = c("extr" = "var"))
                df2 <- s %>% 
                        as_tibble() %>% 
                        filter(Type == 2) %>% 
                        # mutate(var = var$extr, .before = "Low") %>% 
                        select(-Type, -Diff., - S.E.) %>% 
                        round(digits = 2) %>% 
                        mutate(`vis` = "")
                        
                df2 <- data.frame(df2)
                rownames(df2) <- var$label
                df2 <- df2 %>% arrange(desc(Effect))
                
                
                kable(
                        df2, 
                        caption = paste0(
                                "Table ",
                                tblcount,
                                ": Effect measures (odds ratio) conditioning  Bayesian Proportional Odds Ordinal Logistic Model"
                        ),
                        booktabs = T,
                        escape = F,
                        col.names = linebreak(
                                c(
                                        paste0("Contrast between"),
                                        "<-> and",
                                        "Odds ratio",
                                        "95% Credible Interval Lower",
                                        "95% Credible Interval Upper ",
                                        "vis"
                                )
                        )) %>%
                        kable_styling(full_width = TRUE) %>% 
                        kable_classic(lightable_options = "striped") %>%
                        column_spec(length(df2), image = spec_pointrange(
                                x = rownames(df2),
                                xmin = df2[, 3],
                                xmax = df2[, 4],
                                vline = 1
                        )) %>%
                        footnote(
                                c(
                                        paste0("Effect measures were adjusted to: ", attributes(s)$adjust)
                                )
                        )      
        }
        
}          
              
              

kable(df2) %>% kable_classic() %>% column_spec(column=7,  image = spec_pointrange(x = df2[1], xmin = df2[, 3], xmax = df2[, 4], vline = 1))
df3 <- getRmsb(m_imt5)

