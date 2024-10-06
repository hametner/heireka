# COLORS ---

colors_ch_blue_generic <- c('#004178', '#006C9E', '#0096A9', '#00BE9A', '#87E07F', '#F9F871')
colors_ch_red_match  <- c('#9A0000', '#B90055','#AF40A4','#7175E1','#009EFB','#00BDF1')
colors_ch_red_to_blue <- c('#9a0000', '#9a003b', '#7e1664', '#4f347a', '#004178')



#' @title plotSexDiffGradient - Plots the difference between sex using different distribution values of the variable of interest in a Ridges-Density-Plot format
#' @description Plots the difference between sex using different distribution values of the variable of interest in a Ridges-Density-Plot format
#' @param df data frame / tibble the model was build on 
#' @param model name of model - supported in this version only "rmsb" models 
#' @param var Variable of interest - e.g. "imt"
#' @param xlab provide a plot description for x-axis
#' @param ylab provide a plot description for y-axis#'
#' @import ggridges
#' @import ggpubr
#' @import ggtext
#' @import tidyverse
#' @import rms
#' @return A ggplot using ggridges package
#'
plotSexDiffGradient <- function(df, model, var, dist=NULL, xlab="", ylab="", base_size=10){
        require(rms)
        require(dplyr)
        require(tidyr)
        require(ggplot2)
        require(ggtext)
        require(ggridges)
        require(ggpubr)
        require(Hmisc)
        
        # getting distribution of desired variable
        # check if categorical
        if (is.factor(df[[paste(var)]])) {
                factors <- df[[paste(var)]] %>% levels
                l1 <- list(sex = "female")
                l1[[paste(var)]] <- factors
                
                l2 <- list(sex = "male")
                l2[[paste(var)]] <- factors
        } else {
                # check if continous
                x <- df %>% select(!!var) %>% Hmisc::describe()
                
                # extract the values that indicate .05 .10 .25 .50 .75 .90 .95
                if (is.null(dist)) {
                        dist <- parse_number(unlist(map(c(7:13), ~ x[[var]][["counts"]][.][[1]])))
                }
                
                # build contrast
                l1 <- list(sex = "female")
                l1[[paste(var)]] <- dist
                
                l2 <- list(sex = "male")
                l2[[paste(var)]] <- dist
        }
        
        c <- contrast(model,l1,l2)
        
        
        #updateLabels from data_dic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        var_label <- updateLabels(var, data_dic)
        
        # if (ylab == "") ylab <- paste(getLabel(var, df))        
        if (ylab == "") ylab <- data_dic %>% filter(name==var) %>% pull(label)
        
        
        # if (xlab == "") xlab <- paste(getLabel(model$yname, df))
        if (xlab == "") xlab <- data_dic %>% filter(name==model$yname) %>% pull(label)
        
        
        #check variable
        # determine position
        pos <- which(model$Design$name == var)
        variable_assume <- model$Design$assume[pos]
        
        
        c_df <- c$cdraws
        colnames(c_df) <- c[[var]]
        c_df <- c_df %>% 
                as_tibble() %>% 
                pivot_longer(cols = 1:ncol(.), names_to = "y") 
        
        if (variable_assume=="rcspline") {
                c_df <- c_df %>% arrange(-desc(parse_number(as.character(y)))) %>%
                        # mutate(y=as.factor(y))
                        mutate(y = factor(y, levels = unique(y[order(parse_number(as.character(y)))])))
        }
                
        if (variable_assume=="category"){
                c_df <- c_df %>% mutate(y = factor(y))
        }
        
        ggplot(c_df, aes(x = value, y = y, fill = factor(after_stat(quantile)))) +
                geom_vline(xintercept = 0, colour = "darkgrey") +
                stat_density_ridges(
                        geom = "density_ridges_gradient",
                        calc_ecdf = TRUE,
                        quantiles = c(0.025, 0.975),
                        color="grey",
                        fill="grey20",
                        show.legend = FALSE
                ) +
                scale_fill_manual(
                        # name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
                        name = "Probability",
                        values = c("white", "#A0A0A0A0", "white"),
                        labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
                ) +
                # coord_cartesian(clip = "off") +
                theme_pubclean(base_size = base_size) +
                theme(axis.text.y = element_text(vjust = 0))+
                xlab(xlab) +
                ylab(ylab) +
                theme(axis.title.x = element_markdown())
}

# Give the df, the variable of choice and the grouping variable
# Get a ggplot grouped bar chart with count and percentages as description
#choose "bar" plot or "lollipop"
plot_grouped_bar_chart <- function(df, var, group_var, chart_type = "bar"){
        library(dplyr)
        library(ggplot2)
        
        # Convert the variable names to symbols for use in dplyr
        var_sym <- sym(var)
        group_var_sym <- sym(group_var)

        # Create a new factor level order with "Missing" at the end
        original_levels <- df %>%
                pull(!!var_sym) %>%
                levels() %>%
                .[!is.na(.)] # Get the original levels excluding NA
        
        if (is.null(original_levels)) { original_levels <- "available" }
        
        # Add "Missing" to the end
        new_levels <- c(original_levels, "Missing")
        
        # Treat NA as a category and count occurrences by group_var and var
        # count_data <- df %>%
        #         mutate(!!var_sym := ifelse(is.na(!!var_sym), "Missing", as.character(!!var_sym))) %>%
        #         mutate(!!var_sym := factor(!!var_sym, levels = new_levels)) %>%
        #         group_by(!!group_var_sym, !!var_sym) %>%
        #         summarize(count = n(), .groups = 'drop') %>%
        #         mutate(percentage = count / sum(count) * 100) # Calculate percentage
        # 
        count_data <- df %>%
                # Convert NA values to "Missing" and treat the variable as a factor with updated levels
                mutate(!!var_sym := ifelse(is.na(!!var_sym), "Missing", as.character(!!var_sym))) %>%
                mutate(!!var_sym := factor(!!var_sym, levels = new_levels)) %>%
                # Group by both group_var and var
                group_by(!!group_var_sym, !!var_sym) %>%
                # Summarize counts for each group
                summarize(count = n(), .groups = 'drop') %>%
                # Now, group by group_var to calculate percentages within each group
                group_by(!!group_var_sym) %>%
                # Mutate the data to calculate the percentage for each group
                mutate(percentage = count / sum(count) * 100) %>%
                ungroup() 
        
        # Determine the maximum count value to adjust the y-axis limit
        max_count <- max(count_data$count)
        #updateLabels from data_dic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        count_data <- updateLabels(count_data, data_dic)
        
        
        # Create the plot based on the chart_type argument
        if (chart_type == "bar") {
                # Create the grouped bar graph with percentages as labels
                p <- ggplot(count_data, aes(x = !!var_sym, y = count, fill = !!group_var_sym, alpha = !!var_sym)) +
                        geom_bar(stat = "identity", position = position_dodge()) +
                        geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                                  position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
                        labs(title = "",
                             x = attr(count_data[[var]], which = "label"),
                             y = "Count",
                             fill = group_var) +
                        theme_ch() +
                        scale_fill_colorblind() +
                        scale_alpha_manual(values = c("Missing" = 0.5, "Other" = 1), guide = 'none') +
                        theme(legend.position = "bottom") +
                        ylim(0, max_count * 1.1) # Add some space above the highest bar
        } else if (chart_type == "lollipop") {
                # Create the lollipop chart with percentages as labels
                p <- ggplot(count_data, aes(x = !!var_sym, y = count, color = !!group_var_sym, alpha = !!var_sym)) +
                        geom_linerange(aes(ymin = 0, ymax = count), 
                                       position = position_dodge(width = 0.9), linewidth = 0.3) +
                        geom_point(position = position_dodge(width = 0.9), size = 3) +
                        geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                                  position = position_dodge(width = 0.9), vjust = -1, size = 3) +
                        labs(title = "",
                             x = attr(count_data[[var]], which = "label"),
                             y = "Count",
                             color = group_var) +
                        theme_ch() +
                        scale_color_colorblind() +
                        scale_alpha_manual(values = c("Missing" = 0.5, "Other" = 1), guide = 'none') +
                        theme(legend.position = "bottom") +
                        ylim(0, max_count * 1.1) # Add some space above the highest bar
        } else {
                stop("Invalid chart type. Please choose 'bar' or 'lollipop'.")
        }
        
        # Add a vertical line to separate the "Missing" category (only for bar chart)
        p <- p + geom_vline(xintercept = length(new_levels) - 0.5, linetype = "dashed", color = "lightgrey")
        
        # Print the plot
        return(p)
}






# get Label from dataset function
# param d, m either dataset or rmsb model (todo) can be supplied
getLabel <- function(v, d){
        l <- data.frame(label = Hmisc::label(d))
        l[rownames(l)==v,]


}

#' @title getTabInfo 
#' @description Information Table from rsmb model
#' @param m RSMB Model
#' @param d associated data.frame the model (m) was built on
#' @param voi Variable Of Interest - supply to indication what is of specific interest
#' @import tidyverse
#' @import Hmisc
#' @return A latex table, usable via knitr/markdown (print=TRUE) or a data frame (print=FALSE)
getTabInfo <- function(m, d, voi=NULL, print=TRUE, caption = "Bayesian Proportional Odds Ordinal Logistic Model"){
        l <- tibble(label = m$Design$label, var = m$Design$name)
        l1 <- c("Outcome of interest:", getLabel(m$yname, d), names(m$freq))
        l2 <- c("Frequency of responses:", paste0("N = ",m$N), unname(m$freq))
        l3 <- c("Variable of interest:", voi, rep("", length(l1)-2))
        if (is.null(voi))
                df <- data.frame(l1, l2) %>% t() %>% as_tibble()
        else
                df <- data.frame(l1, l2, l3) %>% t() %>% as_tibble()
        # print if True otherwise return df
        if (print) {
                kable(df, 
                    # caption = paste0("Table ",tblcount,": ", "Bayesian Proportional Odds Ordinal Logistic Model"),
                    caption = caption,
                    col.names = NULL,
                    format = "latex",
                    booktabs = TRUE, 
                    align = c("l", rep("r", times=length(l1)-1)),
                    escape = F
                    ) %>%  
                        kable_classic(
                                full_width = F,
                                latex_options = c("hold_position")
                                      )
        } else return(df)
}

formula_to_latex <- function(formula_string) {
        # Replace the parts of the formula
        formula_string <- gsub("~", "=", formula_string)
        formula_string <- gsub("\\+", "+ \\beta_", formula_string, perl = TRUE)
        formula_string <- gsub("rcs\\(([^,]+), ([0-9]+)\\)", "rcs(\\text{\\1}, \\2)", formula_string, perl = TRUE)
        formula_string <- gsub("(\\w+)", "\\text{\\1}", formula_string)
        
        # Convert it to a full regression representation
        latex_formula <- paste("\\text{", formula_string, "} + \\epsilon", sep = "")
        
        # Handle beta coefficients
        terms <- unlist(strsplit(formula_string, "[+=]"))
        terms <- trimws(terms)
        for (i in 2:length(terms)) {
                beta_name <- paste0("\\beta_{", i - 1, "}")
                term_name <- terms[i]
                latex_formula <- gsub(paste0("\\text{", term_name, "}"), paste0(beta_name, " \\times \\text{", term_name, "}"), latex_formula, fixed = TRUE)
        }
        
        return(latex_formula)
}

wrap_interaction_format <- function(input_string) {
        pattern <- "(.+) \\* (.+)"
        replacement <- "Interaction [\\1 * \\2]"
        output_string <- gsub(pattern, replacement, input_string)
        if(output_string == input_string) {
                return(input_string)
        }
        return(output_string)
}

#' @title appendChar 
#' @description Based on variables in the rsmb model a character string is created for footnote indicating labels e.g. imt = intima-media-thickness;
#' @param c character string
#' @param var voriable for which string is created 
#' @param m RSMB Model
#' @import tidyverse
#' @import Hmisc
#' @return A character string
#'
appendChar <- function(c, var, m){
        l <- tibble(label = m$Design$label, var = m$Design$name)
        c <- paste0(var, " = ", l %>% filter(var==!!var) %>% select(label) %>% .[[1]], "; ")
        return(c)
}

#' @title getFootnote
#' @description generate Footnote description for classes "blrm" and "Predict" (rmsb package)
getFootnote <- function(m, adapt=c("kable","other"), add_modelstats=TRUE, double_escape=TRUE){
        library(tidyverse)
        # tar_load(data_dic)
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        if (any(class(m) == "blrm")){
                d <- paste0(m$sformula) %>% as_tibble()
                l <- updateLabels(
                        d,
                        data_dic,
                        rows = TRUE,
                        update_rows_of_col = "value",
                        updateInteraction = FALSE
                )
                formula <- paste0("Formula: ", l[2, ], " ", l[1, ], " ", l[3, ], ". ")
                outcome_str <- data_dic %>% filter(name == m$yname) %>% pull(label)
                freq <- paste0(
                        "Numbers of outcome categories for $\\\\textit{",
                        outcome_str,
                        "}$ were as follows: ",
                        m$freq %>% as_tibble() %>%
                                mutate(
                                        cat = names(m$freq),
                                        value = as.character(value),
                                        freq = paste0(cat, ": ", value)
                                ) %>%
                                pull(freq) %>%
                                paste0(collapse = ", "),
                        ". "
                )
                stan_comment <- capture.output(stan_diagnostic <- stanDx(m))
                stan_numbers <- str_extract_all(stan_comment[1], "\\d+")[[1]]
                sampl_time <- m$sampling_time
                ftn_stan <- paste0(
                        "The model was built using n=",
                        m$iter,
                        " iterations on each of ",
                        m$chains,
                        " chains. Posterior distribution sampling included n=",
                        nrow(m$draws),
                        " samples. Model execution time was ",
                        round(sampl_time, 1),
                        " seconds. "
                )
                
                stan_diagnostic <- stan_diagnostic %>% as_tibble() %>% mutate(name = rownames(stan_diagnostic))
                # rhat <- paste0("Lowest $\\\\hat{R}$ was: ", min(stan_diagnostic$Rhat), ". ($\\\\hat{R}$=1 for convergence of MCMC). ")
                rhat <- paste0(
                        "Lowest $\\\\hat{R}$ was: ",
                        min(stan_diagnostic$Rhat),
                        ". $\\\\hat{R}$=1 for convergence of MCMC. "
                )
                
                
                # construct footnote
                footnote <- paste0(freq, ftn_stan, rhat)
                
                if (add_modelstats) {
                        footnote <- paste0(footnote, prepRsmbModelStats(m, output = "chr"))
                }
                
                if (!double_escape) {
                        footnote <- gsub("\\\\\\\\", "\\\\", footnote)
                }
        }
        
        # t <- blrmStats(bm.delir.3)$stats %>% t()
        
        
        
        # brms.Predict object Footnote extraction for adjusting values
        if (class(m)[[1]] == "Predict") {
                all_vars <- attr(m, "out.attrs")$dimnames
                varying <- attr(m, "info")$varying
                if (is.character(setdiff(names(all_vars), varying)) && length(setdiff(names(all_vars), varying)) == 0) {
                        selected <- varying
                } else {
                        selected <- all_vars[setdiff(names(all_vars), varying)]
                }
                footnote <-
                        selected %>% unlist() %>% as_tibble() %>% updateLabels(data_dic,
                                                                               rows = TRUE,
                                                                               update_rows_of_col = "value") %>% pull(value) %>% paste0(collapse = ", ")
                # footnote <- paste0("\\scriptsize Prediction were adjusted to: ", footnote, " \\normalsize")
                footnote <- paste0("\\begingroup
                \\fontsize{6pt}{6pt}
                \\noindent(^1) 
                Prediction were adjusted to: ", footnote, ". \\endgroup")
                
                
        }
        return(footnote)
}


# mod from primt rmsb function / rmsb package
getRsmb <- function(x, prob=0.95, dec=2, dec_p=3, intercepts=FALSE, col_var=FALSE, pr=TRUE, interactions_only=FALSE, ...) {
        library(Hmisc)
        library(rmsb)
        library(tidyverse)
        nrp   <- num.intercepts(x)
        s     <- x$draws
        param <- round(t(x$param), dec)
        if(! intercepts && nrp > 0) {
                s     <- s[,   -(1 : nrp),  drop=FALSE]
                param <- param[-(1 : nrp),, drop=FALSE]
        }
        means <- param[, 'mean']
        colnames(param) <- Hmisc::upFirst(colnames(param))
        
        se  <- round(sqrt(diag(var(s))), dec)
        hpd <- round(apply(s, 2, HPDint, prob=prob), dec)
        P   <- round(apply(s, 2, function(u) mean(u > 0)), dec_p)
        sym <- apply(s, 2, distSym)
        
        
        w <- cbind(param, SE=se, Lower=hpd[1,], Upper=hpd[2,], P, Symmetry=sym)
        # rownames(w) <- names(means)
        if(! pr) return(w)
        # cat(nrow(s), 'draws from the posterior distribution\n\n')
        # mod
        df <- as.data.frame(w) %>% 
                dplyr::select(Median, SE, Lower, Upper, P) 
        if (col_var){ # variable names as a column
                df$variable <- rownames(df)
                df <- df %>% dplyr::select(variable, everything())
        }
        
        # if (interactions_only){
        #         df <- df %>% filter(grepl("\\*", variable))
        #         if (nrow(df)==0) { return("No interactions present - change to 'interactions_only=FALSE'")}
        # }
        return(df)

}


# takes output of "getRsmb" function and "contrast" function (rmsb-package) and 
# retrives median and CI in format "2.33 (95% credible interval 1.11--3.40)"
getMedCI <- function(df, var=NULL, exp=TRUE, via_summary_rms=TRUE, dec=3){
        # extracting med/ci from a variable in a model
        if (any(class(df) %in% c("blrm", "rmsb", "data.frame"))) {
                if (any(class(df) %in% c("blrm", "rmsb"))) { df <- getRsmb(df) }
                stats <- df[rownames(df)==var,]
        }
        
        # extractin med/ci from a contrast.rms calculation
        if (any(class(df) %in% c("contrast.rms"))) {
                stats <- data.frame(Median=df$Contrast, Upper=df$Upper, Lower=df$Lower)
                if (exp){ 
                        return(paste0("risk ratio ", round(exp(stats$Median),dec) , " (95% credible interval ", round(exp(stats$Lower),dec),"–", round(exp(stats$Upper),dec),")"))
                } else {
                        return(paste0("risk difference ", stats$Median, " (95% credible interval ",stats$Lower,"–",stats$Upper,")"))        
                        }
        }
        # if summary.rms odds ratios are calculated 
        if (any(class(df) %in% c("summary.rms"))){
                df <-  df %>%
                        as.data.frame() %>%
                        rownames_to_column(var = "variable") %>%
                        as_tibble()
                
                
                # Check if there is an exact match first
                exact_match <- which(df[["variable"]] == var)
                
                # If exact match is found, use that; otherwise, fall back to grepl
                if (length(exact_match) > 0) {
                        df <- df %>% filter(., row_number() == (exact_match + 1))
                } else {
                        grepl_match <- which(grepl(var, df[["variable"]]))[[1]]
                        df <- df %>% filter(., row_number() == (grepl_match + 1))
                }
                        
                return(paste0("odds ratio ", round(df$Effect, dec), ", 95% credible interval ", round(df$`Lower 0.95`,dec),"--",round(df$`Upper 0.95`,dec),", low: ", df$Low," vs. high: ", df$High,")"))
        }
        
        
        # check if do 'odds ratios'
        if (exp){ 
                return(paste0("odds ratio ", round(exp(stats$Median),2) , ", 95% credible interval ", round(exp(stats$Lower),2),"–", round(exp(stats$Upper),2),")"))
        } else {
                return(paste0("\beta ", stats$Median, " (95% credible interval ",stats$Lower,"–",stats$Upper,")"))                
        }
        
}
# takes output of "getRmsb" function and retrives Probability of Beta>0"
getP <- function(df_getRmsb, var, text=FALSE){
        df <- df_getRmsb
        sel <- df[rownames(df)==var,]
        p <- transform_P_value(sel$P)
        if (text) {
                return(paste0("probability $$(\\beta>0)$$: ", p))                
        } else {
                return(p)
        }
}

# prepare Bayes model stats either as 1) table or 2) chr-string
prepRsmbModelStats <- function(model, output = c("table", "chr")) {
        modelstats <-
                blrmStats(model)$stats %>% t %>% as.data.frame() %>% round(digits = 3)
        modelstats$variable <-
                c(
                        "Somers' Dxy rank correlation",
                        "c-index (Area Under Curve for binary Y)",
                        "Gini's mean difference",
                        "Gini's mean difference (probability scale)",
                        "Brier Score",
                        "Explained variation",
                        "Variance of linear predictor",
                        "Variable of estimated probabilities"
                )
        modelstats$P <- rep(NA_character_, 8)
        modelstats <-
                modelstats %>% select(variable, Median = Mean, SE, Lower, Upper, P)
        
        if (!(exists("output"))) {
                return("Please specify variable 'output'.")
        } else {
                if (output == "table") {
                        return(modelstats)
                }
                
                if (output == "chr") {
                        modelstats <- modelstats %>%
                                mutate(chr = paste0(
                                        variable,
                                        ": ",
                                        Median,
                                        " [",
                                        Lower,
                                        "--",
                                        Upper,
                                        "]"
                                ))
                        chr <- paste0("Model discrimination indices were as follows: ", paste(modelstats$chr, collapse = ", "), ".")
                        return(chr)
                }
                
        }
        
}

# Function to 1) extract from rmsb 2) make 'tidy' and 3) prepare for printing with 'kable'
# RETURNS a kable Latex table that could be further processed/modified
# output_format can be "latex" or "html"
getLatexTableCH <- function(model, add_modelstats=FALSE, add_n_eff=TRUE, interactions_only=FALSE, font_size=10, output_format = "latex", option="scale_down", ...){
        library(kableExtra)
        if (!is.numeric(font_size)) {font_size = 10}
        # extract from rmsb
        t <- getRsmb(model, dec=2, dec_p=3, intercepts=TRUE, col_var=TRUE, interactions_only = interactions_only, data_dic = data_dic)
        # add modelstats
        if (add_modelstats){
                t <- t %>% rbind(prepRsmbModelStats(model, output="table"))
        }
        # add effective N
        if (add_n_eff) {
                n_eff <- stanDx(model) %>% as.data.frame()
                n_eff$variable <- rownames(n_eff)
                t <- t %>% left_join(n_eff, by = c("variable"))
                t <- t %>% dplyr::select(variable, "$N_{eff}$"=n_eff, everything()) %>% dplyr::select(-Rhat)
        }
        # make 'tidy'
        t <- improve_table(tab = t, m = model, interactions_only = interactions_only, add_n_eff = add_n_eff, data_dic = data_dic)
        
        if (output_format == "html") {
                t <- t %>%
                        kable(format = "html", escape = TRUE, align = c("l", rep("r", 5))) %>%
                        kable_styling(full_width = FALSE, bootstrap_options = "condensed") %>%
                        kableExtra::footnote(general = c("Asterisk (*) if present indicates interaction terms.",
                                             "'spline' or quotation marks (') denote non-linear terms (restricted cubic splines).",
                                             getFootnote(model, add_modelstats = TRUE)),
                                 footnote_as_chunk = TRUE, 
                                 escape = TRUE)
        }
        
        if (output_format == "latex"){
                # prepare to print with 'kable'
        if (option=="scale_down"){
                t <- t %>% kable(booktabs=TRUE, "latex", escape = FALSE,  align=c("l",rep("r", 5)), linesep = "") %>% 
                        kable_styling(full_width = FALSE, latex_options = c( "repeat_header", "HOLD_position", option), font_size = font_size) %>% 
                        kableExtra::footnote(general=c("Asterisk (*) if present indicate interaction terms.", 
                                                       "'spline' or quotation marks (') denote non-linear terms (restricted cubic splines).",
                                                       getFootnote(model, add_modelstats=TRUE)),
                                             footnote_as_chunk=TRUE, threeparttable = TRUE, escape=FALSE)
                
        }
        if (option=="longtable"){
                t <- t %>% kable( "latex", booktabs=TRUE, longtable=TRUE, escape = FALSE,  align=c("l",rep("r", 5)), linesep = "") %>% 
                        kable_styling(full_width = FALSE, latex_options = c("repeat_header"), font_size = font_size)  %>% 
                        kableExtra::footnote(general=c("Asterisk (*) if present indicate interaction terms.",
                                                       "'spline' or quotation marks (') denote non-linear terms (restricted cubic splines).",
                                                       getFootnote(model, add_modelstats=TRUE)),
                                             footnote_as_chunk=TRUE, threeparttable = TRUE, escape=FALSE)
                
        }
                
        
        }
        return(t)
        # blrmStats(bm.delir.3)$stats %>% t()
        # "scale_down",
}


#old
# getTableRsmb - Extracts defined information from RSMB-Model + print if desired
# old - use getLatexTableCH instead
getTableRsmb <- function(m, 
                         print = TRUE, 
                         exp = TRUE, 
                         vis = TRUE, 
                         scale = TRUE,
                         update_labels = TRUE,
                         format = "latex", # or "html"
                         cols = c("3cm", "1.5cm", "1.5cm", "1.5cm", "1.5cm", "1.75cm", "1.75cm"),
                         caption = "Bayesian Proportional Odds Ordinal Logistic Model",
                         add_or_col = FALSE,
                         add_n_eff = FALSE,
                         add_rev = FALSE,
                         prep_for_forest = FALSE,
                         interactions_only = FALSE, data_dic = data_dic) {
        require(kableExtra)
        require(rms)
        require(rmsb)
        
        beta <- "${\\beta}$"
        # beta <- expression(beta)
        # l <- tibble(label = m$Design$label, var = m$Design$name)
        
        var <- data.frame(var = rownames(getRsmb(m))) 
        # groups <- m$Design$nonlinear %>% unlist() %>% as_tibble() %>% mutate(name = var)
        # tar_load(data_dic)
        if (update_labels) {
        labels <- updateLabels(var, data_dic, rows=TRUE, update_rows_of_col = "var")
        }
        m2 <- m # for exp = TRUE
        
        # add effective number of observations
        if (add_n_eff) {
                n_eff = stanDx(m) %>% as.data.frame()
                n_eff$var = rownames(n_eff)
        }
        
        m <- getRsmb(m)
        m <- m %>% mutate(variable = rownames(m)) %>% 
                select(variable, everything())
        
        if (add_n_eff) {
                # m$var <- rownames(m)
                m <- m %>% left_join(n_eff, by = c("var"))
                # m$var2 <- m2$Design$name
        }
        
        if (update_labels) {
                rownames(m) <- labels[[1]]
        }
        
        if (add_or_col) { 
                m$'odds ratio (95% credible interval)' <- paste0(
                sprintf("%.2f", exp(m$Median)), 
                " (", 
                sprintf("%.2f", exp(m$Lower)), 
                " to ", 
                sprintf("%.2f", exp(m$Upper)),
                ")")
        }
        
        if (prep_for_forest) {
                # add blanks 
                m$` ` <- paste(rep(" ", 30), collapse = " ")
        }
        
        if (add_rev){
                rev <- anova(m2) %>% as.data.frame()          
                rev$var <- rownames(rev)
        }
        
        if (interactions_only){
                m <- m %>% filter(grepl("\\*", variable))
                if (nrow(m)==0) { return("No interactions present - change to 'interactions_only=FALSE'")}
        }
        
        if (print == FALSE & exp == FALSE) {
                return(m)
        }
        # print beta
        if (exp == FALSE) {
                m <- m %>% mutate(`Odds Ratio (95% CrI)` = "")
         
                
                #old
                # var <- left_join(var, l, by = c("extr" = "var"))
                colnames(m) <- c(beta,
                                        "standard error",
                                        "Lower bound",
                                        "Upper bound",
                                        "Posterior probability",
                                        "vis")
                # colnames(m) <- paste0("{", colnames(m), "}")
                
                k <- kable(
                        m,
                        caption = caption,
                        format = format,
                        booktabs = TRUE, escape = FALSE
                ) %>%
                        kable_styling(full_width = FALSE) %>%
                        column_spec(length(m) + 1,
                                    image = spec_pointrange(
                                            x = m[, 1],
                                            xmin = m[, 3],
                                            xmax = m[, 4],
                                            vline = 1
                                    )) 
                if (scale) {
                        k <- k %>% kable_styling(latex_options = c("scale_down"))
                } else {
                        k <- k %>% 
                        column_spec(1, width = cols[1]) %>% 
                        column_spec(2, width = cols[2]) %>% 
                        column_spec(3, width = cols[3]) %>% 
                        column_spec(4, width = cols[4]) %>% 
                        column_spec(5, width = cols[5]) %>% 
                        column_spec(6, width = cols[6]) %>% 
                        column_spec(7, width = cols[7])
                        
                }
                k <- k %>% kableExtra::footnote(
                        c(
                                getFootnote(m2),
                                "marks: Quotation marks (') indicate non-linear terms (restricted cubic splines)",
                                "Asterisk (*) indicate interaction terms"
                        ),
                        # threeparttable = TRUE,
                        # escape=FALSE,
                        footnote_as_chunk = TRUE
                )
                return(k)
        } else { 
                # print odds ratios 95% CrI
                s <- summary(m2)
                
                var2 <- data.frame(rownames(s), s %>% as_tibble() %>% select(Type)) %>% as_tibble() %>% filter(Type==1) %>% select(var = 1) %>% 
                        mutate(extr = str_extract(var, ".*?(?=\\s|$)"))
                labels2 <- updateLabels(var2, data_dic, rows=TRUE, update_rows_of_col = "var")
                # var <- left_join(var, l, by = c("extr" = "var"))
                df2 <- s %>% 
                        as_tibble() %>% 
                        filter(Type == 2) %>% 
                        # mutate(var = var$extr, .before = "Low") %>% 
                        select(-Type, -Diff., - S.E.) %>% 
                        round(digits = 2) 
                # %>% 
                #         mutate(`vis` = "")
                
                df2 <- data.frame(df2)
                rownames(df2) <- labels2$var
                # df2 <- df2 %>% arrange(desc(Effect))
                
                if (print == FALSE) { 
                        
                        return(df2) 
                } else {
                        
                kable(
                        df2, 
                        # caption = paste0(
                        #         "Table ",
                        #         tblcount,
                        #         ": Effect measures (odds ratio) conditioning  Bayesian Proportional Odds Ordinal Logistic Model"
                        # ),
                        caption = caption,
                        format = format,
                        booktabs = T,
                        escape = F,
                        col.names = linebreak(
                                c(
                                        paste0("Contrast between"),
                                        "<-> and",
                                        "Odds ratio",
                                        "95% Credible Interval Lower",
                                        "95% Credible Interval Upper "
                                        # ,"vis"
                                )
                        )) %>%
                        kable_styling(latex_options = c("scale_down")) %>% 
                        # kable_styling(full_width = TRUE) %>% 
                        # kable_classic() %>%
                        # column_spec(length(df2)+ 1, image = spec_pointrange(
                        #         x = rownames(df2),
                        #         xmin = df2[, 3],
                        #         xmax = df2[, 4],
                        #         vline = 1
                        # )) %>%
                        kableExtra::footnote(
                                c(
                                        paste0("Effect measures were adjusted to: ", attributes(s)$adjust)
                                        # ,"vis indicates visualization of 95% credible intervals, dotted vertical line indicates an odds ratio of 1"
                                ),
                                threeparttable = TRUE,
                                escape=FALSE,
                                footnote_as_chunk = TRUE
                        )     
                }
        }
        
}        



# Function to improve the table based on design matrix from Frank Harrells rmsb package
# output 
improve_table <- function(tab, m, indent=1, interactions_only=FALSE, add_n_eff=TRUE, data_dic = data_dic) {
        results <- vector("list", length = length(m$Design$name))
        # tar_load(data_dic)

        design <- m$Design
        ia <- FALSE
        i <- 1
        end <- length(design$name)
        if (interactions_only){
                # design$name <- design$name[grep("\\*", design$name)]
                # i<-grep("\\*", tab[["variable"]])[1]
                i<-grep("\\*", design$name)[1] # first appearance of interaction
                if (is.na(i)) {
                        print("Model has no interactions - please set 'interaction_only=FALSE' (default)")
                }
                ia=TRUE # backup to check for interaction modus
                design$name2 <- design$name
        }
        
        for (i in i:end) {
                # design <- m$Design
                
                # check when interactions begin
                ia_begin <- grep("\\*", design$name)[1]
                if (is.na(ia_begin)){ 
                        ia <- FALSE
                } else if (i >= grep("\\*", design$name)[1]) {
                        ia <- TRUE         
                        }
                
                variable_name <- design$name[i]
                variable_assume <- design$assume[i]
                variable_label <- design$label[i]
                if (variable_assume=="interaction") {
                        var1 <- design$name[design$parms[[variable_name]][,1][1]]
                        var2 <- design$name[design$parms[[variable_name]][,1][2]]
                        variable_colname <- design$colnames[grepl(var1, design$colnames) & grepl(var2, design$colnames)][[1]]
                        variable_colnames <- design$colnames[grepl(var1, design$colnames) & grepl(var2, design$colnames)]
                } else {
                        variable_colname <- design$colnames[grepl(paste0("^",variable_name), design$colnames)][[1]]
                }
                # indent <- 1
                
                # Filter rows related to the current variable
                if (variable_assume!="interaction"){
                        rows <- tab %>% filter(grepl(paste0("^",variable_name), variable)) %>% 
                                filter(!(grepl("\\*", variable)))
                } else {
                        rows <- tab %>% 
                                filter(grepl(var1, variable)) %>% 
                                filter(grepl(var2, variable)) %>% 
                                mutate(across(c(-P, -variable), ~NA_character_))
        
                        # variable_assume is set to the interaction factor that has more levels (e.g. spline, cat 4) / rough differentiation, not exact
                        # if (length(design$parms[[var1]]) < length(design$parms[[var2]])) {
                        #         interaction_var <- var2
                        #         variable_assume <- design$assume[match(var1, design$name)]
                        # } else if (length(design$parms[[var1]]) > length(design$parms[[var2]])) {
                        #         interaction_var <- var1
                        #         variable_assume <- design$assume[match(var2, design$name)]
                        # } else {
                        #         if (length(design$parms[[var2]])<3){
                        #                 interaction_var <- var1
                        #                 variable_assume <- design$assume[match(var2, design$name)]
                        #         } else {
                        #                 print("Warning: some interaction P may be missing (3x3, 4x3 interactions)")
                        #                 interaction_var <- var1
                        #                 variable_assume <- design$assume[match(var2, design$name)]
                        #         }
                        # }
                        interaction_var <- var1
                        variable_assume <- design$assume[match(var2, design$name)]
                        
                }
                
                
                rows <- as.tibble(lapply(rows, as.character), stringsAsFactors = FALSE)
                
                # if variable categorical with 0/1 then
                if (variable_assume=="category" && length(design$parms[[variable_name]])==2 && design$parms[[variable_name]][1]==0){
                        variable_assume <- "asis"
                }
                
                if (variable_assume == "asis") {
                        rows[[1, "variable"]] <- variable_name
                        results[[i]] <- rows
                } else {
                        # Prepare a heading row with NA values to be replaced later
                        heading <-
                                tibble(
                                        variable = variable_label,
                                        '$N_{eff}$' = if (add_n_eff) {NA_character_} else {NULL},
                                        Median = NA_character_,
                                        SE = NA_character_,
                                        Lower = NA_character_,
                                        Upper = NA_character_,
                                        P = NA_character_
                                )
                        colnames(heading) <- colnames(rows)
                        results[[i]] <- heading
                        
                        if (variable_assume == "rcspline") {
                                if (ia) { # when modus interaction
                                        n <- length(design$parms[[var2]])-1
                                } else {
                                        n <- length(design$parms[[variable_name]]) - 1        
                                        }
                                
                                for (j in 1:n) {
                                        spline_row <- rows[j, ]
                                        spline_row$variable <-
                                                paste0("$\\hspace{",
                                                       indent,
                                                       "em}_{spline} $",
                                                       j)
                                        # spline_row$variable <- paste0("Spline ", j)
                                        
                                        results[[i]] <-
                                                bind_rows(results[[i]], spline_row)
                                }
                                
                        } else if (variable_assume %in% c("category", "scored")) {
                                if (ia) { # when modus interaction
                                        # levels <- design$parms[[var2]]
                                        levels <- design$parms[[var1]]
                                } else {
                                        levels <- design$parms[[variable_name]]
                                }
                                
                                
                                first <- TRUE
                                # $_{\textit{(Referent)}}$
                                
                                for (level in levels) {
                                        if (first) {
                                                # cat_row <- tibble(variable = paste0("$\\hspace{",indent,"em}",level," $\\textit{(Referent)}$"), Median = NA_real_, SE = NA_real_, Lower = NA_real_, Upper = NA_real_, P = NA_real_)
                                                cat_row <-
                                                        tibble(
                                                                variable = paste0(
                                                                        "$\\hspace{",
                                                                        indent,
                                                                        "em}$",
                                                                        level
                                                                ),
                                                                '$N_{eff}$' = if (add_n_eff) {NA_character_} else {NULL},
                                                                # Median = if (ia) {NA_character_} else {"Ref."},
                                                                Median = if (ia) {NA_character_} else {"Ref."},
                                                                SE = NA_character_,
                                                                Lower = NA_character_,
                                                                Upper = NA_character_,
                                                                P = if (ia) {"Ref."} else {NA_character_}
                                                                # '$$P_{interaction}$$' = if (interaction) {NA_character_} else {NULL}
                                                        )
                                                
                                                first <- FALSE
                                        } else {
                                                cat_row <- tryCatch({
                                                        if (ia) { # when modus interaction
                                                                rows %>% filter(grepl(level, variable))       
                                                        } else {
                                                                rows %>% filter(variable == (
                                                                paste0(
                                                                        variable_name,
                                                                        "=",
                                                                        as.character(level)
                                                                )
                                                        ))}
                                                }, error = function(e) {
                                                        rows %>% filter(variable_name == variable)
                                                })
                                                if (nrow(cat_row) == 0) {
                                                        # IN CASE level 1 is not labelled as 1, but only with name itself
                                                        cat_row <-
                                                                if (ia) { # when modus interaction
                                                                        rows %>% filter(grepl(level, variable))       
                                                                } else {
                                                                        rows %>% filter(variable_name == variable)
                                                                }
                                                }
                                                # cat_row$variable <- paste0("$\\\\hspace\\{",indent,"em\\}$", level)
                                                cat_row$variable <-
                                                        paste0("$\\hspace{",
                                                               indent,
                                                               "em}$",
                                                               level)
                                        }
                                        
                                        results[[i]] <-
                                                bind_rows(results[[i]], cat_row)
                                }
                        } 
                }
                # update the variables to Labels 
                results[[i]] <- updateLabels(results[[i]], data_dic, rows=TRUE, update_rows_of_col = "variable", updateInteraction = FALSE)
                # TODO - Units
                
                
        }
        
        # Combine all results, replace NA with ""
        final_result <- do.call(bind_rows, results) %>% 
                mutate(across(everything(), ~replace_na(as.character(.x), ""))) %>% 
                mutate(variable = gsub("_", " ", variable))
        
        # final_result <- final_result %>% 
        #         rename('$\\beta$'=Median) %>% 
        #         rename('$P(\\beta>0)$'=P)
                
        # '$\\beta$'
        # '$P(\\beta>0)$'
        
        if (interactions_only){
                final_result <- final_result %>% select(variable, "$P_{interaction}$" = P)
        }
        return(final_result)
}












# takes argument of rmsb package anova(rmsb.model) 
shortREV <- function(rmsb_model, keep = 15, include = "sex", output=c("table","graph"), data_dic = data_dic) {
        require(rms)
        require(rmsb)
        require(tidyverse)
        
        if (class(rmsb_model)[1] == "anova.rms"){
                df <- rmsb_model
        } else if (class(rmsb_model)[1] == "blrm") {
                df <- anova(rmsb_model)
        } else {
                return("Error - please supply either 'rmsb-model' or 'anova(rmbs-model)'")
        }
        
        dfx <- df %>% as_tibble() %>% 
                mutate(rownames = rownames(df)) %>% 
                filter(rownames!=" All Interactions",
                       rownames!="TOTAL INTERACTION",
                       rownames!="TOTAL",
                       rownames!=" Nonlinear") %>% 
                mutate(rownames = str_split(rownames, pattern = '\\s\\s\\(', simplify = TRUE)[,1]) %>% 
                mutate(REV = as.numeric(REV),
                       Lower = as.numeric(Lower),
                       Upper = as.numeric(Upper))
        if (keep=="all"){ keep = 999}
        
        l <- dfx %>% arrange(desc(REV)) %>% 
                slice_head(n = keep) %>% pull(rownames)
        if (sum(l %in% include)<1){ l <- l %>% append(include) }
        
        dfx <- dfx %>% filter(rownames %in% l) %>% arrange(desc(REV))
        
        if (output == "table"){ return(dfx) }
        if (output == "graph"){
                # dfx <- dfx %>%
                #         arrange(-REV) %>%
                #         mutate(rownames = factor(rownames, levels = rownames))
                # tar_load(data_dic)
                # data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
                dfx <- updateLabels(dfx, data_dic, rows=TRUE, update_rows_of_col="rownames")
                dfx$label <- factor(dfx$rownames, levels = dfx$rownames[order(dfx$REV)])
                # dfx$label <- factor(dfx$rownames, levels = dfx$rownames[order(dfx$REV)])
                # 
                # Create dotplot with confidence intervals
                g <- ggplot(dfx, aes(x = label, y = REV)) +
                        geom_point() +
                        geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
                        coord_flip() +
                        theme_minimal() +
                        labs(y = "Explained variation (relativ) by variables", x = "", title = "") +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1),
                              panel.grid.major.y = element_blank(),   # Remove major horizontal grid lines
                              panel.grid.minor.y = element_blank())
                total <- attr(df, "chisqBayes") %>% round(1)
                if (keep == 999) {
                        note <- paste0("Dots indicate median explanatory variation, bars upper and lower 95% credible interval. Total Chisq-Bayes ", 
                                       total[["Central"]], 
                                       " (95% credible interval ", 
                                       total[["Lower"]], 
                                       "–", 
                                       total[["Upper"]],
                                       ").")
                } else { 
                        note <-
                                paste0(
                                        "Only most relevant variables are depicted (n = ",
                                        keep,
                                        "). Dots indicate median explanatory variation, bars upper and lower 95% credible interval. Total Chisq-Bayes ",
                                        total[["Central"]],
                                        " (95% credible interval ",
                                        total[["Lower"]],
                                        "–",
                                        total[["Upper"]],
                                        ")."
                                )
                }
                caption <- paste0("Relative explained variation by predictor variables incl. biological sex.")
                
                l <- list(g, caption, note)
                names(l) <- c("graph","caption","footnote")
                return(l)
        }
        
}



# runs univariate blrm models explaining "sex" ("sex ~ ... ")
run_models <- function(df, var) {
        model_list <- list()
        
        # Loop over var
        for (v in var) {
                # Create formula
                formula <- as.formula(paste(v, " ~ sex"))
                
                # Fit the model and store it in the list
                model_list[[v]] <- blrm(formula, data = df)
        }
        return(model_list)
}

# bivariable models outcome
run_bimodels <- function(df, outcome = "outcome_delir", var, group_var = "sex", interaction = TRUE) {
        model_list <- list()
        
        # Loop over var
        for (v in var) {
                # Create formula
                if (interaction) {
                        formula <- as.formula(paste(outcome, " ~ ", v, "*", group_var))
                } else {
                        formula <- as.formula(paste(outcome, " ~ ", v, "+ ", group_var))
                }
                
                # Fit the model and store it in the list
                model_list[[v]] <- try(blrm(formula, data = df, sampling.args = list(control=list(adapt_delta=0.99,max_treedepth=12))), silent = TRUE)
                print(model_list[[v]]) # diagnostic
        }
        return(model_list)
}


# take the models from function run_models and extracts probability of beta > 0, median and credible intervals
extract_bayes_stat <- function(models, var) {
        results <- tibble(variable = character(), `P(β>0)` = numeric(), med_ci = character())
        for (s in var) {
                t <- getRsmb(models[[s]])
                med_ci <- paste("median β=",t['Median'], " (95% credible interval: ", t['Lower'], ", ", t['Upper'],")", sep="")
                results <- add_row(results, variable = as.character(s), `P(β>0)` = as.numeric(t$P), med_ci = as.character(med_ci)) 
        }
        return(results)
}

transform_P_value <- function(x, exact = TRUE) {
        if (x < 0.0001) {
                return("<0.0001")
        } else if (x > 0.9999) {
                return(">0.9999")
        } else {
                if (exact) {
                        return(as.character(as.numeric(x)))
                } else {
                        return(sprintf("%.4f", x))        
                }
        }
}

get_bayes_stat <- function(stat){
        s <- stat %>% pull(`P(β>0)`)
        s <- sapply(s, transform_P_value)
        return(s)
}

replace_H_with <- function(text, replacement = "") {
        # Replace all occurrences of [H] with [ht] in the input string
        updated_text <- gsub("\\[H\\]", replacement, text)
        
        # Return the updated string
        return(updated_text)
}

# plot summary table (aka Table 1)
plot_tbl <- function(data, selected_columns, by = "sex", stat_descr_loc = "bottom", pkg = "kableExtra", kable_option = "latex", pvalue = FALSE, bayes = TRUE, missing = "ifany", ...) {
        library(tidyverse)
        library(gtsummary)
        library(gt)
        missing <- match.arg(missing, choices = c("ifany", "always", "no"))
        
        # Define theme for gtsummary
        my_theme <- list(
                "pkgwide-str:ci.sep" = "–",
                "style_number-arg:big.mark" = ""
        )
        # Set theme
        set_gtsummary_theme(my_theme)
        
        # Select specified columns
        df <- data %>% select(all_of(selected_columns))
        # load DataDictionary
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        df <- updateLabels(df, data_dic)
        
        # Create summary table with gtsummary / tbl_summary()
        cont <- c(0)
        tbl <- df %>% tbl_summary(
                by = by,
                missing = missing, 
                statistic = list(all_categorical() ~"{n} ({p}%)", all_continuous() ~ "{median} ({p25}–{p75})"),
                digits = list(all_categorical() ~ c(0, 1),
                              all_continuous2() ~ c(0,0))
        ) %>%
                add_overall() %>%
                add_n() %>% 
                # add_stat_label(location = "column") %>% 
                italicize_labels() %>%
                modify_footnote(~NA) %>%
                modify_header(update = list(stat_0 ~ "**All Patients**\n (N={N})",
                                            stat_1 ~ "**{level}**\n (N={n})",
                                            stat_2 ~ "**{level}**\n (N={n})"))
        if (pvalue==TRUE) {
                tbl <- tbl %>% 
                        add_p()
        }
        
        # # 
        #         tbl$table_body <- left_join(tbl$table_body, stat, by = c("variable" = "Variable")) %>% 
        #                 rename(`p.value2` = `p.value`) %>% 
        #                 rename(`p.value` = `P(β>0)`) %>% 
        #                 mutate(`p.value` = ifelse(is.na(`p.value2`), NA, `p.value`))
        #         tbl$meta_data$p.value <- tbl$table_body %>% select(`p.value`) %>% drop_na() %>% .[[1]]
        #         
        if (bayes==TRUE) {
                getBayes_fun <- function(data, variable, by, ...) {
                        # Ensure that the output is a single value for each row (e.g., for each 'variable')
                        stat %>%
                                filter(variable == !!variable) %>%
                                pull(`P(β>0)`) %>%
                                sapply(transform_P_value) %>%
                                ifelse(
                                        length(.) == 1,
                                        .,
                                        filter(grepl(!!variable, variable)[[1]]) %>%
                                                pull(`P(β>0)`) %>%
                                                sapply(transform_P_value)
                                ) 
                }

                tbl <- tbl %>%
                        add_stat(fns = everything() ~ getBayes_fun,
                                 location = everything() ~ "label") %>%
                        modify_header(update = list(add_stat_1 ~ "*P(Beta > 0)*"))
        }
                # .escape_latex2(c(*P(β>0)*)
        # data = df, variable = variable, by = by , stat
        
        if (stat_descr_loc!="bottom") {
                # describe each variable 
                tbl$table_body <-  tbl$table_body %>%
                        mutate(label = if_else(
                                var_type == "categorical" & row_type == "label",
                                paste(label, " — N° (%)", sep = ""),
                                if_else(
                                        var_type == "continuous" & row_type == "label",
                                        paste(label, " — Median (IQR 25–75)", sep = ""),
                                        label
                                )
                        ))
        }
        
        # Change binary variable from 0/1 to No/Yes, or Yes/No
        binary_vars <- tbl$table_body %>%
                filter(row_type == "level", var_type == "categorical") %>%
                group_by(variable) %>%
                summarise(
                        n_levels = n_distinct(label),
                        has_zero = any(label == "0"),
                        has_one = any(label == "1")
                ) %>%
                filter(n_levels == 2, has_zero, has_one) %>%
                pull(variable)
        
        # Change labels for these variables only
        tbl$table_body <- tbl$table_body %>%
                mutate(label = case_when(
                        row_type == "level" & variable %in% binary_vars & label %in% c("0", "1") ~ recode(label, "0" = "no", "1" = "yes"),
                        TRUE ~ label
                ))
        
        
        # Get Footnotes of desired variables
        note <- tryCatch({
                notes <- getAttr(df, attr = "footnote") %>%
                        as_tibble() %>%
                        filter(value != "N/A") %>%
                        mutate(value = gsub("\"", "", value)) %>%
                        pull(value) %>%
                        paste0(collapse = "; ")
                paste0("*Abbreviations*: ", notes)
        }, 
        error = function(e) {
                NULL
        })
        
        
        
        # Choose plotting package
        if (pkg == "raw") {
                return(tbl)
        }
        
        if (pkg == "gt") {
                tbl <- tbl %>%
                        as_gt() %>%
                        cols_align(align = "right",
                                   columns = contains("stat_"))
                # if Statistical description is wished to be placed at footnote (otherwise - see above label description within tbl_summary)
                if (stat_descr_loc == "bottom") {
                        # put description in footnote
                        tbl <-
                                tbl %>% tab_footnote(
                                        footnote = "Median (IQR, interquartile range 25–75); n (Percent %)",
                                        locations = cells_column_labels(columns = contains("stat")),
                                        placement = "auto"
                                )
                }
                if (!is.null(note)) {
                        tbl <- tbl %>% 
                                tab_source_note(source_note = md(note))
                }
        }
        
        if (pkg == "kableExtra") {
                require(kableExtra)
                
                if (kable_option == "latex"){
                        
                        tbl <- tbl %>%
                                as_kable_extra(
                                        booktabs = TRUE,
                                        format = "latex",
                                        linesep = "",
                                        align = c('l', 'l', 'r', 'r', 'r', 'r')
                                ) %>%
                                kable_styling(full_width = F
                                              # latex_options = c("hold_position")
                                              )
                } else {
                        tbl <- tbl %>%
                                as_kable_extra(
                                        booktabs = TRUE,
                                        format = "html",
                                        position = "float_left",
                                        wraptable_width = "0pt",
                                        linesep = ""
                                        # align = c('l', 'l', 'r', 'r', 'r', 'r')
                                ) %>%
                                kable_classic(bootstrap_options = c("hover", "condensed", "responsive"), protect_latex = TRUE, 
                                              full_width = F) %>% scroll_box(height = "500px")
                }
                
                # if Statistical description is wished to be placed at footnote (otherwise - see above label description within tbl_summary)
                if (stat_descr_loc == "bottom") {
                        # put description in footnote
                        tbl <- tbl %>% 
                                kableExtra::footnote(general = c("Median (IQR, interquartile range 25–75); n (Percent %)")
                                                     )
                        
                        # names(tbl)[2] <- paste0(names(dt_footnote)[2], 
                        #                         footnote_marker_symbol(1))
                        # row.names(dt_footnote)[4] <- paste0(row.names(dt_footnote)[4], 
                        #                                     footnote_marker_alphabet(1))
                } else {
                        tbl <- tbl %>% 
                                kableExtra::footnote(general =  note)
                }
                    
        }
        
        if (pkg == "hux") {
                # tbl <- tbl %>%
                        
                        # if Statistical description is wished to be placed at footnote (otherwise - see above label description within tbl_summary)
                        if (stat_descr_loc == "bottom") {
                                # put description in footnote
                                # tbl <-
                                        # tbl %>% tab_footnote(
                                        #         footnote = "Median (IQR, interquartile range 25–75); n (Percent %)",
                                        #         locations = cells_column_labels(columns = contains("stat")),
                                        #         placement = "auto"
                                        # )
                        }
        }
        
        
        
        
        
                
        
        
        
                # tab_style(style = list(cell_text(size = "small")), 
                #           locations = cells_footnotes())
                # 
        
        # Return the final table
        return(tbl)
}




brmsCorrPlot <- function(brms_model, output = c("matrix","plot"), ...){
        library(brms)
        library(stringr)
        library(corrplot)
        library(tidyverse)
        s <- summary(brms_model)
        variables <- names(s$formula$forms)
        true_vars <- sapply(brms_model$formula$forms, function(x){ x %>% paste0() %>% str_split(pattern = " ~ 1") %>% .[[1]] %>% .[1]}) %>% as_tibble() %>% rename(name = value)
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
        true_vars <- updateLabels(true_vars, data_dic, rows=TRUE, update_rows_of_col="name")
        true_vars <- true_vars %>%
                mutate(label = ifelse(is.na(label), name, label))
                
        # empty correlation matrix
        cor_matrix <- matrix(0, nrow = length(variables), ncol = length(variables))
        rownames(cor_matrix) <- variables
        colnames(cor_matrix) <- variables
        df_rescor_output <- s$rescor_pars
        
        # fill the matrix
        for(rowname in rownames(df_rescor_output)){
                matches <- str_match(rowname, "rescor\\((.*?),(.*?)\\)")
                if(length(matches) == 3){
                        var1 <- matches[2]
                        var2 <- matches[3]
                        estimate <- df_rescor_output[rowname, "Estimate"]
                        cor_matrix[var1, var2] <- estimate
                        cor_matrix[var2, var1] <- estimate
                }
        }
        
        rownames(cor_matrix) <- true_vars$label
        colnames(cor_matrix) <- true_vars$label
        
        if (is.null(output)) {
                return("please specify 'output' - 'matrix' or 'plot'")
        } else {
                if (output == "matrix"){
                        return(cor_matrix)
                }
                if (output == "plot") {
                        corrplot(cor_matrix, type = 'lower', order = 'alphabet', tl.col = 'black',
                                       cl.ratio = 0.2, tl.srt = 45, col = COL2('RdYlBu', 10), diag=TRUE)
                }
                
        }
        
}

# check Vector for formula, string or actual vector
checkVector <- function(iv){
        if (length(iv)==1)
        {
                if (grepl(pattern = "~", iv)) {
                        iv <- str_split(formula1,
                                        pattern = "~",
                                        simplify = TRUE)[[2]]
                }
        }
        # Check if vector of variables 
        if (is.vector(iv)) {
                if (length(iv) > 1) {
                        iv_terms <- iv
                } else if (is.character(iv)) { # if only length 1, then likely character with term + term + ...
                        iv_terms <- as.vector(str_split(iv, pattern = " \\+ ", simplify = TRUE))
                } else {
                        stop('Invalid input for iv.')
                }
        }
        return(iv_terms)
}

addInteractions <- function(iv, ia="sex", all=FALSE, ...) {
        iv_input <- iv # backup
        # Step 1 Check input
        # Check if formula -if so then keep only right side
        iv_terms <- checkVector(iv)
        
        # Step 2 - Add interactions 
        if (all) {
                interacting_variables <- setdiff(iv_terms, ia)
                iv_terms <- paste0(interacting_variables, '*', ia)
        } else {
                interacting_variables <- (...)
                ia_terms <- checkVector(interacting_variables)
                noninteracting_vars <- setdiff(iv_terms, ia_terms)
                interacting_variables <- paste0(ia_terms, '*', ia)
                # iv_terms <- c(noninteracting_vars, interacting_variables)
                iv_terms <- c(iv_terms, interacting_variables)
        }
        
        # build string term
        iv <- paste(iv_terms, collapse = ' + ')
        return(iv)
}




# Takes a vector of dv and iv and 
# dv dependent variable
getFormula <- function(dv, iv, interaction=FALSE, interaction_var = "sex"){
        iv <- setdiff(iv, dv)
        if (interaction) {
                f <- paste0( dv, " ~ ", paste0(iv, collapse = paste0("*", interaction_var, " + ")), paste0("*", interaction_var))
        } else {
                f <- paste0( dv, " ~ ", paste0(iv, collapse = " + "))
        }
        return(formula(f))
        
}


getRawTerms <- function(iv) {
        # Remove rcs and retain the variable inside it
        iv_without_rcs <- gsub("rcs\\(([^,]+),[^)]+\\)", "\\1", iv)
        
        # Replace interaction marks '*' and ':' with '+', split by '+', and extract unique variables
        variables <- unique(unlist(strsplit(gsub("\\s*[\\*:]\\s*", " + ", iv_without_rcs), "\\s+\\+\\s+")))
        
        # Combine unique variables with '+'
        iv_without_interactions <- paste(variables, collapse = " + ")
        
        return(iv_without_interactions)
}

# Function to create or read model
# 
getModel <- function(dv, iv, file_name, data, loo=TRUE, cppo = FALSE, imputation = FALSE, force_overwrite = FALSE, lrm = FALSE, ...) {
        
        # every time non-bayesian model is calculated and stored
        # options(contrasts=c("contr.treatment", "contr.treatment"))
        #         if (!is.numeric(data[dv])) { dv = paste0("as.numeric(",dv,")") }
        #         formula_str = paste0(dv, " ~ ", iv)
        #         lrm_model <-
        #                 lrm(
        #                         formula = formula(formula_str),
        #                         data = data
        #                 )
        #         saveRDS(lrm_model, file = paste0(file_name, "_lrm"))
        # 
        
        if (imputation) {
                mi_str = paste(dv, "+", getRawTerms(iv))
                
                if (!is.numeric(data[dv])) { dv = paste0("as.numeric(",dv,")") }
                formula_str = paste0(dv, " ~ ", iv)
                
                if (file.exists(file_name) & (force_overwrite == FALSE)) {
                        model <- readRDS(file_name)
                        return(model)
                } else {
                        file.remove(file_name)
                        # Multiple Imputation
                        mi <-
                                aregImpute(
                                        reformulate(mi_str) ,
                                        nk = 4,
                                        data = data,
                                        B = 10,
                                        n.impute = 5,
                                        pr = FALSE
                                )
                        # Models using Imputation
                        if (cppo) {
                                model <-
                                        stackMI(
                                                formula(formula_str),
                                                fitter = blrm,
                                                xtrans = mi,
                                                data = data,
                                                refresh = 50,
                                                file = file_name,
                                                loo = loo,
                                                ppo = reformulate(iv),
                                                cppo = function(y) y
                                        )
                        } else {
                                model <-
                                        stackMI(
                                                formula(formula_str),
                                                fitter = blrm,
                                                xtrans = mi,
                                                data = data,
                                                refresh = 50,
                                                file = file_name,
                                                loo = loo
                                        )   
                        }
                        
                }
        } else {
                if (cppo) {
                        if (!is.numeric(data[dv])) { dv = paste0("as.numeric(",dv,")") }
                        formula_str = paste0(dv, " ~ ", iv)
                        
                        if (file.exists(file_name) & (force_overwrite == FALSE)) {
                                model <- readRDS(file_name)
                                # CPPO is stored / or reimported as chr - fixing with:
                                # model$cppo <- cat(model$cppo)
                        } else {
                                # file.remove(file_name)
                                model <-
                                        blrm(
                                                formula(formula_str),
                                                reformulate(iv),
                                                cppo = function(y) y,
                                                data = data,
                                                file = file_name,
                                                loo = loo
                                        )
                        }
                } else {
                        formula_str = paste0(dv, " ~ ", iv)
                        if (file.exists(file_name) & (force_overwrite == FALSE)) {
                                model <- readRDS(file_name)
                                # CPPO is stored / or reimported as chr - fixing with:
                                # model$cppo <- cat(model$cppo)
                        } else {
                                # file.remove(file_name)
                                model <-
                                        blrm(
                                                formula(formula_str),
                                                data = data,
                                                file = file_name,
                                                loo = loo
                                        )
                        }
                }
        }
        model$call$data <- substitute(data) # necessary because otherwise 'data' is stored instead of e.g. 'df_outcome'
        return(model)
}

# Frank Harrell
# https://hbiostat.org/rmsc/genreg.html#sec-genreg-pcontrast
psigma <- function(r, a, inline=FALSE, pr=! inline) {
        sigma <- abs(log(r)) / qnorm(1 - a)
        dir <- if(r > 1.) '>' else '<'
        x <- if(inline) paste0('$\\Pr(\\text{OR}', dir, r, ') =', a,
                               ' \\Rightarrow \\sigma=', round(sigma, 3), '$')
        else paste0('Pr(OR ', dir, ' ', r, ') = ', a, ' ⇒ σ=', round(sigma, 3))
        if(inline) return(x)
        if(pr) {
                cat('\n', x, '\n\n', sep='')
                return(invisible(sigma))
        }
        sigma
}
# Frank Harrell
# https://hbiostat.org/rmsc/genreg.html#sec-genreg-pcontrast
. <- function(...) list(...)


getNewdataGrid <- function(model, group_var = "sex", stat_for_continuous = c("median", "IQR25", "IQR75"), levels=NULL) {
        
        # Helper function to calculate statistics for continuous variables
        calculate_stat <- function(data, stat) {
                switch(stat,
                       median = median(data, na.rm = TRUE),
                       IQR25 = quantile(data, 0.25, na.rm = TRUE),
                       IQR75 = quantile(data, 0.75, na.rm = TRUE), 
                       min = min(data, na.rm = TRUE),
                       max = max(data, na.rm = TRUE),
                       IQR97.5 = quantile(data, 0.975, na.rm = TRUE),
                       IQR2.5 = quantile(data, 0.025, na.rm = TRUE), 
                       
                )
        }
        
        # Extract terms from the model and split them into a list
        terms_string <- getRawTerms(attr(model$terms, "term.labels"))
        terms <- str_split(terms_string, pattern = "\\+", simplify = TRUE) %>% str_squish()
        
        newdata_values <- list()
        df <- model$call$data %>% eval()
        
        # For each term, determine its type and calculate the required value
        for (term in terms) {
                if (term == group_var) {
                        if ((is.factor(df[[group_var]])) & (is.null(levels))) {
                                # For the grouping variable, use all its levels
                                newdata_values[[term]] <- levels(df[[term]])
                        } else if (!is.null(levels)){
                                newdata_values[[term]] <- levels
                        } else {
                                # For continuous variables, calculate the desired statistics
                                stats_values <-
                                        sapply(stat_for_continuous, function(stat) {
                                                calculate_stat(df[[term]], stat)
                                        })
                                newdata_values[[term]] <- stats_values
                        }
                } else {
                        if (is.factor(df[[term]]) || is.ordered(df[[term]])) {
                                # For categorical/ordinal variables, calculate mode
                                mode_val <-
                                        names(sort(table(df[[term]]), decreasing = TRUE)[1])
                                newdata_values[[term]] <- mode_val
                        } else {
                                # For continuous variables, calculate median
                                median_val <- median(df[[term]], na.rm = TRUE)
                                newdata_values[[term]] <- median_val
                        }
                }
                
        }
        
        # Generate new data using expand.grid
        newdata <- expand.grid(newdata_values)
        return(newdata)
}


      
# plot Grotta Bars
plotProbY <- plotGrottaBars <- function(df, group_var = "sex", model, xlab = "predicted Probability", ylab = "Sex", plot = TRUE, raw_predictions = TRUE, position="stack") {
        require(stringr)
        require(dplyr)
        require(ggplot2)
        require(reshape2)
        
        # Extract terms from the model and split them into a list
        terms_string <- getRawTerms(attr(model$terms, "term.labels"))
        terms <- str_split(terms_string, pattern = "\\+", simplify = TRUE) %>% str_squish()
        
        newdata_values <- list()
        
        # For each term, determine its type and calculate the required value
        for (term in terms) {
                if (term == group_var) {
                        # For the grouping variable, use all its levels
                        newdata_values[[term]] <- levels(df[[term]])
                } else if (is.factor(df[[term]]) || is.ordered(df[[term]])) {
                        # For categorical/ordinal variables, calculate mode 
                        mode_val <- names(sort(table(df[[term]]), decreasing = TRUE)[1])
                        newdata_values[[term]] <- mode_val
                } else {
                        # For continuous variables, calculate median
                        median_val <- median(df[[term]], na.rm = TRUE)
                        newdata_values[[term]] <- median_val
                }
        }
        
        # Generate new data using expand.grid
        newdata <- expand.grid(newdata_values)
        
        # linear_predictors <- posterior_samples %*% t(newdata_matrix)
        # lp <- model$draws %*% t(newdata)
        
        # Predict using the provided model
        p <- predict(model, newdata, type='fitted.ind', posterior.summary='median')
        
        p <- p %>% mutate(
                pos   = cumsum(Median) - Median/2,
                alpha = case_when(
                        y == "outcome_mrsd90=0" ~ 0.05,
                        y == "outcome_mrsd90=1" ~ 0.25,
                        y == "outcome_mrsd90=2" ~ 0.45,
                        y == "outcome_mrsd90=3" ~ 0.65,
                        y == "outcome_mrsd90=4" ~ 0.75,
                        y == "outcome_mrsd90=5" ~ 0.9,
                        y == "outcome_mrsd90=6" ~ 1.0
                ),
                color = case_when(
                        x == 1 ~ "#9A0000",
                        x == 2 ~ "#004178"

                                                # !!sym(group_var) == levels(df[[group_var]])[1] ~ "#9A0000",
                        # !!sym(group_var) == levels(df[[group_var]])[2] ~ "#004178"
                ),
                text_col = case_when(
                        y == "outcome_mrsd90=6" ~ "grey",
                        y == "outcome_mrsd90=5" ~ "grey",
                        TRUE ~ "black"
                ))
        
        
                if ((raw_predictions==TRUE)  & (plot == FALSE)) {
                        return(p2)
                } else if (raw_predictions==FALSE & (plot == FALSE)) {
                        return(p2_means) 
                } else {
                        # Plot
                        # dodge
                        if (position=="dodge") { ggplot(p, aes(x = as.factor(x), y = Median, fill = as.factor(y))) +
                                geom_col(position = "dodge", colour="black", size=0.2) +
                                geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.9), width = 0.25) +
                                geom_text(aes(label = scales::percent(Median, accuracy = 0.1)), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
                                xlab("X Label") +  # Replace with your desired x-axis label
                                ylab("Median Value") +  # Replace with your desired y-axis label
                                scale_fill_brewer(palette = "Set1", name = "Y") +
                                theme_minimal() +
                                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
                        }
                        
                        # stack - Grotta bars
                        if (position=="stack"){
                        # p$x <- factor(p$x, levels = rev(unique(p$x)))
                        # p$y <- factor(p$y, levels = rev(unique(p$y)))
                        
                        ggplot(p, aes(x = x, y = Median, fill = color)) +
                                geom_bar(stat = "identity", colour = "black", size = 0.2, aes(alpha = alpha), position = position_stack(reverse = TRUE)) +
                                geom_text(aes(label = round(Median * 100, 1), color = text_col), position = position_stack(reverse = TRUE, vjust = 0.5), size = 3) +
                                labs(x = "Sex (x)", y = "Median") +
                                scale_fill_identity() +  # Use the color values as is
                                scale_color_manual(values = c("black", "lightgray")) +  # Define text colors based on 'text_col'
                                theme_minimal() + coord_flip() + theme(legend.position = "none")
                        }
                        
                         
                }
        
        
        
        
        # dimnames(p) <- list(NULL, levels(df[[group_var]]), paste0("mRSd90_",0:6))
        # 
        # # Reshape the data
        # p2 <- melt(p, value.name = "value", varnames = c("id", group_var, "mRSd90")) %>% select(-id)
        # 
        # # Calculate mean values
        # p2_means <- p2 %>%
        #         group_by(!!sym(group_var), mRSd90) %>%
        #         summarise(mean_value = mean(value)) 
        # p2_means <- p2_means %>% 
        #         mutate(
        #                 pos   = cumsum(mean_value) - mean_value/2,
        #                 alpha = case_when(
        #                         mRSd90 == "mRSd90_0" ~ 0.05,
        #                         mRSd90 == "mRSd90_1" ~ 0.25,
        #                         mRSd90 == "mRSd90_2" ~ 0.45,
        #                         mRSd90 == "mRSd90_3" ~ 0.65,
        #                         mRSd90 == "mRSd90_4" ~ 0.75,
        #                         mRSd90 == "mRSd90_5" ~ 0.9,
        #                         mRSd90 == "mRSd90_6" ~ 1.0
        #                 ),
        #                 color = case_when(
        #                         !!sym(group_var) == levels(df[[group_var]])[1] ~ "#9A0000",
        #                         !!sym(group_var) == levels(df[[group_var]])[2] ~ "#004178"
        #                 ),
        #                 text_col = case_when(
        #                         mRSd90 == "mRSd90_6" ~ "grey",
        #                         mRSd90 == "mRSd90_5" ~ "grey",
        #                         TRUE ~ "black"
        #                 )
        #         )
        # 
        # if ((raw_predictions==TRUE)  & (plot == FALSE)) {
        #         return(p2)
        # } else if (raw_predictions==FALSE & (plot == FALSE)) {
        #        return(p2_means) 
        # } else {
        # Plot
        # ggplot(p2_means, aes(x = mean_value, y = as.factor(!!sym(group_var)), fill = color)) +
        #         geom_bar(stat="identity", colour="black", size=0.2, aes(alpha=alpha)) +
        #         geom_text(aes(x= pos, y=as.factor(!!sym(group_var)), label = round(mean_value*100, 1), colour=text_col), size=3) +
        #         xlab(xlab) +
        #         ylab(ylab) +
        #         scale_fill_identity() +
        #         scale_color_manual(values = c("black", "grey")) +
        #         guides(color=FALSE, alpha=FALSE) +  
        #         theme_minimal() +
        #         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        # }
}

# from contrast.rms object - write an example statement        
contrastExample <- function(data, var = "sex", ref = "male") {
        # Extract and round relevant data
        age <- round(data[["age"]], 1)
        nihss_d0 <- round(data[["nihss_d0"]], 1)
        time_window <- round(data[["time_window"]], 1)
        contrast <- round(data[["Contrast"]], 1)
        lower <- round(data[["Lower"]], 1)
        upper <- round(data[["Upper"]], 1)
        probability <- round(data[["PP"]] * 100, 1)  # Convert to percentage
        
        # Determine if the contrast is less or more and for which gender
        if (contrast < 0) {
                direction <- "less"
                gender_contrast <- "female"
                reference_gender <- "male"
        } else {
                direction <- "more"
                gender_contrast <- "male"
                reference_gender <- "female"
        }
        
        # Construct the paragraph
        paragraph <- sprintf("A %s-year old %s, who presents with an NIHSS of %s at %s hours after stroke onset, has a mean contrast of %s days (95%% credible interval %s to %s, probability %s%%) %s than a %s.", 
                             age, gender_contrast, nihss_d0, time_window, contrast, lower, upper, probability, direction, reference_gender)
        
        # Return the constructed paragraph
        return(paragraph)
}


# Modified version of pp_check.blrm from Yong-Hao-Pua
# https://hbiostat.org/r/examples/rmsbgraphicsvignette/graphics

## posterior predictive checks using \pkg{bayesplot} package
## codes adapted from brms::pp_check.R 
## https://github.com/paul-buerkner/brms/blob/master/R/pp_check.R

pp_check.blrm <- function(modblrm, 
                          type, 
                          ndraws = NULL,
                          group = NULL, df = df) {
        

        
        if (!any(class(modblrm) %in% Cs(blrm, rmsb))) {
                stop("rms object must be of class blrm", 
                     call. = FALSE)
        }
        
        if (missing(type)) {
                type <- "dens_overlay"
        }
        
        # from brms::pp_check.r
        valid_types <- as.character(bayesplot::available_ppc(""))
        valid_types <- sub("^ppc_", "", valid_types)
        if (!type %in% valid_types) {
                stop("Type '", type, "' is not a valid ppc type. ", 
                     "Valid types are:\n", paste(valid_types, collapse = " , "))
        }
        
        ppc_fun <- get(paste0("ppc_", type), asNamespace("bayesplot"))
        
        # d <- eval(modblrm$call$data)
        vars <- all.vars(modblrm$sformula)
        newdata = df[vars] %>% data.frame()
        if (anyNA(newdata)) {
                warning("NA responses in sample")              ## issue warnings about NAs
                newdata <- newdata[complete.cases(newdata), ]  ## remove NAs (if any)  
        }
        
        valid_vars  <- modblrm$Design$name 
        ## codes from pp_check.brms
        if ("group" %in% names(formals(ppc_fun))) {
                if (is.null(group)) {
                        stop("Argument 'group' is required for ppc type '", type, "'.")
                }
                if (!group %in% valid_vars) {
                        stop("Variable '", group, "' could not be found in the data.")
                }
        }
        
        ## Y and Yrep
        y <- as.vector(newdata[, all.vars(modblrm$sformula)[1]])
        y_length = length(unique(y))
        
        if(is.null(ndraws)) {ndraws = 20}  ## 20 draws to save time 
        
        if(y_length == 2) {
                
                ## binary response variable
                pred_binary <- predict(modblrm, newdata, fun=plogis, funint=FALSE, posterior.summary="all")
                yrep_alldraws <- apply(pred_binary, c(1,2), function (x) rbinom(1,1,x))
                yrep <- yrep_alldraws[c(1:ndraws), ]
                
        } else { 
                
                ## >=3 level ordinal/continuous response variable  
                pred_ordinal <- predict(modblrm, newdata, type="fitted.ind", posterior.summary = "all")
                yrep_alldraws <-   apply(pred_ordinal, c(1,2), function (x) {
                        myvec = unlist(rmultinom(1,1,x))
                        myvec_names = modblrm$ylevels   ## get unique levels of Y
                        return(myvec_names[myvec==1])
                })
                yrep_alldraws <- apply(yrep_alldraws, c(1,2), as.numeric) 
                yrep <- yrep_alldraws[c(1:ndraws), ]
                
        }
        
        ## bayesplot::ppc_funs  
        ppc_args <- list(y, yrep)
        
        if (!is.null(group)) {
                ppc_args$group <- newdata[[group]]
        }
        
        do.call(ppc_fun, ppc_args)
        
}


# Create a DAG from a BayestestR-mediation-output
# bayestestR::mediation(model1) %>% creat_dag()
# layout dot is best for this simple visualization purpose
create_dag <- function(m, output_file = "mediation_dag.svg") {
        library(bayestestR)
        library(DiagrammeR)
        library(grid)
        library(grImport2)
        
        # Extract relevant attributes
        treatment <- attr(m, "treatment")
        mediator <- attr(m, "mediator")
        outcome <- attr(m, "response")
        
        # Extract path estimates
        direct_effect <- round(m$Estimate[m$Effect == "Direct Effect (ADE)"], 3)
        indirect_effect <- round(m$Estimate[m$Effect == "Indirect Effect (ACME)"], 3)
        mediator_effect <- round(m$Estimate[m$Effect == "Mediator Effect"], 3)
        
        # Create Graphviz diagram using DiagrammeR
        diagram <- grViz("
    digraph mediation_dag {
      graph [layout = dot, rankdir = LR]
      
      # Node definitions
      node [shape = circle, style = filled, fillcolor = lightgrey]
      Treatment [label = '@@1']
      Mediator [label = '@@2']
      Outcome [label = '@@3']
      
      # Edge definitions with labels
      Treatment -> Mediator [label = '@@4']
      Treatment -> Outcome [label = '@@5']
      Mediator -> Outcome [label = '@@6']
    }
    
    [1]: treatment
    [2]: mediator
    [3]: outcome
    [4]: paste0('Indirect Effect: ', indirect_effect)
    [5]: paste0('Direct Effect: ', direct_effect)
    [6]: paste0('Mediator Effect: ', mediator_effect)
  ")
        return(diagram)
        # Export the diagram to an SVG file
        # export_graph(diagram, file_name = output_file, file_type = "svg")
        # svg_code <- export_svg(diagram)
        
        # Re-import the SVG file to use within grobs
        # svg_grob <- grImport2::readPicture(grImport2::dPicture(output_file))
        
        # return(svg_grob)
}


recode_df_for_brms <- function(model, df){
        library(brms)
        library(effectsize)
        model
        var <- model[["Design"]][["name"]]
        var <- c(model$yname, var[!grepl(" \\* ", var)])
        d <- df %>% select(var) %>% 
                mutate(across(where(is.numeric), 
                              ~ if (n_distinct(.) > 2) {effectsize::standardize(.)} else {.})) 
        # %>%
        #         mutate(across(everything(),~if (is.factor(.)) {as.numeric(.)} else {.}))
}




# Custom ggplot theme
theme_ch <- function(...){
        library(viridis)
        theme_pubclean() + 
        theme(
                # legend.position = c(0.95, 0.05),
                # legend.justification = c(1, 0),
                legend.position = "bottom",
                legend.title = element_blank(),
                # plot.title = element_text(hjust = 0.5),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.ticks = element_line(),
                axis.text.x = element_text(angle = 0),
                text = element_text(size = 10),
                strip.background = element_blank()
        ) 
}  
       




# plot interactions from rmsb-model and annotate
plotInteraction <- function(model, pred, var_of_interest, group_var, display_annotations = TRUE) {
        library(tidyverse)
        #Extract variable levels 
        var_sym <- sym(var_of_interest)
        group_var_sym <- sym(group_var)
        
        var_levels <- pred %>%
                pull(!!var_sym) %>%
                unique()
        
        group_levels <- pred %>%
                pull(!!group_var_sym) %>%
                unique()
        
        
        # Extract relevant rows with P-values greater than 0.9 or less than 0.1
        annotations <- getRsmb(model, output = "table") %>%
                as.data.frame() %>%
                rownames_to_column(var = "interaction") %>%
                filter(grepl(" * ", interaction)) %>% 
                filter((P > 0.9 | P < 0.1) & str_detect(interaction, paste0(group_var, "="))) %>% 
                filter(str_detect(interaction, var_of_interest))
                
        
        # max height
        height <- pred %>% as_tibble() %>% select(upper) %>% max()
        annotations <- annotations %>% 
                mutate(!!var_sym:= sub(".*=(.*?) \\*.*", "\\1", interaction)) %>% 
                mutate(!!group_var_sym:=group_levels[[1]])
        
        #update Axis Label from data_dic
        data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        xlab <- data_dic %>% dplyr::filter(name %in% var_of_interest) %>% pull(label)
        yname <- model$yname
        ylab <- data_dic %>% dplyr::filter(name %in% yname) %>% pull(label)
        
        # Plot basis 
        p <- ggplot(pred %>% as_tibble(), 
                    aes_string(x = var_of_interest, y = "yhat", color = group_var, group = group_var)) + 
                geom_jitter(position = position_dodge(width = 0.5), size = 3) +  # Adding jittered points
                geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(width = 0.5)) +  # Adding error bars with the same jitter
                geom_line(position = position_dodge(width = 0.5)) +  # Adding a line connecting the points
                theme_ch() +  
                scale_color_colorblind()  +
                theme(axis.text.y = element_text(vjust = 0))+
                xlab(xlab) +
                ylab(expression(P(Delirium ~ "|" ~ Data, x)))  +
                theme(
                        legend.position = "bottom",
                        legend.title = element_blank(),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        axis.ticks = element_line(),
                        axis.text.x = element_text(angle = 0),
                        text = element_text(size = 10)
                )
        
        # Add annotations if requested
        if (display_annotations) {
                p <- p +
                        geom_text(
                                data = annotations,
                                aes(x = !!var_sym, y = height + height * 0.05, 
                                    label = paste0("P = ", round(P, 3))),
                                color = "black",
                                position = position_dodge(width = 0.5),
                                vjust = 0,
                                hjust = 0.5,
                                size=3
                        )
        }
        
        # Print the plot
        print(p)
}

# plot rsmb model with forest plot
plotForest <- function(model, print_oddsratio = TRUE, sig_color = "black", nonsig_color = "grey50", interactions_only=FALSE, size = 2, option="group"){
        
        
        if (interactions_only){
                tbl <- getRsmb(model) %>% as_tibble(rownames="Predictor") %>%
                        filter(grepl("\\*", Predictor)) %>% 
                arrange(-P) %>%
                        mutate(OR_CI_Label = sprintf("%.2f (%.2f—%.2f)", Median, Lower, Upper)) %>% 
                        mutate(Significant = ifelse(Lower > 1 | Upper < 1, "Significant", "Not Significant")) 
                
                colnames(tbl) <- c("Predictor","Effect", "SE", "Lower.0.95","Upper.0.95","P","OR_CI_Label","Significant")
                
        } else {
        tbl <- getTableRsmb(model, print = FALSE) %>%
                as_tibble(rownames = "Predictor") %>%
                mutate(across(-Predictor, as.double)) %>%
                # arrange(-Effect) %>%
                mutate(OR_CI_Label = sprintf("%.2f (%.2f—%.2f)", Effect, Lower.0.95, Upper.0.95)) %>% 
                mutate(Significant = ifelse(Lower.0.95 > 1 | Upper.0.95 < 1, "Significant", "Not Significant"))
        }
        
        if (option=="group"){
                data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
                grp <- data_dic %>% select(label, group)
                tbl <- tbl %>%
                        mutate(label = str_trim(str_extract(Predictor, "^[^:-]+")))  
                tbl <- tbl %>% left_join(grp, by = c("label"))
                
                desired_group_order <- c(
                        "other",
                        "adverse outcome",
                        "therapy",
                        "previous diagnosis",
                        "risk factor",
                        "clinical aspects",
                        "stroke severity",
                        "demographics"
                )
                tbl <- tbl %>% mutate(group = factor(group, levels = desired_group_order)) %>% 
                        group_by(group) %>%
                        arrange(group, Predictor) %>%
                        ungroup() %>%
                        # Create a factor for Predictor with levels in the desired order
                        mutate(Predictor = factor(Predictor, levels = rev(unique(Predictor))))
                # Order predictors within each group
                tbl <- tbl %>% group_by(group) %>%
                        arrange(group, Predictor) %>%
                        ungroup() %>%
                        # Create a factor for Predictor with levels in the desired order
                        mutate(Predictor = factor(Predictor, levels = rev(unique(Predictor))))
                
        }
        
       
        
        
        
        x_max <- max(tbl$Upper.0.95)
        
        # Create the forest plot
        if (interactions_only){
            g <-    ggplot(tbl, aes(x = P, y = reorder(Predictor, P))) +
                        geom_point(aes(color = Significant), shape= 3, size = size) +
                        
                        geom_vline(xintercept = 1,
                                   linetype = "dashed",
                                   color = sig_color) +
                        geom_vline(xintercept = 0,
                                   linetype = "dashed",
                                   color = sig_color) +
                        labs(x = "Probability of Interaction with biological sex $$P(\beta>0)$$", y = "",
                             title = "") +
                        theme_ch() +
                        scale_color_manual(values = c("Significant" = sig_color, "Not Significant" = nonsig_color), guide="none")   
                
        } else {
        # g <- ggplot(tbl, aes(x = Effect, y = reorder(Predictor, Effect))) +
        g <- ggplot(tbl, aes(x = Effect, y = Predictor)) +
                geom_point(aes(color = Significant), size = size) +
                geom_errorbarh(aes(xmin = Lower.0.95, xmax = Upper.0.95, color = Significant), height = 0.2, linewidth = 0.3) +
                geom_vline(xintercept = 1,
                           linetype = "dashed",
                           color = sig_color) +
                labs(x = "Odds Ratio", y = "",
                     title = "") +
                theme_ch() +
                scale_color_manual(values = c("Significant" = sig_color, "Not Significant" = nonsig_color), guide="none") +
                facet_wrap(~ group, scales = "free_y", ncol = 1)
        
        if (print_oddsratio) {
                g <- g + geom_text(
                        aes(
                                label = OR_CI_Label,
                                x = x_max + 0.3,
                                color = Significant
                        ),
                        size = 2.5,
                        # color = "grey20",
                        hjust = 0
                ) + theme(axis.text.y.right = element_blank(),
                          # Remove the secondary axis text
                          plot.margin = unit(c(1, 6, 1, 1), "lines")) +
                        coord_cartesian(xlim = c(0, x_max+0.1), clip = "off")
        }
        }
        
        g <- g + theme(text = element_text(family = "Helvetica", size = 12))
        
        return(g)
                
}

plotForest <- function(model, print_oddsratio = TRUE, sig_color = "black", nonsig_color = "grey50",
                       interactions_only = FALSE, size = 2, option = "group", x_min_plot = NULL, x_max_plot = NULL) {
        # Load necessary libraries
        library(dplyr)
        library(ggplot2)
        library(readr)
        library(stringr)
        library(scales)
        
        # Data preparation
        if (interactions_only) {
                tbl <- getRsmb(model) %>%
                        as_tibble(rownames = "Predictor") %>%
                        filter(grepl("\\*", Predictor)) %>%
                        arrange(-P) %>%
                        mutate(OR_CI_Label = sprintf("%.2f (%.2f—%.2f)", Median, Lower, Upper)) %>%
                        mutate(Significant = ifelse(Lower > 1 | Upper < 1, "Significant", "Not Significant"))
                
                colnames(tbl) <- c("Predictor", "Effect", "SE", "Lower.0.95", "Upper.0.95", "P", "OR_CI_Label", "Significant")
                
        } else {
                tbl <- getTableRsmb(model, print = FALSE) %>%
                        as_tibble(rownames = "Predictor") %>%
                        mutate(across(-Predictor, as.double)) %>%
                        mutate(OR_CI_Label = sprintf("%.2f (%.2f—%.2f)", Effect, Lower.0.95, Upper.0.95)) %>%
                        mutate(Significant = ifelse(Lower.0.95 > 1 | Upper.0.95 < 1, "Significant", "Not Significant"))
        }
        
        if (option == "group") {
                data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
                grp <- data_dic %>% select(label, group)
                tbl <- tbl %>%
                        mutate(label = str_trim(str_extract(Predictor, "^[^:-]+")))
                tbl <- tbl %>% left_join(grp, by = c("label"))
                
                desired_group_order <- c(
                        "demographics",
                        "stroke severity",
                        "clinical aspects",
                        "risk factor",
                        "previous diagnosis",
                        "therapy",
                        "adverse outcome",
                        "other"
                )
                tbl <- tbl %>%
                        mutate(group = factor(group, levels = desired_group_order)) %>%
                        group_by(group) %>%
                        arrange(group, Predictor) %>%
                        ungroup() %>%
                        mutate(Predictor = factor(Predictor, levels = rev(unique(Predictor))))
        }
        
        # Determine x_min and x_max for plotting
        x_max_data <- max(tbl$Upper.0.95, na.rm = TRUE)
        x_min_data <- min(tbl$Lower.0.95, na.rm = TRUE)
        
        if (!is.null(x_max_plot)) {
                x_max <- x_max_plot
        } else {
                x_max <- x_max_data
        }
        
        if (!is.null(x_min_plot)) {
                x_min <- x_min_plot
        } else {
                x_min <- x_min_data
        }
        
        # Adjust data for plotting
        tbl <- tbl %>%
                mutate(
                        Upper.0.95_adj = pmin(Upper.0.95, x_max),
                        Lower.0.95_adj = pmax(Lower.0.95, x_min),
                        Upper_Capped = Upper.0.95 > x_max,
                        Lower_Capped = Lower.0.95 < x_min,
                        Effect_adj = pmin(pmax(Effect, x_min), x_max),
                        Effect_Capped = (Effect > x_max) | (Effect < x_min)
                        # Keep OR_CI_Label with original values
                )
        
        tbl <- tbl %>%
                mutate(
                        Predictor_label = ifelse(
                                Significant == "Significant",
                                sprintf('<span style="color:%s">%s</span>', sig_color, Predictor),
                                sprintf('<span style="color:%s">%s</span>', nonsig_color, Predictor)
                        )
                )
        
        # Split data into subsets for plotting
        data_within_limits <- tbl %>% filter(!Upper_Capped & !Lower_Capped)
        data_upper_capped <- tbl %>% filter(Upper_Capped & !Lower_Capped)
        data_lower_capped <- tbl %>% filter(!Upper_Capped & Lower_Capped)
        data_both_capped <- tbl %>% filter(Upper_Capped & Lower_Capped)
        
        # Create the forest plot
        if (interactions_only) {
                g <- ggplot(tbl, aes(x = P, y = reorder(Predictor, P))) +
                        geom_point(aes(color = Significant), shape = 3, size = size) +
                        geom_vline(xintercept = 1, linetype = "dashed", color = sig_color) +
                        geom_vline(xintercept = 0, linetype = "dashed", color = sig_color) +
                        labs(x = "Probability of Interaction with biological sex $$P(\\beta>0)$$", y = "", title = "") +
                        theme_ch() +
                        scale_color_manual(values = c("Significant" = sig_color, "Not Significant" = nonsig_color), guide = "none")
                
        } else {
                g <- ggplot(tbl, aes(x = Effect_adj, y = Predictor_label)) +
                        geom_point(aes(color = Significant), size = size) +
                        # Error bars within limits
                        geom_errorbarh(
                                data = data_within_limits,
                                aes(xmin = Lower.0.95_adj, xmax = Upper.0.95_adj, color = Significant),
                                height = 0.2, linewidth = 0.3
                        ) +
                        # Error bars with upper capping (arrow pointing right)
                        geom_segment(
                                data = data_upper_capped,
                                aes(x = Lower.0.95_adj, xend = Upper.0.95_adj, y = Predictor, yend = Predictor, color = Significant),
                                linewidth = 0.3,
                                arrow = arrow(length = unit(0.1, "inches"), ends = "last", type = "open")
                        ) +
                        # Error bars with lower capping (arrow pointing left)
                        geom_segment(
                                data = data_lower_capped,
                                aes(x = Lower.0.95_adj, xend = Upper.0.95_adj, y = Predictor, yend = Predictor, color = Significant),
                                linewidth = 0.3,
                                arrow = arrow(length = unit(0.1, "inches"), ends = "first", type = "open")
                        ) +
                        # Error bars with both ends capped (arrows at both ends)
                        geom_segment(
                                data = data_both_capped,
                                aes(x = Lower.0.95_adj, xend = Upper.0.95_adj, y = Predictor, yend = Predictor, color = Significant),
                                linewidth = 0.3,
                                arrow = arrow(length = unit(0.1, "inches"), ends = "both", type = "open")
                        ) +
                        geom_vline(xintercept = 1, linetype = "dashed", color = sig_color) +
                        labs(x = "Odds Ratio", y = "", title = "") +
                        theme_ch() +
                        scale_color_manual(values = c("Significant" = sig_color, "Not Significant" = nonsig_color), guide = "none") +
                        # Facet grid with group labels on the left
                        facet_grid(group ~ ., scales = "free", space = "free", switch = "y") +
                        theme(
                                text = element_text(family = "Helvetica", size = 10),
                                strip.text.y.left = element_text(angle = 0, size = 10),
                                strip.placement = "outside",
                                panel.spacing = unit(0.5, "lines"),
                                axis.text = element_text(size = 10),
                                axis.title = element_text(size = 10),
                                axis.text.y = element_markdown(size = 10)
                        )
                
                # Add odds ratio labels with original values
                if (print_oddsratio) {
                        g <- g + geom_text(
                                aes(
                                        label = OR_CI_Label,
                                        x = x_max + 0.3,
                                        color = Significant
                                ),
                                size = 10 / 2.83,
                                hjust = 0
                        ) + theme(
                                axis.text.y.right = element_blank(),
                                plot.margin = unit(c(1, 6, 1, 1), "lines")
                        ) +
                                coord_cartesian(xlim = c(x_min, x_max + 1), clip = "off")
                }
        }
        
        g <- g + theme(text = element_text(family = "Helvetica", size = 10))
        
        return(g)
}





plotForest <- function(model, print_oddsratio = TRUE, sig_color = "black", nonsig_color = "grey50",
                       interactions_only = FALSE, size = 2, option = "group", font_size = 10,
                       x_min_plot = NULL, x_max_plot = NULL) {
        # Load necessary libraries
        library(dplyr)
        library(ggplot2)
        library(readr)
        library(stringr)
        library(ggtext)
        library(scales)
        
        # Define a custom theme
        theme_ch <- function() {
                theme_minimal() +
                        theme(
                                text = element_text(family = "Helvetica", size = font_size),
                                strip.text.y.left = element_text(angle = 0, size = font_size),
                                strip.placement = "outside",
                                panel.spacing = unit(1, "lines"),
                                axis.text = element_text(size = font_size),
                                axis.title = element_text(size = font_size),
                                axis.text.y = element_markdown(size = font_size)  # Enable markdown for y-axis labels
                        )
        }
        
        # Data preparation
        if (interactions_only) {
                tbl <- getRsmb(model) %>%
                        as_tibble(rownames = "Predictor") %>%
                        filter(grepl("\\*", Predictor)) %>%
                        arrange(-P) %>%
                        mutate(OR_CI_Label = sprintf("%.2f (%.2f—%.2f)", Median, Lower, Upper)) %>%
                        mutate(Significant = ifelse(Lower > 1 | Upper < 1, "Significant", "Not Significant"))
                
                colnames(tbl) <- c("Predictor", "Effect", "SE", "Lower.0.95", "Upper.0.95", "P", "OR_CI_Label", "Significant")
                
        } else {
                tbl <- getTableRsmb(model, print = FALSE) %>%
                        as_tibble(rownames = "Predictor") %>%
                        mutate(across(-Predictor, as.double)) %>%
                        mutate(OR_CI_Label = sprintf("%.2f (%.2f—%.2f)", Effect, Lower.0.95, Upper.0.95)) %>%
                        mutate(Significant = ifelse(Lower.0.95 > 1 | Upper.0.95 < 1, "Significant", "Not Significant"))
                
        }
        
        if (option == "group") {
                data_dic <- read_delim("data_dic.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
                grp <- data_dic %>% select(label, group)
                tbl <- tbl %>%
                        mutate(label = str_trim(str_extract(Predictor, "^[^:-]+")))
                tbl <- tbl %>% left_join(grp, by = c("label"))
                
                desired_group_order <- c(
                        "demographics",
                        "stroke severity",
                        "clinical aspects",
                        "risk factor",
                        "previous diagnosis",
                        "therapy",
                        "adverse outcome",
                        "etiology",
                        "other"
                )
                tbl <- tbl %>%
                        mutate(group = factor(group, levels = desired_group_order)) %>%
                        group_by(group) %>%
                        arrange(group, Predictor) %>%
                        ungroup() %>%
                        mutate(Predictor = factor(Predictor, levels = rev(unique(Predictor))))
                
                tbl$Predictor <- gsub("Aphasia symptoms at admission - ", "", tbl$Predictor)
                tbl$Predictor <- gsub("Neglect severity scale - ", "", tbl$Predictor)
                tbl$Predictor <- gsub("Modified TOAST Classification - ", "", tbl$Predictor)
                tbl$Predictor <- gsub("Stroke of undetermined etiology \\(SUD\\)\\:Cardioembolism \\(CE\\)", "mTOAST - SUD:CE", tbl$Predictor)
                tbl$Predictor <- gsub("Stroke of other determined etiology \\(SOD\\)\\:Cardioembolism \\(CE\\)", "mTOAST - SOD:CE", tbl$Predictor)
                tbl$Predictor <- gsub("Small artery disease \\(SAD\\)\\:Cardioembolism \\(CE\\)", "mTOAST - SAD:CE", tbl$Predictor)
                tbl$Predictor <- gsub("More than two causes identified \\(SUDm\\)\\:Cardioembolism \\(CE\\)", "mTOAST - SUDm:CE", tbl$Predictor)
                tbl$Predictor <- gsub("Uncertain determination \\(SUDu\\)\\:Cardioembolism \\(CE\\)", "mTOAST - SUDu:CE", tbl$Predictor)
                tbl$Predictor <- gsub("Atherothrombosis \\(AT\\)\\:Cardioembolism \\(CE\\)", "mTOAST - AT:CE", tbl$Predictor)
                tbl$Predictor <- gsub("\\(admission\\)", "", tbl$Predictor)
                tbl$Predictor <- gsub("Presence of ", "", tbl$Predictor)
                tbl$Predictor <- gsub("Administered", "", tbl$Predictor)
                # tbl$Predictor <- gsub("\\(\\)", "", tbl$Predictor)
                tbl$Predictor <- gsub("Level of Consciousness", "LOC", tbl$Predictor)
                tbl$Predictor <- gsub("Cerebral", "", tbl$Predictor)
                tbl$Predictor <- gsub("FALSE:TRUE", "0:1", tbl$Predictor)
                
                
        }
        
        # Determine x_min and x_max for plotting
        x_max_data <- max(tbl$Upper.0.95, na.rm = TRUE)
        x_min_data <- min(tbl$Lower.0.95, na.rm = TRUE)
        
        if (!is.null(x_max_plot)) {
                x_max <- x_max_plot
        } else {
                x_max <- x_max_data
        }
        
        if (!is.null(x_min_plot)) {
                x_min <- x_min_plot
        } else {
                x_min <- x_min_data
        }
        
        # Adjust data for plotting
        tbl <- tbl %>%
                mutate(
                        Upper_Capped = Upper.0.95 > x_max,
                        Lower_Capped = Lower.0.95 < x_min,
                        Upper.0.95_adj = ifelse(Upper_Capped, x_max, Upper.0.95),
                        Lower.0.95_adj = ifelse(Lower_Capped, x_min, Lower.0.95),
                        Effect_Capped = (Effect > x_max) | (Effect < x_min)
                )
        
        # adj col
        tbl <- tbl %>%
                mutate(
                        Predictor_label = ifelse(
                                Significant == "Significant",
                                sprintf('<span style="color:%s">%s</span>', sig_color, Predictor),
                                sprintf('<span style="color:%s">%s</span>', nonsig_color, Predictor)
                        )
                )
        
        # Data subsets needed for capped error bars
        data_within_limits <- tbl %>% filter(!Upper_Capped & !Lower_Capped)
        data_upper_capped <- tbl %>% filter(Upper_Capped & !Lower_Capped)
        data_lower_capped <- tbl %>% filter(!Upper_Capped & Lower_Capped)
        data_both_capped <- tbl %>% filter(Upper_Capped & Lower_Capped)
        
        # forest plot
        if (interactions_only) {
                g <- ggplot(tbl, aes(x = P, y = reorder(Predictor_label, P))) +
                        geom_point(aes(color = Significant), shape = 3, size = size) +
                        geom_vline(xintercept = 1, linetype = "dashed", color = sig_color) +
                        geom_vline(xintercept = 0, linetype = "dashed", color = sig_color) +
                        labs(x = "Probability of Interaction with Biological Sex $$P(\\beta>0)$$", y = "", title = "") +
                        theme_ch() +
                        scale_color_manual(values = c("Significant" = sig_color, "Not Significant" = nonsig_color), guide = "none")
                
        } else {
                g <- ggplot(tbl, aes(x = Effect, y = Predictor_label)) +
                        # points
                        geom_point(aes(color = Significant), size = size) +
                        # Error bars within limits
                        geom_errorbarh(
                                data = data_within_limits,
                                aes(xmin = Lower.0.95_adj, xmax = Upper.0.95_adj, color = Significant),
                                height = 0.2, linewidth = 0.3
                        ) +
                        # Error bars with upper capping (arrow pointing right)
                        geom_segment(
                                data = data_upper_capped,
                                aes(x = Lower.0.95_adj, xend = Upper.0.95_adj, y = Predictor_label, yend = Predictor_label, color = Significant),
                                linewidth = 0.3,
                                arrow = arrow(length = unit(0.1, "inches"), ends = "last", type = "open")
                        ) +
                        # Error bars with lower capping (arrow pointing left)
                        geom_segment(
                                data = data_lower_capped,
                                aes(x = Lower.0.95_adj, xend = Upper.0.95_adj, y = Predictor_label, yend = Predictor_label, color = Significant),
                                linewidth = 0.3,
                                arrow = arrow(length = unit(0.1, "inches"), ends = "first", type = "open")
                        ) +
                        # Error bars with both ends capped (arrows at both ends)
                        geom_segment(
                                data = data_both_capped,
                                aes(x = Lower.0.95_adj, xend = Upper.0.95_adj, y = Predictor_label, yend = Predictor_label, color = Significant),
                                linewidth = 0.3,
                                arrow = arrow(length = unit(0.1, "inches"), ends = "both", type = "open")
                        ) +
                        geom_vline(xintercept = 1, linetype = "dashed", color = sig_color) +
                        labs(x = "Odds Ratio", y = "", title = "") +
                        theme_ch() +
                        scale_color_manual(values = c("Significant" = sig_color, "Not Significant" = nonsig_color), guide = "none") +
                        # scale_x_log10(
                        #         breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20),  # Custom breaks for odds ratios
                        #         labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10", "20")  # Corresponding labels
                        # ) + annotation_logticks(sides = "b") +
                        facet_grid(group ~ ., scales = "free", space = "free", switch = "y") +
                        # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        #               labels = trans_format("log10")) + annotation_logticks(sides = "b") +
                        theme(
                                strip.placement = "outside",
                                panel.spacing = unit(1, "lines")
                        )
                
                # odds ratios text 
                if (print_oddsratio) {
                        g <- g + geom_text(
                                aes(
                                        label = OR_CI_Label,
                                        x = x_max + 1,
                                        color = Significant
                                ),
                                size = font_size / 2.83,
                                hjust = 0,
                                family = "Helvetica"
                        ) + theme(
                                axis.text.y.right = element_blank(),
                                plot.margin = unit(c(1, 7, 1, 1), "lines")
                        ) +
                                coord_cartesian(xlim = c(x_min, x_max), clip = "off")
                }
                
        }
        
        # Apply final theme settings
        g <- g + theme(text = element_text(family = "Helvetica", size = font_size))
        
        return(g)
}





































# NOT WORKING - USE  instead plotForest()
# Custom Forest Plot
# based on https://www.khstats.com/blog/forest-plots/#just-the-code
makeForestPlot <- function(df){
        
        df$var <- rownames(df)
        df <- df %>% mutate('Odds Ratio (95% Credible Interval)' = paste0("(",Lower.0.95,"--",Upper.0.95,")"))
        
        x_min <- min(df$Lower.0.95, na.rm = TRUE) - 2
        x_max <- max(df$Upper.0.95, na.rm = TRUE) + 1
        
        part_vis <- 
                ggplot(df, aes(y = var)) +
                geom_point(aes(x = Effect), shape = 15, size = 3) +
                geom_linerange(aes(xmin = Lower.0.95, xmax = Upper.0.95)) +
                labs(x = "Odds Ratio (95% credible intervals)") +
                coord_cartesian(ylim = c(0, 2), xlim = c(x_min, x_max)) +
                geom_vline(xintercept = 0, linetype = "dashed") +
                annotate("text", x = 0 + 0.2, y = 3, hjust = 0, label = "Delirium more likely") +
                annotate("text", x = 0 - 0.2, y = 3, hjust = 1, label = "Delirium less likely") +
                theme_classic() +
                theme(axis.line.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.title.y = element_blank())
        
        part_variables <-  
                ggplot(df, aes(y=var)) +
                geom_text(aes(x = 0, label = var), hjust = 0, fontface = "bold") +
                theme_void() + coord_cartesian(xlim = c(0,0))
        
        
        
        layout <- c(
                area(t = 0, l = 0, b = 30, r = 3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
                area(t = 1, l = 4, b = 30, r = 9) # middle plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
                # area(t = 0, l = 9, b = 30, r = 11) # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9), goes to bottom of page (b=30), and extends two units wide (r=11)
        )
        # final plot arrangement
        part_variables + part_vis + plot_layout(design = layout)
        
}


working <- function(){
        
        
model %>% plot() %>% pluck("data")

ggplot(data = data.frame(x = c(0.01,10)), aes(x)) +        
        scale_x_log10('Odds Ratio', breaks=c(0.1, 0.2,0.33,1,3, 5, 10)) +  
        
        # posterior density plot
        stat_halfeye(
                data=data.frame(x=exp(post$draws)),
                normalize = "none", 
                slab_size = 0.4,slab_color = "black", alpha=0.5,
                aes(fill = stat(x > 0)),   ## 0 on the logged scale = OR of 1 
                color= NA) +
        
        ## Approximate Dirichlet prior with a log-normal distribution
        stat_dist_halfeye(
                orientation = "horizontal",
                linetype = 2,
                slab_size = 0.5,
                slab_fill = NA,
                slab_colour = "black",
                normalize = "none",
                alpha=0.5,
                color= NA,   
                aes(dist = "lnorm", arg1 = 0, arg2 = 0.541))  + # approximate Dirichlet with log-normal
        scale_fill_manual(labels = c("Other: Higher Odds", "Emergency: Higher odds"), 
                          values = c("blue", "skyblue")) +
        stat_interval(data=data.frame(x= exp(post$draws)), aes(y = 0.03), 
                      point_interval= median_hdi, 
                      .width = c(.95), color="black", show_point = TRUE, size=1, show.legend = FALSE) +
        scale_y_continuous('Probability density', expand = c(0, 0), breaks=seq(0, 3.4, 0.2))   + 
        
        coord_cartesian(xlim = c(0.1,11), ylim = c(0, 5)) +
        
        geom_vline(xintercept=1,linetype="solid") +
        
        ## labels to indicate prior and posterior distributions
        annotate("text", x = 0.2, y = 0.4, hjust = 0,
                 label = "Prior (Dirichlet)",
                 size = 5, color="#818283") +
        annotate("text", x = 3, y = 1.4, hjust = 0,
                 label = "Posterior",
                 size = 5, color="#818283") +
        theme_light(base_size = 13) +
        theme(
                legend.position=c(0.2,0.8),
                text = element_text(colour = "#2E3F4F"),
                plot.title = element_text(face = "bold"),
                plot.caption = element_text(face = "italic"),
                panel.grid.major = element_line(colour = "#E7E9E9"),
                panel.grid.minor = element_line(colour = "#F4F5F5"),
                strip.text = element_text(face = "bold", colour = "#2E3F4F"  ),
                strip.background = element_rect(colour = "#2E3F4F", fill = "grey90"),  
                legend.background = element_rect(fill = 'transparent'),
                legend.title=element_blank())


}






































        
        
        




### OLD / WASTE ----

# age_start_x <- bm_delirium_age_contrast_df$age[min(which(bm_delirium_age_contrast_df$probability > 0.95))]

# bm_delirium_age_p2 <-
#         ggplot(bm_delirium_age_contrast_df, aes(x = age, y = probability)) +
#         geom_line(color = "black") +
#         geom_point(color = "grey", size = 1) +
#         geom_ribbon(
#                 data = subset(bm_delirium_age_contrast_df, probability > 0.95),
#                 aes(ymin = 0, ymax = probability),
#                 fill = "lightgrey",
#                 alpha = 0.2
#         ) +
#         labs(title = "", x = "Age", y = "Probability of difference females and males") +
#         theme_minimal() +
#         theme(
#                 legend.position = "right",
#                 legend.title = element_blank(),
#                 plot.title = element_text(hjust = 0.5),
#                 panel.grid.major.x = element_blank(),
#                 panel.grid.minor.x = element_blank(),
#                 axis.ticks = element_line(),
#                 # axis.text.x = element_blank(),
#                 # axis.text.y = element_blank()
#         ) +
#         annotate(
#                 "text",
#                 x = 45,
#                 y = 0.625,
#                 label = paste(
#                         "Prob. > 95% crossing at age ",
#                         as.integer(age_start_x),
#                         "\nfor a meaningful difference \nbetween females and males \nin developing a delirium."
#                 ),
#                 color = "black",
#                 size = 8 / .pt,
#                 hjust = 0,
#                 vjust = 1
#         ) +
#         geom_segment(
#                 aes(
#                         x = age_start_x + 1,
#                         y = 0.925,
#                         xend = 44,
#                         yend = 0.65
#                 ),
#                 color = "black",
#                 linetype = "solid",
#                 linewidth = 0.2
#         ) +
#         annotate(
#                 "point",
#                 x = 26,
#                 y = 0.95,
#                 shape = 21,
#                 color = "black",
#                 fill = NA,
#                 size = 3,
#                 stroke = 0.5
#         )

create_latex_table <- function(d, cat_var, strat_var) {
        # Calculate frequencies and percentages
        df_summary <- d %>% group_by(!!sym(strat_var), !!sym(cat_var)) %>%
                summarise(n = n(), .groups = "drop") %>%
                mutate(percentage = round(n/sum(n)*100, digits=1)) %>%
                unite("n (%)", c("n", "percentage"), sep = " (", remove = FALSE) %>%
                mutate(`n (%)` = paste0(`n (%)`, ")")) %>%
                select(-n, -percentage)
        
        # Pivot the table to wide format
        df_wide <- df_summary %>%
                pivot_wider(names_from = !!sym(strat_var), values_from = `n (%)`, values_fill = "0 (0%)")
        
        # Print the table as a LaTeX table
        kable(df_wide, "latex", booktabs = TRUE, align = c('l','l',"D{.}{.}{2}"), row.names = TRUE) %>%
                kable_styling(full_width = F)
}



