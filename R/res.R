getResDelAge <- function(df_adm_f = df_adm_f, data_dic = data_dic){
        library(Hmisc)
        library(rms)
        library(rmsb)
        library(knitr)
        library(kableExtra)
        library(rvest)
        library(texreg)
        library(huxtable)
        library(flextable)
        library(modelsummary)
        library(magrittr)
        require(qreport)
        library(tidyverse)
        library(ormPlot)
        library(cowplot)
        library(ggpubr)
        
        df <- df_adm_f
        dd <- datadist(df)
        options(datadist = 'dd')
        options(contrasts=c("contr.treatment", "contr.treatment"))
        
        #Variables
        dv = "outcome_delir"
        iv0 = "rcs(age, 5) + sex"
        ia = "rcs(age, 5) * sex"
        #Model
        bm_delirium_age <- getModel(dv, iv0, file_name = "output/models/bm_delirium_age", data = df, force_overwrite = FALSE)
        #Table
        bm_delirium_age_tbl <- getLatexTableCH(bm_delirium_age, font_size=8)
        #interaction model
        bm_delirium_age_ia <- getModel(dv, ia, file_name = "output/models/bm_delirium_age_ia", data = df, force_overwrite = FALSE)
        # Interaction Table
        bm_delirium_age_tbl_ia <- getLatexTableCH(bm_delirium_age_ia, font_size=8, interactions_only = TRUE)
        
        #Prediction
        bm_delirium_age_p <- Predict(bm_delirium_age_ia, age=c(20:100),  sex, funint=FALSE, fun=plogis, conf.int = FALSE)
        bm_delirium_age_footnote <- getFootnote(bm_delirium_age_p)
        # Probability difference
        bm_delirium_age_contrast <- contrast(bm_delirium_age_ia, 
                                             list(sex="male", age=c(bm_delirium_age_p$age)),
                                             list(sex="female", age=c(bm_delirium_age_p$age)))
        
        bm_delirium_age_contrast_df <- data.frame(age=bm_delirium_age_contrast$age, probability=bm_delirium_age_contrast$PP)
        
        # Plots
        bm_delirium_age_p1 <- ggplot(bm_delirium_age_p,
                                     adj.subtitle = 0,
                                     ylim = c(0, 0.4), 
                                     xlim=c(20,100)) + theme_minimal() + scale_color_viridis_d()  +
                ylab(expression(P(Delirium ~ "|" ~ Data, x))) +
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
                        text = element_text(size = 10)
                )
        # ) +
        # scale_x_continuous(sec.axis = sec_axis(~ . , name = "", breaks = NULL, labels = NULL))
        
        bm_delirium_age_p_diff <-
                plotSexDiffGradient(
                        df,
                        bm_delirium_age_ia,
                        var = "age",
                        dist = c(20:100),
                        xlab = expression(P(Delta ~ "|" ~ data, x) == P(theta[F] ~ "|" ~ data, x) - P(theta[M] ~ "|" ~ data, x))) + scale_y_discrete(breaks = c(20, 40, 60, 80, 100)) + coord_flip()
        
        bm_delirium_age_p2_footnote <- paste0(expression(P(Delta ~ "|" ~ data, x)), " is the posterior probability density of the contrast between females and males of developing a delirium at a given age x.")
        
        library(gridExtra)
        library(ormPlot)
        library(cowplot)
        bm_delirium_age_p1 <- ggplot(Predict(bm_delirium_age_ia, age)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        bm_delirium_age_p2 <- ggplot(Predict(bm_delirium_age_ia, sex)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + coord_cartesian(expand = TRUE, ylim = c(-4, 0)) + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        s <- specs(bm_delirium_age_ia)
        s <- s[["how.modeled"]] %>% as.data.frame()
        spline_knots <- as.numeric(unlist(strsplit(trimws(s[["Parameters"]][[1]]), " ")))
        d.f. <- s$d.f.[[1]]
        bm_delirium_age_p3 <- ggplot(Predict(bm_delirium_age_ia, age, sex, conf.int = FALSE)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D")+ theme(plot.margin = unit(c(0.5,0,0,0.5), "cm")) + geom_vline(xintercept = spline_knots, linetype = "dashed", color = "lightgrey") # include splines knot points visually
        bm_delirium_age_p4 <- plot(bm_delirium_age_ia) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + coord_cartesian(expand = TRUE, xlim = c(-8, 12)) + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        bm_delirium_age_p5 <- pp_check.blrm(bm_delirium_age_ia, type = "violin_grouped", group = "sex", df=df) + coord_cartesian(expand = TRUE) + theme(legend.position = "top", plot.margin = unit(c(0.5,0,0,1), "cm"))
        #forest plot
        # p6_temp <- forestplot(summary(bm_delirium_age_ia), return_ggplots = TRUE)
        bm_delirium_age_p6 <- forestplot(summary(bm_delirium_age_ia), return_ggplots = TRUE)
        bm_delirium_age_p7 <- stanDxplot(bm_delirium_age_ia, which="sex=male") + theme_ch() + coord_cartesian(expand = TRUE, xlim=c(0,1400), ylim=c(-10,25)) + coord_flip() + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        res_del_age <-
                list(
                        dv = dv,
                        iv0 = iv0,
                        ia = ia,
                        model = bm_delirium_age,
                        tbl = bm_delirium_age_tbl,
                        model_ia = bm_delirium_age_ia,
                        spline_knots = spline_knots,
                        d.f. = d.f.,
                        tbl_ia = bm_delirium_age_tbl_ia,
                        pred = bm_delirium_age_p,
                        pred_fn = bm_delirium_age_footnote,
                        p1 = bm_delirium_age_p1,
                        p2 = bm_delirium_age_p2,
                        p3 = bm_delirium_age_p3,
                        p4 = bm_delirium_age_p4,
                        p5 = bm_delirium_age_p5,
                        p6 = bm_delirium_age_p6,
                        p7 = bm_delirium_age_p7,
                        p_diff = bm_delirium_age_p_diff
                )
        return(res_del_age)
        
}

getResDelPrs <- function(df_adm_f = df_adm_f, data_dic = data_dic){
        library(Hmisc)
        library(rms)
        library(rmsb)
        library(knitr)
        library(kableExtra)
        library(rvest)
        library(texreg)
        library(huxtable)
        library(flextable)
        library(modelsummary)
        library(magrittr)
        require(qreport)
        library(tidyverse)
        library(ormPlot)
        library(cowplot)
        library(ggpubr)
        
        df <- df_adm_f
        dd <- datadist(df)
        options(datadist = 'dd')
        options(contrasts=c("contr.treatment", "contr.treatment"))
        
        #Variables
        dv = "outcome_delir"
        iv0 = "pRS + sex"
        # ia = "pRS * sex"
        #Model
        bm_delirium_pRS <- getModel(dv, iv0, file_name = "output/models/bm_delirium_pRS", loo=FALSE, data = df, force_overwrite = FALSE)
        #Table
        bm_delirium_pRS_tbl <- getLatexTableCH(bm_delirium_pRS, font_size=8)
        #interaction model
        #FAILED to build despite adapt_delta and max_treedepth ... coallapsing category 4 and 5 still possible
        # bm_delirium_pRS_ia <- getModel(dv, ia, file_name = "output/models/bm_delirium_pRS_ia", data = df, cores = 8, iter = 4000, thin = 5, control = list(adapt_delta = 0.99, max_treedepth = 15), force_overwrite = FALSE)
        
        #Prediction
        bm_delirium_pRS_p <- Predict(bm_delirium_pRS, pRS,  sex, funint=FALSE, fun=plogis, conf.int = FALSE)
        bm_delirium_pRS_footnote <- getFootnote(bm_delirium_pRS_p)
        # Probability difference
        bm_delirium_pRS_contrast <- contrast(bm_delirium_pRS, 
                                             list(sex="male", pRS=c(bm_delirium_pRS_p$pRS)),
                                             list(sex="female", pRS=c(bm_delirium_pRS_p$pRS))) 
        bm_delirium_pRS_contrast_df <- data.frame(pRS=bm_delirium_pRS_contrast$pRS, probability=bm_delirium_pRS_contrast$PP)
        
        # Plots
        bm_delirium_pRS_p1 <- ggplot(bm_delirium_pRS_p,
                                     adj.subtitle = 0,
                                     ylim = c(0, 0.2)) + theme_minimal() + scale_color_viridis_d()  +
                ylab(expression(P(Delirium ~ "|" ~ Data, x))) +
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
                        text = element_text(size = 10)
                )
        
        bm_delirium_pRS_p2 <-
                plotSexDiffGradient(
                        df,
                        bm_delirium_pRS,
                        var = "pRS",
                        xlab = expression(P(Delta ~ "|" ~ data, x) == P(theta[F] ~ "|" ~ data, x) - P(theta[M] ~ "|" ~ data, x)))  +  xlim(-10, 5) + coord_flip()
        # + scale_y_discrete(breaks = c(20, 40, 60, 80, 100))
        bm_delirium_pRS_p2_footnote <- paste0(expression(P(Delta ~ "|" ~ data, x)), " is the posterior probability density of the contrast between females and males of developing a delirium at a given pRS x.")
        
        bm_delirium_pRS_p2 <-
                ggplot(Predict(bm_delirium_pRS, sex)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + coord_cartesian(expand = TRUE, ylim = c(-4, 0)) + theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"))
        
        
        s <- specs(bm_delirium_pRS)
        s <- s[["how.modeled"]] %>% as.data.frame()
        spline_knots <- as.numeric(unlist(strsplit(trimws(s[["Parameters"]][[1]]), " ")))
        d.f. <- s$d.f.[[1]]
        
        bm_delirium_pRS_p3 <- 
                ggplot(Predict(bm_delirium_pRS, pRS, sex, conf.int = FALSE)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D")+ theme(plot.margin = unit(c(0.5,0,0,0.5), "cm")) + geom_vline(xintercept = spline_knots, linetype = "dashed", color = "lightgrey") # include splines knot points visually
        
        bm_delirium_pRS_p4 <- 
                plot(bm_delirium_pRS) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + coord_cartesian(expand = TRUE, xlim = c(-2.2, 2.2)) + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        bm_delirium_pRS_p5 <- 
                pp_check.blrm(bm_delirium_pRS, type = "violin_grouped", group = "sex", df=df) + coord_cartesian(expand = TRUE) + theme(legend.position = c(0.9, 0.9), plot.margin = unit(c(0.5,0,0,1), "cm"))
        #forest plot
        # p6_temp <- forestplot(summary(bm_delirium_age_ia), return_ggplots = TRUE)
        
        theme_set(theme_pubclean() + theme(text = element_text(color = "black", size = 8)))
        
        bm_delirium_pRS_p6 <- forestplot(summary(bm_delirium_pRS))
        bm_delirium_pRS_p7 <- stanDxplot(bm_delirium_pRS, which="sex=male") + theme_ch() + coord_cartesian(expand = TRUE, xlim=c(0,1400), ylim=c(-10,25)) + coord_flip() + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        res_del_pRS <-
                list(
                        dv = dv,
                        iv0 = iv0,
                        # ia = ia,
                        model = bm_delirium_pRS,
                        spline_knots = spline_knots,
                        d.f. = d.f.,
                        tbl = bm_delirium_pRS_tbl,
                        # model_ia = bm_delirium_pRS_ia,
                        pred = bm_delirium_pRS_p,
                        pred_fn = bm_delirium_pRS_footnote,
                        p1 = bm_delirium_pRS_p1,
                        p2 = bm_delirium_pRS_p2,
                        p3 = bm_delirium_pRS_p3,
                        p4 = bm_delirium_pRS_p4,
                        p5 = bm_delirium_pRS_p5,
                        p6 = bm_delirium_pRS_p6,
                        p7 = bm_delirium_pRS_p7
                )
        return(res_del_pRS)
        
        
}

getResDelNihss <- function(df_adm_f = df_adm_f, data_dic = data_dic){
        library(Hmisc)
        library(rms)
        library(rmsb)
        library(knitr)
        library(kableExtra)
        library(rvest)
        library(texreg)
        library(huxtable)
        library(flextable)
        library(modelsummary)
        library(magrittr)
        require(qreport)
        library(tidyverse)
        library(ormPlot)
        library(cowplot)
        library(ggpubr)
        
        df <- df_adm_f
        dd <- datadist(df)
        options(datadist = 'dd')
        options(contrasts=c("contr.treatment", "contr.treatment"))
}

getResDelYear <- function(df_adm_f = df_adm_f, data_dic = data_dic){
        library(Hmisc)
        library(rms)
        library(rmsb)
        library(knitr)
        library(kableExtra)
        library(rvest)
        library(texreg)
        library(huxtable)
        library(flextable)
        library(modelsummary)
        library(magrittr)
        require(qreport)
        library(tidyverse)
        library(ormPlot)
        library(cowplot)
        library(ggpubr)
        library(ggthemes)
  
        df <- df_adm_f
        df <- df %>%
                mutate(therapy = factor(case_when(
                        therapy == "IVT" ~ "IVT",
                        therapy == "EST" | therapy == "IVT/EST" ~ "EST(+/-IVT)"
                ))) %>% 
                mutate(date_therapy_year_cat = factor(date_therapy_year))
        dd <- datadist(df)
        options(datadist = 'dd')
        options(contrasts=c("contr.treatment", "contr.treatment"))
        
        dv = "outcome_delir"
        iv0 = "rcs(date_therapy_year, 4) + sex + therapy"
        ia1 = "rcs(date_therapy_year, 4) * sex"
        ia2 = "rcs(date_therapy_year, 4) + sex * therapy"
        ia3 = "rcs(date_therapy_year, 4) * sex + therapy"
        iv0a = "rcs(date_therapy_year, 4) + sex + therapy + rcs(nihss_d0,3) + rcs(age, 3)"
        #Model
        bm_delirium_date_therapy_year <- getModel(dv, iv0, file_name = "output/models/bm_delirium_date_therapy_year", data = df, force_overwrite = FALSE)
        bm_delirium_date_therapy_year_adj <- getModel(dv, iv0a, file_name = "output/models/bm_delirium_date_therapy_year_iv0a", data = df, force_overwrite = FALSE)
        
        #Table
        bm_delirium_date_therapy_year_tbl <- getLatexTableCH(bm_delirium_date_therapy_year, font_size=8)
        #interaction model
        # bm_delirium_date_therapy_year_ia <- getModel(dv, ia, file_name = "output/models/bm_delirium_date_therapy_year_ia", data = df, force_overwrite = FALSE)
        bm_delirium_date_therapy_year_ia <- getModel(dv, ia2, file_name = "output/models/bm_delirium_date_therapy_year_ia", data = df, force_overwrite = FALSE)
        # bm_delirium_date_therapy_year_ia3 <- getModel(dv, ia3, file_name = "output/models/bm_delirium_date_therapy_year_ia3", data = df, force_overwrite = FALSE)
        
        #Predictions
        bm_delirium_date_therapy_year_p <- Predict(bm_delirium_date_therapy_year_ia, date_therapy_year,  sex, funint=FALSE, fun=plogis, conf.int = FALSE)
        bm_delirium_date_therapy_year_p1 <- Predict(bm_delirium_date_therapy_year_ia, date_therapy_year,  therapy, sex, funint=FALSE, fun=plogis, conf.int = FALSE)
        bm_delirium_date_therapy_year_footnote <- getFootnote(bm_delirium_date_therapy_year_p)
        
        # lollipop
        # bm_delirium_date_therapy_year_p0 <- plot_grouped_bar_chart(df, var="date_therapy_year_cat", group_var = "therapy", chart_type = "lollipop") + coord_cartesian(expand=TRUE, ylim=c(0,500))
                  
        # plot
        bm_delirium_year_p1 <- ggplot(
                bm_delirium_date_therapy_year_p1,
                adj.subtitle = 0) + theme_ch() + scale_color_colorblind()
                
        
        
        # Probability difference
        # bm_delirium_date_therapy_year_contrast <- contrast(bm_delirium_date_therapy_year, list(sex="male", date_therapy_year=c(bm_delirium_date_therapy_year_p$date_therapy_year))  )   
        # 
        # bm_delirium_date_therapy_year_contrast <- contrast(bm_delirium_date_therapy_year, list(sex="male", date_therapy_year=c(bm_delirium_date_therapy_year_p$date_therapy_year)),   
        #                                        list(sex="female", date_therapy_year=c(bm_delirium_date_therapy_year_p$date_therapy_year)))
        
        
        # bm_delirium_date_therapy_year_contrast_df <- data.frame(date_therapy_year=bm_delirium_date_therapy_year_contrast$date_therapy_year, probability=bm_delirium_date_therapy_year_contrast$PP)
        
        # Plots
        # bm_delirium_year_p1 <- ggplot(bm_delirium_date_therapy_year_p,
        #                               adj.subtitle = 0,
        #                               ylim = c(0, 0.4)) + theme_minimal() + scale_color_viridis_d()  +
        #         ylab("Probability of delirium") +
        #         theme(
        #                 # legend.position = c(0.95, 0.05),
        #                 # legend.justification = c(1, 0),
        #                 legend.position = "bottom",
        #                 legend.title = element_blank(),
        #                 # plot.title = element_text(hjust = 0.5),
        #                 panel.grid.major.x = element_blank(),
        #                 panel.grid.minor.x = element_blank(),
        #                 axis.ticks = element_line(),
        #                 axis.text.x = element_text(angle = 0)
        #         )
        # ) +
        # scale_x_continuous(sec.axis = sec_axis(~ . , name = "", breaks = NULL, labels = NULL))
        
        bm_delirium_year_p2 <-
                ggplot(Predict(bm_delirium_date_therapy_year_ia, sex)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + coord_cartesian(expand = TRUE, ylim = c(-4, 0)) + theme(plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"))
        
        bm_delirium_year_p3 <- 
                ggplot(Predict(bm_delirium_date_therapy_year_ia, date_therapy_year, sex, conf.int = FALSE)) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D")+ theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        bm_delirium_year_p4 <- 
                plot(bm_delirium_date_therapy_year_ia) + theme_ch() + scale_color_viridis(discrete = TRUE, option = "D") + coord_cartesian(expand = TRUE, xlim = c(-2.2, 2.2)) + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        bm_delirium_year_p5 <- 
                pp_check.blrm(bm_delirium_date_therapy_year_ia, type = "violin_grouped", group = "sex", df=df) + coord_cartesian(expand = TRUE) + theme(legend.position = c(0.9, 0.9), plot.margin = unit(c(0.5,0,0,1), "cm"))
        #forest plot
        # p6_temp <- forestplot(summary(bm_delirium_age_ia), return_ggplots = TRUE)
        
        theme_set(theme_pubclean() + theme(text = element_text(color = "black", size = 8)))
        
        bm_delirium_year_p6 <- forestplot(summary(bm_delirium_date_therapy_year_ia))
        bm_delirium_year_p7 <- stanDxplot(bm_delirium_date_therapy_year_ia, which="sex=male") + theme_ch() + coord_cartesian(expand = TRUE, xlim=c(0,1400), ylim=c(-10,25)) + coord_flip() + theme(plot.margin = unit(c(0.5,0,0,0.5), "cm"))
        
        
        # MEDIATION
        # library(brms)
        # library(bayestestR)
        # # collapse therapy into IVTonly and EST +/- IVT for purpose of simpler mediation
        # d <- df %>% mutate(therapy = recode(therapy, "IVT/EST" = "EST", "EST" = "EST")) %>% 
        #         mutate(therapy = droplevels(therapy))
        # d <- recode_df_for_brms(bm_delirium_date_therapy_year_ia, d)
        # 
        # # how much of sex effect on delir is mediated trough therapy
        # f1 <- bf(outcome_delir ~ 1 + sex + therapy + date_therapy_year, family = bernoulli("logit"))
        # f2 <- bf(therapy ~ 1 + sex + date_therapy_year, family = bernoulli("logit"))
        # 
        # brms_delir_sex_therapy <- brm(
        #         formula = mvbrmsformula(f1, f2),
        #         data = d,
        #         set_rescor(FALSE),
        #         cores = 8,
        #         file = "output/models/brms_delir_sex_therapy.rds")
        # 
        # bm_delir_mediate_sex_ther <- bayestestR::mediation(brms_delir_sex_therapy)
        # bm_delir_mediate_sex_ther_prop_med <- attr(bm_delir_mediate_sex_ther, "data")$proportion_mediated
        # # create_dag(bm_delir_mediate_sex_ther)
        # 
        # # how much of sex effect on delir is mediated trough date
        # f1 <- bf(outcome_delir ~ 1 + sex + date_therapy_year + therapy, family = bernoulli("logit"))
        # f3 <- bf(date_therapy_year ~ 1 + sex + therapy, family = gaussian())
        # 
        # brms_delir_sex_year <- brm(
        #         formula = mvbrmsformula(f1, f3),
        #         data = d,
        #         set_rescor(TRUE),
        #         cores = 8,
        #         file = "output/models/brms_delir_sex_year.rds")
        # 
        # bm_delir_mediate_sex_year <- bayestestR::mediation(brms_delir_sex_year)
        # bm_delir_mediate_sex_year_prop_med <- attr(bm_delir_mediate_sex_year, "data")$proportion_mediated
        # 
        # mediation_list <- list(
        #         bm_delir_mediate_sex_ther=bm_delir_mediate_sex_ther, 
        #         bm_delir_mediate_sex_ther_prop_med=bm_delir_mediate_sex_ther_prop_med, 
        #         bm_delir_mediate_sex_year=bm_delir_mediate_sex_year, 
        #         bm_delir_mediate_sex_year_prop_med=bm_delir_mediate_sex_year_prop_med
        # )
        # create_dag(bm_delir_mediate_sex_year)
        
        
        
        # LIST
        res_del_year <-
                list(
                        dv = dv,
                        iv0 = iv0,
                        ia1 = ia1,
                        ia2 = ia2,
                        ia3 = ia3,
                        model = bm_delirium_date_therapy_year,
                        modeladj = bm_delirium_date_therapy_year_adj,
                        tbl = bm_delirium_date_therapy_year_tbl,
                        model_ia = bm_delirium_date_therapy_year_ia,
                        pred = bm_delirium_date_therapy_year_p,
                        pred_fn = bm_delirium_date_therapy_year_footnote,
                        # p0 = bm_delirium_year_p0,
                        p1 = bm_delirium_year_p1,
                        p2 = bm_delirium_year_p2,
                        p3 = bm_delirium_year_p3,
                        p4 = bm_delirium_year_p4,
                        p5 = bm_delirium_year_p5,
                        p6 = bm_delirium_year_p6,
                        p7 = bm_delirium_year_p7
                )
        return(res_del_year)
}