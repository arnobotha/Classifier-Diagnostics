# =================================== MODEL SEGMENTATION COMPARISON ==========================================
# Exploring the use of segmentation schemes in predicting the occurrence of default.
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller

# DESCRIPTION:
# This script explores segmentation schemes in predicting the occurrence of default. Segmentation is done
# using the performing leves of [g0_Delinq], i.e., 0-, 1-, and 2-months in arrears.
# ------------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 0a.CustomFunctions.R
#   - 3b.Data_Subsample_Fusion2
#
# -- Inputs:
#   - datCredit_smp | Prepared credit data from script 3b
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#
# -- Outputs:
#   - Some graphs illustrating the differences in embedding delinquency endogenously vs exogenously
# ============================================================================================================




# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_smp')) unpack.ffdf(paste0(genPath,"creditdata_smp"), tempPath)
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells
datCredit_smp <- datCredit_smp %>% subset(DefaultStatus1==0)
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)

# - Load in basic model formula
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)

# - Confidence interval parameter
confLevel <- 0.95




# ------ 2. 12-Month default rate comparisons per segment
# ---- 2.1 12-Month Conditional Default Rate per Segment
# --- Quick analysis of the proportions of the segments
(g0_Delinq_Prop <- datCredit_smp[,g0_Delinq] %>% table() %>% prop.table())

# --- Aggregating the data over the entire sampling window
# - Aggregating over delinquency segment
port.aggr_def_full <- datCredit_smp[DefaultStatus1==0, list(EventRate = sum(DefaultStatus1_lead_12_max==1)/.N), by=list(Date)]
port.aggr_def_full[,g0_Delinq:="Overall"]
# - Aggregating per delinquency segment
port.aggr_def_del <- datCredit_smp[DefaultStatus1==0, list(EventRate = sum(DefaultStatus1_lead_12_max==1)/.N), by=list(Date, g0_Delinq)]

# --- Combining the two aggregation datasets
# - Combining the datasets
port.aggr_def <- rbind(port.aggr_def_full, port.aggr_def_del)
# - Subsetting to exclude the large 12 months since aggregation is not meaningful for this period
port.aggr_def <- subset(port.aggr_def, Date <= max(port.aggr_def$Date)-months(12))
# - Facotrising [g0_Delinq] to facilitate graphing
port.aggr_def_del[, g0_Delinq:=factor(g0_Delinq)]

# --- Clean up
rm(port.aggr_def_full, port.aggr_def_del); gc()

# --- Create annotations for annotation dataset
# - [g0_Delinq] = Full (full dataset)
mean_EventRate_Full <- mean(port.aggr_def[g0_Delinq=="Overall", EventRate], na.rm=T)
stdError_EventRate_Full <- port.aggr_def[g0_Delinq=="Overall", sd(EventRate, na.rm=T)] / sqrt(port.aggr_def[g0_Delinq=="Overall", .N])
# - [g0_Delinq] = 0
mean_EventRate_0 <- mean(port.aggr_def[g0_Delinq==0, EventRate], na.rm=T)
stdError_EventRate_0 <- port.aggr_def[g0_Delinq == 0, sd(EventRate, na.rm=T)] / sqrt(port.aggr_def[g0_Delinq == 0, .N])
# - [g0_Delinq] = 1
mean_EventRate_1 <- mean(port.aggr_def[g0_Delinq==1, EventRate], na.rm=T)
stdError_EventRate_1 <- port.aggr_def[g0_Delinq == 1, sd(EventRate, na.rm=T)] / sqrt(port.aggr_def[g0_Delinq == 1, .N])
# - [g0_Delinq] = 2
mean_EventRate_2 <- mean(port.aggr_def[g0_Delinq==2, EventRate], na.rm=T)
stdError_EventRate_2 <- port.aggr_def[g0_Delinq == 2, sd(EventRate, na.rm=T)] / sqrt(port.aggr_def[g0_Delinq == 2, .N])

# --- Annotation dataset
datAnno <- data.table(g0_Delinq=c("Overall","0","1","2"),
                      g0_Delinq_prop=c(100,sprintf("%1.2f",g0_Delinq_Prop*100)),
                      Mean=c(mean_EventRate_Full, mean_EventRate_0, mean_EventRate_1, mean_EventRate_2),
                      stdError=c(stdError_EventRate_Full, stdError_EventRate_0, stdError_EventRate_1, stdError_EventRate_2))
datAnno[, Mean:=sprintf("%1.2f",Mean*100)]
datAnno[, Margin:=sprintf("%1.3f", qnorm(1-(1-confLevel)/2) * stdError)]
datAnno[g0_Delinq=="Overall", Label:=paste0("'Overall: TTC-mean = ", Mean, "% ± ",Margin,"%'")]
datAnno[g0_Delinq=="0", Label:=paste0("italic(g[0])* '= 0 (", g0_Delinq_prop , "%): TTC-mean = ", Mean, "% ± ",Margin,"%'")]
datAnno[g0_Delinq=="1", Label:=paste0("italic(g[0])* '= 1 (", g0_Delinq_prop , "%): TTC-mean = ", Mean, "% ± ",Margin,"%'")]
datAnno[g0_Delinq=="2", Label:=paste0("italic(g[0])* '= 2 (", g0_Delinq_prop , "%): TTC-mean = ", Mean, "% ± ",Margin,"%'")]
datAnno[,`:=`(x=rep(as.Date("2015-02-28"),4), y=as.numeric(Mean)/100)]; datAnno[,y:=datAnno$y+c(0.03,-0.05,0.12,0.2)]
# dat_anno[1:3, Label := paste0(Label = "'MAE between '*italic(A[t])*' and '*italic(B[t])*'", " = ", sprintf("%.4f",MAE*100), "%'")]

# --- Creating the event rate plot
# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 360
col.v <- brewer.pal(9, "Set1")[c(7,5,2,1)]
# - Create graph
(g_EventRate_g0_Delinq <- ggplot(port.aggr_def, aes(x=Date, y=EventRate, group=g0_Delinq)) + theme_minimal() + 
    labs(x="Reporting date (months)", y=bquote("Conditional 12-month default rate (%)")) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=g0_Delinq, linetype=g0_Delinq)) + 
    geom_point(aes(colour=g0_Delinq, shape=g0_Delinq), size=0.3) + 
    #annotations
    geom_text(data=datAnno, aes(x=x, y=y, label = Label), family=chosenFont, size=3, parse=T) + 
    # geom_text(data=dat_anno, aes(x=x, y=y, hjust=hjust, vjust=vjust, label = Label), family=chosenFont, size=3, parse=T) + 
    # facets & scale options
    scale_colour_manual(name=bquote(italic(g[0])*":"), values=col.v) + 
    scale_shape_discrete(name=bquote(italic(g[0])*":")) +
    scale_linetype_discrete(name=bquote(italic(g[0])*":")) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
# - Save graph
ggsave(g_EventRate_g0_Delinq, file=paste0(genFigPath, "DefaultRates_g0_Delinq_Compare.png"), width=2400/dpi, height=2000/dpi, dpi=dpi, bg="white")

# --- Clean up
rm(port.aggr_def, mean_EventRate_1, mean_EventRate_1, mean_EventRate_2, mean_EventRate_Full,
   stdError_EventRate_0, stdError_EventRate_1, stdError_EventRate_2, stdError_EventRate_Full,
   margin_EventRate_0, margin_EventRate_1, margin_EventRate_2, margin_EventRate_Full, col.v,
   datAnno, g_EventRate_g0_Delinq); gc()


# ---- 2.2 Distributional Analysis of [g0_Delinq] over the sampling window
# --- Aggregating the data to the delinquency level over the sampling window
# - Aggregating by delinquency level over the sampling window
port.aggr_def_del_time <- datCredit_train[DefaultStatus1==0,.N,by=list(DefaultStatus1_lead_12_max,g0_Delinq,Date)]
# - Adding a column for the sum of total observations in each date
port.aggr_def_del_time[,Total_N:=sum(N),by=Date]
# - Getting the proportion of observations in each delinquency level at each date
port.aggr_def_del_time[,Prop:=N/Total_N]
# - Factorising [g0_Delinq] to facilitate graphing
port.aggr_def_del_time[,g0_Delinq:=factor(g0_Delinq)]
# - Factorising [DefaultStatus1_lead_12_max] to facilitate graphing
port.aggr_def_del_time[,DefaultStatus1_lead_12_max:=factor(DefaultStatus1_lead_12_max)]
# - Create summaries for annotations within graph
port.aggr_def_del_time[,Facet:=factor(g0_Delinq, labels=c(paste0("italic(g[0])*'=0 (",sprintf( "%1.2f",g0_Delinq_Prop[1]*100),"%)'"),
                                                          paste0("italic(g[0])*'=1 (",sprintf( "%1.2f",g0_Delinq_Prop[2]*100),"%)'"),
                                                          paste0("italic(g[0])*'=2 (",sprintf( "%1.2f",g0_Delinq_Prop[3]*100),"%)'")))]

# --- Plot
# - Annotations for facets
# Statistics of the aggregated datset
datStrata_aggr <- port.aggr_def_del_time[,list(Count_Strata=.N, Mean_Strata=mean(N,na.rm=T), SD_Strata=sd(N,na.rm=T), Min_Strata=min(N,na.rm=T)), by=list(DefaultStatus1_lead_12_max)]
datStrata_aggr[, Margin_Strata := qnorm(1-(1-confLevel)/2) * SD_Strata / sqrt(Count_Strata)]

# Annotation dataset
datAnno <- data.table(g0_Delinq=factor(c(0,1,2)),
                      Facet=factor(c(paste0("italic(g[0])*'=0 (",sprintf( "%1.2f",g0_Delinq_Prop[1]*100),"%)'"),
                                     paste0("italic(g[0])*'=1 (",sprintf( "%1.2f",g0_Delinq_Prop[2]*100),"%)'"),
                                     paste0("italic(g[0])*'=2 (",sprintf( "%1.2f",g0_Delinq_Prop[3]*100),"%)'"))),
                      Label=c(paste0("'", datStrata_aggr$Count_Strata[1], " total 12-month non-default strata with a mean cell size of ",
                                     comma(datStrata_aggr$Mean_Strata[1], accuracy=0.1),
                                     " ± ", sprintf("%.1f", datStrata_aggr$Margin_Strata[1]), " and a minimum size of ",
                                     sprintf("%.0f", datStrata_aggr$Min_Strata[1]),"'"),
                              paste0("'", datStrata_aggr$Count_Strata[2], " total 12-month default strata with a mean cell size of ",
                                     comma(datStrata_aggr$Mean_Strata[2], accuracy=0.1),
                                     " ± ", sprintf("%.1f", datStrata_aggr$Margin_Strata[2]), " and a minimum size of ",
                                     sprintf("%.0f", datStrata_aggr$Min_Strata[2]),"'")),
                      x=rep(date("2015-02-28"),6),
                      y=c(0, datStrata_aggr$Mean_Strata[1]*0.35, 0, 0, datStrata_aggr$Mean_Strata[1]*0.3, 0))
datAnno[c(1,3,4,6),Label:=NA]
# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 240
col.v <- rep(brewer.pal(8, "Dark2")[c(1,3)],3)
fill.v <- rep(brewer.pal(8, "Set2")[c(1,3)],3)

# - Create graph to evidence minimum strata sizes
(g_distribution_g0 <- ggplot(port.aggr_def_del_time, aes(x=Date, y=N)) + theme_minimal() + 
    labs(x=bquote("Reporting date (months) "*italic(t)), y=bquote("Volume of segmenter ("*italic(g[0])*")")) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main area graph
    geom_col(aes(col=DefaultStatus1_lead_12_max, fill=DefaultStatus1_lead_12_max), position="identity", alpha=c(0.5)) +
    # facet
    facet_wrap(.~Facet, scales = "free_y", strip.position = "right", ncol=1, nrow=3, labeller=label_parsed) +  # label_bquote(italic(g[0])*~"="~.(g0_Delinq))) + # labeller
    # annotations
    geom_text(data=datAnno, aes(x=x, y=y, label = Label), family=chosenFont, size=3, parse=T) + 
    # scale options
    scale_colour_manual(name=bquote(italic(g[0])~":"), values=col.v, guide="none") + 
    scale_fill_manual(name=bquote(italic(g[0])~":"), values=fill.v, guide="none") + 
    scale_y_continuous(breaks=pretty_breaks(), label=comma) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))
# - Save graph
ggsave(g_distribution_g0, file=paste0(genFigPath, "g0_Delinq_Distribution.png"), width=2400/dpi, height=1000/dpi, dpi=dpi, bg="white")




# ------ 3. Modelling per [g0_Delinq] segment: Null models
# --- Preparing the prediction column
datCredit_valid[, prob_g0_seg_Null := NA]; datCredit_valid[, prob_g0_seg_Null := as.numeric(prob_g0_seg_Null)]

# --- Model for delinquency segment 0
# - Fitting the model for delinquency segment 0
logitMod_g0_Delinq_0_Null <- glm(DefaultStatus1_lead_12_max ~ 1, data=datCredit_train[g0_Delinq==0,], family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Delinq_0_Null)
### RESULTS: Insignificant variables: None
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==0, prob_g0_seg_Null := predict(logitMod_g0_Delinq_0_Null, newdata = datCredit_valid[g0_Delinq==0,], type="response")]
# [SANITY CHECK] - Ensuring that the probabilities were correctly assigned
cat( (datCredit_valid[g0_Delinq==0,.N]==datCredit_valid[,.N]-datCredit_valid[is.na(prob_g0_seg_Null),.N]) %?%
       paste0('SAFE: ', datCredit_valid[,.N]-datCredit_valid[is.na(prob_g0_seg_Null),.N], " observations have been assigned predictions for the ", datCredit_valid[g0_Delinq==0,.N], " number of observations with g0_Delinq==0") %:% 
       'WARNING: Failed to assign the correct number of predictions.\n')
# AUC value
auc(datCredit_valid[g0_Delinq==0,DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==0,prob_g0_seg_Null])
### RESTULS: 50%

# --- Model for delinquency segment 1
# - Fitting the model for delinquency segment 1
logitMod_g0_Delinq_1_Null <- glm(DefaultStatus1_lead_12_max ~ 1, data=datCredit_train[g0_Delinq==1,], family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Delinq_1_Null)
### RESULTS: Insignificant variables: None
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==1, prob_g0_seg_Null := predict(logitMod_g0_Delinq_1_Null, newdata = datCredit_valid[g0_Delinq==1,], type="response")]
# [SANITY CHECK] - Ensuring that the probabilities were correctly assigned
cat( (datCredit_valid[g0_Delinq %in% c(0,1),.N]==datCredit_valid[,.N]-datCredit_valid[is.na(prob_g0_seg_Null),.N]) %?%
       paste0('SAFE: ', datCredit_valid[,.N]-datCredit_valid[is.na(prob_g0_seg_Null),.N], " observations have been assigned predictions for the ", datCredit_valid[g0_Delinq %in% c(0,1),.N], " number of observations with g0_Delinq==0/1") %:% 
       'WARNING: Failed to assign the correct number of predictions.\n')
# AUC Value
auc(datCredit_valid[g0_Delinq==1,DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==1,prob_g0_seg_Null])
### RESTULS: 50%

# --- Model for delinquency segment 2
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_2_Null <- glm(DefaultStatus1_lead_12_max ~ 1, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Delinq_2_Null)
### RESULTS: Insignificant variables: None
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==2, prob_g0_seg_Null := predict(logitMod_g0_Delinq_2_Null, newdata = datCredit_valid[g0_Delinq==2,], type="response")]
# [SANITY CHECK] - Ensuring that the probabilities were correctly assigned
cat( (datCredit_valid[g0_Delinq %in% c(0,1,2),.N]==datCredit_valid[,.N]-datCredit_valid[is.na(prob_g0_seg_Null),.N]) %?%
       paste0('SAFE: ', datCredit_valid[,.N]-datCredit_valid[is.na(prob_g0_seg_Null),.N], " observations have been assigned predictions for the ", datCredit_valid[g0_Delinq %in% c(0,1,2),.N], " number of observations with g0_Delinq==0/1/2") %:% 
       'WARNING: Failed to assign the correct number of predictions.\n')
# AUC value
auc(datCredit_valid[g0_Delinq==2,DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==2,prob_g0_seg_Null])
### RESTULS: 50%

# --- Model for overall delinquency
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_Full_Null <- glm(DefaultStatus1_lead_12_max ~ 1, data=datCredit_train, family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Delinq_Full_Null)
### RESULTS: Insignificant variables: None
# - ROC analysis
# Get predictions
datCredit_valid[, prob_g0_Full_Null := predict(logitMod_g0_Delinq_Full_Null, newdata = datCredit_valid, type="response")]
# AUC value
auc(datCredit_valid[,DefaultStatus1_lead_12_max], datCredit_valid[,prob_g0_Full_Null])
### RESTULS: 50%

# --- Comparison
### The segmented- and full/overall models have no predictive power as they result in AUCs of 50%
### Propogate goodness-of-fit into above section

# --- Clean up
datCredit_valid[,prob_g0_seg_Null:=NULL]
rm(logitMod_g0_Delinq_0_Null, logitMod_g0_Delinq_1_Null, logitMod_g0_Delinq_2_Null, logitMod_g0_Delinq_Full_Null); gc()




# ------ 4. Modelling per [g0_Delinq] segment: Fixed/constant input space
# --- Preparing the prediction column
datCredit_valid[, prob_g0_seg := NA]; datCredit_valid[, prob_g0_seg := as.numeric(prob_g0_seg)]

# --- Model for delinquency segment 0
# - Fitting the model for delinquency segment 0
logitMod_g0_Delinq_0 <- glm(inputs_fin_bas, data=datCredit_train[g0_Delinq==0,], family="binomial")
# - Deviance and AIC
summary(logitMod_g0_Delinq_0)
### RESULTS: Insignificant variables: None
# Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_0)
### RESTULS: 1.59%
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==0, prob_g0_seg := predict(logitMod_g0_Delinq_0, newdata = datCredit_valid[g0_Delinq==0,], type="response")]
# AUC value
auc(datCredit_valid[g0_Delinq==0,DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==0,prob_g0_seg])
### RESTULS: 62.06%

# --- Model for delinquency segment 1
# - Fitting the model for delinquency segment 1
logitMod_g0_Delinq_1 <- glm(inputs_fin_bas, data=datCredit_train[g0_Delinq==1,], family="binomial")
# - Model analysis
# Deviance and AIC
summary(logitMod_g0_Delinq_1)
### RESULTS: Insignificant variables: [AgeToTerm], [Balance]
# Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_1)
### RESTULS: 0.26%
# ROC analysis
datCredit_valid[g0_Delinq==1, prob_g0_seg := predict(logitMod_g0_Delinq_1, newdata = datCredit_valid[g0_Delinq==1,], type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_seg)
### RESTULS: 76.44%
### AB: Investigate the use of subsetting in assigning probabilities per segment. Remove condition for g0_Delinq==1 and use an ifelse() and assign NA to where g0_Delinq!=1
###     Create a one line unit test to ensure that all predicted probabilities created in this step (and the previous one) exists and the probabilities for the next steps are missing

# --- Model for delinquency segment 2
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_2 <- glm(inputs_fin_bas, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_2)
### RESULTS: Insignificant variables: [AgeToTerm], [Term], [Balance], [Principal], [InterestRate_Margin_imputed_mean]
# Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_2)
### RESTULS: 0.10%
# ROC analysis
datCredit_valid[g0_Delinq==2, prob_g0_seg := predict(logitMod_g0_Delinq_2, newdata = datCredit_valid[g0_Delinq==2,], type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_seg)
### RESTULS: 79.64%

# --- Combined predictions assessment (ensemble approach)
### Combine the predictions of the segments together and then assess the
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_seg)
### RESULTS: 79.64%




# ------ 5. Modelling overall [g0_Delinq] segments: Fixed/constant input space
# --- Model over all segments (model by input)
# - Amending the basic model formula to include delinquency levels
inputs_fin_bas_del <- as.formula(paste0("DefaultStatus1_lead_12_max ~", paste0(c(labels(terms(inputs_fin_bas)), "g0_Delinq"), collapse = "+")))
# - Fitting the model for delinquency segment 0
logitMod_g0_Delinq_Full <- glm(inputs_fin_bas_del, data=datCredit_train, family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_Full)
### RESULTS: Insignificant variables: [Term]
# Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_Full)
### RESTULS: 22.13%
# Overall ROC analysis
datCredit_valid[, prob_full := predict(logitMod_g0_Delinq_Full, newdata = datCredit_valid, type="response")]
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_full)
### RESULTS: Overall: 79.55%




# ------ 6. Comparison of segmented- vs full model : Fixed/constant input space
### Segmented Model: [g0_Delinq]=0: AUC = 62.06% | Coef. of Deter. = 1.59%
###                  [g0_Delinq]=1: AUC = 76.44% | Coef. of Deter. = 0.26%
###                  [g0_Delinq]=2: AUC = 79.64% | Coef. of Deter. = 0.10%
###                  Overall      : AUC = 79.64% | Coef. of Deter. = NA
### Full Model:      [g0_Delinq]=0: AUC = 62.01% | Coef. of Deter. = NA
###                  [g0_Delinq]=1: AUC = 00.61% | Coef. of Deter. = NA
###                  [g0_Delinq]=2: AUC = 52.30% | Coef. of Deter. = NA
###                  Overall      : AUC = 79.55% | Coef. of Deter. = 22.13%




# ------ 7. Modelling per [g0_Delinq] segment: Bespoke input space per segment
# --- 7.0 Preparing the prediction column
datCredit_valid[, prob_g0_seg_b := NA]; datCredit_valid[, prob_g0_seg_b := as.numeric(prob_g0_seg_b)]


# ---- 7.1 [g0_Delinq] = 0
# --- 7.1.1 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_0_b <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_adv))[!(labels(terms(inputs_adv)) %in% "g0_Delinq")], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_0_b <- glm(inputs_g0_Delinq_0_b, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_0_b)
### RESULTS: Insignificant variables: [Term], [Principal], [InterestRate_Margin_imputed_mean], [M_DTI_Growth_6 ], [M_DTI_Growth_9], [slc_acct_roll_ever_24_imputed_mean],
###                                   [slc_acct_arr_dir_3], [slc_past_due_amt_imputed_med], [slc_acct_pre_lim_perc_imputed_med], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_5]
### CONCLUSION: Remove insignificant variables sand refit model

# --- 7.1.2 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_0_b2 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_g0_Delinq_0_b))[!(labels(terms(inputs_g0_Delinq_0_b)) %in%
                                                                                                                           c("Term", "Principal", "InterestRate_Margin_imputed_mean", "M_DTI_Growth_6", "M_DTI_Growth_9", "slc_acct_roll_ever_24_imputed_mean",
                                                                                                                             "slc_acct_arr_dir_3", "slc_past_due_amt_imputed_med", "slc_acct_pre_lim_perc_imputed_med", "NewLoans_Aggr_Prop_3", "NewLoans_Aggr_Prop_5"))], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_0_b2 <- glm(inputs_g0_Delinq_0_b2, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_0_b2)
### RESULTS: Insignificant variables: [M_Emp_Growth_9], [M_RealGDP_Growth_12]
### CONCLUSION: Remove insignificant variables sand refit model

# --- 7.1.3 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_0_b3 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_g0_Delinq_0_b2))[!(labels(terms(inputs_g0_Delinq_0_b2)) %in%
                                                                                                                            c("M_Emp_Growth_9", "M_RealGDP_Growth_12"))], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_0_b3 <- glm(inputs_g0_Delinq_0_b3, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_0_b3)
### RESULTS: Insignificant variables: None
### CONCLUSION: Proceed with model analysis

# --- 7.1.4 Model analysis
# - Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_0_b3)
### RESTULS: 8.08%
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==0, prob_g0_seg_b := predict(logitMod_g0_Delinq_0_b3, newdata = datCredit_valid[g0_Delinq==0,], type="response")]
# AUC value
auc(datCredit_valid[g0_Delinq==0,DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==0,prob_g0_seg_b])
### RESTULS: 79.88%

# --- 7.1.5 Clean up
rm(inputs_g0_Delinq_0_b, inputs_g0_Delinq_0_b2, logitMod_g0_Delinq_0_b, logitMod_g0_Delinq_0_b2); gc()



# ---- 7.2 [g0_Delinq] = 1
# --- 7.2.1 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_1_b <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_adv))[!(labels(terms(inputs_adv)) %in% "g0_Delinq")], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_1_b <- glm(inputs_g0_Delinq_1_b, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_1_b)
### RESULTS: Insignificant variables: [Term], [Principal], [InterestRate_Margin_imputed_mean], [M_DTI_Growth_6], [slc_acct_roll_ever_24_imputed_mean], [slc_acct_arr_dir_3],
###                                   [slc_acct_pre_lim_perc_imputed_med], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_5]
### CONCLUSION: Remove insignificant variables sand refit model

# --- 7.2.2 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_1_b2 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_g0_Delinq_1_b))[!(labels(terms(inputs_g0_Delinq_1_b)) %in%
                                                                                                                           c("Term", "Principal", "InterestRate_Margin_imputed_mean", "M_DTI_Growth_6", "slc_acct_roll_ever_24_imputed_mean", "slc_acct_arr_dir_3",
                                                                                                                             "slc_acct_pre_lim_perc_imputed_med", "NewLoans_Aggr_Prop_3", "NewLoans_Aggr_Prop_5"))], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_1_b2 <- glm(inputs_g0_Delinq_1_b2, data=datCredit_train[g0_Delinq==1,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_1_b2)
### RESULTS: Insignificant variables: None
### CONCLUSION: Proceed with model analysis

# --- 7.2.3 Model analysis
# - Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_1_b2)
### RESTULS: 10.95%
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==1, prob_g0_seg_b := predict(logitMod_g0_Delinq_1_b2, newdata = datCredit_valid[g0_Delinq==1,], type="response")]
# AUC value
auc(datCredit_valid[g0_Delinq==1, DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==1, prob_g0_seg_b])
### RESTULS: 72.38%

# --- 7.2.4 Clean up
rm(inputs_g0_Delinq_1_b, logitMod_g0_Delinq_1_b); gc()



# ---- 7.3 [g0_Delinq] = 2
# --- 7.3.1 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_2_b <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_adv))[!(labels(terms(inputs_adv)) %in% "g0_Delinq")], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_2_b <- glm(inputs_g0_Delinq_2_b, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_2_b)
### RESULTS: Insignificant variables: [Term], [Principal], [InterestRate_Margin_imputed_mean], [M_DTI_Growth_6], [M_DTI_Growth_9], [slc_acct_roll_ever_24_imputed_mean], [slc_acct_arr_dir_3],
###                                   [slc_acct_pre_lim_perc_imputed_med], [NewLoans_Aggr_Prop_3], [NewLoans_Aggr_Prop_5]
### CONCLUSION: Remove insignificant variables sand refit model

# --- 7.3.2 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_2_b2 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_g0_Delinq_2_b))[!(labels(terms(inputs_g0_Delinq_2_b)) %in%
                                                                                                                           c("Term", "Principal", "InterestRate_Margin_imputed_mean", "M_DTI_Growth_6", "M_DTI_Growth_9", "slc_acct_roll_ever_24_imputed_mean", "slc_acct_arr_dir_3",
                                                                                                                             "slc_acct_pre_lim_perc_imputed_med", "NewLoans_Aggr_Prop_3", "NewLoans_Aggr_Prop_5"))], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_2_b2 <- glm(inputs_g0_Delinq_2_b2, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_2_b2)
### RESULTS: Insignificant variables: [Balance], [M_Emp_Growth_9], [M_RealGDP_Growth_12]
### CONCLUSION: Remove insignificant variables sand refit model

# --- 7.3.3 Dynamic variable selection
# - Adjusting the input space
inputs_g0_Delinq_2_b3 <- as.formula(paste0("DefaultStatus1_lead_12_max ~ ", paste0(labels(terms(inputs_g0_Delinq_2_b2))[!(labels(terms(inputs_g0_Delinq_2_b2)) %in%
                                                                                                                            c("Balance", "M_Emp_Growth_9", "M_RealGDP_Growth_12"))], collapse = "+")))
# - Fitting the model for delinquency segment 2
logitMod_g0_Delinq_2_b3 <- glm(inputs_g0_Delinq_2_b3, data=datCredit_train[g0_Delinq==2,], family="binomial")
# - Model analysis
summary(logitMod_g0_Delinq_2_b3)
### RESULTS: Insignificant variables: None
### CONCLUSION: Proceed with model analysis

# --- 7.3.4 Model analysis
# - Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_2_b3)
### RESTULS: 8.20%
# - ROC analysis
# Get predictions
datCredit_valid[g0_Delinq==2, prob_g0_seg_b := predict(logitMod_g0_Delinq_2_b3, newdata = datCredit_valid[g0_Delinq==2,], type="response")]
# AUC value
auc(datCredit_valid[g0_Delinq==2, DefaultStatus1_lead_12_max], datCredit_valid[g0_Delinq==2, prob_g0_seg_b])
### RESTULS: 68.08%

# --- 7.3.5 Clean up
rm(inputs_g0_Delinq_2_b, inputs_g0_Delinq_2_b2, logitMod_g0_Delinq_2_b, logitMod_g0_Delinq_2_b2); gc()



# ---- 7.4 Combined ROC analysis (ensemble approach)
### Combine the predictions of the segments together and then assess the
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_g0_seg_b)
### RESULTS: 73.65%




# ------ 8. Modelling overall [g0_Delinq] segments: Bespoke input space per segment
# --- Model over all segments (model by input)
# - Amending the basic model formula to exclude delinquency levels
inputs_fin_adv_delb <- as.formula(paste0("DefaultStatus1_lead_12_max ~", paste0(c(labels(terms(inputs_adv)), "g0_Delinq"), collapse = "+")))
# - Fitting the model for delinquency segment 0
logitMod_g0_Delinq_Fullb <- glm(inputs_fin_adv_delb, data=datCredit_train, family="binomial")
# - Model analysis
# Deviance and AIC
summary(logitMod_g0_Delinq_Fullb)
### RESULTS: Insignificant variables: [NewLoans_Aggr_Prop_3] - P-value is ~ 11%, but the variables is nevertheless kept in the model
# Coefficient of determination (McFadden)
coefDeter_glm(logitMod_g0_Delinq_Fullb)
### RESTULS: 34.17%
# - Overall ROC analysis
# Get predictions
datCredit_valid[, prob_fullb := predict(logitMod_g0_Delinq_Fullb, newdata = datCredit_valid, type="response")]
# AUC value
auc(datCredit_valid$DefaultStatus1_lead_12_max, datCredit_valid$prob_fullb)
### RESULTS: Overall: 90.44%




# ------ 9. Comparison of segmented- vs full model : Bespoke input space per segment
### Segmented Model: Overall      : AUC = 73.65% | Coef. of Deter. = NA
### Full Model:      Overall      : AUC = 90.44% | Coef. of Deter. = 34.17%


