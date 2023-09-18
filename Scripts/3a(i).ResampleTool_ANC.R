# =============================== RESAMPLING SCHEMES FOR SURVIVAL MODELS ===============================
# A tool for investigating subsampling & resampling parameters iteratively
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Default survival modelling
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This ancillary & exploratory script implements a given sample size by first subsampling raw data
# using 2-way stratified sampling before resampling into a basic cross-validation set (training:validation)
# controlled by the sampling fraction, also using the same 2-way stratified sampling design.
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup
#   - 2d.Data_Fusion

# -- Inputs:
#   - datCredit_real | Prepared from script 2f.
#
# -- Outputs:
#   - Event rate graph across resampled set
# ------------------------------------------------------------------------------------------------------




# ------ 1. Preliminaries

# - Load in Dataset
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# - Confidence interval parameter
confLevel <- 0.95

# - Subsampling & resampling parameters
smp_size <- 100000 # fixed size of downsampled set
smp_frac <- 0.7 # sampling fraction for resampling scheme
stratifiers <- c("DefaultStatus1_lead_12_max", "Date") # Must at least include target variable used in graphing event rate


# ------ 2. Subsampled resampling scheme: basic cross-validation with random sampling

# - Preliminaries
smp_perc <- smp_size/datCredit_real[, .N] # Implied sampling fraction for downsampling step

# - Downsample data into a set with a fixed size (using stratified sampling) before implementing resampling scheme
datCredit_smp <- datCredit_real %>% group_by(vars(stratifiers)) %>% slice_sample(prop=smp_perc) %>% as.data.table()
datCredit_smp[, Ind := 1:.N] # prepare for resampling scheme

# - Implement resampling scheme using given main sampling fraction
set.seed(1)
datCredit_train <- datCredit_smp %>% group_by(vars(stratifiers)) %>% slice_sample(prop=smp_frac) %>% mutate(Sample="Train") %>% as.data.table()
datCredit_valid <- subset(datCredit_smp, !(Ind %in% datCredit_train$Ind)) %>% mutate(Sample="Validation") %>% as.data.table()




# ------ 3. Graphing event rates over time given resampled sets

# - Check representatives | dataset-level proportions should be similar
table(datCredit_smp$DefaultStatus1_lead_12_max) %>% prop.table()
table(datCredit_train$DefaultStatus1_lead_12_max) %>% prop.table()
table(datCredit_valid$DefaultStatus1_lead_12_max) %>% prop.table()

# - Merge samples together
datGraph <- rbind(datCredit_real[, list(LoanID, Date, DefaultStatus1, DefaultStatus1_lead_12_max, Sample = "a_Full")],
                   datCredit_train[, list(LoanID, Date, DefaultStatus1, DefaultStatus1_lead_12_max, Sample = "b_Train")],
                   datCredit_valid[, list(LoanID, Date, DefaultStatus1, DefaultStatus1_lead_12_max, Sample = "c_Valid")])

# - Setting some aggregation parameters, purely to facilitate graphing aesthetics
def_StartDte <- min(datCredit_real$Date, na.rm=T)
def_EndDte <- max(datCredit_real$Date, na.rm=T)
maxDate <- def_EndDte - years(1) # A post-hoc filter, used for graphing purposes

# - Aggregate to monthly level and observe up to given point
port.aggr <- datGraph[DefaultStatus1==0, list(EventRate = sum(DefaultStatus1_lead_12_max, na.rm=T)/.N, AtRisk = .N),
                           by=list(Sample, Date)][Date >= def_StartDte & Date <= maxDate,] %>% setkey(Sample,Date)

# - Aesthetics engineering
port.aggr[, Facet_label := "Worst-ever aggregation approach"]

# - calculate TTC event rate and confidence interval for one sample, dichotomous outcome (population proportion)
mean_EventRate <- port.aggr[Sample == "b_Train", mean(EventRate, na.rm=T)]
stdError_EventRate <- port.aggr[Sample == "b_Train", sd(EventRate, na.rm=T)] / sqrt(port.aggr[Sample == "b_Train", .N])
margin_EventRate <- qnorm(1-(1-confLevel)/2) * stdError_EventRate
cat("\nMean event rate with 95% confidence intervals in training sample: ", sprintf("%.2f", mean_EventRate*100) , "% +-", sprintf("%.3f", margin_EventRate*100), "%")

# - Calculate MAE over time by sample
port.aggr2 <- port.aggr %>% pivot_wider(id_cols = c(Date), names_from = c(Sample), values_from = c(EventRate))
(diag.samplingRep.train <- mean(abs(port.aggr2$a_Full - port.aggr2$b_Train)) * 100)
(diag.samplingRep.valid <- mean(abs(port.aggr2$a_Full - port.aggr2$c_Valid)) * 100)
(diag.samplingRep.trainValid <- mean(abs(port.aggr2$b_Train - port.aggr2$c_Valid)) * 100)
### RESULTS: Sample-size dependent
# 100k-sample: Train: 0.74%; Validation: 0.98%

# - Graphing parameters
chosenFont <- "Cambria"; dpi <- 170
col.v <- brewer.pal(9, "Set1")[c(1,5,2,4)]; size.v <- c(0.5,0.3,0.3,0.3)
label.v <- c("a_Full"=expression(italic(A)[t]*": Full set "*italic(D)),
             "b_Train"=bquote(italic(C)[t]*": Training set "*italic(D)[italic(T)]~"("*.(round(smp_frac*smp_size/1000))*"k)"),
             "c_Valid"=bquote(italic(D)[t]*": Validation set "*italic(D)[italic(V)]~"("*.(round((1-smp_frac)*smp_size/1000))*"k)"))

# - Create graph 1 (all sets)
(g2 <- ggplot(port.aggr, aes(x=Date, y=EventRate, group=Sample)) + theme_minimal() + 
    labs(x="Reporting date (months)", y=bquote("Conditional 12-month default rate (%) across sample "*italic(bar(D)))) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom",
          axis.text.x=element_text(angle=90), #legend.text=element_text(family=chosenFont), 
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # main line graph with overlaid points
    geom_line(aes(colour=Sample, linetype=Sample, linewidth=Sample)) + 
    geom_point(aes(colour=Sample, shape=Sample), size=1) + 
    #annotations
    annotate("text", x=as.Date("2013-02-28"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*1.5, size=3, family=chosenFont,
             label=paste0("'TTC-mean '*E(italic(B[t]))*': ", sprintf("%.3f", mean_EventRate*100), "% ± ", 
                          sprintf("%.3f", margin_EventRate*100),"%'"), parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*1.35,
             label=paste0("'MAE between '*italic(A)[t]*' and '*italic(B)[t]*': ", sprintf("%.3f", diag.samplingRep.train),"%'"),
             family=chosenFont, size=3, parse=T) +     
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*1.275,
             label=paste0("'MAE between '*italic(A)[t]*' and '*italic(C)[t]*': ", sprintf("%.3f", diag.samplingRep.valid),"%'"),
             family=chosenFont, size=3, parse=T) +      
    annotate(geom="text", x=as.Date("2012-12-31"), y=port.aggr[Date <= "2008-12-31", mean(EventRate)]*1.2,
             label=paste0("'MAE between '*italic(B)[t]*' and '*italic(C)[t]*': ", sprintf("%.3f", diag.samplingRep.trainValid),"%'"),
             family=chosenFont, size=3, parse=T) +     
    # facets & scale options
    facet_grid(Facet_label ~ .) + 
    scale_colour_manual(name=bquote("Sample "*italic(bar(D))), values=col.v, labels=label.v) + 
    scale_linewidth_manual(name=bquote("Sample "*italic(bar(D))), values=size.v, labels=label.v) + 
    scale_shape_discrete(name=bquote("Sample "*italic(bar(D))), labels=label.v) + scale_linetype_discrete(name=bquote("Sample "*italic(bar(D))), labels=label.v) + 
    scale_y_continuous(breaks=pretty_breaks(), label=percent) + 
    scale_x_date(date_breaks=paste0(6, " month"), date_labels = "%b %Y"))

# - Save graph
ggsave(g2, file=paste0(genFigPath, "DefaultRates_SampleRates.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")



# --- Cleanup
suppressWarnings(rm(port.aggr, port.aggr2, datGraph, datCredit_real, datCredit_smp, datCredit_train, datCredit_valid, g2))