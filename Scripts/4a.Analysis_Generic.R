# ============================================ GENRIC ANALYSES =========================================
# High-level exploratory analysis on certain diverse aspects
# ------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha (AB)
# ------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R

# -- Inputs:
#   - datCredit_real | Prepared from script 2f.
#
# -- Outputs:
#   - <insight>
# ------------------------------------------------------------------------------------------------------


# ----------------- 1. Max spell number
# NOTE: To facilitate Bernard's MSc-project from 2024

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4a"), tempPath)

# lookup
lookup <- subset(datCredit_real, LoanID == datCredit_real[PerfSpell_Num == 8 & PerfSpell_Counter == 1, LoanID][1] )

# - Aggregation to account-level
# NOTE: Assign max conditionally since there are loans that are forever in default and hence will have no 
# information on performing spells
datAggr <- datCredit_real[, list(MaxPerfNum = ifelse(all(is.na(PerfSpell_Num)), 0, 
                                   max(PerfSpell_Num, na.rm=T)) ), by=list(LoanID)]

# - Analysis on Maximum performing spell number
describe(datAggr$MaxPerfNum); hist(datAggr$MaxPerfNum)
### RESULTS: Mean of 1.092 max spells (median: 1), with 5%-95% at [1, 2]. Large outliers of up to 10 spells

# - Rebin
datAggr[, MaxPerfNum_Binned := ifelse(MaxPerfNum >= 5, 5, MaxPerfNum)]
describe(datAggr$MaxPerfNum_Binned); hist(datAggr$MaxPerfNum_Binned)
### REUSLTS: Mean of 1.091 max spells. 1 spell: 92.6%; 2 spells: 5%; 3 spells: 1.3%; 4 Spells: 0.4%; 5+ spells: 0.2%
# Note: rebinning at 4 cap also tried, though this resulted in a mean of 1.89, which is too far from 'true' mean of 0.92,
# at least anecdotally. However, when subsampling, this binning scheme may need to be revisited to allow feasible sample sizes

# - Aesthetic engineering
chosenFont <- "Cambria"; dpi <- 200; colPalette <- "BrBG"
datAggr[MaxPerfNum_Binned > 0, MaxPerfNum_Binned_Total := .N]
totFreq <- datAggr[MaxPerfNum_Binned > 0, .N]
datAggr2 <- unique(datAggr[MaxPerfNum_Binned > 0, list(MaxPerfNum_Binned_Pc = .N / MaxPerfNum_Binned_Total,
                                  MaxPerfNum_Binned_Freq = .N), by=list(MaxPerfNum_Binned)])
datAggr2[, MaxPerfNum_Binned_Pc_labelY := MaxPerfNum_Binned_Freq + totFreq*0.005]
vLabelX <- c("1"="1", "2"="2", "3"="3", "4"="4", "5"="5+")
vBreaks <- 1:length(vLabelX)
vCol <- brewer.pal(10, "Paired")

# - Graph
(g <- ggplot(datAggr[MaxPerfNum_Binned > 0,], aes(x=MaxPerfNum_Binned)) + theme_minimal() + 
  theme(text=element_text(family=chosenFont)) + 
  labs(y="Frequency", x="Maximum Number of Performance Spells (pre-binned)") + 
  geom_bar(fill = vCol[2]) +
  geom_label(data=datAggr2, aes(y=MaxPerfNum_Binned_Pc_labelY, label=paste(percent(MaxPerfNum_Binned_Pc, accuracy=0.1))), 
             family=chosenFont, fill = vCol[1]) +
  scale_y_continuous(label=comma) +
  scale_x_continuous(labels=vLabelX, breaks=vBreaks) )

# - Save graph
ggsave(g, file=paste0(genFigPath, "MaxPerfSpellNum_hist.png"), width=1200/dpi, height=1000/dpi, dpi=dpi, bg="white")


