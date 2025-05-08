# ========================== ===DEFAULT RISK - INPUT DIAGNOSTICS ===========================================
# This script performs input diagnostics on the three logit models. Mainly it assesses the variable importance
# of the three models using z-scores and standardised coefficients.
# -----------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Marcel Muller (MM), Dr Arno Botha (AB)

# DESCRIPTION:
# This script uses the previously selected variables in fitting different logit models according to their
# level of complexity. The variable importance of these variables are then assessed.
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Data_Prepare_Credit_Advanced.R
#   - 2c.Data_Prepare_Credit_Advanced2.R
#   - 2d.Data_Enrich.R
#   - 2f.Data_Fusion1.R
#   - 3b.Data_Subsampled_Fusion2.R
#   - 3b.Data_Subsample_Fusion2
#   - 3c(i).Model_DefaultRisk_Basic
#   - 3c(ii).Model_DefaultRisk_Intermediate
#   - 3c(iii).Model_DefaultRisk_Advanced

# -- Inputs:
#   - datCredit_train | Prepared credit data from script 3b
#   - datCredit_valid | Prepared credit data from script 3b
#   - Final model input variables
#
# -- Outputs:
#   - <analytics> | Graphs showing various input-level diagnostics
# ===========================================================================================================

# ------ 1. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Graphing Parameters
chosenFont <- "Cambria"
dpi <- 180

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_train')) unpack.ffdf(paste0(genPath,"creditdata_train"), tempPath)
if (!exists('datCredit_valid')) unpack.ffdf(paste0(genPath,"creditdata_valid"), tempPath)

# - Subset to exclude default spells
datCredit_train <- datCredit_train %>% subset(DefaultStatus1==0)
datCredit_valid <- datCredit_valid %>% subset(DefaultStatus1==0)

# - Load in basic, intermediate, and advanced model formulas
# Basic model
unpack.ffdf(paste0(genObjPath, "Basic_Com_Formula"), tempPath)
# Intermediate model
unpack.ffdf(paste0(genObjPath, "Int_Formula"), tempPath)
# Advanced model
unpack.ffdf(paste0(genObjPath, "Adv_Formula"), tempPath)




# ------ 2. Variable importance assessment
# --- 2.1 Fitting the models
# - Basic model
logitMod_Basic <- glm(inputs_bas, data=datCredit_train, family="binomial")

# - Intermediate model
logitMod_Int <- glm(inputs_int, data=datCredit_train, family="binomial")

# - Advanced model
logitMod_Adv <- glm(inputs_adv, data=datCredit_train, family="binomial")


# --- 2.2 Variable importance
# - Standardised Coefficients using Z-Scores (i.e., standardising the input space and fitting the model)
# Basic model
(varImp_Bas_ZScore <- varImport_logit(logitMod_Basic, method="stdCoef_ZScores", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_ZScore_Bas.png")))
# Intermediate model
(varImp_Int_ZScore <- varImport_logit(logitMod_Int, method="stdCoef_ZScores", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_ZScore_Int.png")))
# Advanced model
(varImp_Adv_ZScore <- varImport_logit(logitMod_Adv, method="stdCoef_ZScores", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_ZScore_Adv.png")))

# - Standardised Coefficients using Goodman's method (i.e., standardising the coefficients of the model directly with the mean and standard deviation of each assocaited input variables)
(varImp_Bas_Goodman <- varImport_logit(logitMod_Basic, method="stdCoef_Goodman", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Goodman_Bas.png")))
# Intermediate model
(varImp_Int_Goodman <- varImport_logit(logitMod_Int, method="stdCoef_Goodman", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Goodman_Int.png")))
# Advanced model
(varImp_Adv_Goodman <- varImport_logit(logitMod_Adv, method="stdCoef_Goodman", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Goodman_Adv.png")))

# - Standardised Coefficients using Menard's method (i.e., standardising the coefficients of the model)
# Basic model
(varImp_Bas_Menard <- varImport_logit(logitMod_Basic, method="stdCoef_Menard", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Menard_Bas.png")))
# Intermediate model
(varImp_Int_Menard <- varImport_logit(logitMod_Int, method="stdCoef_Menard", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Menard_Int.png")))
# Advanced model
(varImp_Adv_Menard <- varImport_logit(logitMod_Adv, method="stdCoef_Menard", impPlot=T, pd_plot=T, plotName=paste0(genFigPath,"VariableImportance_Menard_Adv.png")))


# --- 2.3 Bespoke variable importance graphs
# - Graph for top 8 variables
# Graphing parameters
chosenFont <- "Cambria"; dpi <- 180
colPalette <- "BrBG"; colPaletteDir <- 1
# Variable for assisting the calculation of the contribution of each variable to the predictions
sumVarImport <- sum(varImp_Adv_Menard$data$Value_Abs, na.rm=T)
# Graph
# AB: Look at position of legend and change it to be in the graphing space
(g_varImp_Adv_Menard <- ggplot(varImp_Adv_Menard$data[1:8,], aes(x=reorder(Variable, Value_Abs))) + theme_minimal() + theme(text=element_text(family=chosenFont), legend.position = c(0.85,0.5)) + 
   geom_col(aes(y=Value_Abs, fill=Value_Abs)) + geom_label(aes(y=sumVarImport*0.05, label=paste(percent(Contribution, accuracy=0.1)), fill=Value_Abs), family=chosenFont) + 
   annotate(geom="text", x=varImp_Adv_Menard$data[8, Variable], y=varImp_Adv_Menard$data$Value_Abs[1]*0.75, label=paste0("Variable Importance (sum): ", comma(sumVarImport, accuracy=0.1)), family=chosenFont, size=3) + 
   coord_flip() + scale_fill_distiller(palette=colPalette, name="Absolute value", direction=colPaletteDir) +
   scale_colour_distiller(palette=colPalette, name="Absolute value", direction=colPaletteDir) + 
   labs(x="Variable name", y=varImp_Adv_Menard$Method))
# Save graph
ggsave(g_varImp_Adv_Menard, file=paste0(genFigPath, "VarImp_Adv_Men_Std.png"), width=1200/dpi, height=1000/dpi, dpi=400, bg="white")


