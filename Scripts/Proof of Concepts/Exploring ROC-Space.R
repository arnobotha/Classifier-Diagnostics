# ================================= EMPIRICAL ROC-CURVE CONSTRUCTION =========================================
# Explores and illustrates concepts described in Fawcett2006 about ROC-space
# Sources: Fawcett2006 (DOI: https://doi.org/10.1016/j.patrec.2005.10.010)
# ------------------------------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha
# -----------------------------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R | Only for plotting purposes
# ============================================================================================================




# ------- 0. Setup

require(ggplot2)
require(scales)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(tidyr)
require(data.table)




# ------- 1. Designed dataset | Various discrete classifiers

# - Define the x and y axis points
x_points <- c(0, 0.1, 0.6, 0, 0.5, 1)
y_points <- c(0, 0.4, 0.9, 1, 0.5, 1)

# - Assign id and labels to vector elements
id <- c("a_A", "b_C", "c_L", "d_D", "e_E", "f_B")
label <- c("A", "C", "L", "D", "E", "B")

# - Create a dataset for plotting purposes
plot_data <- data.table(x=x_points, y=y_points, id, label)

ids <- factor(c("A","D","E","E2","B","D2"))
positions <- data.frame(
   id = rep(ids, each = 3),
   x = c( # red triangle
         x_points[1], x_points[5], x_points[4], 
          # blue triangle
         x_points[5], x_points[6], x_points[4]),
   y = c( # red triangle
         y_points[1], y_points[5], y_points[4],
          # blue triangle
         y_points[5], y_points[6], y_points[4])
)
values <- data.frame(
   id = ids,
   value = c("a_red", "b_blue")
)
datapoly <- merge(values, positions, by = c("id"))




# ------- 2. Create ROC-graph of various discrete classifiers

# - Graph options
chosenFont <- "Cambria"
vCol <- brewer.pal(6, "Set1")[c(1,5,4,3,6,2)]
vFill <- brewer.pal(6, "RdBu")[c(1,6)]

# - Plot the data
(g <- ggplot(plot_data, aes(x = x, y = y)) + theme_minimal() +
      geom_polygon(data=datapoly, aes(x=x, y=y, group=id, fill=value), alpha=0.1, show.legend=F) + 
      geom_point(aes(x=x, y=y, color=id), size=4) +
      geom_abline(slope = 1, intercept = 0) +
      geom_text(aes(label = label), hjust = - 1, vjust=0.3, family=chosenFont) +
      scale_y_continuous(breaks=pretty_breaks(), labels = percent) +
      scale_x_continuous(breaks=pretty_breaks(), labels = percent) +
      scale_color_manual(values=vCol, name="Discrete classifier", labels=label) + 
      scale_fill_manual(values=vFill, name="") + 
      theme(text=element_text(family=chosenFont,size=13),
            legend.position="bottom") + 
      labs(x = bquote(False~positive~rate~~italic(F)^'+'~'='~1-italic(S)^'-'),
           y = bquote(True~positive~rate~~italic(T)^'+'~'='~italic(S)^{'+'}))
)

dpi <- 150 #130 dpi used here for clarity in floating subplots in latex
ggsave(g, file=paste0(genFigPath, "Graph_ROC.png"), width=1000/dpi, height=1000/dpi,dpi=dpi)




# ------- 3. Designed dataset | 2 probabilistic/discriminative classifiers

df <- structure(list(TPR = c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 
                             0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28, 0.3, 0.32, 0.34, 0.36, 
                             0.38, 0.4, 0.42, 0.44, 0.46, 0.48, 0.5, 0.52, 0.54, 0.56, 0.58, 
                             0.6, 0.62, 0.64, 0.64, 0.64, 0.66, 0.68, 0.7, 0.72, 0.74, 0.76, 
                             0.78, 0.8, 0.8, 0.82, 0.82, 0.84, 0.84, 0.84, 0.86, 0.86, 0.86, 
                             0.86, 0.88, 0.88, 0.9, 0.92, 0.92, 0.92, 0.92, 0.94, 0.94, 0.96, 
                             0.96, 0.96, 0.96, 0.96, 0.96, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 
                             0.98, 0.98, 0.98, 0.98, 0.98, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 
                             0.12, 0.14, 0.16, 0.18, 0.2, 0.22, 0.24, 0.24, 0.26, 0.28, 0.3, 
                             0.32, 0.34, 0.36, 0.38, 0.4, 0.42, 0.42, 0.42, 0.44, 0.46, 0.48, 
                             0.5, 0.52, 0.54, 0.56, 0.58, 0.6, 0.6, 0.6, 0.6, 0.62, 0.62, 
                             0.62, 0.64, 0.66, 0.66, 0.68, 0.68, 0.68, 0.7, 0.72, 0.74, 0.76, 
                             0.78, 0.8, 0.8, 0.8, 0.82, 0.82, 0.84, 0.84, 0.84, 0.86, 0.86, 
                             0.86, 0.86, 0.86, 0.88, 0.88, 0.88, 0.9, 0.9, 0.9, 0.9, 0.9, 
                             0.9, 0.9, 0.92, 0.94, 0.96, 0.96, 0.96, 0.96, 0.96, 0.96, 0.96, 
                             0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 1, 1, 1, 
                             1, 1, 1, 1, 1, 1, 0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.1, 0.1, 0.12, 
                             0.14, 0.16, 0.18, 0.2, 0.22, 0.24, 0.24, 0.26, 0.28, 0.28, 0.3, 
                             0.32, 0.34, 0.36, 0.38, 0.4, 0.42, 0.42, 0.42, 0.42, 0.44, 0.44, 
                             0.44, 0.46, 0.48, 0.48, 0.5, 0.52, 0.54, 0.56, 0.58, 0.58, 0.6, 
                             0.62, 0.62, 0.62, 0.64, 0.66, 0.68, 0.68, 0.7, 0.72, 0.72, 0.72, 
                             0.72, 0.74, 0.74, 0.74, 0.76, 0.76, 0.78, 0.78, 0.8, 0.82, 0.84, 
                             0.84, 0.84, 0.86, 0.88, 0.88, 0.9, 0.9, 0.92, 0.92, 0.92, 0.92, 
                             0.92, 0.92, 0.92, 0.92, 0.94, 0.94, 0.96, 0.96, 0.96, 0.96, 0.98, 
                             0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 0.98, 
                             1, 1, 1, 1, 0, 0.02, 0.04, 0.06, 0.06, 0.06, 0.08, 0.08, 0.1, 0.12, 
                             0.14, 0.16, 0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28, 0.28, 0.3, 
                             0.32, 0.32, 0.34, 0.34, 0.36, 0.38, 0.4, 0.42, 0.42, 0.44, 0.46, 
                             0.46, 0.46, 0.48, 0.48, 0.5, 0.52, 0.54, 0.56, 0.56, 0.58, 0.6, 
                             0.62, 0.64, 0.64, 0.64, 0.64, 0.64, 0.66, 0.68, 0.68, 0.7, 0.7, 
                             0.7, 0.7, 0.7, 0.72, 0.74, 0.76, 0.76, 0.78, 0.78, 0.78, 0.8, 
                             0.8, 0.82, 0.82, 0.84, 0.86, 0.86, 0.86, 0.86, 0.88, 0.9, 0.92, 
                             0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.94, 0.94, 0.94, 
                             0.94, 0.94, 0.94, 0.96, 0.98, 0.98, 0.98, 0.98, 1, 1, 1, 1, 1, 
                             1), FPR = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0.04, 0.04, 
                                         0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.06, 0.06, 0.08, 0.08, 
                                         0.1, 0.12, 0.12, 0.14, 0.16, 0.18, 0.18, 0.2, 0.2, 0.2, 0.22, 
                                         0.24, 0.26, 0.26, 0.28, 0.28, 0.3, 0.32, 0.34, 0.36, 0.38, 0.38, 
                                         0.4, 0.42, 0.44, 0.46, 0.48, 0.5, 0.52, 0.54, 0.56, 0.58, 0.58, 
                                         0.6, 0.62, 0.64, 0.66, 0.68, 0.7, 0.72, 0.74, 0.76, 0.78, 0.8, 
                                         0.82, 0.84, 0.86, 0.88, 0.9, 0.92, 0.94, 0.96, 0.98, 1, 1, 0, 0, 
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 
                                         0.02, 0.02, 0.02, 0.02, 0.04, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 
                                         0.06, 0.06, 0.06, 0.06, 0.08, 0.1, 0.12, 0.12, 0.14, 0.16, 0.16, 
                                         0.16, 0.18, 0.18, 0.2, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 0.22, 
                                         0.24, 0.26, 0.26, 0.28, 0.28, 0.3, 0.32, 0.32, 0.34, 0.36, 0.38, 
                                         0.4, 0.4, 0.42, 0.44, 0.44, 0.46, 0.48, 0.5, 0.52, 0.54, 0.56, 
                                         0.56, 0.56, 0.56, 0.58, 0.6, 0.62, 0.64, 0.66, 0.68, 0.68, 0.7, 
                                         0.72, 0.74, 0.76, 0.78, 0.8, 0.82, 0.84, 0.84, 0.86, 0.88, 0.9, 
                                         0.92, 0.94, 0.96, 0.98, 1, 1, 0, 0, 0, 0, 0, 0.02, 0.04, 0.04, 0.04, 
                                         0.04, 0.04, 0.04, 0.04, 0.04, 0.06, 0.06, 0.06, 0.08, 0.08, 0.08, 
                                         0.08, 0.08, 0.08, 0.08, 0.08, 0.1, 0.12, 0.14, 0.14, 0.16, 0.18, 
                                         0.18, 0.18, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.22, 0.22, 0.22, 0.24, 
                                         0.26, 0.26, 0.26, 0.26, 0.28, 0.28, 0.28, 0.3, 0.32, 0.34, 0.34, 
                                         0.36, 0.38, 0.38, 0.4, 0.4, 0.42, 0.42, 0.42, 0.42, 0.44, 0.46, 
                                         0.46, 0.46, 0.48, 0.48, 0.5, 0.5, 0.52, 0.54, 0.56, 0.58, 0.6, 
                                         0.62, 0.64, 0.64, 0.66, 0.66, 0.68, 0.7, 0.72, 0.72, 0.74, 0.76, 
                                         0.78, 0.8, 0.82, 0.84, 0.86, 0.88, 0.9, 0.92, 0.94, 0.94, 0.96, 
                                         0.98, 1, 1, 0, 0, 0, 0.02, 0.04, 0.04, 0.06, 0.06, 0.06, 0.06, 0.06, 
                                         0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.1, 0.1, 0.1, 0.12, 
                                         0.12, 0.14, 0.14, 0.14, 0.14, 0.14, 0.16, 0.16, 0.16, 0.18, 0.2, 
                                         0.2, 0.22, 0.22, 0.22, 0.22, 0.22, 0.24, 0.24, 0.24, 0.24, 0.24, 
                                         0.26, 0.28, 0.3, 0.32, 0.32, 0.32, 0.34, 0.34, 0.36, 0.38, 0.4, 
                                         0.42, 0.42, 0.42, 0.42, 0.44, 0.44, 0.46, 0.48, 0.48, 0.5, 0.5, 
                                         0.52, 0.52, 0.52, 0.54, 0.56, 0.58, 0.58, 0.58, 0.58, 0.6, 0.62, 
                                         0.64, 0.66, 0.68, 0.7, 0.72, 0.74, 0.74, 0.76, 0.78, 0.8, 0.82, 
                                         0.84, 0.84, 0.84, 0.86, 0.88, 0.9, 0.9, 0.92, 0.94, 0.96, 0.98, 
                                         1, 1), GeneSet = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
                                                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 
                                                                      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
                                                                      3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("A", "C", 
                                                                                                                  "D", "B"), class = "factor")), .Names = c("TPR", "FPR", "Classifier"), row.names = c(NA, -404L), class = "data.frame")

# - Create a new dataset that drops Classifiers B and C
df2<-df[!(df$Classifier=="C" | df$Classifier=="D"),]




# ------- 2. Create ROC-graph of 2 probabilistic/discriminative classifiers

# - Graph options
chosenFont <- "Cambria"; dpi <- 180
vCol <- brewer.pal(6, "Set1")[c(3,1)]

(g <- ggplot(df2,aes(x = FPR, y = TPR)) + theme_minimal() +
    geom_polygon(data=df2, aes(x=FPR, y=TPR, group=Classifier, fill=Classifier), alpha=0.1, show.legend=F) +
    geom_line(aes(x = FPR, y = TPR, color = Classifier), linewidth = 1, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0) +
    scale_y_continuous(breaks=pretty_breaks(), labels = percent) +
    scale_x_continuous(breaks=pretty_breaks(), labels = percent) +
    scale_color_manual(values=vCol, name="Classifier") +
    scale_fill_manual(values=vCol, name="") + 
    theme(text=element_text(family=chosenFont,size=13),
          legend.position="bottom") + 
    labs(x = bquote(False~positive~rate~~italic(F)^'+'~'='~1-italic(S)^'-'),
         y = bquote(True~positive~rate~~italic(T)^'+'~'='~italic(S)^{'+'}))
)

# - Save graph
ggsave(g, file=paste0(genFigPath, "ROC_Curve.png"), width=1000/dpi, height=1000/dpi,dpi=dpi, bg="white")



