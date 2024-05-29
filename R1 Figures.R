rm(list=ls())

# 本文档若Bio_ID报错需要去excel添加Bio_ID header


library(readxl)
library(cowplot)
library(plyr)
library(tidyverse)
library(ggpubr)
library(Hmisc)
library(tableone)
library(ggstatsplot)
library(corrplot)
library(viridis)
library(hrbrthemes)
library(showtext)
library(ggprism)
library(pheatmap)
library(RColorBrewer)
library(umap)


# Collection time 10X5#-----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')

DB_integration_meta %>%
  ggplot( aes(x=Visiting_date, fill=Category_2)) +
  geom_histogram(color="#e9ecef",alpha=0.8, position = 'identity') +
  scale_fill_manual(values=c("#2166ac", "#b2182b", "#f4a582")) +
  scale_x_datetime(limits = as.POSIXct(c("2019-01-01","2024-01-01"))) + 
  theme_ipsum() +
  labs(fill="")

# Leiden proportion #----
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')

DB_integration_meta %>%  
  group_by(leiden) %>% count(Immunosuppression_before)  %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(leiden, pct, fill=Immunosuppression_before) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Percent")+ theme(text = element_text(size=20)) +
  scale_fill_manual(values=c("#2166ac", "#b2182b", "#f4a582"))+
  theme_ipsum()

## Complements in different clusters #----
DB_integration_data <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.endotype')
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration <- left_join(DB_integration_data, DB_integration_meta, by = 'Bio_ID')

unique(DB_integration$Antibody)
DB_integration$Antibody <- factor(DB_integration$Antibody, levels = c("Control", "AChR+", "MuSK+", "SNMG"))

Plasma_integration <- DB_integration %>% pivot_longer(cols = c('IL_6':'IL_18'), names_to = "Cytokine", values_to = "Num")
Plasma_integration <-  subset(Plasma_integration, !is.na(Num))
Plasma_integration$Cytokine <- factor(Plasma_integration$Cytokine , levels = c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma", "IL_4", "IL_6", "IL_18", "IL_17", "IL_23", "IL_2", "IL_10", "IL_19", "IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5", "BAFF", "APRIL", "C2", "C5a", "C9"))

Plasma_integration_MG <- Plasma_integration %>% filter(Category_1 != 'Control')


ggplot(Plasma_integration_MG, aes(x=leiden, y=Num, fill=leiden))+ 
  geom_bar(aes(fill=leiden), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = leiden, colour =leiden), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#b2182b80", "#f4a58280","#2166ac80"))+
  scale_color_manual(values = c("#b2182b", "#f4a582","#2166ac"))+
  geom_pwc(
    aes(group = leiden), tip.length = 0,
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr",
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+theme_prism()+rotate_x_text()


# Overall MG vs. HC #----
DB_integration_data <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.endotype')
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration <- left_join(DB_integration_data, DB_integration_meta, by = 'Bio_ID')

unique(DB_integration$Antibody)
DB_integration$Antibody <- factor(DB_integration$Antibody, levels = c("Control", "AChR+", "MuSK+", "SNMG"))

Plasma_integration <- DB_integration %>% pivot_longer(cols = c('IL_6':'IL_18'), names_to = "Cytokine", values_to = "Num")
Plasma_integration <-  subset(Plasma_integration, !is.na(Num))
Plasma_integration$Cytokine <- factor(Plasma_integration$Cytokine , levels = c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma", "IL_4", "IL_6", "IL_18", "IL_17", "IL_23", "IL_2", "IL_10", "IL_19", "IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5", "BAFF", "APRIL", "C2", "C5a", "C9"))

## Log2fold calculation #----
Log2fold <- Plasma_integration %>% group_by(Category_1, Cytokine) %>% dplyr::summarize(mean_value = mean(Num)) %>%  
  pivot_wider(names_from = Category_1, values_from = mean_value)  

Log2fold$L2F <- log2(Log2fold$MG/Log2fold$Control)

## Overall bar plot 20x12 #----
ggplot(Plasma_integration, aes(x=Category_1, y=Num, fill=Category_1))+ 
  geom_bar(aes(fill=Category_1), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Category_1, colour =Category_1), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b" ))+
  geom_pwc(
    aes(group = Category_1), tip.length = 0,
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr",
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+theme_prism()+rotate_x_text()


# Different MG Antibdoy types #-----
DB_integration_data <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.endotype')
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration <- left_join(DB_integration_data, DB_integration_meta, by = 'Bio_ID')
DB_Random <- DB_integration %>% filter(Category_2 != 'Naïve AChR+ MG')

unique(DB_Random$Antibody)
DB_Random$Antibody <- factor(DB_Random$Antibody, levels = c("Control", "AChR+", "MuSK+", "SNMG"))

DB_Random <- DB_Random %>% pivot_longer(cols = c('IL_6':'IL_18'), names_to = "Cytokine", values_to = "Num")
DB_Random <-  subset(DB_Random, !is.na(Num))
DB_Random$Cytokine <- factor(DB_Random$Cytokine , levels = c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma", "IL_4", "IL_6", "IL_18", "IL_17", "IL_23", "IL_2", "IL_10", "IL_19", "IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5", "BAFF", "APRIL", "C2", "C5a", "C9"))


## Antibdoy classification (errorbar plot - pairwise comparison)  20x12#----
ggplot(DB_Random, aes(x=Antibody, y=Num, fill=Antibody))+ 
  geom_bar(aes(fill=Antibody), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Antibody, colour =Antibody), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Antibody), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+theme_prism()+rotate_x_text() # p.adj.signif, p.adj.format

## Antibdoy classification (errorbar plot - control reference)  20x12#----
ggplot(DB_Random, aes(x=Antibody, y=Num, fill=Antibody))+ 
  geom_bar(aes(fill=Antibody), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Antibody, colour =Antibody), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Antibody), tip.length = 0, ref.group = "Control",
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr",
    bracket.nudge.y = -0.1, step.increase = 0, remove.bracket=TRUE
  )+theme_prism()+rotate_x_text()


## Antibdoy classification (Statistical test)
stat.test.Antibody <- DB_Random %>% group_by(Cytokine) %>%rstatix::t_test(Num ~ Antibody) %>% rstatix::adjust_pvalue(method = "fdr") %>%rstatix::add_significance("p") %>% rstatix::add_xy_position(x = "Antibody", scales = "free")

Statistics.Total <-ddply(Plasma_integration, c("Cytokine", "Antibody"), summarise,mean = mean(Num), sd = sd(Num))


# Clinical correlation #----
## Netplot #----
library(corrr)

DB_MG_corrr <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.corrr')
DB_MG_corrr <- DB_MG_corrr[, -1]

x <- correlate(DB_MG_corrr)


#> Correlation computed with
#> • Method: 'pearson'
#> • Missing treated using: 'pairwise.complete.obs'
network_plot(x,   colours = c("#2166ac", "#ffffff","#b2182b"))

## Cor Matrix 14x14 #----
colnames(DB_MG_corrr)

DB_corMatrix <- select(DB_MG_corrr, c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma", "IL_4", "IL_6", "IL_18", "IL_17", "IL_23", "IL_2", "IL_10", "IL_19", "IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5", "BAFF", "APRIL", "C2", "C5a", "C9", "Visting_age", "Onset_age", "Duration", "ADL_all", 
                                      "QOL_all", "MGC_all" ,"QMG_all"))

ggcorrmat(DB_corMatrix, type = "pearson", p.adjust.method="fdr",ggcorrplot.args = list(insig = "blank"), colors = c("#2166ac", "#ffffff","#b2182b"),lab = TRUE, messages=TRUE, output = "plot",
          ggtheme=theme_ipsum())

## Basic corr 14x4 #---- 
TNF_C9 <- DB_integration %>% filter(Category_1 == "MG") %>% select(c('Category_1','TNF_alpha', 'C9', 'ADL_all', 'QOL_all', 'MGC_all', 'QMG_all')) %>% gather(`ADL_all`: `QMG_all`, key = "Clinical_score", value = "Score")

ggscatter(TNF_C9, x = "TNF_alpha", y = "Score", color = '#2166ac',
          add = "reg.line", add.params = list(color = "#b2182b", fill = "lightgray"),
          facet.by = 'Clinical_score', ncol = 4, scales="free",
          conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
)+theme_prism()

ggscatter(TNF_C9, x = "C9", y = "Score", color = '#2166ac',
          add = "reg.line", add.params = list(color = "#b2182b", fill = "lightgray"),
          facet.by = 'Clinical_score', ncol = 4, scales="free",
          conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
)+theme_prism()

# Clinical features #----

## Thymoma 20x12#----
Plasma_integration$Thymoma <- factor(Plasma_integration$Thymoma, levels = c("Control", "Thymoma", "None"))

stat.test.Thymoma <- Plasma_integration %>% group_by(Cytokine) %>%rstatix::t_test(Num ~ Thymoma) %>% rstatix::adjust_pvalue(method = "fdr") %>%rstatix::add_significance("p") %>% rstatix::add_xy_position(x = "Thymoma", scales = "free")

ggplot(Plasma_integration, aes(x=Thymoma, y=Num, fill=Thymoma))+ 
  geom_bar(aes(fill=Thymoma), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Thymoma, colour =Thymoma), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Thymoma), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()

## Thymectomy 20x12#----
Plasma_integration$Thymectomy <- factor(Plasma_integration$Thymectomy, levels = c("Control", "Yes", "None"))

ggplot(Plasma_integration, aes(x=Thymectomy, y=Num, fill=Thymectomy))+ 
  geom_bar(aes(fill=Thymectomy), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Thymectomy, colour =Thymectomy), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Thymectomy), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()

Statistics.Thymectomy <-ddply(Plasma_integration_6m, c("Cytokine", "Thymectomy"), summarise,mean = mean(Num), sd = sd(Num))

## ClinicalType 20x12#----
Plasma_integration$ClinicalType <- factor(Plasma_integration$ClinicalType, levels = c("Control", "Generalized", "Ocular"))

ggplot(Plasma_integration, aes(x=ClinicalType, y=Num, fill=ClinicalType))+ 
  geom_bar(aes(fill=ClinicalType), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = ClinicalType, colour =ClinicalType), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = ClinicalType), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()

## Prognosis_6m 20x12#----
unique(Plasma_integration$Prognosis_6m)
Plasma_integration_6m <- filter (Plasma_integration, Prognosis_6m != "Unknown")
Plasma_integration_6m$Prognosis_6m <- factor(Plasma_integration_6m$Prognosis_6m, levels = c("Control", "Improved", "Unchanged (MM)","Unchanged", "Worse"))

ggplot(Plasma_integration_6m, aes(x=Prognosis_6m, y=Num, fill=Prognosis_6m))+ 
  geom_bar(aes(fill=Prognosis_6m), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Prognosis_6m, colour =Prognosis_6m), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#67a9cf80", "#d1e5f080","#ef8a6280", "#b2182b80" ))+
  scale_color_manual(values = c("#2166ac", "#67a9cf", "#d1e5f0","#ef8a62", "#b2182b" ))+
  geom_pwc(
    aes(group = Prognosis_6m), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()

Statistics.Prognosis_6m <-ddply(Plasma_integration_6m, c("Cytokine", "Prognosis_6m"), summarise,mean = mean(Num), sd = sd(Num))


# Heatmap plot-----
## Reference: https://biostatsquid.com/step-by-step-heatmap-tutorial-with-pheatmap/

DB_integration_data <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.endotype')
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration <- left_join(DB_integration_data, DB_integration_meta, by = 'Bio_ID')
DB_integration[, 2:23] <- scale(DB_integration[, 2:23])


unique(DB_integration$Antibody)
DB_integration$Antibody <- factor(DB_integration$Antibody, levels = c("Control", "AChR+", "MuSK+", "SNMG"))


Heat.data <- t(cbind(DB_integration$Bio_ID, DB_integration[, 2:23]))# Transpose 
colnames(Heat.data) <- Heat.data["DB_integration$Bio_ID", ] # Assign the first row as column names
Heat.data <- Heat.data[-1, ] # Remove the first row as it is now the column names
Heat.data <- Heat.data %>% data.frame() %>% mutate(across(where(is.character), as.numeric)) %>% as.matrix()  # Convert the character matrix to numeric


Heat.data_roworder <- c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma", "IL_4", "IL_6", "IL_18", "IL_17", "IL_23", "IL_2", "IL_10", "IL_19", "IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5", "BAFF", "APRIL", "C2", "C5a", "C9")

Heat.data_colorder <- DB_integration %>% arrange(Antibody) %>% select(Bio_ID)
Heat.data_colorder <- Heat.data_colorder$Bio_ID

Heat.data <- Heat.data[Heat.data_roworder, Heat.data_colorder] # reorder row names


## create a data frame for column annotation ##
Sample_anno <- as.data.frame(cbind(DB_integration$Antibody, DB_integration$ClinicalType))
row.names(Sample_anno) <- DB_integration$Bio_ID
colnames(Sample_anno) <- c('Antibody', 'ClinicalType')



unique(DB_primary$ClinicalType)
unique(DB_primary$Antibody)

# 因为R会把ACHR的+号变没掉，所以这里用1-4代表抗体：Levels: Control AChR+ MuSK+ SNMG

ann_colors <- list(
  Antibody = c("1" = "#2166ac", "2" = "#b2182b",
               "3" = "#f4a582",
               "4" = "#d6604d"),
  ClinicalType = c("Control" = "#2166ac", 
                   "Generalized" = "#170c46",
                   "Ocular" = "#63a85e")
)


pheatmap(Heat.data, cluster_rows = F, cluster_cols = F, 
         #col = brewer.pal(11, 'RdYlGn'),
         #col = hcl.colors(50, "RdYlGn"), 
         col = colorRampPalette(c("#2166ac", "white", "#b2182b"))(100),
         #annotation_row = gene_functions_df, 
         annotation_col = Sample_anno, 
         annotation_colors = ann_colors,
         main = "Super heatmap with annotations") 



# Radar plot 6x6-----
library(ggradar)
library(dplyr)
library(scales)
library(tibble)


DB_integration_data <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.endotype')
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration$Antibody <- factor(DB_integration$Antibody, levels = c("Control", "AChR+", "MuSK+", "SNMG"))
DB_integration[, 2:23] <- scale(DB_integration[, 2:23])

DB_integration['Th1'] <- DB_integration[c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma")] %>% rowMeans()
DB_integration['Th2'] <- DB_integration[c("IL_4", "IL_6", "IL_18")] %>% rowMeans()
DB_integration['Th17'] <- DB_integration[c("IL_17", "IL_23")] %>% rowMeans()
DB_integration['Treg'] <- DB_integration[c("IL_2", "IL_10", "IL_19")] %>% rowMeans()
DB_integration['N_cell'] <- DB_integration[c("IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5")] %>% rowMeans()
DB_integration['B_cell'] <- DB_integration[c("BAFF", "APRIL")] %>% rowMeans()
DB_integration['Complement'] <- DB_integration[c("C5a", "C9")] %>% rowMeans()


Radar <- DB_integration %>% filter(Status != "Pre-treatment") %>% select(c("Antibody", 'Th1', 'Th2', 'Th17', 'Treg', 'N_cell', 'B_cell', 'Complement'))%>% filter(complete.cases(.)) %>% 
  group_by(Antibody) %>%
  summarize_all(mean) %>%  mutate_at(vars(-Antibody), rescale)


ggradar(Radar, fill = TRUE, fill.alpha = 0.1, group.colours = c("#2166ac", "#b2182b", "#f4a582", "#d6604d"))

## Radar_data comparison 10x7#----
Radar_data <-  DB_integration %>% select(c("Antibody", 'Th1', 'Th2', 'Th17', 'Treg', 'N_cell', 'B_cell', 'Complement'))%>% filter(complete.cases(.)) %>% 
  group_by(Antibody) %>%  pivot_longer(cols = c('Th1':'Complement'), names_to = "Cell_response", values_to = "Num") # %>% filter(Status != "Post-treatment")

ggplot(Radar_data, aes(x=Antibody, y=Num, fill=Antibody))+ 
  geom_bar(aes(fill=Antibody), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Antibody, colour =Antibody), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cell_response, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Antibody), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()

# AChR+ Naive > 1 year 20x12 #-----
DB_integration_data <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.endotype')
DB_integration_meta <- read_excel('Endotype_scanpy.xlsx', sheet = 'Integration.meta')
DB_integration <- left_join(DB_integration_data, DB_integration_meta, by = 'Bio_ID')

unique(DB_integration$Drug_1y)
DB_integration$Steroid_response <- factor(DB_integration$Steroid_response, levels = c("Non-response", "Response", "Control"))
DB_Naive <- DB_integration %>% filter(Category_2 != "Random MG")


Plasma_Naive <- DB_Naive %>% pivot_longer(cols = c('IL_6':'IL_18'), names_to = "Cytokine", values_to = "Num")
Plasma_Naive <-  subset(Plasma_Naive, !is.na(Num))
Plasma_Naive$Cytokine <- factor(Plasma_Naive$Cytokine , levels = c("TNF_alpha", "IFN_alpha", "IFN_beta", "IFN_gamma", "IL_4", "IL_6", "IL_18", "IL_17", "IL_23", "IL_2", "IL_10", "IL_19", "IL_8", "CCL3", "CXCL2", "CXCL4", "CXCL5", "BAFF", "APRIL", "C2", "C5a", "C9"))

ggplot(Plasma_Naive, aes(x=Steroid_response, y=Num, fill=Steroid_response))+ 
  geom_bar(aes(fill=Steroid_response), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Steroid_response, colour =Steroid_response), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Steroid_response), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()

Statistics.Drug_1y <-ddply(Plasma_Naive, c("Cytokine", "Drug_1y"), summarise,mean = mean(Num), sd = sd(Num))

# MC #-----
MC_data <- read_excel('Endotype_MC_huan.xlsx', sheet = 'MC.endotype')
MC_meta <- read_excel('Endotype_MC_huan.xlsx', sheet = 'MC.meta')
MC_integration <- left_join(MC_data, MC_meta, by = 'Bio_ID')

unique(MC_integration$Stage)
MC_integration$Stage <- factor(MC_integration$Stage, levels = c("CT", "Crisis", "1w", "1m", "2m", "3m", "6m"))

MC_results <- MC_integration %>% pivot_longer(cols = c('IL_6':'IL_18'), names_to = "Cytokine", values_to = "Num")
MC_results <-  subset(MC_results, !is.na(Num))

ggplot(MC_results, aes(x=Stage, y=Num, fill=Stage))+ 
  geom_bar(aes(fill=Stage), position = 'dodge', stat = 'summary', color="#e9ecef") +
  geom_errorbar(stat = 'summary', position = 'dodge', width = 0.5, size = 0.8) +
  geom_point(aes(x = Stage, colour =Stage), size = 1, shape = 19, position = "jitter", alpha = 0.9)+
  facet_wrap(~Cytokine, nrow=3, scales="free") + 
  #scale_fill_manual(values = c("#2166ac80", "#b2182b80", "#f4a58280", "#d6604d80" ))+
  #scale_color_manual(values = c("#2166ac", "#b2182b", "#f4a582", "#d6604d" ))+
  geom_pwc(
    aes(group = Stage), tip.length = 0, 
    method = "t_test", label = "p.adj.signif", hide.ns = TRUE , p.adjust.method = "fdr", 
    bracket.nudge.y = -0.1, step.increase = 0.1
  )+ scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))+
  theme_prism()+rotate_x_text()
