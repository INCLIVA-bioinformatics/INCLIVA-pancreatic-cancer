### R CODE FOR ALL STATISTICAL ANALYSES, FIGURES AND TABLES


####Figure 3

mar <- read_excel("C:/Users/raistlol/Juanbox/Marisol Huerta/MH_1/Base pancreas corregida.xlsx", na = "NA")
mar$dif <- (mar$`FA_PostTreatment 6_8 weeks`-mar$FA_PreTreatment)/mar$FA_PreTreatment
mar_train <- subset(mar, cohort=="first" & FA_PreTreatment > 0)
mar_test <- subset(mar, cohort=="second" & FA_PreTreatment > 0)
mod1 <- coxph(Surv(PFS1_dias, staus_PFS)~dif, data=mar_test)
summary(mod1)

res.cut <- surv_cutpoint(mar_train, time = "PFS1_dias", event = "staus_PFS",
                         variables = c("dif"))

summary(res.cut)


plot(res.cut, "dif", palette = "npg")


mar_test$dif_cat1 <- car::recode(mar_test$dif,
                                 "lo:-0.8475='Low risk';
                              -0.8475:hi='High risk'")

mar_train$dif_cat1 <- car::recode(mar_train$dif,
                                  "lo:-0.8475='Low risk';
                              -0.8475:hi='High risk'")

test1 <- survfit(Surv(PFS1_dias, staus_PFS)~dif_cat1, data=mar_train)
survminer::ggsurvplot(test1, title="Training cohort", legend.title="", legend.labs= c("High risk", "Low Risk"),
                      pval = T, pval.method = T, xlab="Days", ylab="PFS", 
                      risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Patients at risk (%)",
                      risk.table.y.text = FALSE,
                      tables.y.text = FALSE,surv.median.line = "hv",
                      break.time.by = 60)

mar_test$PFS1_meses <- mar_test$PFS1_dias /30.417

test1 <- survfit(Surv(PFS1_meses, staus_PFS)~dif_cat1, data=mar_test)
survminer::ggsurvplot(test1, title="Cohorte de validación", legend.title="", legend.labs= c("Alto riesgo", "Bajo riesgo"),
                      pval = T, pval.method = T, xlab="Meses", ylab="SLP", 
                      risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Pacientes en riesgo (%)",
                      risk.table.y.text = FALSE,
                      tables.y.text = FALSE,surv.median.line = "hv",
                      break.time.by = 1)

####Sup Figure 7

library(dplyr)
library(ggplot2)
library(readxl)
library(readxl)

marisol <- read_excel("marisol.xlsx", 
                      col_types = c("text", "date", "text", 
                                    "date", "date", "text", "date", "date", 
                                    "text", "date", "date", "date", "text", 
                                    "date", "text", "date", "text", "date", 
                                    "text", "date", "text", "date", "text", 
                                    "date", "numeric", "skip"))

marisol$linea1_start <- (marisol$StartTr1 - marisol$Diagnosis)/30.417
marisol$linea2_start <- (marisol$StartTr2 - marisol$Diagnosis)/30.417
marisol$linea3_start <- (marisol$StartTr3 - marisol$Diagnosis)/30.417


marisol$linea1_fin <- (marisol$StopTr1 - marisol$Diagnosis)/30.417
marisol$linea2_fin <- (marisol$StopTr2 - marisol$Diagnosis)/30.417
marisol$linea3_fin <- (marisol$StopTr3 - marisol$Diagnosis)/30.417

marisol$L1_status <- as.factor(marisol$`Status LB1`)
marisol$L2_status <- as.factor(marisol$`Status LB2`)
marisol$L3_status <- as.factor(marisol$`Status LB3`)
marisol$L4_status <- as.factor(marisol$`Status L4`)
marisol$L5_status <- as.factor(marisol$`Status L5`)
marisol$L6_status <- as.factor(marisol$`Status L6`)

marisol$L1_date <- difftime(marisol$LB1, marisol$Diagnosis, units = "days")/30.417
marisol$L2_date <- (marisol$LB2 - marisol$Diagnosis)/30.417
marisol$L3_date <- (marisol$L3 - marisol$Diagnosis)/30.417
marisol$L4_date <- (marisol$L4 - marisol$Diagnosis)/30.417
marisol$L5_date <- (marisol$L5 - marisol$Diagnosis)/30.417
marisol$L6_date <- (marisol$L6 - marisol$Diagnosis)/30.417

marisol$T_LFU <- (marisol$`last FUP` -  marisol$Diagnosis)/30.417

marisol$status <- as.factor(marisol$`status last FUP`)


marisol %>% ggplot(aes(as.factor(Patient), T_LFU))+
  coord_flip()+
  labs(x="Patient", y="Months since diagnose", colour="", shape="")+
  geom_segment(aes(x=as.factor(Patient), xend=as.factor(Patient), y=0, yend=T_LFU), size=1, alpha=0.7)+
  geom_segment(mapping=aes(x=as.factor(Patient), xend=as.factor(Patient), y=linea1_start, yend=linea1_fin,  colour = "palegreen3"), alpha = 0.3, 
               size=5)+
  geom_segment(mapping=aes(x=as.factor(Patient), xend=as.factor(Patient), y=linea2_start, yend=linea2_fin,  colour = "orange3"), alpha = 0.3, 
               size=5)+ 
  geom_segment(mapping=aes(x=as.factor(Patient), xend=as.factor(Patient), y=linea3_start, yend=linea3_fin,  colour = "orchid2"), alpha = 0.3, 
               size=5)+ 
  geom_point(aes(as.factor(Patient), L1_date, shape=L1_status),size=3,show.legend = T, fill="blue")+
  geom_point(aes(as.factor(Patient), L2_date, shape=L2_status),size=3,show.legend = T, fill="blue")+
  geom_point(aes(as.factor(Patient), L3_date, shape=L3_status),size=3,show.legend = T, fill="blue")+
  geom_point(aes(as.factor(Patient), L4_date, shape=L4_status),size=3,show.legend = T, fill="blue")+
  geom_point(aes(as.factor(Patient), L5_date, shape=L5_status),size=3,show.legend = T, fill="blue")+
  geom_point(aes(as.factor(Patient), L6_date, shape=L6_status),size=3,show.legend = T, fill="blue")+
  geom_point(aes(as.factor(Patient), T_LFU, shape=status),size=3,show.legend = T, fill="red")+
  theme(legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill=NA),
        aspect.ratio = 1, axis.text = element_text(colour = 1, size = 12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  guides(colour=guide_legend(override.aes=list(shape=NA)))+
  scale_shape_manual(values=c(1, 21,17, 18), name="direc", labels=c("Alive", "Dead", "KRAS wildtype", "KRAS mutated"))+
  scale_colour_manual(breaks=c("palegreen3",  "orange3","orchid2"),
                      values=c("palegreen3",  "orange3","orchid2"),
                      labels=c("First line","Second line","Third line"))

#Sup Figure 8


mar <- read_excel("C:/Users/raistlol/Juanbox/Marisol Huerta/MH_1/Base pancreas corregida.xlsx", na = "NA")
mar_train <- subset(mar, cohort=="first")
mar <- subset(mar, cohort=="second")

#PFS, validation cohort
mar$PFS1_meses <- mar$PFS1_dias /30.417
test1 <- survfit(Surv(PFS1_meses, staus_PFS)~FA_PreTreatment_cat, data=mar)
survminer::ggsurvplot(test1, title="Pre treatment", legend.title="", legend.labs= c("Negative", "Positive"),
                      pval = T, pval.method = T, xlab="Months", ylab="PFS", conf.int = F,
                      risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Patients at risk (%)",
                      risk.table.y.text = FALSE,
                      tables.y.text = FALSE,surv.median.line = "hv",
                      break.time.by = 6)

mod <- coxph(Surv(PFS1_meses, staus_PFS)~FA_PreTreatment_cat, data=mar)
summary(mod)


test1 <- survfit(Surv(PFS1_meses, staus_PFS)~FA_PostTreatment_6_8_weeks_cat, data=mar)
survminer::ggsurvplot(test1, title="Post treatment", legend.title="", legend.labs= c("Negative", "Positive"),
                      pval = T, pval.method = T, xlab="Days", ylab="PFS", conf.int = F,
                      risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Patients at risk (%)",
                      risk.table.y.text = FALSE,
                      tables.y.text = FALSE,surv.median.line = "hv",
                      break.time.by = 6)



#PFS, training cohort
mar_train$PFS1_meses <- mar_train$PFS1_dias /30.417
test1 <- survfit(Surv(PFS1_meses, staus_PFS)~FA_PreTreatment_cat, data=mar_train)
survminer::ggsurvplot(test1, title="Pre treatment", legend.title="", legend.labs= c("Negative", "Positive"),
                         pval = T, pval.method = T, xlab="Months", ylab="PFS", conf.int = F,
                         risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Patients at risk (%)",
                         risk.table.y.text = FALSE,
                         tables.y.text = FALSE,surv.median.line = "hv",
                         break.time.by = 6,
                         pval.method.coord = c(0.2, 0.30),
                         pval.coord = c(0.2,0.23))
mod <- coxph(Surv(PFS1_dias, staus_PFS)~FA_PreTreatment_cat, data=mar_train)
summary(mod)

test1 <- survfit(Surv(PFS1_meses, staus_PFS)~FA_PostTreatment_6_8_weeks_cat, data=mar_train)
survminer::ggsurvplot(test1, title="Post treatment", legend.title="", legend.labs= c("Negative", "Positive"),
                         pval = T, pval.method = T, xlab="Months", ylab="PFS", conf.int = F,
                         risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Patients at risk (%)",
                         risk.table.y.text = FALSE,
                         tables.y.text = FALSE,surv.median.line = "hv",
                         break.time.by = 6)

#Sup Figure 9

mar$evol <- paste(mar$FA_PreTreatment_cat, mar$FA_PostTreatment_6_8_weeks_cat)
mar_evol <- subset(mar, evol=="NEG NEG" | evol=="POS NEG" | evol=="POS POS")


test1 <- survfit(Surv(PFS1_dias, staus_PFS)~evol, data=mar_evol)
survminer::ggsurvplot(test1, title="KRAS stauts baseline to first line treatment", legend.title="", legend.labs= c("Remains negative", "Positive to negative", "Remains positive"),
                      pval = "Log-rank p<0.001", pval.method = T, xlab="Days", ylab="PFS", 
                      risk.table = "abs_pct", risk.table.col="strata", risk.table.title= "Patients at risk (%)",
                      risk.table.y.text = FALSE,
                      tables.y.text = FALSE,surv.median.line = "hv",
                      break.time.by = 60)
pairwise_survdiff(Surv(PFS1_dias, staus_PFS)~evol, data=mar_evol)
#Differences found between POS-POS vs NEG-NEG and vs POS-POS and POS-NEG.


###Table 1

mar <- read_excel("C:/Users/Raistlol/JuanBox/Marisol Huerta/MH_1/Base pancreas Juan multivariable_V2.xlsx", na = "NA")
mar$LIVER_METS <- mar$`LIVER METS`
mar$ECOG <- as.factor(mar$ECOG)
mar$N_METS_SITES <- car::recode(mar$`N METS SITES`,
                                "3:hi = 'More than 2'")
mar$PERITONEUM_METS <- mar$`PERITONEUM METS`
mar$firstline <- mar$`1ST LINE TREATMENT`
mar$LUNG_METS <- mar$`LUNG METS`
mar$dif <- (mar$`FA_PostTreatment 6_8 weeks`-mar$FA_PreTreatment)/mar$FA_PreTreatment



explanatory = c("AGE_CONT", "SEX", "ECOG", "LIVER_METS", "PERITONEUM_METS", "LUNG_METS",
                "OTHER", "N_METS_SITES", "firstline", "OS","CA_L1_cat")
dependent = "cohort"
mar %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE, p_cat="fisher",  cont="median",total_col =  T) -> t1
knitr::kable(t1, row.names=FALSE, align=c("l", "l", "r", "r", "r"))


###Table 3

mar_test <- subset(mar, cohort=="second" & FA_PreTreatment > 0)
mar_test$INCLIVA_score <- car::recode(mar_test$dif,
                                      "lo:-0.8475='Low risk';
                              -0.8475:hi='High risk'")
mar_test$INCLIVA_score <- relevel(as.factor(mar_test$INCLIVA_score),ref="Low risk")

dependent_pfs = "Surv(PFS1_dias, staus_PFS)"
explanatory = c("AGE", "ECOG", "LIVER_METS", "INCLIVA_score", "CA_L1_cat")

mar_test_pfs <- na.omit(mar_test[,c(explanatory, "PFS1_dias", "staus_PFS")])
# 
mod1 <- coxph(Surv(PFS1_dias, staus_PFS)~ AGE + ECOG + LIVER_METS + INCLIVA_score + CA_L1_cat, data=mar_test_pfs)
summary(mod1)
step(mod1)
#  
best<-coxph(formula = Surv(PFS1_dias, staus_PFS) ~ ECOG + INCLIVA_score, 
            data = mar_test_pfs)
summary(best)
explanatory_multi = c("ECOG", "INCLIVA_score")

mar_test_pfs %>% 
  finalfit(dependent_pfs, explanatory, explanatory_multi) %>% 
  kable(row.names = F, caption= "PFS (Validation set). INCLIVA Score") # for vignette only



####    Figure 1B       ######

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(pheatmap)

colnames(PDAC_plasma_cancer_somatic)
head(PDAC_plasma_cancer_somatic)
table(DATA_CLINICA$Nº.WES)
colnames(DATA_CLINICA)

DATA_CLINICA$TIEMPO.DE.FUP..DÍAS. <- as.numeric(DATA_CLINICA$TIEMPO.DE.FUP..DÍAS.)
DATA_CLINICA <- DATA_CLINICA %>% 
  mutate(meses_fup = TIEMPO.DE.FUP..DÍAS. / 30)
columnas_deseadas <- c( "Nº.WES", "meses_fup")
nuevo_df <- DATA_CLINICA %>% 
  select(all_of(columnas_deseadas)) %>%
  filter_all(any_vars(. != ""))
nuevo_df <- nuevo_df %>% 
  mutate(fup_cat = ifelse(meses_fup > 11, "long", "short"))

wes_somaticas <- PDAC_plasma_cancer_somatic
wes_somaticas <- wes_somaticas %>%
  filter(MAX_AF == "-" | (as.numeric(MAX_AF) <= 0.01)) %>%
  filter(GENE_SYMBOL != "-")

columnas_a_eliminar <- c(
  "POSITION", "REF", "ALT", "GAF", "MAX_FREQ", "CONSEQUENCE", "IMPACT", 
  "EXON", "HGVSp", "INCLIVA_PATH_MUTATION", "HOTSPOT", "MIXED_DB", 
  "MIXED_ORIGIN", "MIXED_CLIN_SIG", "VARIANT_ID", "X1000G_AF", "MAX_AF", 
  "GNOMAD", "CLINVAR", "SIFT", "POLYPHEN", "REVIEWED_VARIANT", 
  "ONCOGENIC", "TREATMENT", "GENEINFO", "EVIDENCE_LEVEL", "PUBMED"
)
wes_somaticas <- wes_somaticas %>%
  select(-one_of(columnas_a_eliminar))

wes_somaticas <- wes_somaticas %>%
  select(matches("GENE_SYMBOL|HGVSc|FREQUENCY"))

replace_frequency_values <- function(column) {
  ifelse(is.na(column), NA, ifelse(column == "-", FALSE, TRUE))
}
wes_somaticas <- wes_somaticas %>%
  mutate_at(vars(contains("FREQUENCY")), list(~ replace_frequency_values(.)))

wes_somaticas <- wes_somaticas %>%
  mutate(HGVSc = sub("^[^:]+:", "", HGVSc))
wes_somaticas <- wes_somaticas %>%
  filter(!grepl("-", HGVSc, fixed = TRUE))


rename_columns <- function(column_name) {
  gsub("\\.", "-", gsub("^.*?\\.|_L1_FREQUENCY", "", column_name))
}
wes_somaticas <- wes_somaticas %>%
  rename_with(~rename_columns(.), -c(GENE_SYMBOL, HGVSc))

wes_somaticas_long <- wes_somaticas %>%
  pivot_longer(cols = -c(GENE_SYMBOL, HGVSc), names_to = "paciente", values_to = "valor")

merged_df <- left_join(wes_somaticas_long, nuevo_df, by = c("paciente" = "Nº.WES"))

summary_df <- merged_df %>%
  group_by(fup_cat, GENE_SYMBOL) %>%
  summarise(TRUE_count = sum(valor == TRUE, na.rm = TRUE),
            FALSE_count = sum(valor == FALSE, na.rm = TRUE))

pivot_df <- summary_df %>%
  pivot_wider(names_from = fup_cat, values_from = c("TRUE_count", "FALSE_count"), names_sep = "_")


fisher_test_results <- apply(pivot_df[, c("TRUE_count_long", "FALSE_count_long", "TRUE_count_short", "FALSE_count_short")], 1, function(row) {
  mat <- matrix(row, ncol = 2)
  fisher.test(mat)
})

fisher_test_df <- data.frame(
  GENE_SYMBOL = pivot_df$GENE_SYMBOL,
  p_value = sapply(fisher_test_results, function(x) x$p.value),
  odds_ratio = sapply(fisher_test_results, function(x) {
    odds_ratio_short = x$estimate
    return(odds_ratio_short)
  })
)


fisher_test_df$p_value_adj <- p.adjust(fisher_test_df$p_value, method = "fdr")

write.csv(fisher_test_df, file="mut_lower_than_11_months.csv")

options(repr.plot.width=8, repr.plot.height=6)
significance_level <- 0.05


plot_object <- ggplot(fisher_test_df, aes(x = log2(odds_ratio), y = -log10(p_value), label = ifelse(abs(log2(odds_ratio)) > 3 & p_value < significance_level, as.character(GENE_SYMBOL), ""))) +
  geom_point(aes(color = ifelse(p_value < significance_level, "red", "black")), alpha = 0.7) +
  geom_text_repel(
    box.padding = 0.5,
    max.overlaps = Inf,  
    force = 5,           
    segment.size = 0.2,   
    size = 3              
  ) +
  theme_minimal() +
  labs(title = "Volcano Plot",
       x = "log2(Odds Ratio)",
       y = "-log10(p-value)",
       color = "Significant") +
  scale_color_manual(values = c("black", "red")) +
  ylim(c(0, 10)) + 
  theme(legend.position = "none")

ggsave(file.path("~/Desktop", "volcano_plot_lower_11_moths.pdf"), plot_object, width = 8, height = 6)


filtered_genes <- fisher_test_df$GENE_SYMBOL[abs(fisher_test_df$odds_ratio) > 1 & fisher_test_df$p_value_adj < significance_level]
filtered_wes_somaticas <- wes_somaticas[wes_somaticas$GENE_SYMBOL %in% filtered_genes, ]
logical_columns <- sapply(filtered_wes_somaticas, is.logical)
filtered_wes_somaticas[, logical_columns] <- lapply(filtered_wes_somaticas[, logical_columns], as.numeric)
filtered_wes_somaticas$gene <- paste(filtered_wes_somaticas$GENE_SYMBOL, filtered_wes_somaticas$HGVSc, sep = "_")
filtered_wes_somaticas <- filtered_wes_somaticas[, !(names(filtered_wes_somaticas) %in% c("GENE_SYMBOL", "HGVSc"))]
rownames(filtered_wes_somaticas) <- filtered_wes_somaticas$gene
filtered_wes_somaticas$gene <- NULL 


colors <- colorRampPalette(c("grey", "blue"))(2)

plot_object <- pheatmap(
  filtered_wes_somaticas,  
  cluster_rows = FALSE,      
  show_rownames = TRUE,        
  col = colors,                   
  main = "Heatmap de Genes Significativos"
)

ggsave(file.path("~/Desktop", "volcano_plot.pdf"), plot_object, width = 8, height = 6)


#############################################################################
############################liver vs not liver###############################
#############################################################################


####    Figure 1C       ######


columnas_deseadas_2 <- c( "Nº.WES", "LOCALIZ.RECAÍDA.M1.AL.DIAG")
nuevo_df_higado <- DATA_CLINICA %>% 
  select(all_of(columnas_deseadas_2)) %>%
  filter_all(any_vars(. != ""))

nuevo_df_higado <- nuevo_df_higado %>%
  mutate(met.liver = ifelse(grepl("HIGADO", LOCALIZ.RECAÍDA.M1.AL.DIAG, ignore.case = TRUE), "higado", "no higado"))

merged_df_higado <- left_join(wes_somaticas_long, nuevo_df_higado, by = c("paciente" = "Nº.WES"))

summary_df_higado <- merged_df_higado %>%
  group_by(met.liver, GENE_SYMBOL) %>%
  summarise(TRUE_count = sum(valor == TRUE, na.rm = TRUE),
            FALSE_count = sum(valor == FALSE, na.rm = TRUE))

pivot_df_higado <- summary_df_higado %>%
  pivot_wider(names_from = met.liver, values_from = c("TRUE_count", "FALSE_count"), names_sep = "_")
colnames(pivot_df_higado)

fisher_test_results_higado <- apply(pivot_df_higado[, c("TRUE_count_higado", "FALSE_count_higado", "TRUE_count_no higado", "FALSE_count_no higado")], 1, function(row) {
  mat <- matrix(row, ncol = 2)
  fisher.test(mat)
})

fisher_test_results_higado <- data.frame(
  GENE_SYMBOL = pivot_df_higado$GENE_SYMBOL,
  p_value = sapply(fisher_test_results_higado, function(x) x$p.value),
  odds_ratio = sapply(fisher_test_results_higado, function(x) {
    odds_ratio_higado = x$estimate
    return(odds_ratio_higado)
  })
)


fisher_test_results_higado$p_value_adj <- p.adjust(fisher_test_results_higado$p_value, method = "fdr")

write.csv(fisher_test_results_higado, file="mut_patients_higado.csv")

options(repr.plot.width=8, repr.plot.height=6)
significance_level <- 0.01


plot_object <- ggplot(fisher_test_results_higado, aes(x = log2(odds_ratio), y = -log10(p_value), label = ifelse(abs(log2(odds_ratio)) > 3 & p_value < significance_level, as.character(GENE_SYMBOL), ""))) +
  geom_point(aes(color = ifelse(p_value < significance_level, "red", "black")), alpha = 0.7) +
  geom_text_repel(
    box.padding = 0.5,
    max.overlaps = Inf,  
    force = 5,           
    segment.size = 0.2,   
    size = 3              
  ) +
  theme_minimal() +
  labs(title = "Volcano Plot",
       x = "log2(Odds Ratio)",
       y = "-log10(p-value)",
       color = "Significant") +
  scale_color_manual(values = c("black", "red")) +
  ylim(c(0, 10)) + 
  theme(legend.position = "none")

ggsave(file.path("~/Desktop", "volcano_plot_higado.pdf"), plot_object, width = 8, height = 6)
