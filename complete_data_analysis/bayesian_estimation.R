#https://cran.r-project.org/web/packages/BEST/vignettes/BEST.pdf

library(BEST)

a1 <- c(5.77, 5.33, 4.59, 4.33, 3.66, 4.48)
a2 <- c(3.88, 3.55, 3.29, 2.59, 2.33, 3.59)

prior_vals <- list(muM = 10.5, muSD = 11,sigmaMode = 11,sigmaSD=8.5)

BESTout <- BESTmcmc(y1, y2, priors=prior_vals, parallel=FALSE)

# Difference in Means Plot
plot(BESTout)

# Differences in Means Standard Deviation
plot(BESTout, which="sd")
plot(BESTout, compVal=1, ROPE=c(-2,1))
plotAll(BESTout)

#Summaries
summary(BESTout)
summary(BESTout, credMass=0.8, ROPEm=c(-0.1,0.1), ROPEsd=c(-0.15,0.15),compValeff=1)

class(BESTout)

print(BESTout)

plotPostPred(BESTout)

meanDiff <- (BESTout$mu1 - BESTout$mu2)
meanDiffGTzero <- mean(meanDiff > 0)


treatment_dr <- only_sms_dr[treatment == "Treatment"]
treatment_dr_list <- treatment_dr$month_diff

control_dr <- only_sms_dr[treatment == "Control"]
control_dr_list <- control_dr$month_diff







time_series_data_table <- data_time_series_nooutliers.dt
energia_text_var <-'energia'
energy_file <- 'no_outliers'


count.plot.list <- list()
count.plot.name <- list()
text.list <- list()
count.plot <- 0
options(warn=-1)

###### Monthly Differences one Year Afterwards Energia   
month_diffs_dt <- month_diffs(time_series_data_table,energia_text_var)
# Adding variables to the dt 
month_diffs_dt$treatment_v2 <- ifelse(month_diffs_dt$Casa == 'A3'| month_diffs_dt$Casa == 'A6'| month_diffs_dt$Casa == 'A11'| month_diffs_dt$Casa == 'A12'| month_diffs_dt$Casa == 'A17'| month_diffs_dt$Casa == 'A18'| month_diffs_dt$Casa == 'A20'| month_diffs_dt$Casa == 'A21'| month_diffs_dt$Casa == 'A25'|  month_diffs_dt$Casa == 'A26','Treatment - Won Information',ifelse(month_diffs_dt$Casa == 'A1' | month_diffs_dt$Casa == 'A7' | month_diffs_dt$Casa == 'A9' | month_diffs_dt$Casa == 'A14' | month_diffs_dt$Casa == 'A16' | month_diffs_dt$Casa == 'A19' | month_diffs_dt$Casa == 'A22' | month_diffs_dt$Casa == 'A24' | month_diffs_dt$Casa == 'A28' | month_diffs_dt$Casa == 'A29','Treatment - Lost Information','Control'))
month_diffs_dt$only_paper <- ifelse(month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Febrero' | month_diffs_dt$Ano == 2016 &  month_diffs_dt$Mes == 'Marzo' | month_diffs_dt$Ano == 2016 &  month_diffs_dt$Mes == 'Abril' | month_diffs_dt$Ano == 2016 &  month_diffs_dt$Mes == 'Mayo','Only Paper Intervention','Other')
month_diffs_dt$sms_dr <- ifelse(month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Junio' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Julio' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Agosto' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Septiembre' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Octubre' ,'DR Intervetnion + SMS','Other')
month_diffs_dt$all_months_intervention <- ifelse(month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Junio' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Julio' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Agosto' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Septiembre' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Octubre' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Noviembre' | month_diffs_dt$Ano == 2016 & month_diffs_dt$Mes == 'Diciembre','DR Intervetnion + SMS All','Other')

only_paper <- subset(month_diffs_dt,month_diffs_dt$only_paper == "Only Paper Intervention")
only_sms_dr <- subset(month_diffs_dt,month_diffs_dt$sms_dr == "DR Intervetnion + SMS")
only_sms_dr_full <- subset(month_diffs_dt,month_diffs_dt$all_months_intervention == "DR Intervetnion + SMS All")

sub_treat <- subset(only_sms_dr,only_sms_dr$treatment == "Treatment")
sub_control  <- subset(only_sms_dr,only_sms_dr$treatment == "Control")

y1 <- sub_treat[['month_diff']]
y2 <- sub_control[['month_diff']]


# Two Variable Plots
only_paper_tr_cl <- call_plot_subset(only_paper,'treatment','Treatment','Control','month_diff',"Monthly Annual Differences (kWh)","Density","Only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_tr_cl
count.plot.name[[count.plot]] = 'only_paper_tr_cl' 

only_dr_sms_tr_cl <- call_plot_subset(only_sms_dr,'treatment','Treatment','Control','month_diff',"Monthly Annual Differences (kWh)","Density","DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_tr_cl
count.plot.name[[count.plot]] = 'only_dr_sms_tr_cl' 

only_dr_sms_full_tr_cl <- call_plot_subset(only_sms_dr_full,'treatment','Treatment','Control','month_diff',"Monthly Annual Differences (kWh)","Density","DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_tr_cl
count.plot.name[[count.plot]] = 'only_dr_sms_full_tr_cl' 

# Three Variable PLots
only_paper_info <- call_plot_subset_three(only_paper,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','month_diff','Monthly Annual Differences (kWh)','Density',"Only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_info
count.plot.name[[count.plot]] = 'only_paper_info' 

only_dr_sms_info <- call_plot_subset_three(only_sms_dr,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','month_diff','Monthly Annual Differences (kWh)','Density',"DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_info
count.plot.name[[count.plot]] = 'only_dr_sms_info' 

only_dr_sms_full_info <- call_plot_subset_three(only_sms_dr_full,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','month_diff','Monthly Annual Differences (kWh)','Density','DR + SMS Full Implementation')
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_info
count.plot.name[[count.plot]] = 'only_dr_sms_full_info' 

# Five Variable PLots
only_paper_info_wtp <- call_plot_subset_five(only_paper,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Differences (kWh)","Density","WTP: Only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_info_wtp
count.plot.name[[count.plot]] = 'only_paper_info_wtp' 

only_dr_sms_info_wtp<- call_plot_subset_five(only_sms_dr,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Differences (kWh)","Density","WTP: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_info_wtp
count.plot.name[[count.plot]] = 'only_dr_sms_info_wtp' 

only_dr_sms_full_info_wtp <- call_plot_subset_five(only_sms_dr_full,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Differences (kWh)","Density","WTP: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_info_wtp
count.plot.name[[count.plot]] = 'only_dr_sms_full_info_wtp' 

only_paper_info_wtp_fr <-call_plot_subset_five(only_paper,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Differences (kWh)","Density","WTP Fraction: Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_info_wtp_fr
count.plot.name[[count.plot]] = 'only_paper_info_wtp_fr' 

only_dr_sms_info_wtp_fr <- call_plot_subset_five(only_sms_dr,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Differences (kWh)","Density","WTP Fraction: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_info_wtp_fr
count.plot.name[[count.plot]] = 'only_dr_sms_info_wtp_fr' 

only_dr_sms_full_info_wtp_fr <-call_plot_subset_five(only_sms_dr_full,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Differences (kWh)","Density","WTP Fraction: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_info_wtp_fr
count.plot.name[[count.plot]] = 'only_dr_sms_full_info_wtp_fr' 


###### Monthly Differences one Year Afterwards Cost
month_diffs_cost_dt <- month_diffs(time_series_data_table,'importe_dl')
# Adding variables to the dt 
month_diffs_cost_dt$treatment_v2 <- ifelse(month_diffs_cost_dt$Casa == 'A3'| month_diffs_cost_dt$Casa == 'A6'| month_diffs_cost_dt$Casa == 'A11'| month_diffs_cost_dt$Casa == 'A12'| month_diffs_cost_dt$Casa == 'A17'| month_diffs_cost_dt$Casa == 'A18'| month_diffs_cost_dt$Casa == 'A20'| month_diffs_cost_dt$Casa == 'A21'| month_diffs_cost_dt$Casa == 'A25'|  month_diffs_cost_dt$Casa == 'A26','Treatment - Won Information',ifelse(month_diffs_cost_dt$Casa == 'A1' | month_diffs_cost_dt$Casa == 'A7' | month_diffs_cost_dt$Casa == 'A9' | month_diffs_cost_dt$Casa == 'A14' | month_diffs_cost_dt$Casa == 'A16' | month_diffs_cost_dt$Casa == 'A19' | month_diffs_cost_dt$Casa == 'A22' | month_diffs_cost_dt$Casa == 'A24' | month_diffs_cost_dt$Casa == 'A28' | month_diffs_cost_dt$Casa == 'A29','Treatment - Lost Information','Control'))
month_diffs_cost_dt$only_paper <- ifelse(month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Febrero' | month_diffs_cost_dt$Ano == 2016 &  month_diffs_cost_dt$Mes == 'Marzo' | month_diffs_cost_dt$Ano == 2016 &  month_diffs_cost_dt$Mes == 'Abril' | month_diffs_cost_dt$Ano == 2016 &  month_diffs_cost_dt$Mes == 'Mayo','Only Paper Intervention','Other')
month_diffs_cost_dt$sms_dr <- ifelse(month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Junio' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Julio' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Agosto' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Septiembre' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Octubre' ,'DR Intervetnion + SMS','Other')
month_diffs_cost_dt$all_months_intervention <- ifelse(month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Junio' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Julio' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Agosto' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Septiembre' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Octubre' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Noviembre' | month_diffs_cost_dt$Ano == 2016 & month_diffs_cost_dt$Mes == 'Diciembre','DR Intervetnion + SMS All','Other')

only_paper_ct <- subset(month_diffs_cost_dt,month_diffs_cost_dt$only_paper == "Only Paper Intervention")
only_sms_dr_ct <- subset(month_diffs_cost_dt,month_diffs_cost_dt$sms_dr == "DR Intervetnion + SMS")
only_sms_dr_full_ct <- subset(month_diffs_cost_dt,month_diffs_cost_dt$all_months_intervention == "DR Intervetnion + SMS All")

# Two Variable Plots
only_paper_tr_cl_ct <- call_plot_subset(only_paper_ct,'treatment','Treatment','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_tr_cl_ct
count.plot.name[[count.plot]] = 'only_paper_tr_cl_ct' 

only_dr_sms_tr_cl_ct <- call_plot_subset(only_sms_dr_ct,'treatment','Treatment','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_tr_cl_ct
count.plot.name[[count.plot]] = 'only_dr_sms_tr_cl_ct' 

only_dr_sms_full_tr_cl_ct <- call_plot_subset(only_sms_dr_full_ct,'treatment','Treatment','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_tr_cl_ct
count.plot.name[[count.plot]] = 'only_dr_sms_full_tr_cl_ct' 

# Three Variable PLots
only_paper_info_ct <- call_plot_subset_three(only_paper_ct,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','month_diff','Same Month Annual Cost Differences ($US)','Density',"only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_info_ct
count.plot.name[[count.plot]] = 'only_paper_info_ct' 

only_dr_sms_info_ct <- call_plot_subset_three(only_sms_dr_ct,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','month_diff','Same Month Annual Cost Differences ($US)','Density',"DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_info_ct
count.plot.name[[count.plot]] = 'only_dr_sms_info_ct' 

only_dr_sms_full_info_ct <- call_plot_subset_three(only_sms_dr_full_ct,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','month_diff','Same Month Annual Cost Differences ($US)','Density','DR + SMS Full Implementation')
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_info_ct
count.plot.name[[count.plot]] = 'only_dr_sms_full_info_ct' 

# Five Variable PLots
only_paper_info_wtp_ct <- call_plot_subset_five(only_paper_ct,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","WTP: only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_info_wtp_ct
count.plot.name[[count.plot]] = 'only_paper_info_wtp_ct' 

only_dr_sms_info_wtp_ct <- call_plot_subset_five(only_sms_dr_ct,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","WTP: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_info_wtp_ct
count.plot.name[[count.plot]] = 'only_dr_sms_info_wtp_ct' 

only_dr_sms_full_info_wtp_ct <- call_plot_subset_five(only_sms_dr_full_ct,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","WTP: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_info_wtp_ct
count.plot.name[[count.plot]] = 'only_dr_sms_full_info_wtp_ct' 

only_paper_info_wtp_fr_ct <-call_plot_subset_five(only_paper_ct,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","WTP Fraction: only Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_paper_info_wtp_fr_ct
count.plot.name[[count.plot]] = 'only_paper_info_wtp_fr_ct' 

only_dr_sms_info_wtp_fr_ct <- call_plot_subset_five(only_sms_dr_ct,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","WTP Fraction: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_info_wtp_fr_ct
count.plot.name[[count.plot]] = 'only_dr_sms_info_wtp_fr_ct' 

only_dr_sms_full_info_wtp_fr_ct <-call_plot_subset_five(only_sms_dr_full_ct,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','month_diff',"Same Month Annual Cost Differences ($US)","Density","WTP Fraction: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = only_dr_sms_full_info_wtp_fr_ct
count.plot.name[[count.plot]] = 'only_dr_sms_full_info_wtp_fr_ct' 



###### Month by Month Analysis Energia
month_by_month_dt <- month_by_month(time_series_data_table,energia_text_var)

# Adding variables to the dt 
month_by_month_dt$treatment_v2 <- ifelse(month_by_month_dt$Casa == 'A3'| month_by_month_dt$Casa == 'A6'| month_by_month_dt$Casa == 'A11'| month_by_month_dt$Casa == 'A12'| month_by_month_dt$Casa == 'A17'| month_by_month_dt$Casa == 'A18'| month_by_month_dt$Casa == 'A20'| month_by_month_dt$Casa == 'A21'| month_by_month_dt$Casa == 'A25'|  month_by_month_dt$Casa == 'A26','Treatment - Won Information',ifelse(month_by_month_dt$Casa == 'A1' | month_by_month_dt$Casa == 'A7' | month_by_month_dt$Casa == 'A9' | month_by_month_dt$Casa == 'A14' | month_by_month_dt$Casa == 'A16' | month_by_month_dt$Casa == 'A19' | month_by_month_dt$Casa == 'A22' | month_by_month_dt$Casa == 'A24' | month_by_month_dt$Casa == 'A28' | month_by_month_dt$Casa == 'A29','Treatment - Lost Information','Control'))
month_by_month_dt$mbm_paper <- ifelse(month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Febrero' | month_by_month_dt$Ano == 2016 &  month_by_month_dt$Mes == 'Marzo' | month_by_month_dt$Ano == 2016 &  month_by_month_dt$Mes == 'Abril' | month_by_month_dt$Ano == 2016 &  month_by_month_dt$Mes == 'Mayo','Only Paper Intervention','Other')
month_by_month_dt$sms_dr <- ifelse(month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Junio' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Julio' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Agosto' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Septiembre' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Octubre' ,'DR Intervetnion + SMS','Other')
month_by_month_dt$all_months_intervention <- ifelse(month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Junio' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Julio' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Agosto' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Septiembre' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Octubre' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Noviembre' | month_by_month_dt$Ano == 2016 & month_by_month_dt$Mes == 'Diciembre','DR Intervetnion + SMS All','Other')

mbm_paper <- subset(month_by_month_dt,month_by_month_dt$mbm_paper == "Only Paper Intervention")
mbm_sms_dr <- subset(month_by_month_dt,month_by_month_dt$sms_dr == "DR Intervetnion + SMS")
mbm_sms_dr_full <- subset(month_by_month_dt,month_by_month_dt$all_months_intervention == "DR Intervetnion + SMS All")

# Two Variable Plots
mbm_paper_tr_cl <- call_plot_subset(mbm_paper,'treatment','Treatment','Control','diff_variable',"Month by Month Differences (kWh)","Density","Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_tr_cl
count.plot.name[[count.plot]] = 'mbm_paper_tr_cl' 

mbm_dr_sms_tr_cl <- call_plot_subset(mbm_sms_dr,'treatment','Treatment','Control','diff_variable',"Month by Month Differences (kWh)","Density","DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_tr_cl
count.plot.name[[count.plot]] = 'mbm_dr_sms_tr_cl' 

mbm_dr_sms_full_tr_cl <- call_plot_subset(mbm_sms_dr_full,'treatment','Treatment','Control','diff_variable',"Month by Month Differences (kWh)","Density","DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_tr_cl
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_tr_cl' 

# Three Variable PLots
mbm_paper_info <- call_plot_subset_three(mbm_paper,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','diff_variable','Month by Month Differences (kWh)','Density',"Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_info
count.plot.name[[count.plot]] = 'mbm_paper_info' 

mbm_dr_sms_info <- call_plot_subset_three(mbm_sms_dr,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','diff_variable','Month by Month Differences (kWh)','Density',"DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_info
count.plot.name[[count.plot]] = 'mbm_dr_sms_info' 

mbm_dr_sms_full_info <- call_plot_subset_three(mbm_sms_dr_full,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','diff_variable','Month by Month Differences (kWh)','Density','DR + SMS Full Implementation')
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_info
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_info' 

# Five Variable PLots
mbm_paper_info_wtp <- call_plot_subset_five(mbm_paper,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Differences (kWh)","Density","WTP: Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_info_wtp
count.plot.name[[count.plot]] = 'mbm_paper_info_wtp' 

mbm_dr_sms_info_wtp<- call_plot_subset_five(mbm_sms_dr,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Differences (kWh)","Density","WTP: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_info_wtp
count.plot.name[[count.plot]] = 'mbm_dr_sms_info_wtp' 

mbm_dr_sms_full_info_wtp <- call_plot_subset_five(mbm_sms_dr_full,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Differences (kWh)","Density","WTP: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_info_wtp
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_info_wtp' 

mbm_paper_info_wtp_fr <-call_plot_subset_five(mbm_paper,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Differences (kWh)","Density","WTP Fraction:Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_info_wtp_fr
count.plot.name[[count.plot]] = 'mbm_paper_info_wtp_fr' 

mbm_dr_sms_info_wtp_fr <- call_plot_subset_five(mbm_sms_dr,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Differences (kWh)","Density","WTP Fraction: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_info_wtp_fr
count.plot.name[[count.plot]] = 'mbm_dr_sms_info_wtp_fr' 

mbm_dr_sms_full_info_wtp_fr <-call_plot_subset_five(mbm_sms_dr_full,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Differences (kWh)","Density","WTP Fraction: Post DR + SMS Full Implementatio")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_info_wtp_fr
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_info_wtp_fr' 

###### Month by Month Analysis Costo
time_series_data_table$importe_dl <- as.numeric(time_series_data_table$importe_dl)
month_by_month_costo_dt <- month_by_month(time_series_data_table,'importe_dl')

# Adding variables to the dt 
month_by_month_costo_dt$treatment_v2 <- ifelse(month_by_month_costo_dt$Casa == 'A3'| month_by_month_costo_dt$Casa == 'A6'| month_by_month_costo_dt$Casa == 'A11'| month_by_month_costo_dt$Casa == 'A12'| month_by_month_costo_dt$Casa == 'A17'| month_by_month_costo_dt$Casa == 'A18'| month_by_month_costo_dt$Casa == 'A20'| month_by_month_costo_dt$Casa == 'A21'| month_by_month_costo_dt$Casa == 'A25'|  month_by_month_costo_dt$Casa == 'A26','Treatment - Won Information',ifelse(month_by_month_costo_dt$Casa == 'A1' | month_by_month_costo_dt$Casa == 'A7' | month_by_month_costo_dt$Casa == 'A9' | month_by_month_costo_dt$Casa == 'A14' | month_by_month_costo_dt$Casa == 'A16' | month_by_month_costo_dt$Casa == 'A19' | month_by_month_costo_dt$Casa == 'A22' | month_by_month_costo_dt$Casa == 'A24' | month_by_month_costo_dt$Casa == 'A28' | month_by_month_costo_dt$Casa == 'A29','Treatment - Lost Information','Control'))
month_by_month_costo_dt$mbm_paper <- ifelse(month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Febrero' | month_by_month_costo_dt$Ano == 2016 &  month_by_month_costo_dt$Mes == 'Marzo' | month_by_month_costo_dt$Ano == 2016 &  month_by_month_costo_dt$Mes == 'Abril' | month_by_month_costo_dt$Ano == 2016 &  month_by_month_costo_dt$Mes == 'Mayo','Only Paper Intervention','Other')
month_by_month_costo_dt$sms_dr <- ifelse(month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Junio' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Julio' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Agosto' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Septiembre' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Octubre' ,'DR Intervetnion + SMS','Other')
month_by_month_costo_dt$all_months_intervention <- ifelse(month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Junio' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Julio' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Agosto' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Septiembre' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Octubre' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Noviembre' | month_by_month_costo_dt$Ano == 2016 & month_by_month_costo_dt$Mes == 'Diciembre','DR Intervetnion + SMS All','Other')

mbm_paper <- subset(month_by_month_costo_dt,month_by_month_costo_dt$mbm_paper == "Only Paper Intervention")
mbm_sms_dr <- subset(month_by_month_costo_dt,month_by_month_costo_dt$sms_dr == "DR Intervetnion + SMS")
mbm_sms_dr_full <- subset(month_by_month_costo_dt,month_by_month_costo_dt$all_months_intervention == "DR Intervetnion + SMS All")

# Two Variable Plots
mbm_paper_tr_cl_ct <- call_plot_subset(mbm_paper,'treatment','Treatment','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_tr_cl_ct
count.plot.name[[count.plot]] = 'mbm_paper_tr_cl_ct' 

mbm_dr_sms_tr_cl_ct <- call_plot_subset(mbm_sms_dr,'treatment','Treatment','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_tr_cl_ct
count.plot.name[[count.plot]] = 'mbm_dr_sms_tr_cl_ct' 

mbm_dr_sms_full_tr_cl_ct <- call_plot_subset(mbm_sms_dr_full,'treatment','Treatment','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_tr_cl_ct
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_tr_cl_ct' 

# Three Variable PLots
mbm_paper_info_dt <- call_plot_subset_three(mbm_paper,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','diff_variable','Month by Month Cost Differences ($US)','Density',"Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_info_dt
count.plot.name[[count.plot]] = 'mbm_paper_info_dt' 

mbm_dr_sms_info_dt <- call_plot_subset_three(mbm_sms_dr,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','diff_variable','Month by Month Cost Differences ($US)','Density',"DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_info_dt
count.plot.name[[count.plot]] = 'mbm_dr_sms_info_dt' 

mbm_dr_sms_full_info_dt <- call_plot_subset_three(mbm_sms_dr_full,'treatment_v2','Treatment - Won Information','Treatment - Lost Information','Control','diff_variable','Month by Month Cost Differences ($US)','Density','DR + SMS Full Implementation')
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_info_dt
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_info_dt' 

# Five Variable PLots
mbm_paper_info_wtp_dt <- call_plot_subset_five(mbm_paper,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","WTP: Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_info_wtp_dt
count.plot.name[[count.plot]] = 'mbm_paper_info_wtp_dt' 

mbm_dr_sms_info_wtp_dt <- call_plot_subset_five(mbm_sms_dr,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","WTP: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_info_wtp_dt
count.plot.name[[count.plot]] = 'mbm_dr_sms_info_wtp_dt' 

mbm_dr_sms_full_info_wtp_dt <- call_plot_subset_five(mbm_sms_dr_full,"wtp_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","WTP: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_info_wtp_dt
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_info_wtp_dt' 

mbm_paper_info_wtp_fr_dt <-call_plot_subset_five(mbm_paper,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","WTP Fraction:Paper Reports")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_paper_info_wtp_fr_dt
count.plot.name[[count.plot]] = 'mbm_paper_info_wtp_fr_dt' 

mbm_dr_sms_info_wtp_fr_dt <- call_plot_subset_five(mbm_sms_dr,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","WTP Fraction: Post DR + SMS Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_info_wtp_fr_dt
count.plot.name[[count.plot]] = 'mbm_dr_sms_info_wtp_fr_dt' 

mbm_dr_sms_full_info_wtp_fr_dt <-call_plot_subset_five(mbm_sms_dr_full,"fraction_lw_md_h",'low','high','medium-low','medium-high','Control','diff_variable',"Month by Month Cost Differences ($US)","Density","WTP Fraction: Post DR + SMS Full Implementation")
count.plot <- count.plot + 1
count.plot.list[[count.plot]] = mbm_dr_sms_full_info_wtp_fr_dt
count.plot.name[[count.plot]] = 'mbm_dr_sms_full_info_wtp_fr_dt' 

for (j in 1:count.plot) {
  plot.name = count.plot.name[[j]]
  mypath <- file.path("/Users/diego/Desktop/Projects/nicaragua_dr_ee_behavior/plots/treatment_control",energy_file,energia_text_var,paste(plot.name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(count.plot.list[[j]])
  dev.off()
}

}



