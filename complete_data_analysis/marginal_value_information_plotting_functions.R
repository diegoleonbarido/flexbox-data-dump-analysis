######### Plotting functions for the Marginal Value of Information

#########  1. Timeseries Plots for Treatment and Control 

plot_energy_cost_ft <- function(treatment_time_series_data_table,control_time_series_data_table,data_time_series_data_table,energia_text_var,energy_file,energy_var){
  
  count.plot.list <- list()
  count.plot.name <- list()
  text.list <- list()
  count.plot <- 0
  options(warn=-1)

time_series_energy_treatment <- ggplot(subset(treatment_time_series_data_table,treatment_time_series_data_table$fecha>"2015-06-01" & treatment_time_series_data_table$fecha<"2017-02-01"),aes(fecha,get(energia_text_var),group=Casa,colour=Casa)) + geom_path(alpha=0.5) + xlab("Date") + ylab("Monthly Energy Consumption (kwh)") + ggtitle("Treatment Group") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + labs(colour= 'ID') + theme(legend.key = element_rect(fill = "white")) +  theme(legend.position="bottom")
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = time_series_energy_treatment
    count.plot.name[[count.plot]] = 'time_series_energy_treatment'

time_series_energy_control <- ggplot(subset(control_time_series_data_table,control_time_series_data_table$fecha>"2015-09-01"),aes(fecha,get(energia_text_var),group=Casa,colour=Casa)) + geom_path(alpha=0.5) + xlab("Date") + ylab("Monthly Energy Consumption (kwh)") +  ggtitle("Control Group") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + labs(colour= 'ID') + theme(legend.key = element_rect(fill = "white")) +  theme(legend.position="bottom")
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = time_series_energy_control
    count.plot.name[[count.plot]] = 'time_series_energy_control'
    
time_series_importe_treatment <-ggplot(subset(treatment_time_series_data_table,treatment_time_series_data_table$fecha>"2015-06-01" & treatment_time_series_data_table$fecha<"2017-02-01"),aes(fecha,importe_dl,group=Casa,colour=Casa)) + geom_path(alpha=0.5) +  xlab("Date") + ylab("Monthly Energy Cost ($US)") + ggtitle("Treatment Group") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + labs(colour= 'ID') + theme(legend.key = element_rect(fill = "white")) +  theme(legend.position="bottom")
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = time_series_importe_treatment
    count.plot.name[[count.plot]] = 'time_series_importe_treatment'

time_series_importe_control <-ggplot(control_time_series_data_table,aes(fecha,importe_dl,group=Casa,colour=Casa)) + geom_path(alpha=0.5) + xlab("Date") + ylab("Monthly Energy Cost ($US)") +  ggtitle("Control Group") + theme(panel.background = element_blank(),axis.text=element_text(size=13),axis.title=element_text(size=14,face="bold")) + labs(colour= 'ID') + theme(legend.key = element_rect(fill = "white")) +  theme(legend.position="bottom")
    count.plot <- count.plot + 1
    count.plot.list[[count.plot]] = time_series_importe_control
    count.plot.name[[count.plot]] = 'time_series_importe_control'   
    
    
    # Densities Energy and Cost
    
    # All Data
    densities_t_c_e <- call_plot_subset(data_time_series_data_table,"treatment","Treatment","Control",energia_text_var,"Monthly Energy Consumption (kWh)","Density","All Data (kWH/Month): Treatment vs. Control")
        count.plot <- count.plot + 1
        count.plot.list[[count.plot]] = densities_t_c_e
        count.plot.name[[count.plot]] = 'densities_t_c_e'  
    
    densities_t_c_c <- call_plot_subset(data_time_series_data_table,"treatment","Treatment","Control","importe_dl","Monthly Energy Expenditure ($US)","Density","All Data ($US/Month): Treatment vs. Control")
        count.plot <- count.plot + 1
        count.plot.list[[count.plot]] = densities_t_c_c
        count.plot.name[[count.plot]] = 'densities_t_c_c'  
        
        
        # Pre & Post Implementation 
        
        ### All data for ENERGY treatment and control
        
        # Post and Pre Implementation
        post_pre_all_data_tr <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399],"intervention_group","Treatment Pre-Intervention","Treatment Post-Intervention",energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post and Pre-Implementation (kWH/Month): Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = post_pre_all_data_tr
              count.plot.name[[count.plot]] = 'post_pre_all_data_tr' 
        
        post_pre_all_data_cl <- call_plot_subset(control_time_series_data_table,"intervention_group","Control Pre-Intervention","Control Post-Intervention",energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post and Pre-Implementation (kWH/Month): Contol")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = post_pre_all_data_cl
              count.plot.name[[count.plot]] = 'post_pre_all_data_cl' 
        
        #Months with and without PAPER ENERGY REPORTS
        paper_all_data_tr <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"intervention_group","Treatment Pre-Intervention","Treatment Post-Intervention","energia_ajustada","Monthly Energy Consumption (kWh)","Density","Post Paper Reports Pre and Post Intervention: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_tr
              count.plot.name[[count.plot]] = 'paper_all_data_tr' 
        
        paper_all_data_info_group <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"information_group","Lost Willingness to Pay Information Bid","Won Willingness to Pay Information Bid" ,energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post Paper Reports - Won or Lost the WTP Bid: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_info_group
              count.plot.name[[count.plot]] = 'paper_all_data_info_group' 
              
        paper_all_data_wtp_tr <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"wtp_lw_md_h",'low','high','medium-low','medium-high',energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post SMS - Won or Lost the WTP Bid: Treatment")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = paper_all_data_wtp_tr
            count.plot.name[[count.plot]] = 'paper_all_data_wtp_tr'       
        
        paper_all_data_wtpfraction_tr <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"fraction_lw_md_h",'low','high','medium-low','medium-high' ,energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post Paper Reports - Won or Lost the WTP Bid, Fraction of Bill: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_wtpfraction_tr
              count.plot.name[[count.plot]] = 'paper_all_data_wtpfraction_tr' 
        
        paper_all_data_cl <- call_plot_subset(time.series.receipt.control.dt[report_intervention_month==1],"intervention_group","Control Pre-Intervention","Control Post-Intervention",energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post Paper Reports Pre and Post Intervention: Control")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_cl
              count.plot.name[[count.plot]] = 'paper_all_data_cl' 
        
        #Months with and without SMS
        sms_all_data_tr <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & sms_intervention_month==1],"intervention_group","Treatment Pre-Intervention","Treatment Post-Intervention",energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post SMS Pre and Post Intervention: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_tr
              count.plot.name[[count.plot]] = 'sms_all_data_tr' 
      
        sms_all_data_info_group <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & sms_intervention_month==1],"information_group","Lost Willingness to Pay Information Bid","Won Willingness to Pay Information Bid" ,energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post SMS - Won or Lost the WTP Bid: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_info_group
              count.plot.name[[count.plot]] = 'sms_all_data_info_group' 
              
        sms_all_data_wtp_tr <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & sms_intervention_month==1],"wtp_lw_md_h",'low','high','medium-low','medium-high',energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post SMS - Won or Lost the WTP Bid: Treatment")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = sms_all_data_wtp_tr
            count.plot.name[[count.plot]] = 'sms_all_data_wtp_tr' 
              
        
        sms_all_data_wtpfraction_tr <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & sms_intervention_month==1],"fraction_lw_md_h",'low','high','medium-low','medium-high' ,energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post SMS - Won or Lost the WTP Bid, Fraction of Bill: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_wtpfraction_tr
              count.plot.name[[count.plot]] = 'sms_all_data_wtpfraction_tr' 
        
        sms_all_data_cl <- call_plot_subset(control_time_series_data_table[sms_intervention_month==1],"intervention_group","Control Pre-Intervention","Control Post-Intervention",energia_text_var,"Monthly Energy Consumption (kWh)","Density","Post SMS - Pre and Post Intervention: Control")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_cl
              count.plot.name[[count.plot]] = 'sms_all_data_cl' 
              
              
        ### All data for COSTS treatment and control
        
        # Post and Pre Implementation 
        post_pre_all_data_tr_cost <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399],"intervention_group","Treatment Pre-Intervention","Treatment Post-Intervention","importe_dl","Monthly Energy Costs ($US)","Density","Post and Pre-Implementation ($US/Month): Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = post_pre_all_data_tr_cost
              count.plot.name[[count.plot]] = 'post_pre_all_data_tr_cost' 
        
        
        post_pre_all_data_cl_cost <- call_plot_subset(control_time_series_data_table,"intervention_group","Control Pre-Intervention","Control Post-Intervention","importe_dl","Monthly Energy Costs ($US)","Density","Post and Pre-Implementation ($US/Month): Contol")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = post_pre_all_data_cl_cost
              count.plot.name[[count.plot]] = 'post_pre_all_data_cl_cost' 
        
        #Months with and without PAPER ENERGY REPORTS
        paper_all_data_tr_cost <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"intervention_group","Treatment Pre-Intervention","Treatment Post-Intervention","importe_dl","Monthly Energy Cost ($US)","Density","Post Paper Reports Pre and Post Intervention: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_tr_cost
              count.plot.name[[count.plot]] = 'paper_all_data_tr_cost' 
       
        paper_all_data_info_group <- call_plot_subset(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"information_group","Lost Willingness to Pay Information Bid","Won Willingness to Pay Information Bid" ,"importe_dl","Monthly Energy Cost ($US)","Density","Post Paper Reports - Won or Lost the WTP Bid: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_info_group
              count.plot.name[[count.plot]] = 'paper_all_data_info_group' 
              
              
        paper_all_data_wtp_tr <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"wtp_lw_md_h",'low','high','medium-low','medium-high',"importe_dl","Monthly Energy Cost ($US)","Density","Post SMS - Won or Lost the WTP Bid: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = paper_all_data_wtp_tr
              count.plot.name[[count.plot]] = 'paper_all_data_wtp_tr' 
        
        paper_all_data_wtpfraction_tr_cost <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & report_intervention_month==1],"fraction_lw_md_h",'low','high','medium-low','medium-high' ,"importe_dl","Monthly Energy Cost ($US)","Density","Post Paper Reports - Won or Lost the WTP Bid, Fraction of Bill: Treatment")
                count.plot <- count.plot + 1
                count.plot.list[[count.plot]] = paper_all_data_wtpfraction_tr_cost
                count.plot.name[[count.plot]] = 'paper_all_data_wtpfraction_tr_cost' 

        paper_all_data_cl_cost <- call_plot_subset(time.series.receipt.control.dt[report_intervention_month==1],"intervention_group","Control Pre-Intervention","Control Post-Intervention","importe_dl","Monthly Energy Cost ($US)","Density","Post Paper Reports Pre and Post Intervention: Control")
                count.plot <- count.plot + 1
                count.plot.list[[count.plot]] = paper_all_data_cl_cost
                count.plot.name[[count.plot]] = 'paper_all_data_cl_cost' 
        
        #Months with and without SMS
        sms_all_data_tr_cost <- call_plot_subset(treatment_time_series_data_table[sms_intervention_month==1],"intervention_group","Treatment Pre-Intervention","Treatment Post-Intervention","importe_dl","Monthly Energy Cost ($US)","Density","Post SMS Pre and Post Intervention: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_tr_cost
              count.plot.name[[count.plot]] = 'sms_all_data_tr_cost' 
        
        sms_all_data_info_group <- call_plot_subset(treatment_time_series_data_table[sms_intervention_month==1],"information_group","Lost Willingness to Pay Information Bid","Won Willingness to Pay Information Bid" ,"importe_dl","Monthly Energy Cost ($US)","Density","Post SMS - Won or Lost the WTP Bid: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_info_group
              count.plot.name[[count.plot]] = 'sms_all_data_info_group' 
              
              
        sms_all_data_wtp_tr <- call_plot_subset_several(treatment_time_series_data_table[get(energia_text_var)<398 | get(energia_text_var)>=399 & sms_intervention_month==1],"wtp_lw_md_h",'low','high','medium-low','medium-high',"importe_dl","Monthly Energy Cost ($US)","Density","Post SMS - Won or Lost the WTP Bid: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_wtp_tr
              count.plot.name[[count.plot]] = 'sms_all_data_wtp_tr' 
        
        sms_all_data_wtpfraction_tr_cost <- call_plot_subset_several(treatment_time_series_data_table[sms_intervention_month==1],"fraction_lw_md_h",'low','high','medium-low','medium-high' ,"importe_dl","Monthly Energy Cost ($US)","Density","Post SMS - Won or Lost the WTP Bid, Fraction of Bill: Treatment")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = sms_all_data_wtpfraction_tr_cost
              count.plot.name[[count.plot]] = 'sms_all_data_wtpfraction_tr_cost' 
        
        sms_all_data_cl_cost <- call_plot_subset(control_time_series_data_table[sms_intervention_month==1],"intervention_group","Control Pre-Intervention","Control Post-Intervention","importe_dl","Monthly Energy Cost ($US)","Density","Post SMS - Pre and Post Intervention: Control")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = sms_all_data_cl_cost
            count.plot.name[[count.plot]] = 'sms_all_data_cl_cost' 

            # All densities for each unique house (you can plot energia or importe here)
            #for(i in 1:length(unique(data_time_series$Casa))){
             # subset_house_data <- subset(data_time_series,data_time_series$Casa == unique(data_time_series$Casa)[i])
              #mean_pre_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[1])$energia,na.rm=TRUE)
              #mean_post_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[2])$energia,na.rm=TRUE)
              #density_plot <- ggplot(subset_house_data, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
              #selected.house <- unique(data_time_series$Casa)[i] 
              #group_type <- unique(subset_house_data$treatment)
              #plot.name = paste(selected.house,"_",group_type,sep="")
              #mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/densities",paste(plot.name,".jpg",sep=""))
              #jpeg(file=mypath)
              #print(density_plot)
              #dev.off()
            #}
            
            # Comparing the energy report intervention and the SMS intervention
            # NOTE CHANGE: report_intervention_month & sms_intervention_month
            
            #for(i in 1:length(unique(data_time_series$Casa))){
             # subset_house_data <- subset(data_time_series,data_time_series$Casa == unique(data_time_series$Casa)[i])
             #subset_reports <- subset(subset_house_data,subset_house_data$sms_intervention_month ==1)
             # mean_pre_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[1])$energia,na.rm=TRUE)
             #mean_post_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[2])$energia,na.rm=TRUE)
              #density_plot <- ggplot(subset_reports, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
              #selected.house <- unique(data_time_series$Casa)[i] 
              #group_type <- unique(subset_reports$treatment)
              #plot.name = paste(selected.house,"_",group_type,sep="")
              #mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/densities/sms_reports",paste(plot.name,".jpg",sep=""))
              #jpeg(file=mypath)
              #print(density_plot)
              #dev.off()
            #}
        
##############################################################################################################
##############################################################################################################

###  1.2. Month differences: Differences for each month one year afterwards (e.g June 2016 - June 2015)


          ### Energy 
          month_e_difference <- month_diffs(treatment_time_series_data_table,energia_text_var) # Treatment
          month_e_difference_control <- month_diffs(control_time_series_data_table,energia_text_var) # Control
          distribution_e_differences <- rbind(month_e_difference,month_e_difference_control)
        
          # All Data for Treatment and Control
          an_e_t_c <- call_plot_subset(distribution_e_differences,'treatment','Treatment','Control','month_diff','Same Month Annual Differences ($US)','Density','Same Month Comparisons (All Data): Treatment vs Control')
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_t_c
              count.plot.name[[count.plot]] = 'an_e_t_c' 
          
          # Post and Pre Implementation
          an_e_treatment_intervention <- call_plot_subset(month_e_difference,'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences (Treatment): Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_intervention' 
     
          an_e_treatment_information_group <- call_plot_subset(month_e_difference,'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences - Won or Lost WTP")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_information_group
              count.plot.name[[count.plot]] = 'an_e_treatment_information_group' 
          
          an_e_treatment_wtp_intervention <- call_plot_subset_several(subset(month_e_difference,month_e_difference$intervention_group == 'Treatment Post-Intervention'),'wtp_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Comparisons (Treatment - Willingness to Pay): Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_wtp_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_wtp_intervention' 
          
          an_e_treatment_wtpfraction_intervention <- call_plot_subset_several(subset(month_e_difference,month_e_difference$intervention_group == 'Treatment Post-Intervention'),'fraction_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Comparisons (Treatment - Willingness to Pay, Fraction of Bill): Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_wtpfraction_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_wtpfraction_intervention' 
          
          an_e_control_intervention <- call_plot_subset(month_e_difference_control,'intervention_group','Control Pre-Intervention','Control Post-Intervention','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences (Control): Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_control_intervention
              count.plot.name[[count.plot]] = 'an_e_control_intervention' 
          
          #Months with and without PAPER ENERGY REPORTS
          an_e_treatment_paper_intervention <- call_plot_subset(subset(month_e_difference, month_e_difference$report_intervention_month==1),'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','month_diff',"Same Month Annual Differences (kWh)","Density","Post Paper Reports (Treatment): Pre and Post Paper Reports")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_paper_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_paper_intervention' 
        
          an_e_treatment_paper_information_group <- call_plot_subset(subset(month_e_difference, month_e_difference$report_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','month_diff',"Same Month Annual Differences (kWh)","Density","Post Paper Reports (Treatment): Won or Lost the WTP Bid")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_paper_information_group
              count.plot.name[[count.plot]] = 'an_e_treatment_paper_information_group' 
          
          an_e_treatment_wtp_paper_intervention <- call_plot_subset_several(subset(month_e_difference, month_e_difference$report_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences (Treatment - Willingness to Pay): Post Paper Reports")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_wtp_paper_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_wtp_paper_intervention' 
          
          an_e_treatment_wtpfraction_paper_intervention <- call_plot_subset_several(subset(month_e_difference, month_e_difference$report_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences (Treatment - Willingness to Pay, Fraction of Bill): Post Paper Reports")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_wtpfraction_paper_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_wtpfraction_paper_intervention' 
          
          an_e_control_paper_intervention <- call_plot_subset(subset(month_e_difference_control, month_e_difference_control$report_intervention_month==1),'intervention_group','Control Pre-Intervention','Control Post-Intervention','month_diff',"Monthly Energy Consumption (kWh)","Density","Post Paper Reports (Control): Pre and Post Paper Reports")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_control_paper_intervention
              count.plot.name[[count.plot]] = 'an_e_control_paper_intervention' 
          
          #Months with and without SMS: Months with and without SMS
          an_e_treatment_sms_intervention <- call_plot_subset(month_e_difference,'sms_intervention_month_text','Actively Receiving SMS','No SMS','month_diff',"Same Month Annual Differences (kWh)","Density","SMS - Treatment Group: Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_sms_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_sms_intervention' 
          
          an_e_treatment_sms_information_group <- call_plot_subset(subset(month_e_difference, month_e_difference$sms_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','month_diff',"Same Month Annual Differences (kWh)","Density","SMS Post Intervention - Treatment Group: Won or Lost the WTP Bid")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_sms_information_group
              count.plot.name[[count.plot]] = 'an_e_treatment_sms_information_group' 
              
          an_e_treatment_wtp_sms_intervention  <- call_plot_subset_several(subset(month_e_difference, month_e_difference$sms_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences (Treatment - Willingness to Pay): Post SMS")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_wtp_sms_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_wtp_sms_intervention' 
          
          an_e_treatment_wtpfraction_sms_intervention  <- call_plot_subset_several(subset(month_e_difference, month_e_difference$sms_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences (kWh)","Density","Same Month Annual Differences (Treatment - Willingness to Pay, Fraction of Bill): Post SMS")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_treatment_wtpfraction_sms_intervention
              count.plot.name[[count.plot]] = 'an_e_treatment_wtpfraction_sms_intervention' 
          
          an_e_control_sms_intervention <- call_plot_subset(month_e_difference_control,'sms_intervention_month_text','Actively Receiving SMS','No SMS','month_diff',"Same Month Annual Differences (kWh)","Density","SMS - Control Group: Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = an_e_control_sms_intervention
              count.plot.name[[count.plot]] = 'an_e_control_sms_intervention' 
          
##############################################################################################################
##############################################################################################################
              
        ### Cost
              
        #treatment_time_series_data_table$importe_dl <- as.integer(treatment_time_series_data_table$importe_dl)  
        #control_time_series_data_table$importe_dl <- as.integer(control_time_series_data_table$importe_dl)       
        month_c_difference <- month_diffs(treatment_time_series_data_table,'importe_dl') # Treatment
        month_c_difference_control <- month_diffs(control_time_series_data_table,'importe_dl') # Control
        distribution_differences_money <- rbind(month_c_difference,month_c_difference_control)
        
        # All Data for Treatment and Control
        an_c_t_c <- call_plot_subset(distribution_differences_money,'treatment','Treatment','Control','month_diff','Same Month Annual Differences ($US)','Density','Same Month Comparisons (All Data): Treatment vs Control')
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_t_c
            count.plot.name[[count.plot]] = 'an_c_t_c' 
        
        # Post and Pre Implementation
        an_c_treatment_intervention <- call_plot_subset(month_c_difference,'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences (Treatment): Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_intervention' 
        
        an_c_treatment_information_group <- call_plot_subset(month_c_difference,'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences - Won or Lost WTP")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_information_group
            count.plot.name[[count.plot]] = 'an_c_treatment_information_group' 
        
        an_c_treatment_wtp_intervention <- call_plot_subset_several(subset(month_c_difference,month_c_difference$intervention_group == 'Treatment Post-Intervention'),'wtp_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Comparisons (Treatment - Willingness to Pay): Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_wtp_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_wtp_intervention' 
        
        an_c_treatment_wtpfraction_intervention <- call_plot_subset_several(subset(month_c_difference,month_c_difference$intervention_group == 'Treatment Post-Intervention'),'fraction_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Comparisons (Treatment - Willingness to Pay, Fraction of Bill): Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_wtpfraction_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_wtpfraction_intervention' 
        
        an_c_control_intervention <- call_plot_subset(month_c_difference_control,'intervention_group','Control Pre-Intervention','Control Post-Intervention','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences (Control): Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_control_intervention
            count.plot.name[[count.plot]] = 'an_c_control_intervention' 
        
        #Months with and without PAPER ENERGY REPORTS
        an_c_treatment_paper_intervention <- call_plot_subset(subset(month_c_difference, month_c_difference$report_intervention_month==1),'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','month_diff',"Same Month Annual Differences ($US)","Density","Post Paper Reports (Treatment): Pre and Post Paper Reports")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_paper_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_paper_intervention' 
        
        an_c_treatment_paper_information_group <- call_plot_subset(subset(month_c_difference, month_c_difference$report_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','month_diff',"Same Month Annual Differences ($US)","Density","Post Paper Reports (Treatment): Won or Lost the WTP Bid")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_paper_information_group
            count.plot.name[[count.plot]] = 'an_c_treatment_paper_information_group' 
        
        an_c_treatment_wtp_paper_intervention <- call_plot_subset_several(subset(month_c_difference, month_c_difference$report_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences (Treatment - Willingness to Pay): Post Paper Reports")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_wtp_paper_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_wtp_paper_intervention' 
      
       an_c_treatment_wtpfraction_paper_intervention <- call_plot_subset_several(subset(month_c_difference, month_c_difference$report_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences (Treatment - Willingness to Pay, Fraction of Bill): Post Paper Reports")
           count.plot <- count.plot + 1
           count.plot.list[[count.plot]] = an_c_treatment_wtp_paper_intervention
           count.plot.name[[count.plot]] = 'an_c_treatment_wtp_paper_intervention' 
       
        an_c_control_paper_intervention <- call_plot_subset(subset(month_c_difference_control, month_c_difference_control$report_intervention_month==1),'intervention_group','Control Pre-Intervention','Control Post-Intervention','month_diff',"Monthly Energy Consumption ($US)","Density","Post Paper Reports (Control): Pre and Post Paper Reports")
          count.plot <- count.plot + 1
          count.plot.list[[count.plot]] = an_c_control_paper_intervention
          count.plot.name[[count.plot]] = 'an_c_control_paper_intervention' 
        
        #Months with and without SMS: Months with and without SMS
        an_c_treatment_sms_intervention <- call_plot_subset(month_c_difference,'sms_intervention_month_text','Actively Receiving SMS','No SMS','month_diff',"Same Month Annual Differences ($US)","Density","SMS - Treatment Group: Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_sms_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_sms_intervention' 
        
        an_c_treatment_sms_information_group <- call_plot_subset(subset(month_c_difference, month_c_difference$sms_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','month_diff',"Same Month Annual Differences ($US)","Density","SMS Post Intervention - Treatment Group: Won or Lost the WTP Bid")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_sms_information_group
            count.plot.name[[count.plot]] = 'an_c_treatment_sms_information_group' 
        
        an_c_treatment_wtp_sms_intervention  <- call_plot_subset_several(subset(month_c_difference, month_c_difference$sms_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences (Treatment - Willingness to Pay): Post SMS")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_wtp_sms_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_wtp_sms_intervention' 
        
        an_c_treatment_wtpfraction_sms_intervention  <- call_plot_subset_several(subset(month_c_difference, month_c_difference$sms_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','month_diff',"Same Month Annual Differences ($US)","Density","Same Month Annual Differences (Treatment - Willingness to Pay, Fraction of Bill): Post SMS")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_treatment_wtpfraction_sms_intervention
            count.plot.name[[count.plot]] = 'an_c_treatment_wtpfraction_sms_intervention' 
        
        an_c_control_sms_intervention <- call_plot_subset(month_c_difference_control,'sms_intervention_month_text','Actively Receiving SMS','No SMS','month_diff',"Same Month Annual Differences ($US)","Density","SMS - Control Group: Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = an_c_control_sms_intervention
            count.plot.name[[count.plot]] = 'an_c_control_sms_intervention' 
            
            
##############################################################################################################
##############################################################################################################

###   1.3. Month by month reductions: (e.g Feb2016 - Jan2016 )
            
        ## Energy
        mbm <- month_by_month(treatment_time_series_data_table,energia_text_var)
        mbm_control <- month_by_month(control_time_series_data_table,energia_text_var)
        mbm_bind <- rbind(mbm,mbm_control)
        
        mbm_e_t_c <- call_plot_subset(mbm_bind,'treatment','Treatment','Control','diff_variable','Month by Month Differences (kWh)','Density','Month by Month Differences (All Data): Treatment vs Control')
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_t_c
            count.plot.name[[count.plot]] = 'mbm_e_t_c' 
        
        # Post and Pre Implementation
        mbm_e_treatment_intervention <- call_plot_subset(mbm,'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','diff_variable',"Month by Month Differences (kWh)","Density","Month by Month Differences (Treatment): Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_intervention' 
        
        mbm_e_treatment_wtp_intervention <- call_plot_subset_several(subset(mbm,mbm$intervention_group == 'Treatment Post-Intervention'),'wtp_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Monthy by Month Differences (kWh)","Density","Month by Month Comparisons (Treatment - Willingness to Pay): Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_wtp_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_wtp_intervention' 
        
        mbm_e_treatment_wtpfraction_intervention <- call_plot_subset_several(subset(mbm,mbm$intervention_group == 'Treatment Post-Intervention'),'fraction_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences (kWh)","Density","Month by Month Comparisons (Treatment - Willingness to Pay, Fraction of Bill): Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_wtpfraction_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_wtpfraction_intervention' 
        
        mbm_e_control_intervention <- call_plot_subset(mbm_control,'intervention_group','Control Pre-Intervention','Control Post-Intervention','diff_variable',"Monthy by Month Differences (kWh)","Density","Monthy by Month Differences (Control): Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_control_intervention
            count.plot.name[[count.plot]] = 'mbm_e_control_intervention' 
        
        #Months with and without PAPER ENERGY REPORTS
        mbm_e_treatment_paper_intervention <- call_plot_subset(subset(mbm, mbm$report_intervention_month==1),'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','diff_variable',"Month by Month Differences (kWh)","Density","Post Paper Reports Month by Month Differences (Treatment): Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_paper_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_paper_intervention' 
        
        mbm_e_treatment_paper_information_group <- call_plot_subset(subset(mbm, mbm$report_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','diff_variable',"Month by Month Differences (kWh)","Density","Post Paper Reports (Treatment): Won or Lost the WTP Bid")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_paper_information_group
            count.plot.name[[count.plot]] = 'mbm_e_treatment_paper_information_group' 
        
        mbm_e_treatment_wtp_paper_intervention <- call_plot_subset_several(subset(mbm, mbm$report_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences (kWh)","Density","Month by Month Differences (Treatment - Willingness to Pay): Post Paper Reports")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_wtp_paper_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_wtp_paper_intervention' 
        
        mbm_e_treatment_wtpfraction_paper_intervention <- call_plot_subset_several(subset(mbm, mbm$report_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences (kWh)","Density","Month by Month Differences (Treatment - Willingness to Pay, Fraction of Bill): Post Paper Reports")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_wtpfraction_paper_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_wtpfraction_paper_intervention' 
        
        mbm_e_control_paper_intervention <- call_plot_subset(subset(mbm_control, mbm_control$report_intervention_month==1),'intervention_group','Control Pre-Intervention','Control Post-Intervention','diff_variable',"Month by Month Differences (kwh)","Density","Post Paper Reports Month by Month Differences (Control): Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_control_paper_intervention
            count.plot.name[[count.plot]] = 'mbm_e_control_paper_intervention' 
        
        #Months with and without SMS: Months with and without SMS
        mbm_e_treatment_sms_intervention <- call_plot_subset(mbm,'sms_intervention_month_text','Actively Receiving SMS','No SMS','diff_variable',"Month by Month Differences (kWh)","Density","SMS - Treatment Group: Pre and Post Intervention")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_sms_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_sms_intervention' 
            
        mbm_e_treatment_sms_information_group <- call_plot_subset(subset(mbm, mbm$sms_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','diff_variable',"Month by Month Differences (kWh)","Density","SMS Post Intervention - Treatment Group: Won or Lost the WTP Bid")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_sms_information_group
            count.plot.name[[count.plot]] = 'mbm_e_treatment_sms_information_group' 
        
        mbm_e_treatment_wtp_sms_intervention  <- call_plot_subset_several(subset(mbm, mbm$sms_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences (kWh)","Density","Month by Month Differences (Treatment - Willingness to Pay): Post SMS")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_wtp_sms_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_wtp_sms_intervention' 
        
        mbm_e_treatment_wtpfraction_sms_intervention  <- call_plot_subset_several(subset(mbm, mbm$sms_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences (kWh)","Density","Month by Month Differences (Treatment - Willingness to Pay, Fraction of Bill): Post SMS")
            count.plot <- count.plot + 1
            count.plot.list[[count.plot]] = mbm_e_treatment_wtpfraction_sms_intervention
            count.plot.name[[count.plot]] = 'mbm_e_treatment_wtpfraction_sms_intervention' 
          
        mbm_e_control_sms_intervention <- call_plot_subset(mbm_control,'sms_intervention_month_text','Actively Receiving SMS','No SMS','diff_variable',"Same Month Annual Differences (kWh)","Density","SMS - Control Group: Pre and Post Intervention")
          count.plot <- count.plot + 1
          count.plot.list[[count.plot]] = mbm_e_control_sms_intervention
          count.plot.name[[count.plot]] = 'mbm_e_control_sms_intervention' 
          
          ####
          ######
          ###### Cordobas
          mbm_c <- month_by_month(treatment_time_series_data_table,'importe_dl')
          mbm_c_control <- month_by_month(control_time_series_data_table,'importe_dl')
          mbm_bind_cordobas <- rbind(mbm_c,mbm_c_control)
          
          mbm_c_t_c <- call_plot_subset(mbm_bind_cordobas,'treatment','Treatment','Control','diff_variable','Month by Month Differences ($US)','Density','Month by Month Differences (All Data): Treatment vs Control')
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_t_c
              count.plot.name[[count.plot]] = 'mbm_c_t_c' 
          
          # Post and Pre Implementation
          mbm_c_treatment_intervention <- call_plot_subset(mbm_c,'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','diff_variable',"Month by Month Differences ($US)","Density","Month by Month Differences (Treatment): Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_intervention' 
          
          mbm_c_treatment_wtp_intervention <- call_plot_subset_several(subset(mbm_c,mbm_c$intervention_group == 'Treatment Post-Intervention'),'wtp_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Monthy by Month Differences ($US)","Density","Month by Month Comparisons (Treatment - Willingness to Pay): Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_wtp_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_wtp_intervention' 
          
          mbm_c_treatment_wtpfraction_intervention <- call_plot_subset_several(subset(mbm_c,mbm_c$intervention_group == 'Treatment Post-Intervention'),'fraction_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences ($US)","Density","Month by Month Comparisons (Treatment - Willingness to Pay, Fraction of Bill): Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_wtpfraction_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_wtpfraction_intervention' 
          
          mbm_c_control_intervention <- call_plot_subset(mbm_c_control,'intervention_group','Control Pre-Intervention','Control Post-Intervention','diff_variable',"Monthy by Month Differences ($US)","Density","Monthy by Month Differences (Control): Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_control_intervention
              count.plot.name[[count.plot]] = 'mbm_c_control_intervention' 
          
          #Months with and without PAPER ENERGY REPORTS
          mbm_c_treatment_paper_intervention <- call_plot_subset(subset(mbm_c, mbm_c$report_intervention_month==1),'intervention_group','Treatment Pre-Intervention','Treatment Post-Intervention','diff_variable',"Month by Month Differences ($US)","Density","Post Paper Reports Month by Month Differences (Treatment): Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_paper_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_paper_intervention' 
          
          mbm_c_treatment_paper_information_group <- call_plot_subset(subset(mbm_c, mbm_c$report_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','diff_variable',"Month by Month Differences ($US)","Density","Post Paper Reports (Treatment): Won or Lost the WTP Bid")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_paper_information_group
              count.plot.name[[count.plot]] = 'mbm_c_treatment_paper_information_group' 
          
          mbm_c_treatment_wtp_paper_intervention <- call_plot_subset_several(subset(mbm_c, mbm_c$report_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences ($US)","Density","Month by Month Differences (Treatment - Willingness to Pay): Post Paper Reports")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_wtp_paper_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_wtp_paper_intervention' 
          
          mbm_c_treatment_wtpfraction_paper_intervention <- call_plot_subset_several(subset(mbm_c, mbm_c$report_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences ($US)","Density","Month by Month Differences (Treatment - Willingness to Pay, Fraction of Bill): Post Paper Reports")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_wtpfraction_paper_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_wtpfraction_paper_intervention' 
          
          mbm_c_control_paper_intervention <- call_plot_subset(subset(mbm_c_control, mbm_c_control$report_intervention_month==1),'intervention_group','Control Pre-Intervention','Control Post-Intervention','diff_variable',"Month by Month Differences (kwh)","Density","Post Paper Reports Month by Month Differences (Control): Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_control_paper_intervention
              count.plot.name[[count.plot]] = 'mbm_c_control_paper_intervention' 
          
          #Months with and without SMS: Months with and without SMS
          mbm_c_treatment_sms_intervention <- call_plot_subset(mbm_c,'sms_intervention_month_text','Actively Receiving SMS','No SMS','diff_variable',"Month by Month Differences ($US)","Density","SMS - Treatment Group: Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_sms_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_sms_intervention' 
          
          mbm_c_treatment_sms_information_group <- call_plot_subset(subset(mbm_c, mbm_c$sms_intervention_month==1),'information_group','Lost Willingness to Pay Information Bid','Won Willingness to Pay Information Bid','diff_variable',"Month by Month Differences ($US)","Density","SMS Post Intervention - Treatment Group: Won or Lost the WTP Bid")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_sms_information_group
              count.plot.name[[count.plot]] = 'mbm_c_treatment_sms_information_group' 
          
          mbm_c_treatment_wtp_sms_intervention  <- call_plot_subset_several(subset(mbm_c, mbm_c$sms_intervention_month==1),'wtp_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences ($US)","Density","Month by Month Differences (Treatment - Willingness to Pay): Post SMS")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_wtp_sms_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_wtp_sms_intervention' 
          
          mbm_c_treatment_wtpfraction_sms_intervention  <- call_plot_subset_several(subset(mbm_c, mbm_c$sms_intervention_month==1),'fraction_lw_md_h','low','high','medium-low','medium-high','diff_variable',"Month by Month Differences ($US)","Density","Month by Month Differences (Treatment - Willingness to Pay, Fraction of Bill): Post SMS")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_treatment_wtpfraction_sms_intervention
              count.plot.name[[count.plot]] = 'mbm_c_treatment_wtpfraction_sms_intervention' 
          
          mbm_c_control_sms_intervention <- call_plot_subset(mbm_c_control,'sms_intervention_month_text','Actively Receiving SMS','No SMS','diff_variable',"Same Month Annual Differences ($US)","Density","SMS - Control Group: Pre and Post Intervention")
              count.plot <- count.plot + 1
              count.plot.list[[count.plot]] = mbm_c_control_sms_intervention
              count.plot.name[[count.plot]] = 'mbm_c_control_sms_intervention' 
          
for (j in 1:count.plot) {
  plot.name = count.plot.name[[j]]
  mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/energy_cost",energy_file,energy_var,paste(plot.name,".jpg",sep = ""))
  jpeg(file=mypath)
  print(count.plot.list[[j]])
  dev.off()
}
  
}









#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

# All densities for each unique house (you can plot energia or importe here)
for(i in 1:length(unique(data_time_series$Casa))){
  subset_house_data <- subset(data_time_series,data_time_series$Casa == unique(data_time_series$Casa)[i])
  mean_pre_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[1])$energia,na.rm=TRUE)
  mean_post_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[2])$energia,na.rm=TRUE)
  
  density_plot <- ggplot(subset_house_data, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
  
  selected.house <- unique(data_time_series$Casa)[i] 
  group_type <- unique(subset_house_data$treatment)
  

}

# All Houses Month by Month
for(i in 1:length(unique(mbm_bind$Casa))){
  subset_house_data <- subset(mbm_bind,mbm_bind$Casa == unique(mbm_bind$Casa)[i])
  mean_pre_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[1])$energia,na.rm=TRUE)
  mean_post_intervention = mean(subset(subset_house_data,subset_house_data$intervention_group == unique(subset_house_data$intervention_group)[2])$energia,na.rm=TRUE)
  
  density_plot <- ggplot(subset_house_data, aes(diff_variable, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")
  
  selected.house <- unique(mbm_bind$Casa)[i] 
  group_type <- unique(subset_house_data$treatment)
  
  plot.name = paste(selected.house,"_",group_type,sep="")
  mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/month_by_month",paste(plot.name,".jpg",sep=""))
  
  jpeg(file=mypath)
  print(density_plot)
  dev.off()
}

# Houses Month by Month only comparing the months with energy reports
for(i in 1:length(unique(data_time_series$Casa))){
  subset_house_data <- subset(mbm_bind,mbm_bind$Casa == unique(mbm_bind$Casa)[i])
  subset_reports <- subset(subset_house_data,subset_house_data$sms_intervention_month ==1)
  mean_pre_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[1])$energia,na.rm=TRUE)
  mean_post_intervention = mean(subset(subset_reports,subset_reports$intervention_group == unique(subset_reports$intervention_group)[2])$energia,na.rm=TRUE)
  
  density_plot <- ggplot(subset_reports, aes(energia, fill = intervention_group)) + geom_density(alpha = 0.2) + xlab("Energy (kWh") + ylab("Density") + geom_vline(xintercept=mean_pre_intervention,colour="blue") + geom_vline(xintercept=mean_post_intervention,colour="red")  
  
  selected.house <- unique(data_time_series$Casa)[i] 
  group_type <- unique(subset_reports$treatment)
  
  plot.name = paste(selected.house,"_",group_type,sep="")
  mypath <- file.path("/Users/Diego/Desktop/Projects/Exploring the Marginal Value of Information/plots/month_by_month/month_by_month_sms_intervention",paste(plot.name,".jpg",sep=""))
  
  jpeg(file=mypath)
  print(density_plot)
  dev.off()
}


