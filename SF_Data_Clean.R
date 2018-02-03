# Check, Load and Install Packages
list.of.packages <- c("dplyr","zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(dplyr)
require(zoo)


# Read Table Sample Orig 2009

origclass <- c('integer','integer','character', 'integer', 'character', 
               'real', 'integer', 'character','real','integer','integer',
               'integer','real','character','character','character','character', 
               'character','character','character','character', 'integer', 
               'integer','character','character' ,'character' ) 

orig2009 <- read.table("sample_orig_2009.txt", sep="|", header=FALSE, colClasses=origclass) 

names(orig2009)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",
                  'cnt_units','occpy_sts','cltv','dti','orig_upb','ltv','int_rt',
                  'channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode',
                  'id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name',
                  'servicer_name', 'flag_sc') 

# select variables 'id_loan', 'fico', 'dti','orig_upb', 'ltv'

myvars <- c('id_loan','fico','dti','orig_upb','ltv')
o2009 <-orig2009[myvars]
rm(orig2009)

# Read Table sample_svcg_2009

svcgclass <- c('character','integer','real','character','integer',
               'integer','character','character', 'character','integer',
               'real','real','integer', 'integer', 'character','integer',
               'integer', 'integer','integer','integer','integer','real','real') 
svcg2009 <- read.table("sample_svcg_2009.txt", sep="|", header=FALSE, 
                              colClasses=svcgclass) 

names(svcg2009)=c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age','mths_remng', 
                  'repch_flag','flag_mod', 'cd_zero_bal', 'dt_zero_bal','current_int_rt',
                  'non_int_brng_upb','dt_lst_pi','mi_recoveries','net_sale_proceeds',
                  'non_mi_recoveries','expenses', 'legal_costs', 'maint_pres_costs',
                  'taxes_ins_costs','misc_costs','actual_loss', 'modcost')

# Subset data
myvars <- c("id_loan","svcg_cycle","current_upb","delq_sts","cd_zero_bal",
            "current_int_rt","non_int_brng_upb","dt_lst_pi","mi_recoveries",
            "net_sale_proceeds","non_mi_recoveries","expenses","actual_loss" )

svcg2009<-svcg2009[myvars]

# Select the last two records of each loan

s2009 <- svcg2009 %>% 
            group_by(id_loan) %>%  
            filter(row_number() %in% c(n()-1,n()))


# calculate EAD and RAD and impute Actual Loss
s2009 <- as.data.frame(s2009)
s2009$delq_acc <- NA
s2009$EAD <- NA
s2009$RAD <- NA

for (i in 2:dim(s2009)[1]){
  if (s2009$cd_zero_bal[i] == "03"){
    # Delinquent Accrued Interst
    s2009$delq_acc[i] = as.numeric(s2009$delq_sts[i]) * (s2009$current_upb[i-1] - 
                        s2009$non_int_brng_upb[i]) * (s2009$current_int_rt[i] - 0.35) * 30/360/100
    # Exposure At Default
    s2009$EAD[i] = s2009$current_upb[i-1] + s2009$delq_acc[i] - 
                    if (is.na(s2009$expenses[i])) {0} else {s2009$expenses[i]}
    # Recovery At Default
    s2009$RAD[i] = if (s2009$net_sale_proceeds[i] == "C") {0}
                    else { if (is.na(as.numeric(s2009$net_sale_proceeds[i]))) {0} 
                      else {as.numeric(s2009$net_sale_proceeds[i])}  + 
                    if (is.na(s2009$mi_recoveries[i])) {0} else {s2009$mi_recoveries[i]} +
                    if (is.na(s2009$non_mi_recoveries[i])) {0} else {s2009$non_mi_recoveries[i]}
                  }
    # Calculate Missing Actual Loss
    if (is.na(s2009$actual_loss[i])) {s2009$actual_loss[i] = s2009$EAD[i]-s2009$RAD[i]}
    # Impute Missing LLDPI
    if (is.na(s2009$dt_lst_pi[i])) {if ((s2009$svcg_cycle[i] %% 100 + as.numeric(s2009$delq_sts[i-1]) %% 12) %% 12 > 0) {
      s2009$dt_lst_pi[i] = floor(s2009$svcg_cycle[i] / 100) * 100 +                                         # year
                      floor((s2009$svcg_cycle[i] %% 100 + as.numeric(s2009$delq_sts[i-1])) / 12) * 100 +   # year
                      (s2009$svcg_cycle[i] %% 100 + as.numeric(s2009$delq_sts[i-1])) %% 12                 # month
    } else {s2009$dt_lst_pi[i] = s2009$svcg_cycle[i] + as.numeric(s2009$delq_sts[i-1]) / 12 * 100} 
      }
      
  }else if (s2009$cd_zero_bal[i]=="09"){
    # Delinquent Times
    s2009$delq_sts[i] = as.character((floor(s2009$svcg_cycle[i]/100) - floor(s2009$dt_lst_pi[i]/100)) * 12 + 
      s2009$svcg_cycle[i] %% 100 - s2009$dt_lst_pi[i] %% 100)
    
    # Delinquent Accrued Interst
    
    s2009$delq_acc[i] = as.numeric(s2009$delq_sts[i]) * (s2009$current_upb[i-1] - 
                        s2009$non_int_brng_upb[i]) * (s2009$current_int_rt[i-1] - 0.35) * 30/360/100
    
    # Exposure At Default
    s2009$EAD[i] = s2009$current_upb[i-1] + s2009$delq_acc[i] - 
      if (is.na(s2009$expenses[i])) {0} else {s2009$expenses[i]}
    
    # Recovery At Default
    s2009$RAD[i] = if (s2009$net_sale_proceeds[i] == "C") {0}
    else { if (is.na(as.numeric(s2009$net_sale_proceeds[i]))) {0} else {as.numeric(s2009$net_sale_proceeds[i])}  + 
        if (is.na(s2009$mi_recoveries[i])) {0} else {s2009$mi_recoveries[i]} +
        if (is.na(s2009$non_mi_recoveries[i])) {0} else {s2009$non_mi_recoveries[i]}
    }
    # Calculate Missing Actual Loss
    if (is.na(s2009$actual_loss[i])) {s2009$actual_loss[i]=s2009$EAD[i]-s2009$RAD[i]}
  }
}

# drop variables
myvars <- c("id_loan","svcg_cycle","current_upb","cd_zero_bal","dt_lst_pi","actual_loss",
            "EAD","RAD")

s2009 <- s2009[myvars]

# Select the last record of each loan

s2009 <- s2009 %>% 
  group_by(id_loan) %>%  
  filter(row_number() %in% c(n()))

# Add a loss flag
s2009 <- as.data.frame(s2009)
s2009$loss_flag <- 0
s2009$loss_flag[s2009$cd_zero_bal == c("03","09")]= 1

# LGD

s2009$LGD <- 0
s2009$LGD[s2009$loss_flag == 1] <- 1 - s2009$RAD[s2009$loss_flag == 1] / s2009$EAD[s2009$loss_flag == 1]    

os2009 <- inner_join(s2009,o2009, by = "id_loan")

# read table Macrovariables

macr <- read.table("mevdata.csv", sep=",", header=TRUE) 

os2009$UR <- NA
os2009$MR <- NA
os2009$HPI <- NA

for (i in 1:dim(os2009)[1]){
  if (is.na(os2009$dt_lst_pi[i])){
    j = which.min(os2009$svcg_cycle[i] >= macr$Date)
    os2009$UR[i] <- macr$UR[j]
    os2009$MR[i] <- macr$MR[j]
    os2009$HPI[i] <- macr$HPI[j]
  }else{
    j = which.min(os2009$dt_lst_pi[i] >= macr$Date)
    os2009$UR[i] <- macr$UR[j]
    os2009$MR[i] <- macr$MR[j]
    os2009$HPI[i] <- macr$HPI[j]    
  }
}

for (i in 1:dim(os2009)[1]){
  if (is.na(os2009$EAD[i])){
    os2009$EAD[i]=os2009$current_upb[i]
  }
}


# drop variables
myvars <- c("fico","dti","ltv","UR","MR","HPI","loss_flag","LGD","EAD")

mydata <- os2009[myvars]

write.table(mydata, file = "mydata.csv", sep = ",")