#---------{ SET DUONG DAN, KHAI BAO THU VIEN }-------


setwd('C:\\Users\\Admin\\Desktop\\Project\\Python\\Pj-FinProDataSort')

library(dplyr)
library(openxlsx)
library(stargazer)
library(utf8)
library(rlang)
library(tidyverse)
require(multiwayvcov)
require(lmtest)
library(SciViews)
require(psych)
#-----{ TINH TOAN MP }--------

# KHAI BAO DUONG DAN
PATH_GDP = 'RCode/MP/LS.csv'
PATH_INTEREST = 'Output/Interest.csv'


df_gdp = read.csv(PATH_GDP)
df_interest = read.csv(PATH_INTEREST)

df_gdp <- df_gdp %>% subset(Year >= 2008) %>% as.data.frame()


dep_col_names <- c(paste0("HighDep", c("Low1", "1to6", "6to12", "High12")),
                   paste0("LowDep", c("Low1", "1to6", "6to12", "High12")))
lend_col_names <- c(paste0("HighLend", c("SGov", "SCom", "LGov")),
                    paste0("LowLend", c("SGov", "SCom", "LGov")))
col_names <- c(dep_col_names, lend_col_names)
colnames(df_interest) <- col_names


MP <- seq(2008,2021) %>% as.data.frame()
colnames(MP) <- c('Year')

for (i in seq(7,12)){
  BienPhuThuoc <- df_interest[[c(i)]]
  BienDocLap1 <- df_gdp$Inflation 
  BienDocLap2 <- df_gdp$OutGap 
  reg.c <- lm(BienPhuThuoc ~ BienDocLap1+BienDocLap2)
  nameCol <- 'MP' %>% paste(i, sep = '')
  MP[nameCol] <- residuals(reg.c)
  rm(BienDocLap1,BienDocLap2,BienPhuThuoc,i, nameCol,reg.c)
}


colnames(MP) <- c('Year', 'MP_HighDep', 'MP_LowDep' , 'MP_HighLendSBV', 'MP_LowLendSBV', 'MP_HighLendCom', 'MP_LowLendCom')

rm(df_gdp, df_interest, col_names, dep_col_names, lend_col_names, PATH_INTEREST, PATH_GDP)



#-----{ XU LY DATA BCTC }--------

# KHAI BAO DUONG DAN
PATH_BCTC = 'Output/dp_BCTC.csv'
PATH_SOE = 'Output/SOE.csv'
PATH_BRANCH = 'Output/Branch.csv'

# CAC COT CAN LAY
colNames_select = c('Ticker', 'Year','Exchange','1', '2', '5',
                    '37', '46','50','66','67',
                    '68','83','140','144', '150',
                    '151', '170')
colNames_decrypt = c('Firm', 'Year','Exchange','TSNH','TVTDT', 'TSDTNH',
                     'TSCDHH', 'XDDDT2015','DDDH', 'TTS','NPT',
                     'NNH','NDH', 'EBIT','NI', 'NIBDA',
                     'DA','CF')
# Detail:
# 1 Tai san ngan han
# 2 tien va cac khoan tuong duong tien
# 5 Tai san dau tu ngan han
# 37 TSCD HH
# 46 Xay dung do dang truoc 2015 
# 50 Tai san do dang dai han
# 66 Total Asset
# 67 Total Liabilites
# 68 No ngan han
# 83 No dai han
# 140 EBIT
# 144 Net Income
# 150 operating income before depreciation
# 151 Depreciation
# 170 Luu chuyen tien te thuan cua hoat dong dau tu

# DANH SACH CAC COT TRONG BCTC
colNames_BCTC <- read.xlsx('RCode/Col.xlsx', colNames = FALSE)

# LAY DU LIEU TRONG BCTC
dBCTC <- read.csv(PATH_BCTC) 
dBCTC <- dBCTC %>% subset(dBCTC$Year >=2008) %>% as.data.frame()
colnames(dBCTC) <- c('Ticker', 'Year','Exchange', seq(1,383))
dBCTC <- dBCTC %>% select(colNames_select)
colnames(dBCTC) <- colNames_decrypt

rm(colNames_decrypt,colNames_select,PATH_BCTC)
rm(colNames_BCTC)

#-----{ MERGE DU LIEU KHAC }-------

dMerged <- merge(dBCTC, MP, by = 'Year')
dMerged <- dMerged %>% arrange(desc(dMerged$Exchange), dMerged$Firm, dMerged$Year)

dSOE <- read.csv(PATH_SOE)
colnames(dSOE) <- c('Firm', 'Year', 'Bool_SOE', 'Bool_Foreign')
dMerged <- merge(dMerged,dSOE, by = c('Firm', 'Year'))

dBranch <- read.csv(PATH_BRANCH)
colnames(dBranch) <- c('Firm', 'Nganh')
dMerged <- merge(dMerged, dBranch, by = c('Firm'))

rm(MP, dSOE,dBranch, PATH_SOE, PATH_BRANCH)



#-----{ XU LY DU LIEU HOI QUY }-----

# GAN CHO MOT BIEN TAM THOI
df_temp <- dMerged %>% group_by(Firm)

# LAY NHUNG COT CAN THIET
columns_select <- c('Firm', 'Year', 'Bool_SOE')


# THEM BIEN MOI
columns_new <- list('TSXXDD = XDDDT2015 + DDDH', 
                    'INVEST1 = (TSCDHH + TSXXDD)/TTS',
                    'INVEST2 = -CF/TTS',
                    'CASH = (TVTDT+TSDTNH)/TTS',
                    'LEV = NPT/TTS', 
                    'SIZE = ln(TTS)',
                    'ROA = NI/TTS',
                    'Tangible = TSCDHH/TTS',
                    
                    
                    'SDEBT = NNH/TTS',
                    'deltaSDEBT = SDEBT - lag(SDEBT,1)',
                    'NWC = (TSNH - NNH - TVTDT)/ TTS',
                    'deltaNWC = NWC - lag(NWC,1)',
                    
                    'lagINVEST1 = lag(INVEST1,1)',
                    'lagINVEST2 = lag(INVEST2,1)',
                    'lagMPDep = lag(( MP_HighDep + MP_LowDep)/2,1)',
                    'lagMPLend = lag(( MP_HighLendSBV + MP_LowLendSBV)/2,1)',
                    
                    
                    # CAC BIEN DELTA VA SQR
                    'deltaINVEST1 = INVEST1 - lag(INVEST1,1)',
                    'deltaINVEST2 = INVEST2 - lag(INVEST2,1)',
                    'deltaCASH = CASH - lag(CASH,1)',
                    'sqrINVEST1 = INVEST1 ^ 2',
                    'sqrINVEST2 = INVEST2 ^ 2',
                    
                    # CASH FLOW (cash flow nay la cash flow dung trong mo hinh khac voi cash flow trong code o tren)
                    'CASHFLOW = CF/TTS',
                    
                    # BIEN TICH
                    'MPCF = lagMPDep * CASHFLOW',
                    'MPCASH = lagMPDep * CASH')

# CAUTIONS: Nho chu y khoang trang khi them bien

for (x in columns_new) {
  lhs <- str_remove(strsplit(x,split =  "=")[[1]][1], " ")
  rhs <- strsplit(x,split =  "=")[[1]][2]
  columns_select <- c(columns_select, lhs)
  df_temp = mutate(.data = df_temp, !!lhs := !!parse_quo(rhs, env = caller_env()))
  print(x)
}

dREGC <- df_temp %>% 
  select(all_of(columns_select)) %>% 
  subset(lagMPDep != 'NA' & 
           lagINVEST1 != 'NA' & 
           lagINVEST1 != '-Inf' & 
           INVEST1 != '-Inf' & 
           INVEST1 != 'NaN' & 
           lagINVEST1 != 'NaN')

# WINSOR MP

dtemp1 <- dREGC %>% subset(select=-c(Firm,Year, lagMPDep))
dtemp2 <- dREGC %>% subset(select=c(Firm,Year,lagMPDep))
dtemp1 <- winsor(dtemp1,trim = 0.01)
dREGC <- cbind(dtemp2,dtemp1)
rm(dtemp1,dtemp2)

rm(lhs,rhs,x,columns_select,df_temp,columns_new)



#-----{ HOI QUY }-------

depVar1 <- 'INVEST1'
depVar2 <- 'INVEST2'
indepVar <- 'lagINVEST1+ lagMPDep+ LEV + SIZE + ROA +  CASH +  Bool_SOE + as.factor(Year)'

regOLS1 <- lm(INVEST1 ~ lagINVEST1+ lagMPDep+ LEV + SIZE + ROA +  CASH +  Bool_SOE + as.factor(Year) ,data=dREGC)
summary(regOLS1)

regOLS2 <- lm(INVEST2 ~ lagINVEST2+ lagMPDep+ LEV + SIZE + ROA +  CASH +  Bool_SOE + as.factor(Year) ,data=dREGC)
summary(regOLS2)

regOLS3 <- lm(INVEST1 ~ lagINVEST1+ lagMPLend+ LEV + SIZE + ROA +  CASH +  Bool_SOE + as.factor(Year) ,data=dREGC)
summary(regOLS3)

regOLS4 <- lm(INVEST2 ~ lagINVEST2+ lagMPLend+ LEV + SIZE + ROA +  CASH +  Bool_SOE + as.factor(Year) ,data=dREGC)
summary(regOLS4)


Table2 <- list(regOLS1,regOLS2)
remove(depVar1, depVar2,indepVar, regOLS1, regOLS2, regOLS3, regOLS4)

#-----{ FINANCIAL CONSTRAINTS }------

devar   <- c('INVEST1', 'INVEST2')
indeVar <- 'lagMPDep+ CASH+ LEV+ ROA + as.factor(Year)'
conditionVar <- c('Bool_SOE == 1','Bool_SOE == 0', 'SIZE > median(SIZE)', 'SIZE <= median(SIZE)')


for (i in 1:2) {
  for (x in 1:4){
    M  <- formula(paste( devar[i],"~", indeVar))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    assign(paste0('REG',i,x),reg);
  }
}
Table3 <- list(REG11,REG12,REG13,REG14,
               REG21,REG22,REG23,REG24)

rm(REG11,REG12,REG13,REG14,REG21,REG22,REG23,REG24, indeVar, devar, conditionVar,i,M,x,reg)



#-----{ CASH-CASH FLOW SENSITIVITY }------

devar   <- c('deltaCASH')
indepVar <- c('CASHFLOW + lagMPDep + MPCF + SIZE + deltaNWC + deltaSDEBT ') # Thieu Tobin Q, CAPEX
conditionVar <- c('Bool_SOE == 1','Bool_SOE == 0', 'SIZE > median(SIZE)', 'SIZE <= median(SIZE)')

for (x in 1:4){
  M  <- formula(paste0( devar,"~", indepVar))
  reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
  assign(paste0('REG',x),reg);
}             

Table6 <- list(REG1, REG2, REG3, REG4)
rm(x, REG1, REG2, REG3, REG4, devar, indepVar, conditionVar, M,reg)

#-----{ INVESTMENT SMOOTHING }------

devar   <- c('deltaINVEST1', 'deltaINVEST2')
indepVar <- c('INVEST1 + sqrINVEST1 + lagMPDep + deltaCASH',
              'INVEST2 + sqrINVEST2 + lagMPDep + deltaCASH' )

conditionVar <- c('Bool_SOE == 1','Bool_SOE == 0', 'SIZE > median(SIZE)', 'SIZE <= median(SIZE)')

for (i in 1:2) {
  for (x in 1:4){
    M  <- formula(paste0( devar[i],"~", indepVar[i]))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    assign(paste0('REG',i,x),reg);
  }
}

Table7 <- list(REG11,REG12,REG13,REG14,
               REG21,REG22,REG23,REG24)
rm(REG11,REG12,REG13,REG14,REG21,REG22,REG23,REG24, indepVar, devar, conditionVar,i,M,x, reg)

#-----{ BANG KET QUA }----------------------------------------------------------


# TABLE 1

Table1 <- dREGC %>% select(INVEST1, INVEST2, lagINVEST1, lagINVEST2, lagMPDep, LEV, SIZE, ROA, CASH, Bool_SOE) %>% as.data.frame()
stargazer(Table1, title = "Table 1. Variable definitions and descriptive statistics",
          summary = T,
          summary.stat =c("n","mean","median", "min", "max", "sd"), digits = 4,
          type = 'text', style = 'all', out = 'Table1.htm')


# TABLE 2

stargazer(Table2, type = 'text',
          omit = c('Year', 'Firm'), align = TRUE,
          intercept.bottom = T, style = 'all', 
          title = 'Table 2 Baseline results - monetary policy effects on corporate investment', 
          digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t', out = 'Table2.htm')


# TABLE 3

stargazer(Table3, type = 'text', column.labels = c('SOE', 'Non-SOE', 'SIZE-Big', 'SIZE-Small','SOE', 'Non-SOE', 'SIZE-Big', 'SIZE-Small'),
          covariate.labels = c('MP'),
          omit = c('Year','Firm'), align = TRUE, out = 'Table3.htm',
          intercept.bottom = T, style = 'all', title = 'Table 3 Financial constraints, cash holdings and corporate investment.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')


# TABLE 6

stargazer(Table6, type = 'text', column.labels = c('SOE', 'Non-SOE', 'SIZE-Big', 'SIZE-Small','SOE', 'Non-SOE', 'SIZE-Big', 'SIZE-Small'),
          omit = c('Year','Firm'), align = TRUE, out = 'Table6.htm',
          intercept.bottom = T, style = 'all', title = 'Table 6 Monetary policy and cash-cash flow sensitivity.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')


# TABLE 7

stargazer(Table7, type = 'text', column.labels = c('SOE', 'Non-SOE', 'SIZE-Big', 'SIZE-Small','SOE', 'Non-SOE', 'SIZE-Big', 'SIZE-Small'),
          omit = c('Year','Firm'), align = TRUE, out = 'Table7.htm',
          intercept.bottom = T, style = 'all', title = 'Table 7 Monetary policy, cash holding and investment smoothing.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')


#------{ LEGACY }---------------------------------------------------------------

for (i in 1:2) {
  M  <- formula(paste( devar[i],"~", indevar))
  reg <- lm(M, data=subset(df_test, BoolGov == 1))
  R2 <- round(summary(reg)$adj.r.squared,4); assign(paste0("R2Adj",i), R2)
  reg_formula <- cluster.vcov(reg, ~ NGANH)
  reg <- coeftest(reg, reg_formula)
  assign(paste0("reg",i), reg); 
}

