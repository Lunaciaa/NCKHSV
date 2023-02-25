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
PATH_MARKETCAP = 'Output/MarketCap.csv'

# CAC COT CAN LAY
colNames_select = c('Ticker', 'Year','Exchange','1', '2', '5',
                    '37', '43', '46','50','66',
                    '67','68','83','140','144',
                    '149','150','151', '170', '171',
                    '172','184')
colNames_decrypt = c('Firm', 'Year','Exchange','TSNH','TVTDT', 'TSDTNH',
                     'TSCDHH','TSCDVH', 'XDDDT2015','DDDH', 'TTS',
                     'NPT','NNH','NDH', 'EBIT','NI', 
                     'CF','NIBDA','DA','ICF', 'BUYTSCD',
                     'SELLTSCD','Div')
# Detail:
# 1 Tai san ngan han
# 2 tien va cac khoan tuong duong tien
# 5 Tai san dau tu ngan han
# 37 TSCD HH
# 43 TSCD VH
# 46 Xay dung do dang truoc 2015 
# 50 Tai san do dang dai han
# 66 Total Asset
# 67 Total Liabilites
# 68 No ngan han
# 83 No dai han
# 140 EBIT
# 144 Net Income
# 149 Luu chuyen tien te thuan tu hoat dong kinh doanh
# 150 operating income before depreciation
# 151 Depreciation
# 170 Luu chuyen tien te thuan cua hoat dong dau tu]
# 171 Tien chi de mua TSCD
# 184 Co tuc da tra

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

dMarketCap <- read.csv(PATH_MARKETCAP)
colnames(dMarketCap) <- c('Firm', 'Year', 'MarketCap')
dMarketCap <- dMarketCap %>% subset(Year <= 2021 & Year >= 2008)
dMarketCap$MarketCap <-  dMarketCap$MarketCap * 1000000000
dMerged <- merge(dMerged,dMarketCap, by = c('Firm', 'Year'))

rm(MP, dSOE,dBranch, dMarketCap, PATH_MARKETCAP, PATH_SOE, PATH_BRANCH)



#-----{ XU LY DU LIEU HOI QUY }-----

# GAN CHO MOT BIEN TAM THOI
df_temp <- dMerged %>% group_by(Firm)

# LAY NHUNG COT CAN THIET
columns_select <- c('Firm', 'Year','Nganh', 'Bool_SOE')

# CHANGE IN 25/2/2023 - SIZE / LN (tts/(10^6)) # CAPEX: ktr lai cong thuc # INVEST2 sai bien
# THEM BIEN MOI
columns_new <- list('TSXXDD = XDDDT2015 + DDDH', 
                    'INVEST1 = (TSCDHH + TSXXDD)/TTS',
                    'INVEST2 = -BUYTSCD/TTS', # Thay doi INVEST 2
                    'CASH = (TVTDT+TSDTNH)/TTS',
                    'LEV = NPT/TTS', 
                    'SIZE = ln(TTS/(10^6))', 
                    'ROA = NI/TTS',
                    'Tangible = TSCDHH/TTS',
                    'CAPEX = BUYTSCD',
                    
                    
                    'SDEBT = NNH/TTS',
                    'deltaSDEBT = SDEBT - lag(SDEBT,1)',
                    'NWC = (TSNH - NNH - TVTDT)/ TTS',
                    'deltaNWC = NWC - lag(NWC,1)',
                    
                    'lagINVEST1 = lag(INVEST1,1)',
                    'lagINVEST2 = lag(INVEST2,1)',
                    'lagMPDep = lag(( MP_HighDep + MP_LowDep)/2,1)',
                    'lagMPLend = lag(( MP_HighLendSBV + MP_LowLendSBV)/2,1)',
                    
                    # MP WITHOUT LAG
                    'MPDep = (MP_HighDep + MP_LowDep)/2',
                    'MPLend = (MP_HighLendSBV + MP_LowLendSBV)/2',
                    
                    # CAC BIEN DELTA VA SQR
                    'deltaINVEST1 = INVEST1 - lag(INVEST1,1)',
                    'deltaINVEST2 = INVEST2 - lag(INVEST2,1)',
                    'deltaCASH = CASH - lag(CASH,1)',
                    'sqrINVEST1 = INVEST1 ^ 2',
                    'sqrINVEST2 = INVEST2 ^ 2',
                    
                    # CASH FLOW (cash flow nay la cash flow dung trong mo hinh khac voi cash flow trong code o tren), GROWTH
                    'CASHFLOW = CF/TTS',
                    'GROWTH = (CF-lag(CF))/lag(CF)',
                    
                    # BIEN TICH
                    'MPCF = lagMPDep * CASHFLOW',
                    'MPCASH = lagMPDep * CASH',
                    'deltaCASHMP = lagMPDep * deltaCASH',
                    
                    # TOBIN Q
                    'BV = TTS-NPT-TSCDVH',
                    'Q = MarketCap/BV',
                    
                    # BIEN TICH VOI TOBIN Q
                    'CASHQ = CASH * Q',
                    'CASHQMP = MPCASH * Q',
                    
                    # Kaplan-Zingales index
                    # CAUTIONS: ko biet co bien do tre o day hay ko
                    'KZ = -1.001909* (CF/BV) +0.2826389* Q -39.3678 *(Div/BV) -1.314759 * ((TVTDT+TSDTNH)/BV) + 3.139193 * ((NNH+NDH)/TTS)',
                    
                    # CRISIS
                    'CRISIS = case_when(Year >= 2008 & Year <= 2010 ~ 1, Year > 2010 ~ 0)'
                    )

# CAUTIONS: Nho chu y khoang trang khi them bien

for (x in columns_new) {
  lhs <- str_remove(regmatches(x, regexpr("=",x), invert = TRUE)[[1]][1], " ")
  rhs <- regmatches(x, regexpr("=",x), invert = TRUE)[[1]][2] # " =.* " TACH DAU BANG ( '=' ) VA IGNORE NHUNG DAU BANG CON LAI
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
indepVar <- c('lagINVEST1+ lagMPDep+ LEV + SIZE + ROA + CASH + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year) + Firm',
              'lagINVEST2+ lagMPDep+ LEV + SIZE + ROA + CASH + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year) + Firm')

regOLS1 <- lm(formula = paste0(depVar1,'~',indepVar[1]),data=dREGC)
regOLS1_formula <- cluster.vcov(regOLS1, ~ Firm + Year)
regOLS1 <- coeftest(regOLS1, regOLS1_formula)


regOLS2 <- lm(formula = paste0(depVar2,'~',indepVar[2]),data=dREGC)
regOLS2_formula <- cluster.vcov(regOLS2, ~ Firm + Year)
regOLS2 <- coeftest(regOLS2, regOLS2_formula)


Table2 <- list(regOLS1,regOLS2)
remove(depVar1, depVar2,indepVar, regOLS1, regOLS2, regOLS1_formula, regOLS2_formula)

#-----{ FINANCIAL CONSTRAINTS }------

devar   <- c('INVEST1', 'INVEST2')
indeVar <- c('lagMPDep+ CASH  +LEV + SIZE + ROA  + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year) + Firm'
             ,'lagMPDep+ CASH + MPCASH +LEV + SIZE + ROA  + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year) + Firm')
conditionVar <- c('KZ <= median(KZ)','KZ > median(KZ)', 'SIZE > median(SIZE)', 'SIZE <= median(SIZE)')


for (i in 1:2) {
  for (x in 1:4){
    M  <- formula(paste( devar[i],"~", indeVar[2]))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    reg_formula <- cluster.vcov(reg, ~ Firm + Year)
    reg <- coeftest(reg, reg_formula)
    assign(paste0('REG',i,x),reg);
  }
}

REG1 <- lm(formula = paste0(devar[1],'~',indeVar[1]),data= dREGC)
REG2 <- lm(formula = paste0(devar[1],'~',indeVar[2]),data= dREGC)
REG3 <- lm(formula = paste0(devar[2],'~',indeVar[1]),data= dREGC)
REG4 <- lm(formula = paste0(devar[2],'~',indeVar[2]),data= dREGC)
  
Table3 <- list(REG1, REG2, REG3, REG4,
               REG11,REG12,REG13,REG14,
               REG21,REG22,REG23,REG24)

rm(REG1, REG2, REG3, REG4,REG11,REG12,REG13,REG14,REG21,REG22,REG23,REG24, indeVar, devar, conditionVar,i,M,x,reg, reg_formula)


#-----{ CORPORATE INVESTMENT: SOEs vs non.SOEs }------------- 
# CHANGE 25/2/2023
# CHINH INDEPVAR : Thay doi as.factor(Year), Firm, Nganh -> Chon to hop tot nhat # Dong 286, 293 chon to hop tot nhat phu hop voi ky vong. 

devar   <- c('INVEST1', 'INVEST2')
indeVar <- 'lagMPDep+ CASH+ MPCASH +LEV + SIZE + ROA  + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year) + Firm'
conditionVar <- c('Bool_SOE == 1','Bool_SOE == 0')

for (i in 1:2) {
  for (x in 1:2){
    M  <- formula(paste( devar[i],"~", indeVar))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    reg_formula <- cluster.vcov(reg, ~ Firm + Year)
    reg <- coeftest(reg, reg_formula)
    assign(paste0('REG',i,x),reg);
  }
}

Table4 <- list(REG11,REG12,
               REG21,REG22)

rm(REG11,REG12,REG21,REG22, indeVar, devar, conditionVar,i,M,x,reg)



#-----{ CASH-CASH FLOW SENSITIVITY }------

devar   <- c('deltaCASH')
indepVar <- c('CASHFLOW + lagMPDep + MPCF + SIZE + deltaNWC + deltaSDEBT + Q + CAPEX')
conditionVar <- c('KZ <= median(KZ)','KZ > median(KZ)','SIZE <= median(SIZE)', 'SIZE > median(SIZE)','Bool_SOE == 1','Bool_SOE == 0')

for (x in 1:6){
  M  <- formula(paste0( devar,"~", indepVar))
  reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
  assign(paste0('REG',x),reg);
}             

REG <- lm(formula = paste0(devar,'~',indepVar),data= dREGC)

Table6 <- list(REG,REG1, REG2, REG3, REG4, REG5, REG6)

rm(x, 
   REG, REG1, REG2, REG3, REG4, REG5,REG6, 
   devar, indepVar, conditionVar, M, reg)



#-----{ INVESTMENT SMOOTHING }------

# CAUTIONS: deltaCASH duoc them vao bien doc lap, nhung trong mo hinh trong bai tham khao goc ko co
devar   <- c('deltaINVEST1', 'deltaINVEST2')
indepVar <- c('deltaCASH + INVEST1 + sqrINVEST1 +  lagMPDep + deltaCASHMP + LEV + SIZE + ROA + CASH + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year)', 
              'deltaCASH + INVEST2 + sqrINVEST2 +  lagMPDep + deltaCASHMP + LEV + SIZE + ROA + CASH + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year)' )

conditionVar <- c('KZ <= median(KZ)','KZ > median(KZ)','SIZE <= median(SIZE)', 'SIZE > median(SIZE)','Bool_SOE == 1','Bool_SOE == 0')

for (i in 1:2) {
  for (x in 1:6){
    M  <- formula(paste0( devar[i],"~", indepVar[i]))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    assign(paste0('REG',i,x),reg);
  }
}

REG1 <- lm(formula = paste0(devar[1],'~',indepVar[1]),data= dREGC)
REG2 <- lm(formula = paste0(devar[2],'~',indepVar[2]),data= dREGC)

Table7 <- list(REG1, REG2,
               REG11,REG12,REG13,REG14, REG15, REG16,
               REG21,REG22,REG23,REG24, REG25, REG26)

rm(REG1,REG2,REG11,REG12,REG13,REG14,REG15, REG16,REG21,REG22,REG23,REG24,REG25, REG26, indepVar, devar, conditionVar,i,M,x, reg)



#-------{ Monetary policy, cash holding and investment efficiencies. }-----------

devar   <- c('INVEST1', 'INVEST2')
indepVar <- c('Q + CASH + lagMPDep + CASHQ + CASHQMP + CRISIS + LEV + SIZE + ROA + CASH + CASHFLOW + GROWTH + Tangible + Bool_SOE')

conditionVar <- c('KZ <= median(KZ)','KZ > median(KZ)','SIZE <= median(SIZE)', 'SIZE > median(SIZE)','Bool_SOE == 1','Bool_SOE == 0')

for (i in 1:2) {
  for (x in 1:6){
    M  <- formula(paste0( devar[i],"~", indepVar))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    assign(paste0('REG',i,x),reg);
  }
}

REG1 <- lm(formula = paste0(devar[1],'~',indepVar),data= dREGC)
REG2 <- lm(formula = paste0(devar[2],'~',indepVar),data= dREGC)

Table8 <- list(REG1, REG2,
               REG11,REG12,REG13,REG14, REG15, REG16,
               REG21,REG22,REG23,REG24, REG25, REG26)

rm(REG1,REG2,REG11,REG12,REG13,REG14,REG15, REG16,REG21,REG22,REG23,REG24,REG25, REG26, indepVar, devar, conditionVar,i,M,x, reg)



#-------{ CRISIS }-----------

devar   <- c('INVEST1', 'INVEST2')
indepVar <- c('lagMPDep+ CASH  +LEV + SIZE + ROA  + CASHFLOW + GROWTH + Q + Tangible + Bool_SOE + as.factor(Year)')

conditionVar <- c('CRISIS == 0', 'CRISIS == 1')


for (i in 1:2) {
  for (x in 1:2){
    M  <- formula(paste0( devar[i],"~", indepVar))
    reg <- lm(M, data=subset(dREGC, eval(parse(text= conditionVar[x]))))
    assign(paste0('REG',i,x),reg);
  }
}

Table9 <- list(REG11,REG12,
               REG21,REG22)


rm(REG11,REG12,REG21,REG22, indepVar, devar, conditionVar,i,M,x, reg)

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

stargazer(Table3, type = 'text', column.labels = c('All Sample [1]','All Sample [1]','All Sample [2]','All Sample [2]','KZ-Low', 'KZ-High', 'SIZE-Big', 'SIZE-Small','KZ-Low', 'KZ-High', 'SIZE-Big', 'SIZE-Small'),
          covariate.labels = c('MP'),
          omit = c('Year','Firm', 'LEV', 'SIZE', 'ROA', 'CASHFLOW', 'GROWTH', 'Q', 'Tangible', 'Bool_SOE'), align = TRUE, out = 'Table3.htm',
          intercept.bottom = T, style = 'all', title = 'Table 3 Financial constraints, cash holdings and corporate investment.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')


# TABLE 4

stargazer(Table4, type = 'text', column.labels = c('SOE', 'Non-SOE','SOE', 'Non-SOE'), out = 'Table4.htm',
          covariate.labels = c('MP'),
          omit = c('Year','Firm', 'LEV', 'SIZE', 'ROA', 'CASHFLOW', 'GROWTH', 'Q', 'Tangible', 'Bool_SOE'), align = TRUE, 
          intercept.bottom = T, style = 'all', title = 'Table 4 Corporate investment: SOEs vs. non-SOEs.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')



# TABLE 6

stargazer(Table6, type = 'text', column.labels = c('All Sample', 'KZ-Low', 'KZ-High', 'Size-Low', 'Size-High', 'SOE', 'Non-SOE'),
          omit = c('Year','Firm','SIZE', 'deltaNWC', 'deltaSDEBT', 'Q','CAPEX'), align = TRUE, out = 'Table6.htm',
          intercept.bottom = T, style = 'all', title = 'Table 6 Monetary policy and cash-cash flow sensitivity.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')


# TABLE 7

stargazer(Table7, type = 'text', column.labels = c('All Sample','All Sample', 'KZ-Low', 'KZ-High', 'Size-Low', 'Size-High', 'SOE', 'Non-SOE', 'KZ-Low', 'KZ-High', 'Size-Low', 'Size-High', 'SOE', 'Non-SOE'),
          omit = c('Year','Firm','LEV', 'SIZE', 'ROA', 'CASHFLOW', 'GROWTH', 'Q', 'Tangible', 'Bool_SOE'), align = TRUE, out = 'Table7.htm',
          intercept.bottom = T, style = 'all', title = 'Table 7 Monetary policy, cash holding and investment smoothing.', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')


# TABLE 8

stargazer(Table8, type = 'text', column.labels = c('All Sample','All Sample', 'KZ-Low', 'KZ-High', 'Size-Low', 'Size-High', 'SOE', 'Non-SOE', 'KZ-Low', 'KZ-High', 'Size-Low', 'Size-High', 'SOE', 'Non-SOE'),
          omit = c('Year','Firm','CRISIS', 'LEV', 'SIZE', 'ROA', 'CASHFLOW', 'GROWTH', 'Tangible', 'Bool_SOE'), align = TRUE, out = 'Table8.htm',
          intercept.bottom = T, style = 'all', title = 'Table 8 Monetary policy, cash holding and investment efficiencies..', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')

# TABLE 9 TEST TABLE

stargazer(Table8, type = 'text', column.labels = c('Non-CRISIS', 'CRISIS'),
          omit = c('Year','Firm','CRISIS', 'LEV', 'SIZE', 'ROA', 'CASHFLOW', 'GROWTH', 'Tangible', 'Bool_SOE'), align = TRUE, out = 'Table9.htm',
          intercept.bottom = T, style = 'all', title = 'Table 9', digits = 3, notes.align = 'c', notes.append = T,
          report = 'v*c*t')

#------{ LEGACY }---------------------------------------------------------------

for (i in 1:2) {
  M  <- formula(paste( devar[i],"~", indevar))
  reg <- lm(M, data=subset(df_test, BoolGov == 1))
  R2 <- round(summary(reg)$adj.r.squared,4); assign(paste0("R2Adj",i), R2)
  
  # Xu ly phuong sai thay doi
  reg_formula <- cluster.vcov(reg, ~ NGANH)
  reg <- coeftest(reg, reg_formula)
  assign(paste0("reg",i), reg); 
}

