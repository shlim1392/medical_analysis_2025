library(data.table);library(magrittr);library(survey);library(survival);library(survminer)
library(jstable);library(jskm)
## csv
bnc <- fread("csv/nsc2_bnc_1000.csv") 

## Death date: last day in month
bnd <- fread("csv/nsc2_bnd_1000.csv")[, Deathdate := (lubridate::ym(DTH_YYYYMM) %>% lubridate::ceiling_date(unit = "month") - 1)][]
bnd[BTH_YYYY == "1921LE"]$BTH_YYYY <- "1921"
m20 <- fread("csv/nsc2_m20_1000.csv") 
m30 <- fread("csv/nsc2_m30_1000.csv") 
m40 <- fread("csv/nsc2_m40_1000.csv")[SICK_CLSF_TYPE %in% c(1, 2, NA)]            ## Exclude 3
m60 <- fread("csv/nsc2_m60_1000.csv") 
g1e_0915 <- fread("csv/nsc2_g1e_0915_1000.csv") 
g1e_0208 <- fread("csv/nsc2_g1e_0208_1000.csv") 


g1e <- rbind(g1e_0915[,.(RN_INDI,HME_YYYYMM, G1E_BP_SYS,G1E_BMI,Q_SMK_YN,G1E_FBS)],g1e_0208[,.(RN_INDI,HME_YYYYMM, G1E_BP_SYS,G1E_BMI,Q_SMK_YN,G1E_FBS)])


g1e[, Indexdate := (lubridate::ym(HME_YYYYMM) %>% lubridate::floor_date(unit = "month") - 1)][]

#1. Logistic 회귀 분석

g1e[order(HME_YYYYMM),.SD[1],keyby="RN_INDI"]

g1e <- na.omit(g1e)

g1e[,BMI_cat:= ifelse(G1E_BMI < 18.5,"< 18.5",
                      ifelse(18.5 <= G1E_BMI & G1E_BMI <25, ">= 18.5 & < 25",
                             ifelse(25 <= G1E_BMI & G1E_BMI <30, ">= 25 & < 30",">= 30")))]

g1e[,smoke:=ifelse(Q_SMK_YN==1,"Never",
                   ifelse(Q_SMK_YN==3,"Current","Ex-smoker"))]

dm_cd <- paste0("E", c(10:14))

g1e[,dm:=0]

g1e[RN_INDI %in% unique(m40[like(MCEX_SICK_SYM, paste(dm_cd, collapse = "|"))]$RN_INDI)]$dm <- 1

g1e[,.N,keyby="dm"]

g1e <- merge(g1e,unique(bnd[,.(RN_INDI,BTH_YYYY)]),by="RN_INDI")

g1e[,Age := as.integer(substr(HME_YYYYMM,1,4)) - as.integer(BTH_YYYY)]

glm_result <- glm(dm ~ smoke + BMI_cat + Age, data = g1e, family = "binomial")
summary(glm_result)

glmshow.display(glm_result)



#2. CCI 점수별 생존분석

data.incl <- g1e[order(HME_YYYYMM),.SD[.N],keyby="RN_INDI"][,.(RN_INDI,Indexdate)]

data.asd <- merge(bnd, bnc[, .(SEX = SEX[1]), keyby = "RN_INDI"], by = "RN_INDI") %>% 
  merge(data.incl, by = "RN_INDI") %>% 
  .[, `:=`(Age = year(Indexdate) - as.integer(substr(BTH_YYYY, 1, 4)),
           Death = as.integer(!is.na(DTH_YYYYMM)),
           Day_FU = as.integer(pmin(as.Date("2015-12-31"), Deathdate, na.rm =T) - Indexdate))] %>% .[, -c("BTH_YYYY", "DTH_YYYYMM", "Deathdate")] 



code.cci <- list(
  MI = c("I21", "I22", "I252"),
  CHF = c(paste0("I", c("099", 110, 130, 132, 255, 420, 425:429, 43, 50)), "P290"),
  Peripheral_VD = c(paste0("I", 70, 71, 731, 738, 739, 771, 790, 792), paste0("K", c(551, 558, 559)), "Z958", "Z959"),
  Cerebro_VD = c("G45", "G46", "H340", paste0("I", 60:69)),
  Dementia = c(paste0("F0", c(0:3, 51)), "G30", "G311"),
  Chronic_pulmonary_dz = c("I278", "I279", paste0("J", c(40:47, 60:67, 684, 701, 703))),
  Rheumatologic_dz = paste0("M", c("05", "06", 315, 32:34, 351, 353, 360)),
  Peptic_ulcer_dz = paste0("K", 25:28),
  Mild_liver_dz = c("B18", paste0("K", c(700:703, 709, 713:715, 717, 73, 74, 760, 762:764, 768, 769)), "Z944"),
  DM_no_complication = paste0("E", c(100, 101, 106, 108:111, 116, 118:121, 126, 128:131, 136, 138:141, 146, 148, 149)),
  DM_complication = paste0("E", c(102:105, 107, 112:115, 117, 122:125, 127, 132:135, 137, 142:145, 147)),
  Hemi_paraplegia = paste0("G", c("041", 114, 801, 802, 81, 82, 830:834, 839)),
  Renal_dz = c("I120", "I131", paste0("N", c("032", "033", "034", "035", "036", "037", "052", "053", "054", "055", "056", "057",
                                             18, 19, 250)), paste0("Z", c(490:492, 940, 992))),
  Malig_with_Leuk_lymphoma = paste0("C", c(paste0("0", 0:9), 10:26, 30:34, 37:41, 43, 45:58, 60:76, 81:85, 88, 90, 97)),
  Moderate_severe_liver_dz = c(paste0("I", c(85, 859, 864, 982)), paste0("K", c(704, 711, 721, 729, 765:767))),
  Metastatic_solid_tumor = paste0("C", 77:80),
  AIDS_HIV = paste0("B", c(20:22, 24))
)
cciscore <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 6, 6)
names(cciscore) <- names(code.cci)

#roll merge 
#dt의 날짜를 기준으로 data.asd를 연결할 때 정확히 일치하는 날짜가 없다면 data.asd의 날짜를 뒤로 밀어서 일치하는 날짜를 찾는다. 
#결과적으로 data.asd의 Indexdate 기준으로 앞 날짜에 m40의 Incidate가 존재한다면 병합이 되는 로직이라고 할 수 있다.

info.cci <- lapply(names(code.cci), function(x){
  data.asd[, MDCARE_STRT_DT := Indexdate]
  dt <- m40[like(MCEX_SICK_SYM, paste(code.cci[[x]], collapse = "|"))][, MDCARE_STRT_DT := as.Date(as.character(MDCARE_STRT_DT), format = "%Y%m%d")][, .(RN_INDI, MDCARE_STRT_DT, Incidate = MDCARE_STRT_DT)]  
  dt[, .SD[1], keyby = c("RN_INDI", "MDCARE_STRT_DT")][data.asd, on = c("RN_INDI", "MDCARE_STRT_DT"), roll = 365][, ev := as.integer(!is.na(Incidate))][]$ev * cciscore[x]
}) %>%  do.call(cbind, .) %>% cbind(rowSums(.))
colnames(info.cci) <- c(paste0("Prev_", names(code.cci)), "CCI")


data.asd <- cbind(data.asd,info.cci[,18])
setnames(data.asd,"V2","CCI")

data.asd[,.N,keyby="CCI"]
data.asd[,CCI_group := ifelse(CCI >= 3 , ">= 3", "< 3")]

data.asd[,.N,keyby="CCI_group"]


fit <- survfit(Surv(Day_FU, Death) ~ CCI_group, data = data.asd)

summary(fit)
#그래프

jskm(fit,pval=T,legendposition = c(0.85, 0.5), timeby = 365, table=T)
jskm(fit,pval=T,legendposition = c(0.85, 0.5), timeby = 365, table=T, cumhaz = T)



#Hazard ratio
cox <- coxph(Surv(Day_FU, Death) ~ CCI_group+Age+SEX,
                  data=data.asd, model=T)



cox <- cox2.display(cox)$table
cox



#
library(survival)
library(jskm)

head(colon)

km <- survfit(Surv(time, status) ~ rx, data = colon)

jskm(km, pval = T, marks = F, table = T, surv.scale = "percent", cumhaz = T, ylab = "Cumulative incidence")

km %>% summary


cmodel <- coxph(Surv(time, status) ~ age + sex + rx, data = colon, model = T)
cmodel

cox2.display(cmodel)$table


library(openxlsx)

tb <- createWorkbook("tb")

#
addWorksheet(tb, "test")
writeDataTable(tb, "test", aa, rowNames = F)



saveWorkbook(tb, "output.xlsx", overwrite = T)
