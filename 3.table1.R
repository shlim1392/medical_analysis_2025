library(data.table);library(magrittr);library(tableone)

## Exam data: 09-15
dt <- fread("example_g1e.csv")

CreateTableOne(data=dt)

# - 범주형 변수는 개수(백분율)로 요약된다. 
# - 연속형 변수는 normal인 경우 mean(sd)로 나타나고, nonnormal인 경우 median(IQR)로 요약된다.

# 함수에는 다양한 옵션이 존재한다. 세부적인 옵션 설정을 통해 원하는 table1을 만들 수 있다. 주요 옵션은 다음과 같다.
# 
# - `CreateTableOne`
# - vars : 테이블에 들어갈 변수들
# - factorVars : 범주형 변수들
# - data : 데이터
# - strata : 그룹 변수 지정
# - includeNA : 범주형 변수에서 `NA`를 하나의 범주로 포함할지 여부

## Categorical variable conversion

# **factorVars** 인자를 사용하여 범주형 변수를 지정할 수 있다. 
# 이때 **vars** 인자를 통해 전체 데이터 셋 중 테이블에 들어갈 변수를 설정할 수 있고, 지정하지 않을 시 데이터 셋의 모든 변수가 포함된다.

myVars <- c("HGHT", "WGHT", "BMI", "HDL", "LDL", "TG", "SGPT", 
            "Q_PHX_DX_STK", "Q_PHX_DX_HTDZ", "Q_HBV_AG", "Q_SMK_YN")
# Categorical variables
catVars <- c("Q_PHX_DX_STK", "Q_PHX_DX_HTDZ", "Q_HBV_AG", "Q_SMK_YN")
t1 <- CreateTableOne(vars = myVars, factorVars = catVars, data = dt)
t1

# - 범주형 변수로 설정한 컬럼의 요약값이 mean(sd)에서 n(percentage)로 바뀐 것을 볼 수 있다.
# 
# - 두 개의 범주가 있는 범주형 변수의 경우, 두 번째 범주의 요약값만 출력된다. 예를 들어 0과 1의 범주가 있을 때, 범주1의 개수와 백분율이 출력된다. 
#이는 옵션 설정을 통해 전체 범주의 요약값을 출력하도록 변경할 수 있다.
# 
# - 3개 이상의 범주가 있을 때에는 모든 범주의 값이 요약되며, 백분율은 누락된 값을 제외한 후 계산된다.

## Multiple group summary 

#strata 인자를 설정하여 그룹별 연산을 할 수 있다. **strata**는 dplyr 패키지의 group_by() 함수와 유사하며, 그룹 연산을 할 변수를 지정하여 사용할 수 있다.
t2 <- CreateTableOne(data = dt,
                     vars = myVars,
                     strata = "Q_SMK_YN",
                     factorVars = catVars,
                     includeNA = F)
t2

# - 연속형 변수의 경우, 기본적으로 one-way ANOVA test가 적용되며 nonnormal일 경우 옵션 설정을 통해 Kruskal–Wallis one-way ANOVA test를 적용할 수 있다.
# 
# - 범주형 변수의 경우, 기본적으로 chisq-test가 적용되며 print 함수의 exact 옵션 설정을 통해 fisher-test를 적용할 수 있다.

# CreateTableOne() 함수를 사용하여 테이블을 만든 후, **print** 명령어로 세부 옵션을 지정할 수 있다. 주요 옵션은 다음과 같다.
# 
# - `print`
# - showAllLevels, cramVars : 2범주인 변수에서 2범주를 다 보여줄 변수

print(t1, showAllLevels = T)
print(t1, cramVars="Q_PHX_DX_STK")

# - nonnormal : 비모수통계를 쓸 연속 변수
print(t1, nonnormal="LDL")
print(t2, nonnormal="LDL")

# - exact : fisher-test를 쓸 범주형 변수
print(t2, exact=c("Q_PHX_DX_STK", "Q_PHX_DX_HTDZ"))

# - smd : standardized mean difference smd(standardized mean difference)를 table1에 포함할 수 있다
print(t2, smd = TRUE)


#summary 함수를 쓰면 누락값을 포함한 table1의 자세한 정보를 알 수 있다. 연속형 변수의 값들이 먼저 출력되며 그 다음으로 범주형 변수의 요약값이 출력된다.
summary(t1)

#범주형, 연속형 따로 구분하여 출력
t2$CatTable
t2$ContTable


#`write.csv` 함수를 사용하여 table1을 csv파일로 저장할 수 있다.
table1 <- print(t2)
write.csv(table1, file = "table1.csv")

