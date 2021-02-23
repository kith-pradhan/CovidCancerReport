## ----part1, include=F, warning=F, message=FALSE, cache=F----------------------

#load the R packages we need. 
library("pander")
library("knitr")
library("rmarkdown")

library("data.table")
library("ggplot2")
library("stringr")



#x = fread("master_11212020.csv")
#x = fread("master_11252020.csv")
#x = fread("master_12012020.csv")
#x = fread("master_12022020.csv")
#x = fread("master_12062020.csv")
#x = fread("master_12072020.csv")
#x = fread("master_01102021.csv")
#x = fread("master_01122021.csv")
#x = fread("master_01142021.csv")
#x = fread("master_01152021.csv")
#x = fread("master_01162021.csv")
x = fread("master_01182021.csv")
colnames(x) = make.names(colnames(x))
#cbind(colnames(x))

#TODO: make sure these numbers are correct
#there are two columns labelled diagnosis
which(colnames(x)=="diagnosis")
x[,2]
x[,6]
x[x[[2]] != x[[6]], 1:6]
#get rid of column 2
colnames(x)[2] = "diagnosis.old"

#we need to remove these patients
`%notin%` <- Negate(`%in%`)
x = x[MRN %notin% c(02974444, 03101756,01263443, 05796848),]



#exclude patients that do not have cancer
x = subset(x, Malignancy.category!="")

#There's a category call "GI lower" and one dcalled "GI-lower"
#I'm assuming these should be merged
x[["Malignancy.category"]] = gsub(x[["Malignancy.category"]], pat="-", rep=" ")
table(x[["Malignancy.category"]])


#here we'll fix some of the problem variables
x[["YITZ.AB.INDEX.DATA"]] = as.numeric(x[["YITZ.AB.INDEX.DATA"]])


#Alive/Dead (column AZ) 
temp1 = trimws(tolower(x[["alive"]]))
table(temp1)
alive.dead=temp1
#alive.dead = NA
#alive.dead[grep(temp1, pat="a[lvi][vli]")] = "alive"
#alive.dead[grep(temp1, pat="d[ie][aei]")] = "dead"
##cbind(temp1, alive.dead)
x[,alive.dead:=alive.dead]


#Recent cancer tx (defined as in last 12 months) Column AF : yes/no 
table(x[["Medical.Cancer.Tx.in.the.past.12.months...Yes.No."]])
recent.cancer.tx = trimws(tolower(x[["Medical.Cancer.Tx.in.the.past.12.months...Yes.No."]]))
#table(recent.cancer.tx)
recent.cancer.tx.yes.no = NA
recent.cancer.tx.yes.no[grep(recent.cancer.tx, pat="^no")]="no" 
recent.cancer.tx.yes.no[grep(recent.cancer.tx, pat="^yes")]="yes" 
#cbind(recent.cancer.tx.yes.no, recent.cancer.tx)
x[,recent.cancer.tx.yes.no:=recent.cancer.tx.yes.no]


#Active cancer (column AB ) how many w active cancer in symptomatic vs asymptomatic
table(x[["Is.cancer.active."]])
#clean up a bit, only use entries that are yes/no
cancer.active = trimws(tolower(x[["Is.cancer.active."]]))
#table(cancer.active)
cancer.active.yes.no1 = NA
#cancer.active.yes.no[grep(cancer.active, pat="(no$)|(no )", perl=T)]="no"
cancer.active.yes.no1[grep(cancer.active, pat="(no)", perl=T)]="no"
cancer.active.yes.no1[grep(cancer.active, pat="(yes)|(mds)|(mgus)", perl=T)]="yes"
#cbind(cancer.active, cancer.active.yes.no)
x[,cancer.active.yes.no:=cancer.active.yes.no1]


#change the variable name
colnames(x)[which(colnames(x) == "Comorbidities.category.A.0.1.B.2.3.C....3")] = "Comorbidities.category"


#fix entries a bit
x[["COVID.Antibody.Test.Result"]] = tolower(x[["COVID.Antibody.Test.Result"]])


#BMI (column P)- mean/median for both sheets
x[["BMI"]] = as.numeric(x[["BMI"]])



#Age (column G)- mean/median for both sheets
x[["Age.at.Index.Date"]] = as.numeric(x[["Age.at.Index.Date"]])


hemeMalig = NA
hemeMalig[x[["Malignancy.category"]] == "Heme malignancy"] = "heme"
hemeMalig[x[["Malignancy.category"]] != "Heme malignancy"] = "nonheme"
x[,hemeMalig:=hemeMalig]



x[["Treatment.setting..Home.ED..GMF.ICU."]] = trimws(tolower(x[["Treatment.setting..Home.ED..GMF.ICU."]]))

#set the blank entires to N/AW to be consistent
x[Treatment.setting..Home.ED..GMF.ICU.=="",Treatment.setting..Home.ED..GMF.ICU.:="n/a"]


#fix these new variables
x[["Cancer.Active.Relapsed.Remission.POD"]] = (trimws(tolower(x[["Cancer.Active.Relapsed.Remission.POD"]])))
x[["Cancer.Active.Relapsed.Remission.POD"]] = gsub(x[["Cancer.Active.Relapsed.Remission.POD"]], pat="relapsed", rep="relapse")
table(x[,"Cancer.Active.Relapsed.Remission.POD"])


x[["baseline.steroids"]] = trimws(tolower(x[["baseline.steroids"]]))
table(x[["baseline.steroids"]])

steroid = x[["Steroid.use"]]
steroid[steroid != "Yes"] = "No"
table(steroid)
x[["Steroid.use"]] = steroid

x[["Charlson.Comorbidity.Index"]]
#the categories for charlson comorbidity index per DR. Halmos 
#should be 0-1, 2-3 and 4+
CCI = NA
CCI[x[["Charlson.Comorbidity.Index"]] <= 1] = "cci_0.1"
CCI[x[["Charlson.Comorbidity.Index"]] > 1 & x[["Charlson.Comorbidity.Index"]] <= 3] = "cci_2.3"
CCI[x[["Charlson.Comorbidity.Index"]] >= 4] = "cci_4up"
table(CCI)
x[,CCI:=CCI]


#new variable for active/cancer after 3 months
act.can = x[["Cancer.active.3.months.from.COVID.PCR.IGG"]]
act.can = trimws(tolower(act.can))
x[["Cancer.active.3.months.from.COVID.PCR.IGG"]] = act.can




## ----echo=T-------------------------------------------------------------------


tab1 = table(alive.dead=x[["alive.dead"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1)



## ----echo=T-------------------------------------------------------------------
table(x[["recent.cancer.tx.yes.no"]])
tab1 = table(recent.cancer=x[["recent.cancer.tx.yes.no"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1)



## ----echo =T------------------------------------------------------------------

#COVID IgG pos (column L) : numbers yes/no
table(x[["COVID.Antibody.Test.Result"]])
tab1 = table(covid.antibody.test=x[["COVID.Antibody.Test.Result"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1)



## ----echo =T------------------------------------------------------------------
table(x[["cancer.active.yes.no"]])
tab1 = table(cancer.active=x[["cancer.active.yes.no"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1)



## ----echo =T------------------------------------------------------------------

#Sex (column J) -how many M/F in each group
table(x[["Gender"]])
tab1 = table(gender=x[["Gender"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1)



## ----echo =T------------------------------------------------------------------
#Comorbidity category (Column O) - A/B/C- numbers for each group

table(x[["Comorbidities.category"]])
tab1 = table(comorb=x[["Comorbidities.category"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1)



## ----echo = T-----------------------------------------------------------------

#Malignancy category (column C)- how many in each group?
table(x[["Malignancy.category"]])
tab1 = table(mal.cat=x[["Malignancy.category"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1, simulate.p.value=T)


## ----echo = T-----------------------------------------------------------------

#Malignancy category (column C)- how many in each group?
table(x[["Dx_Cat"]])
tab1 = table(dx.cat=x[["Dx_Cat"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
fisher.test(tab1, simulate.p.value=T)


## ----echo=T-------------------------------------------------------------------
ggplot(x, aes(x=BMI, color=Asymptomatic.infection.yes.no, fill=Asymptomatic.infection.yes.no)) + 
    geom_histogram(alpha=0.5)
t.test(BMI~Asymptomatic.infection.yes.no, data=x)
wilcox.test(BMI~Asymptomatic.infection.yes.no, data=x)



## ----echo=T-------------------------------------------------------------------

summary(x[["Age.at.Index.Date"]])

ggplot(x, aes(x=Age.at.Index.Date, color=Asymptomatic.infection.yes.no, fill=Asymptomatic.infection.yes.no)) + 
    geom_histogram(alpha=0.5)
t.test(Age.at.Index.Date~Asymptomatic.infection.yes.no, data=x)
wilcox.test(Age.at.Index.Date~Asymptomatic.infection.yes.no, data=x)



## ----echo =T------------------------------------------------------------------


table(x[["Treatment.setting..Home.ED..GMF.ICU."]])




## ----echo =T------------------------------------------------------------------

table(x[["Race.Ethnicity.from.epic"]])



## -----------------------------------------------------------------------------
table(x[["Malignancy.category"]])

tab1 = table(malig=x[["hemeMalig"]], asymp.inf=x[["Asymptomatic.infection.yes.no"]])
print(tab1)
res = fisher.test(tab1)
print(res)


## -----------------------------------------------------------------------------
tab1 = table(malig=x[["hemeMalig"]], mortality=x[["alive.dead"]])
print(tab1)
res = fisher.test(tab1)
print(res)


## -----------------------------------------------------------------------------
res = t.test(Age.at.Index.Date~alive.dead, data=x)
print(res)
res = wilcox.test(Age.at.Index.Date~alive.dead, data=x)
print(res)



## -----------------------------------------------------------------------------
tab1 = table(abtest=x[["COVID.Antibody.Test.Result"]], mortality=x[["alive.dead"]])
print(tab1)
res = fisher.test(tab1)
print(res)



## -----------------------------------------------------------------------------

#get the categorical variables
ix.cat = which(sapply(colnames(x), function(var){
    #non numeric variables with less than 5 categories
    (!is.numeric(x[[var]])) & (length(unique(x[[var]])) < 5) ||
    #or variables with 2 distinct values
    (length(unique(na.omit(x[[var]]))) == 2)
}))

#get the numeric variables
ix.num = which(sapply(colnames(x), function(var){
    is.numeric(x[[var]])
}))
ix.num = setdiff(ix.num, ix.cat)

print(colnames(x)[ix.cat])
print(colnames(x)[ix.num])



## ---- results="asis"----------------------------------------------------------
#examine all cat vars
abtest = x[["COVID.Antibody.Test.Result"]]
for (i in ix.cat){
    vals = x[[i]] 
    res = NA
    try({
        tab1 = table(abtest, vals)
        res = fisher.test(tab1)
        if (res$p.value < 0.05){
            cat(paste0("\n###", colnames(x)[i]), "\n")
            #print(colnames(x)[i])
            cat(pander(tab1))
            cat(pander(res))
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
        }
    }, silent=T)
}


#examine numeric variables
#run t.test and wilcox
for (i in ix.num){
    vals = x[[i]] 
    res = NA
    res2 = NA
    try({
        tab1 = table(abtest, vals)
        res = t.test(vals~abtest)
        res2 = wilcox.test(vals~abtest)
        if (res$p.value < 0.05 || res2$p.value < 0.05){
            cat(paste0("\n###", colnames(x)[i]), "\n")
            #print(colnames(x)[i])
            cat(pander(res))
            cat(pander(res2))
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
        }
    }, silent=T)
}



## -----------------------------------------------------------------------------

abtest.val = x[["COVID.Antibody.Numerical.Value"]]
abtest.result = x[["COVID.Antibody.Test.Result"]]
plot(table(abtest.val))
abtest.result = as.numeric(factor(abtest.result))+runif(length(abtest.result))*.5
plot(abtest.val, abtest.result)
points(abtest.val[abtest.result < 1.75], abtest.result[abtest.result < 1.75], col="blue")
points(abtest.val[abtest.result > 1.75], abtest.result[abtest.result > 1.75], col="red")

pander(table(abtest.val))


#tab1 = table(abtest.result, paste0(x[[3]],"_", x[[4]]))
#write.table(file="malig.abneg.csv", cbind(tab1[1,tab1[1,]>0]), sep=",", col.names=F)



## ---- results="asis"----------------------------------------------------------


abtest.val = x[["COVID.Antibody.Numerical.Value"]]
for (i in c(ix.cat, ix.num)){
    vals = x[[i]] 
    res = NA
    try({
        mod0 = glm(abtest.val~1)
        mod1 = glm(abtest.val~vals)
        res = anova(mod0, mod1, test="LRT")
        pval = tail(res$Pr, 1)
        #pval = coefficients(summary(mod1))["vals","Pr(>|t|)"]
        if (pval < 0.10){
            cat(paste0("\n##", colnames(x)[i]), "\n")
            #print(colnames(x)[i])
            cat(pander(res))
            cat(pander(summary(mod1)))
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
        }
    }, silent=T)
}



## -----------------------------------------------------------------------------
#categorical variables
print(colnames(x)[ix.cat])

#numeric variables
print(colnames(x)[ix.num])


yitz.val = x[["YITZ.AB.INDEX.DATA"]]
hist(na.omit(yitz.val), breaks=100)



## ----results="asis"-----------------------------------------------------------

for (i in c(ix.cat, ix.num)){
    vals = x[[i]] 
    res = NA
    try({
        mod0 = glm(yitz.val~1)
        mod1 = glm(yitz.val~vals)
        res = anova(mod0, mod1, test="LRT")
        pval = tail(res$Pr, 1)
        #pval = coefficients(summary(mod1))["vals","Pr(>|t|)"]
        if (pval < 0.10){
            cat(paste0("\n##", colnames(x)[i]), "\n")
            #print(colnames(x)[i])

            cat("\n\n")
            df = data.frame(vals, yitz.val)
            g = ggplot(df, aes(x=vals, y=yitz.val)) + geom_point()
            print(g)
            cat("\n\n")

            cat("\n####LRT\n")
            cat(pander(res))
            cat("\n####Model Coefficients\n")
            cat(pander(summary(mod1)))
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
            cat("<br/><br/>\n")
        }
    }, silent=T)
}




## -----------------------------------------------------------------------------

var1 =  "Dx_Cat"                                                                                                                            
var2 = "cancer.active.yes.no"
#a function that takes in 2 categorical variables
#(the 2nd should have just 2 levels)
#Make a table that shows 
#for each level of the first var
#   the split by levels of the 2nd
#   result of fishertest vs remaining observations
makeTable <- function(x, var1, var2){
    vals1 = x[[var1]]
    vals2 = x[[var2]]

    tab1 = table(vals1,vals2)
    ns = colSums(tab1)

    tab2 = t(sapply(1:nrow(tab1), function(i){
        a1 = tab1[i,1]
        b1 = tab1[i,2]
        c1 = ns[1]-a1
        d1 = ns[2]-b1
        mat1 = matrix(c(a1,b1,c1,d1), ncol=2)
        pval = NA
        try({
            res = fisher.test(mat1)
            pval = res$p.value
        }, silent=T)
        p1 = format(round(a1/ns[1]*100, 2), nsmall = 2) 
        p2 = format(round(b1/ns[2]*100, 2), nsmall = 2) 
        s1 = paste0(a1, "(", p1, "%)")
        s2 = paste0(b1, "(", p2, "%)")
        s3 = format(round(pval, 2), nsmall=2)
        c(s1, s2, s3)
    }))
    rownames(tab2) = rownames(tab1)
    colnames(tab2) = c(colnames(tab1), "pval")
    tab2

    #add FDR qval to table
    if (nrow(tab2) > 2){
        tab2 = cbind(tab2, qval=p.adjust(tab2[,3]))
    }
    #find missing patients
    ix.missing = which(apply(cbind(vals1, vals2),1, function(a){any(is.na(a))}))
    list(tab2, x$MRN[ix.missing])
}


#tab1 = makeTable(x, var1, var2)
#knitr::kable(tab1)

sepvars = c("Cancer.active.3.months.from.COVID.PCR.IGG", "cancer.active.yes.no", "Asymptomatic.infection.yes.no", "COVID.Antibody.Test.Result", "alive.dead")
catvars = c("Dx_Cat", "Malignancy.category", "Malignancy.category_Version2", "Race.Ethnicity.from.epic", "Ethnicity", "Gender", "Comorbidities.category", "Solid.liquid", "CD20", "endocrine", "CAR.T", "BiTE", "endocrine...hormonal", "transplant.sct", "car.t.cellular.tx", "hemeMalig", "immuno", "Treatment.setting..Home.ED..GMF.ICU.", "Asymptomatic.infection.yes.no", "Cancer.Active.Relapsed.Remission.POD", "baseline.steroids", "Steroid.use", "CCI")



## ---- results='asis', echo=F--------------------------------------------------
#library("kableExtra")

for (var2 in sepvars){
    tab0 = table(x[[var2]])
    info = paste0(paste0(names(tab0), "(", tab0, ")"), collapse="  ")
    cat(paste0("\n##", var2, ":  ", info, "\n"))
    for (var1 in catvars){
        cat(paste0("\n###", var1, "\n"))
        res = makeTable(x, var1, var2)
        tab1  = res[[1]]
        missing.pats =  res[[2]]
        print(knitr::kable(tab1))
        cat("\n")
        if (length(missing.pats) > 0){
            pats = missing.pats
            cat(paste0("missing:  ", paste0(missing.pats, collapse=", "), "\n"))
        }
    }
}







## -----------------------------------------------------------------------------

getDate <- function(a){
    d = NA
    try({
        d = as.Date(a, format="%m/%d/%Y")
    }, silent=T)
    d
}

#find all the data with valid dates (first/last)
d1 = sapply(x[["Date.of.first.positive.SARS.CoV.2.PCR."]], getDate)
d2 = sapply(x[["Date.of.last.positive.SARS.CoV.2.PCR."]], getDate)
shedding.time = d2 - d1
table(shedding.time)
x[,shedding.time:=shedding.time]



## -----------------------------------------------------------------------------

#list all shedding times by cat
x[shedding.time>0,shedding.time, by=Malignancy.category]

#average shedding times by category
x[shedding.time>0,mean(shedding.time), by=Malignancy.category]


#is there a difference in average shedding times by category?
res = aov(shedding.time~Malignancy.category, data=subset(x, shedding.time > 0))
summary(res)


#non-parametric test (more than 2 groups)
res = kruskal.test(shedding.time~Malignancy.category, data=subset(x, shedding.time > 0))
print(res)





## -----------------------------------------------------------------------------



#list all shedding times by cat
x[shedding.time>0,shedding.time, by=Solid.liquid]

#average shedding times by category
x[shedding.time>0,mean(shedding.time), by=Solid.liquid]


#is there a difference in average shedding times by category?
res = aov(shedding.time~Solid.liquid, data=subset(x, shedding.time > 0))
summary(res)

#non-parametric(can be used for 2 groups)
#
res = kruskal.test(shedding.time~Solid.liquid, data=subset(x, shedding.time > 0))
print(res)

#non-parametric wilcox 2-group comparison
res = wilcox.test(shedding.time~Solid.liquid, data=subset(x, shedding.time > 0))
print(res)




## ---- echo=F, results=F-------------------------------------------------------

sol.liq = x[["Solid.liquid"]]
ab.test = x[["COVID.Antibody.Test.Result"]]
act.can = x[["Cancer.active.3.months.from.COVID.PCR.IGG"]]
med.treat = x[["Medical.Cancer.treatment.90.days"]]

table(act.can)
table(med.treat)
table(ab.test)
table(sol.liq)

#need to fix the confounding variables
act.can = trimws(tolower(act.can))

table(trimws(tolower(med.treat)))
med.treat = (trimws(tolower(med.treat)))
#if it starts with n set it to no
#if it starts with y set to y
#NA otherwise
med.treat2 = NA
med.treat2[grep(med.treat, pat="^n")] = "no"
med.treat2[grep(med.treat, pat="^y")] = "yes"
#the one unknown patient has be reclassified as yes!
med.treat2[grep(med.treat, pat="^unkno")] = "yes"
table(med.treat2)


fisher.test(table(sol.liq, ab.test))

df1 = data.frame(ab.test, sol.liq, act.can, med.treat2)




## -----------------------------------------------------------------------------


by(df1, IND=df1[,"act.can"], FUN=function(a){
    tab1 = table(a[,"sol.liq"], a[,"ab.test"])
})

by(df1, IND=df1[,"act.can"], FUN=function(a){
    tab1 = table(a[,"sol.liq"], a[,"ab.test"])
    fisher.test(tab1)
})




## -----------------------------------------------------------------------------


by(df1, IND=df1[,"med.treat2"], FUN=function(a){
    tab1 = table(a[,"sol.liq"], a[,"ab.test"])
})

by(df1, IND=df1[,"med.treat2"], FUN=function(a){
    tab1 = table(a[,"sol.liq"], a[,"ab.test"])
    fisher.test(tab1)
})



## -----------------------------------------------------------------------------

df1$ab.test = factor(df1$ab.test)

#just solid/liquid
res = glm(df1, formula="ab.test~sol.liq", family="binomial")
summary(res)


#with confounders
res2 = glm(df1, formula="ab.test~med.treat2 + act.can + sol.liq", family="binomial")
summary(res2)





## -----------------------------------------------------------------------------


v1 = x[["Date.of.first.positive.SARS.CoV.2.PCR."]]
v2 = x[["Date.and.Time.of.COVID.Antibody.Test"]]


d1 = as.Date(v1, format="%m/%d/%Y")
#remove clock time stamp
v2 = gsub(v2, pat=" .*", rep="")
d2 = as.Date(v2, format="%m/%d/%Y")

diff.time = as.numeric(d2-d1)

#plot(sort(table(diff.time)))
hist(diff.time, 100)

mean(diff.time, na.rm=T)
median(diff.time, na.rm=T)




## ----exit, echo=T, warning=F, message=FALSE, cache=F--------------------------
knit_exit()

