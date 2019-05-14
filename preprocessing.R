# Importing Dataset
library(readxl)
dataset <- read_excel("C://Users//nEW u//Documents//R//Placement-Survey//KIIT Placements Survey 2019 (Responses).xlsx")

#df = c("timestamp","email","roll","cgpa","future_plans","company","job_profile","salary","posting_site","C","OOP","DSA+DAA","DBMS","COA+HPCA","OS","SE+OOSD","CN",
#    "FLAT+CD","VQT","coding","GD","CS","TI","HRI","psychometry","technology","soft_skill","IRT","ICT","INT","is_extracurricular","society","name","hostel","remark","contact")

df = subset(dataset, select = c( -Timestamp, -`Email Address`, - `Roll number`, -`Full Name`))

#cf = factor(df$`Which company were you placed at?`)
#ucf = c(levels(cf))
ucf = na.exclude(df$`Which company were you placed at?`)
ucf = unlist(strsplit(ucf, split=","))
ucf = unlist(strsplit(ucf, split=" and "))
ucf = stringi::stri_trim_both(tolower(ucf))
#ucf = levels(factor(ucf))

c=1
cucf = character(length(ucf))
for(i in ucf){
  cucf[c] = i
  cucf[c] = switch(i,"capegemini" = "capgemini",
                 "ericssion" = "ericsson",
                 "i got offers from zycus" = "zycus",
                 "mu sigma" = "musigma",
                 "netcracker technologies" = "netcracker",
                 "tata consultancy services" = "tcs",
                 "tata technologies" = "tcs",
                 "hsbc technology" = "hsbc" ,
                 i
                 )
  c = c+1
}
#The companies and no.of offers
print(table(factor(cucf)))

             