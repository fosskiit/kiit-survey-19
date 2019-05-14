# Importing Dataset
library(readxl)
formatted <- read_excel("C://Users//nEW u//Documents//R//Placement-Survey//github//formatted.xlsx")

library(scales)
library(ggplot2)
# User-implemented mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculating the Average Salary
lpa = gsub('>','',formatted$`salary-LPA`)
lpa = c(as.integer(lpa))
#Highest Lowest Mean Mode
hlpa = max(lpa, na.rm = TRUE)
mlpa = mean(lpa, na.rm = TRUE)
llpa = min(lpa, na.rm = TRUE)
medlpa = median(lpa, na.rm = TRUE)
modelpa = Mode(lpa)


#cgpa
cgpa = formatted$CGPA
cgpa = c(sapply(cgpa, switch, "<7.5" = 7.0, ">7.5" = 7.5, ">8" = 8.0, ">8.5" = 8.5, ">9" = 9, ">9.5" = 9.5))


hist(cgpa, xlab= "CGPA", col = "yellow", border = "blue")
# ggplot(data = formatted, aes=(x=CGPA)) + geom_bar()

# Importance of C
clang = c(as.integer(formatted$`OOP (C++/JAVA)`))
clang = na.exclude(clang)
tab = table(clang)
datalabel = paste(c(tab),sep = "%")
total = sum(tab)
dataperc = (tab/total)*100
colors = c('red','orange','green')
pie(dataperc, labels = datalabel, col=colors)

# LPA vs CGPA (hostels)
lpa[is.na(lpa)]<-0
ggplot(data = formatted, aes(x = CGPA, y = lpa, color = hostel, size = 3)) + geom_point()

# Societies
sf = factor(formatted$`society/group`)
usf = c(levels(sf))



#Creating a ggplot pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#########################################
# Calculating the relative importance of subjects
scorer <- function(subject){
  
  subject = as.integer(c(unlist(subject)))
  subject = na.exclude(subject)
  tab = table(subject)
  score = c(c(tab) * c(1,2,3))
  #print(score)
  return (sum(score))
}

drawPie<-function(startIndex, leng){
  score = numeric(leng)
  subjects = colnames(formatted)[startIndex:(startIndex+leng)]
  j=1
  for(i in startIndex:(startIndex+leng)){
    score[j] = scorer(formatted[i])
    j=j+1
    print(score[j-1])
  }
  
  df = data.frame("subject" = subjects, "xcore" = score)
  print(df)
  total=sum(score)
  bp<- ggplot(df, aes(x="", y=xcore, fill=subject)) + geom_bar(width = 1, stat = "identity")
  pie <- bp + coord_polar("y", start=0)
  
  pie + scale_fill_manual(values=c('#683C42','#6E4C5F','#66627A','#547989','#488E8A','#59A07D','#82AE6A','#B7B65D','#F0B865'))+ blank_theme +
    theme(axis.text.x=element_blank())+
    geom_text(aes(y = xcore/3 + c(0, cumsum(xcore)[-length(xcore)]), 
                  label = percent(xcore/total)), size=5)+title(main = "Areas to focus on:")  
}

drawPie(7,8)
#drawPie(15,7)
#print(subjects)


# Scores for shortlisting process
getScores<-function(topic)
{
  scores = as.numeric(topic)
  for(c in seq_along(scores)){
    if(is.na(scores[c]))
    {
      st = topic[c]
      st = strsplit(st,",")[[1]]
      #st = trimws(st)
      
      scores[c] = max(as.numeric(st))
      
    }
  }
  return (scores)
}

#print(getScores(formatted$`Coding Test`))


drawBarPlot<-function(startIndex, leng){
  score = numeric(leng)
  cdf = formatted[,startIndex:(startIndex+leng)]
  subjects = colnames(cdf)#colnames(formatted)[startIndex:(startIndex+leng)]
  j=1
  for(subject in subjects){
    
    cdf[subject] = getScores(unlist(cdf[subject]))
     
    #cat(subjects[i-startIndex+1],score[j])
    j=j+1
  }
  print(score)
  return (cdf)
}

cdf = drawBarPlot(16,6)

#draw bar plots
ei = table(factor(formatted$`extra-curricular-importance`))
eic = prop.table(ei)*100
eidf = as.data.frame(ei)
eidf = cbind(eidf,data.frame(eic))
colnames(eidf) = c('cat','count','cat1','percentage')
ggplot() + geom_bar(aes(y=percentage, x=1, fill = cat), data = eidf, stat = "identity")

#bdf = data.frame(VQR=integer(3),Coding=integer(3),GD=integer(3),Case=integer(3),TI=integer(3),PI=integer(3),Psych=integer(3))
#colnames(bdf) = colnames(cdf)rownames(bdf) = c('L','M','H')
bdf = data.frame(Round=character(),Percentage=double(),State=character())

for(column in colnames(cdf)){
  t = table(factor(unlist(cdf[column])))
  t = c(prop.table(t)*100)
  b = data.frame(Round=c(column, column, column), Percentage=c(t[1], t[2], t[3]), State=c('L', 'M', "H") )
  bdf = rbind(bdf,b)
} 

#Ordering
#bdf = within(bdf, State = factor(State, levels = names(sort(table(State), decreasing = TRUE))))
ggplot(data = bdf) + geom_bar(aes(y=Percentage, x= Round , fill = State), stat = "identity") +
  coord_flip() + scale_fill_brewer(palette = 10)

