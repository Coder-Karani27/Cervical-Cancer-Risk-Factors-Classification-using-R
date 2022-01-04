#load the packages
options(warn=-1)
library('caret')
library('lattice')
library('ggplot2')
library('naniar')
library('ROSE')
library('rpart')

cervical<-read.csv(file="C:\\Users\\hamza\\Desktop\\CourseProject\\risk_factors_cervical_cancer.csv",sep=',',header=TRUE)

#removing 2 attributes that are irrelevant 
cervical<-subset(cervical,select= -c(STDs..Time.since.first.diagnosis,STDs..Time.since.last.diagnosis))
print("AFTER REMOVING IRRELEVANT COLUMNS")
print(str(cervical))

#converting the target variables into factors 
target<-c("Hinselmann","Schiller","Citology","Biopsy")
for(x in target){
  cervical[,x]=as.factor(cervical[,x])}
print(str(cervical))

#converting the "?" into NA 
na_string=c("?"," ?"," ? ","? ")
cervical<- cervical %>% replace_with_na_all(condition = ~. %in% na_string) #gives us a tibble 

#convert tibble back to dataframe
cervical<-as.data.frame(cervical)

#converting the attributes to numerical and bool type
intData<-c("Number.of.sexual.partners","First.sexual.intercourse","Num.of.pregnancies","STDs..number.")
boolData<-c("Smokes","Hormonal.Contraceptives","IUD","STDs","STDs.condylomatosis","STDs.cervical.condylomatosis","STDs.vaginal.condylomatosis","STDs.vulvo.perineal.condylomatosis","STDs.syphilis","STDs.pelvic.inflammatory.disease","STDs.genital.herpes","STDs.molluscum.contagiosum","STDs.AIDS","STDs.HIV","STDs.Hepatitis.B","STDs.HPV","Dx.Cancer","Dx.CIN","Dx.HPV","Dx")
realData<-c("Smokes..years.","Smokes..packs.year.","Hormonal.Contraceptives..years.","IUD..years.")

for(x in intData){
  cervical[,x]=as.integer(cervical[,x])
}
for(x in boolData){
  cervical[,x]= as.factor(cervical[,x])
}
for(x in realData){
  cervical[,x]=as.numeric(cervical[,x])
}

print("AFTER CONVERSION INTO APPROPRIATE CATEGORY")
print(str(cervical))

View(cervical)

#check for na values
colSums(is.na(cervical))
na_vec<-(which(!complete.cases(cervical)))
print(na_vec)

#discard na values
cervical_no_na<-cervical[-na_vec,]
print(str(cervical_no_na))
View(cervical_no_na)

#check if any na exists
colSums(is.na(cervical_no_na))
