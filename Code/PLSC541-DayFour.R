#########################################################
# PLSC 541: American Political Institutions (Spring 2022)
#
# Code for February 14: Finding and working with
# court data.
#
# Set working directory (uncomment / change as necessary):

setwd("~/Dropbox (Personal)/PLSC 541")

# Load packages (install as needed, using install.packages();
# e.g.:
# install.packages("haven")
# etc.):

library(RCurl)
library(haven)
library(readr)
library(dataverse)
library(DescTools)
library(tidyverse)
library(lubridate)
library(usmap)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places
########################################################
# 1a. Supreme Court database (OT 1946-2010)...
#
# Case-Centered data, organized by docket number:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2021_01/SCDB_2021_01_caseCentered_Docket.csv.zip",ZIP)
scdb.cc<-read.csv(unz(ZIP,"SCDB_2021_01_caseCentered_Docket.csv"),
               stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)    # clean up...

# Data structure (not shown):

head(scdb.cc[1:5])

#########
# Justice-Centered data, organized by docket number:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2021_01/SCDB_2021_01_justiceCentered_Docket.csv.zip",ZIP)
scdb.jc<-read.csv(unz(ZIP,"SCDB_2021_01_justiceCentered_Docket.csv"),
               stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)    # clean up...

# "Legacy" data: OT1791-1945:
#
# Case-centered:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/Legacy_07/SCDB_Legacy_07_caseCentered_Citation.csv.zip",ZIP)
legacy.cc<-read.csv(unz(ZIP,"SCDB_Legacy_07_caseCentered_Citation.csv"),
                  stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)

# Justice-centered:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/Legacy_07/SCDB_Legacy_07_justiceCentered_Citation.csv.zip",ZIP)
legacy.jc<-read.csv(unz(ZIP,"SCDB_Legacy_07_justiceCentered_Citation.csv"),
                    stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)

######
# What can we do with all this?
#
# Modern, case-centered: Plot the number of Right to Privacy
# cases in each term, OT1946-2020:

privacy<-table(scdb.cc[scdb.cc$issueArea==5,]$term)

pdf("Plots/PrivacyCasesByTerm.pdf",6,5)
par(mar=c(4,4,2,2))
plot(privacy,xlab="Term",ylab="Frequency")
dev.off()

# Modern, justice-centered: Plot the proportion of liberal
# votes cast by each justice in cases involving the
# First Amendment:

FirstA<-with(scdb.jc[scdb.jc$issueArea==3,],
             prop.table(xtabs(~justiceName+(direction-1)),1))
FirstA<-FirstA[order(FirstA[,2]),] # sort

pdf("Plots/FirstAmdtLibVoting.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(FirstA[,2]*100,horiz=TRUE,las=2,cex.names=0.5,
        xlim=c(0,100),xlab="Liberal Voting Percentage")
dev.off()

# Legacy, case-centered: Histogram of the number of cases
# involving Native American tribes as litigants (either 
# as petitioner or respondent):

NATdata<-legacy.cc[(legacy.cc$petitioner==170 | 
                        legacy.cc$respondent==170),]

pdf("Plots/NATCasesByTerm.pdf",6,5)
par(mar=c(4,4,2,2))
hist(NATdata$term,xlim=c(1790,1945),breaks=(1945-1790),
     xlab="Term",main=" ")
dev.off()

##############
# 1b. The Supreme Court Justices database:
#
# Can't use this until Lee renews the SLL certificate:
#
# Justices<-read_csv("http://www.epstein.wustl.edu/s/justicesdata2021.csv")
#
# So instead use this for now:

Justices<-read_dta("https://github.com/PrisonRodeo/PLSC541-2022-git/raw/main/Data/justicesdata2021.dta")

# Example:

pdf("Plots/SCOTUS-DateNom-AgeNom.pdf",6,5)
par(mar=c(4,4,2,2))
with(Justices[Justices$recess==0,], plot(yrnom,agenom,
     main=" ",pch=19,
     ylab="Age at Nomination",xlab="Year of Nomination"))
dev.off()

##############
# 1c. "Segal-Cover" and "Martin-Quinn" scores
#
# Segal-Cover:

url<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC541-2022-git/main/Data/Segal-Cover.csv")
SC<-read.csv(text = url,stringsAsFactors = FALSE)
rm(url)

# Martin-Quinn:

url<-getURL("https://mqscores.lsa.umich.edu/media/2020/justices.csv")
MQ<-read.csv(text = url,stringsAsFactors = FALSE)
rm(url)

# Example: Aggregate MQ scores to the justice level 
# (one score per justice for his/her whole career):

MQ.j <- aggregate(post_med~justice,data=MQ,mean)

# Merge with Segal-Cover data:

SC2<-merge(SC,MQ.j,by=c("justice"))

# Plot them against each other:

pdf("Plots/SCvsMQ.pdf",6,5)
par(mar=c(4,4,2,2))
with(SC2, plot(Ideology, post_med,pch=20,
               xlab="Segal-Cover (liberalism)",
               ylab="Martin-Quinn (conservatism)",
               xlim=c(-0.3,1.3),ylim=c(-5,3))
     )
with(SC2, text(Ideology,post_med,
               labels=SC2$Nominee,pos=1,cex=0.7)
     )
abline(lm(post_med~Ideology,data=SC2),lwd=2)
dev.off()

#################################################
# 2. Federal Courts of Appeals Database (old...)
#
# 1925-1996 data:

ZIP<-tempfile()
download.file("http://www.cas.sc.edu/poli/juri/cta96_stata.zip",ZIP)
CoA1<-read_dta(unz(ZIP,"cta96_stata.dta"))
unlink(ZIP)
rm(ZIP)

# 1997-2002 data (annoyingly, the two datasets don't share identical
# columns, so they can't just be appended together):

ZIP<-tempfile()
download.file("http://www.cas.sc.edu/poli/juri/KH_update_stata.zip",ZIP)
CoA2<-read_dta(unz(ZIP,"KH_update_stata.dta"))
unlink(ZIP)
rm(ZIP)

# Quick figure: Percentage of cases in the database with at least
# one dissenting opinion, by year:

CoA1$Dissent<-ifelse(CoA1$dissent>0,1,0)
DissYearData<-aggregate(Dissent~year,data=CoA1,mean)
pdf("Plots/CoADissentsByYear.pdf",6,5)
par(mar=c(4,4,2,2))
with(DissYearData, plot(year,Dissent*100,t="l",lwd=2,
                        xlab="Year",ylab="Percentage Of Cases With Dissent"))
dev.off()

############################################
# 3. All federal courts...
#
# The Federal Judicial Center's (FJC's) biographical 
# database:
#
# https://www.fjc.gov/history/judges

FJC<-read_csv("https://www.fjc.gov/sites/default/files/history/judges.csv")

# Federal judges by Zodiac sign:

FJC$BDate<-paste(FJC$`Birth Month`,
                 FJC$`Death Day`,
                 FJC$`Birth Year`,
                 sep="-")
FJC$BDate<-mdy(FJC$BDate)
FJC$Sign<-Zodiac(FJC$BDate)

pdf("Plots/Judge-Zodiac.pdf",7,5)
par(mar=c(6,4,2,2))
barplot(table(FJC$Sign),las=2,ylab="Frequency")
abline(h=mean(table(FJC$Sign)),lty=2,col="red")
text(1,190,labels="Mean",col="red",cex=0.8)
dev.off()

########################################
# 4. State Supreme Courts...
#
# State Supreme Court Database (1995-1998):

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
SSC<-get_dataframe_by_name(filename="NSF 95-98 Beta Version_July 2005.tab",
                          dataset="10.7910/DVN/Z80F7P")

# Quick map of # of criminal cases by state, 1995-1998...
#
# Limit to criminal cases:

SSC.Crim<-SSC[SSC$genissue==1,]
SSC.Crim$NCases<-1

# ... and aggregate to the state level:

SSC.plot<-aggregate(NCases~state_2,data=SSC.Crim,sum)

# Merge in state names, which are bizarrely omitted...

data(state)
State<-data.frame(state_2=seq(1:50),
                  State=state.name)
SSC.plot<-merge(SSC.plot,State,by=c("state_2"),all=TRUE)

# ...get FIPS codes...

SSC.plot$fips<-fips(SSC.plot$State)

# ...specify legend colors, low and high:

low<-"yellow"
high<-"navy"

# ... and make the map:

pdf("Plots/SSCD-Map.pdf",10,7)
par(mar=c(0,1,0,1))
plot_usmap(data=SSC.plot,values="NCases",color="black",
           labels=TRUE) + 
        scale_fill_continuous(low=low, high=high,
                              name="Cases in the Database",
                              label=scales::comma) + 
        theme(legend.position="right")
dev.off()

