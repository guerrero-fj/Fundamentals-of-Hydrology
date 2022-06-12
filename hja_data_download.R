################################################################################
# Predicting Total Suspended Solids in Headwater Streams in the Oregon Cascades
################################################################################

#EXPLORATORY DATA ANALYSIS

#SET UP

#Packages

#DATA

# Package ID: knb-lter-and.4021.23 Cataloging System:https://pasta.edirepository.org.
# Data set title: Stream chemistry concentrations and fluxes using proportional sampling in the Andrews Experimental Forest, 1968 to present.
# Data set creator:    - Andrews Forest LTER Site 
# Data set creator:  Richard Fredriksen -  
# Contact:    - Information Manager   - hjaweb@fsl.orst.edu
# Contact:  Donald Henshaw -    - don.henshaw@oregonstate.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

################################################################################
#Proportional stream water sample nutrient concentrations (Oct 1 1968 - May 22 2019)
################################################################################

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4021/23/1d31b3fa3d35cefc35cc42f35c3b3a8d" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "STCODE",     
                 "ENTITY",     
                 "SITECODE",     
                 "WATERYEAR",     
                 "DATE_TIME",     
                 "LABNO",     
                 "TYPE",     
                 "INTERVAL",     
                 "MEAN_LPS",     
                 "Q_AREA_CM",     
                 "QCODE",     
                 "PH",     
                 "PHCODE",     
                 "COND",     
                 "CONDCODE",     
                 "ALK",     
                 "ALKCODE",     
                 "SSED",     
                 "SSEDCODE",     
                 "SI",     
                 "SICODE",     
                 "UTP",     
                 "UTPCODE",     
                 "TDP",     
                 "TDPCODE",     
                 "PARTP",     
                 "PARTPCODE",     
                 "PO4P",     
                 "PO4PCODE",     
                 "UTN",     
                 "UTNCODE",     
                 "TDN",     
                 "TDNCODE",     
                 "DON",     
                 "DONCODE",     
                 "PARTN",     
                 "PARTNCODE",     
                 "UTKN",     
                 "UTKNCODE",     
                 "TKN",     
                 "TKNCODE",     
                 "NH3N",     
                 "NH3NCODE",     
                 "NO3N",     
                 "NO3NCODE",     
                 "NA",     
                 "NACODE",     
                 "K",     
                 "KCODE",     
                 "CA",     
                 "CACODE",     
                 "MG",     
                 "MGCODE",     
                 "SO4S",     
                 "SO4SCODE",     
                 "CL",     
                 "CLCODE",     
                 "DOC",     
                 "DOCCODE",     
                 "PVOL",     
                 "PVOLCODE",     
                 "ANCA",     
                 "ANCACODE"    ), check.names=TRUE)

unlink(infile1)

# attempting to convert dt1$DATE_TIME dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1DATE_TIME<-as.POSIXct(dt1$DATE_TIME,format=tmpDateFormat)

# Keep the new dates only if they all converted correctly
if(length(tmp1DATE_TIME) == length(tmp1DATE_TIME[!is.na(tmp1DATE_TIME)])){dt1$DATE_TIME <- tmp1DATE_TIME } else {print("Date conversion failed for dt1$DATE_TIME. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE_TIME) 


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$STCODE)!="factor") dt1$STCODE<- as.factor(dt1$STCODE)
if (class(dt1$ENTITY)=="factor") dt1$ENTITY <-as.numeric(levels(dt1$ENTITY))[as.integer(dt1$ENTITY) ]               
if (class(dt1$ENTITY)=="character") dt1$ENTITY <-as.numeric(dt1$ENTITY)
if (class(dt1$SITECODE)!="factor") dt1$SITECODE<- as.factor(dt1$SITECODE)
if (class(dt1$WATERYEAR)=="factor") dt1$WATERYEAR <-as.numeric(levels(dt1$WATERYEAR))[as.integer(dt1$WATERYEAR) ]               
if (class(dt1$WATERYEAR)=="character") dt1$WATERYEAR <-as.numeric(dt1$WATERYEAR)                                   
if (class(dt1$LABNO)!="factor") dt1$LABNO<- as.factor(dt1$LABNO)
if (class(dt1$TYPE)!="factor") dt1$TYPE<- as.factor(dt1$TYPE)
if (class(dt1$INTERVAL)=="factor") dt1$INTERVAL <-as.numeric(levels(dt1$INTERVAL))[as.integer(dt1$INTERVAL) ]               
if (class(dt1$INTERVAL)=="character") dt1$INTERVAL <-as.numeric(dt1$INTERVAL)
if (class(dt1$MEAN_LPS)=="factor") dt1$MEAN_LPS <-as.numeric(levels(dt1$MEAN_LPS))[as.integer(dt1$MEAN_LPS) ]               
if (class(dt1$MEAN_LPS)=="character") dt1$MEAN_LPS <-as.numeric(dt1$MEAN_LPS)
if (class(dt1$Q_AREA_CM)=="factor") dt1$Q_AREA_CM <-as.numeric(levels(dt1$Q_AREA_CM))[as.integer(dt1$Q_AREA_CM) ]               
if (class(dt1$Q_AREA_CM)=="character") dt1$Q_AREA_CM <-as.numeric(dt1$Q_AREA_CM)
if (class(dt1$QCODE)!="factor") dt1$QCODE<- as.factor(dt1$QCODE)
if (class(dt1$PH)=="factor") dt1$PH <-as.numeric(levels(dt1$PH))[as.integer(dt1$PH) ]               
if (class(dt1$PH)=="character") dt1$PH <-as.numeric(dt1$PH)
if (class(dt1$PHCODE)!="factor") dt1$PHCODE<- as.factor(dt1$PHCODE)
if (class(dt1$COND)=="factor") dt1$COND <-as.numeric(levels(dt1$COND))[as.integer(dt1$COND) ]               
if (class(dt1$COND)=="character") dt1$COND <-as.numeric(dt1$COND)
if (class(dt1$CONDCODE)!="factor") dt1$CONDCODE<- as.factor(dt1$CONDCODE)
if (class(dt1$ALK)=="factor") dt1$ALK <-as.numeric(levels(dt1$ALK))[as.integer(dt1$ALK) ]               
if (class(dt1$ALK)=="character") dt1$ALK <-as.numeric(dt1$ALK)
if (class(dt1$ALKCODE)!="factor") dt1$ALKCODE<- as.factor(dt1$ALKCODE)
if (class(dt1$SSED)=="factor") dt1$SSED <-as.numeric(levels(dt1$SSED))[as.integer(dt1$SSED) ]               
if (class(dt1$SSED)=="character") dt1$SSED <-as.numeric(dt1$SSED)
if (class(dt1$SSEDCODE)!="factor") dt1$SSEDCODE<- as.factor(dt1$SSEDCODE)
if (class(dt1$SI)=="factor") dt1$SI <-as.numeric(levels(dt1$SI))[as.integer(dt1$SI) ]               
if (class(dt1$SI)=="character") dt1$SI <-as.numeric(dt1$SI)
if (class(dt1$SICODE)!="factor") dt1$SICODE<- as.factor(dt1$SICODE)
if (class(dt1$UTP)=="factor") dt1$UTP <-as.numeric(levels(dt1$UTP))[as.integer(dt1$UTP) ]               
if (class(dt1$UTP)=="character") dt1$UTP <-as.numeric(dt1$UTP)
if (class(dt1$UTPCODE)!="factor") dt1$UTPCODE<- as.factor(dt1$UTPCODE)
if (class(dt1$TDP)=="factor") dt1$TDP <-as.numeric(levels(dt1$TDP))[as.integer(dt1$TDP) ]               
if (class(dt1$TDP)=="character") dt1$TDP <-as.numeric(dt1$TDP)
if (class(dt1$TDPCODE)!="factor") dt1$TDPCODE<- as.factor(dt1$TDPCODE)
if (class(dt1$PARTP)=="factor") dt1$PARTP <-as.numeric(levels(dt1$PARTP))[as.integer(dt1$PARTP) ]               
if (class(dt1$PARTP)=="character") dt1$PARTP <-as.numeric(dt1$PARTP)
if (class(dt1$PARTPCODE)!="factor") dt1$PARTPCODE<- as.factor(dt1$PARTPCODE)
if (class(dt1$PO4P)=="factor") dt1$PO4P <-as.numeric(levels(dt1$PO4P))[as.integer(dt1$PO4P) ]               
if (class(dt1$PO4P)=="character") dt1$PO4P <-as.numeric(dt1$PO4P)
if (class(dt1$PO4PCODE)!="factor") dt1$PO4PCODE<- as.factor(dt1$PO4PCODE)
if (class(dt1$UTN)=="factor") dt1$UTN <-as.numeric(levels(dt1$UTN))[as.integer(dt1$UTN) ]               
if (class(dt1$UTN)=="character") dt1$UTN <-as.numeric(dt1$UTN)
if (class(dt1$UTNCODE)!="factor") dt1$UTNCODE<- as.factor(dt1$UTNCODE)
if (class(dt1$TDN)=="factor") dt1$TDN <-as.numeric(levels(dt1$TDN))[as.integer(dt1$TDN) ]               
if (class(dt1$TDN)=="character") dt1$TDN <-as.numeric(dt1$TDN)
if (class(dt1$TDNCODE)!="factor") dt1$TDNCODE<- as.factor(dt1$TDNCODE)
if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]               
if (class(dt1$DON)=="character") dt1$DON <-as.numeric(dt1$DON)
if (class(dt1$DONCODE)!="factor") dt1$DONCODE<- as.factor(dt1$DONCODE)
if (class(dt1$PARTN)=="factor") dt1$PARTN <-as.numeric(levels(dt1$PARTN))[as.integer(dt1$PARTN) ]               
if (class(dt1$PARTN)=="character") dt1$PARTN <-as.numeric(dt1$PARTN)
if (class(dt1$PARTNCODE)!="factor") dt1$PARTNCODE<- as.factor(dt1$PARTNCODE)
if (class(dt1$UTKN)=="factor") dt1$UTKN <-as.numeric(levels(dt1$UTKN))[as.integer(dt1$UTKN) ]               
if (class(dt1$UTKN)=="character") dt1$UTKN <-as.numeric(dt1$UTKN)
if (class(dt1$UTKNCODE)!="factor") dt1$UTKNCODE<- as.factor(dt1$UTKNCODE)
if (class(dt1$TKN)=="factor") dt1$TKN <-as.numeric(levels(dt1$TKN))[as.integer(dt1$TKN) ]               
if (class(dt1$TKN)=="character") dt1$TKN <-as.numeric(dt1$TKN)
if (class(dt1$TKNCODE)!="factor") dt1$TKNCODE<- as.factor(dt1$TKNCODE)
if (class(dt1$NH3N)=="factor") dt1$NH3N <-as.numeric(levels(dt1$NH3N))[as.integer(dt1$NH3N) ]               
if (class(dt1$NH3N)=="character") dt1$NH3N <-as.numeric(dt1$NH3N)
if (class(dt1$NH3NCODE)!="factor") dt1$NH3NCODE<- as.factor(dt1$NH3NCODE)
if (class(dt1$NO3N)=="factor") dt1$NO3N <-as.numeric(levels(dt1$NO3N))[as.integer(dt1$NO3N) ]               
if (class(dt1$NO3N)=="character") dt1$NO3N <-as.numeric(dt1$NO3N)
if (class(dt1$NO3NCODE)!="factor") dt1$NO3NCODE<- as.factor(dt1$NO3NCODE)
if (class(dt1$NA)=="factor") dt1$NA <-as.numeric(levels(dt1$NA))[as.integer(dt1$NA) ]               
if (class(dt1$NA)=="character") dt1$NA <-as.numeric(dt1$NA)
if (class(dt1$NACODE)!="factor") dt1$NACODE<- as.factor(dt1$NACODE)
if (class(dt1$K)=="factor") dt1$K <-as.numeric(levels(dt1$K))[as.integer(dt1$K) ]               
if (class(dt1$K)=="character") dt1$K <-as.numeric(dt1$K)
if (class(dt1$KCODE)!="factor") dt1$KCODE<- as.factor(dt1$KCODE)
if (class(dt1$CA)=="factor") dt1$CA <-as.numeric(levels(dt1$CA))[as.integer(dt1$CA) ]               
if (class(dt1$CA)=="character") dt1$CA <-as.numeric(dt1$CA)
if (class(dt1$CACODE)!="factor") dt1$CACODE<- as.factor(dt1$CACODE)
if (class(dt1$MG)=="factor") dt1$MG <-as.numeric(levels(dt1$MG))[as.integer(dt1$MG) ]               
if (class(dt1$MG)=="character") dt1$MG <-as.numeric(dt1$MG)
if (class(dt1$MGCODE)!="factor") dt1$MGCODE<- as.factor(dt1$MGCODE)
if (class(dt1$SO4S)=="factor") dt1$SO4S <-as.numeric(levels(dt1$SO4S))[as.integer(dt1$SO4S) ]               
if (class(dt1$SO4S)=="character") dt1$SO4S <-as.numeric(dt1$SO4S)
if (class(dt1$SO4SCODE)!="factor") dt1$SO4SCODE<- as.factor(dt1$SO4SCODE)
if (class(dt1$CL)=="factor") dt1$CL <-as.numeric(levels(dt1$CL))[as.integer(dt1$CL) ]               
if (class(dt1$CL)=="character") dt1$CL <-as.numeric(dt1$CL)
if (class(dt1$CLCODE)!="factor") dt1$CLCODE<- as.factor(dt1$CLCODE)
if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]               
if (class(dt1$DOC)=="character") dt1$DOC <-as.numeric(dt1$DOC)
if (class(dt1$DOCCODE)!="factor") dt1$DOCCODE<- as.factor(dt1$DOCCODE)
if (class(dt1$PVOL)=="factor") dt1$PVOL <-as.numeric(levels(dt1$PVOL))[as.integer(dt1$PVOL) ]               
if (class(dt1$PVOL)=="character") dt1$PVOL <-as.numeric(dt1$PVOL)
if (class(dt1$PVOLCODE)!="factor") dt1$PVOLCODE<- as.factor(dt1$PVOLCODE)
if (class(dt1$ANCA)=="factor") dt1$ANCA <-as.numeric(levels(dt1$ANCA))[as.integer(dt1$ANCA) ]               
if (class(dt1$ANCA)=="character") dt1$ANCA <-as.numeric(dt1$ANCA)
if (class(dt1$ANCACODE)!="factor") dt1$ANCACODE<- as.factor(dt1$ANCACODE)

# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)   

################################################################################
#Stream water nutrient mean monthly concentrations (Oct 1 1968 - Oct 1 2018)
################################################################################

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4021/23/8aae0f87dd241af5f4fd4dad6af482a0" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "STCODE",     
                 "ENTITY",     
                 "SITECODE",     
                 "WATERYEAR",     
                 "YEAR",     
                 "MONTH",     
                 "TYPE",     
                 "MEAN_LPS",     
                 "Q_AREA_MO",     
                 "QCODE_MO",     
                 "PH_MO",     
                 "PHCODE_MO",     
                 "COND_MO",     
                 "CONDCODE_MO",     
                 "ALK_MO",     
                 "ALKCODE_MO",     
                 "SSED_MO",     
                 "SSEDCODE_MO",     
                 "SI_MO",     
                 "SICODE_MO",     
                 "UTP_MO",     
                 "UTPCODE_MO",     
                 "TDP_MO",     
                 "TDPCODE_MO",     
                 "PO4P_MO",     
                 "PO4PCODE_MO",     
                 "PARTP_MO",     
                 "PARTPCODE_MO",     
                 "UTN_MO",     
                 "UTNCODE_MO",     
                 "TDN_MO",     
                 "TDNCODE_MO",     
                 "DON_MO",     
                 "DONCODE_MO",     
                 "PARTN_MO",     
                 "PARTNCODE_MO",     
                 "UTKN_MO",     
                 "UTKNCODE_MO",     
                 "TKN_MO",     
                 "TKNCODE_MO",     
                 "NH3N_MO",     
                 "NH3NCODE_MO",     
                 "NO3N_MO",     
                 "NO3NCODE_MO",     
                 "NA_MO",     
                 "NACODE_MO",     
                 "K_MO",     
                 "KCODE_MO",     
                 "CA_MO",     
                 "CACODE_MO",     
                 "MG_MO",     
                 "MGCODE_MO",     
                 "SO4S_MO",     
                 "SO4SCODE_MO",     
                 "CL_MO",     
                 "CLCODE_MO",     
                 "DOC_MO",     
                 "DOCCODE_MO"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$STCODE)!="factor") dt2$STCODE<- as.factor(dt2$STCODE)
if (class(dt2$ENTITY)=="factor") dt2$ENTITY <-as.numeric(levels(dt2$ENTITY))[as.integer(dt2$ENTITY) ]               
if (class(dt2$ENTITY)=="character") dt2$ENTITY <-as.numeric(dt2$ENTITY)
if (class(dt2$SITECODE)!="factor") dt2$SITECODE<- as.factor(dt2$SITECODE)
if (class(dt2$WATERYEAR)=="factor") dt2$WATERYEAR <-as.numeric(levels(dt2$WATERYEAR))[as.integer(dt2$WATERYEAR) ]               
if (class(dt2$WATERYEAR)=="character") dt2$WATERYEAR <-as.numeric(dt2$WATERYEAR)
if (class(dt2$YEAR)=="factor") dt2$YEAR <-as.numeric(levels(dt2$YEAR))[as.integer(dt2$YEAR) ]               
if (class(dt2$YEAR)=="character") dt2$YEAR <-as.numeric(dt2$YEAR)
if (class(dt2$MONTH)=="factor") dt2$MONTH <-as.numeric(levels(dt2$MONTH))[as.integer(dt2$MONTH) ]               
if (class(dt2$MONTH)=="character") dt2$MONTH <-as.numeric(dt2$MONTH)
if (class(dt2$TYPE)!="factor") dt2$TYPE<- as.factor(dt2$TYPE)
if (class(dt2$MEAN_LPS)=="factor") dt2$MEAN_LPS <-as.numeric(levels(dt2$MEAN_LPS))[as.integer(dt2$MEAN_LPS) ]               
if (class(dt2$MEAN_LPS)=="character") dt2$MEAN_LPS <-as.numeric(dt2$MEAN_LPS)
if (class(dt2$Q_AREA_MO)=="factor") dt2$Q_AREA_MO <-as.numeric(levels(dt2$Q_AREA_MO))[as.integer(dt2$Q_AREA_MO) ]               
if (class(dt2$Q_AREA_MO)=="character") dt2$Q_AREA_MO <-as.numeric(dt2$Q_AREA_MO)
if (class(dt2$QCODE_MO)!="factor") dt2$QCODE_MO<- as.factor(dt2$QCODE_MO)
if (class(dt2$PH_MO)=="factor") dt2$PH_MO <-as.numeric(levels(dt2$PH_MO))[as.integer(dt2$PH_MO) ]               
if (class(dt2$PH_MO)=="character") dt2$PH_MO <-as.numeric(dt2$PH_MO)
if (class(dt2$PHCODE_MO)!="factor") dt2$PHCODE_MO<- as.factor(dt2$PHCODE_MO)
if (class(dt2$COND_MO)=="factor") dt2$COND_MO <-as.numeric(levels(dt2$COND_MO))[as.integer(dt2$COND_MO) ]               
if (class(dt2$COND_MO)=="character") dt2$COND_MO <-as.numeric(dt2$COND_MO)
if (class(dt2$CONDCODE_MO)!="factor") dt2$CONDCODE_MO<- as.factor(dt2$CONDCODE_MO)
if (class(dt2$ALK_MO)=="factor") dt2$ALK_MO <-as.numeric(levels(dt2$ALK_MO))[as.integer(dt2$ALK_MO) ]               
if (class(dt2$ALK_MO)=="character") dt2$ALK_MO <-as.numeric(dt2$ALK_MO)
if (class(dt2$ALKCODE_MO)!="factor") dt2$ALKCODE_MO<- as.factor(dt2$ALKCODE_MO)
if (class(dt2$SSED_MO)=="factor") dt2$SSED_MO <-as.numeric(levels(dt2$SSED_MO))[as.integer(dt2$SSED_MO) ]               
if (class(dt2$SSED_MO)=="character") dt2$SSED_MO <-as.numeric(dt2$SSED_MO)
if (class(dt2$SSEDCODE_MO)!="factor") dt2$SSEDCODE_MO<- as.factor(dt2$SSEDCODE_MO)
if (class(dt2$SI_MO)=="factor") dt2$SI_MO <-as.numeric(levels(dt2$SI_MO))[as.integer(dt2$SI_MO) ]               
if (class(dt2$SI_MO)=="character") dt2$SI_MO <-as.numeric(dt2$SI_MO)
if (class(dt2$SICODE_MO)!="factor") dt2$SICODE_MO<- as.factor(dt2$SICODE_MO)
if (class(dt2$UTP_MO)=="factor") dt2$UTP_MO <-as.numeric(levels(dt2$UTP_MO))[as.integer(dt2$UTP_MO) ]               
if (class(dt2$UTP_MO)=="character") dt2$UTP_MO <-as.numeric(dt2$UTP_MO)
if (class(dt2$UTPCODE_MO)!="factor") dt2$UTPCODE_MO<- as.factor(dt2$UTPCODE_MO)
if (class(dt2$TDP_MO)=="factor") dt2$TDP_MO <-as.numeric(levels(dt2$TDP_MO))[as.integer(dt2$TDP_MO) ]               
if (class(dt2$TDP_MO)=="character") dt2$TDP_MO <-as.numeric(dt2$TDP_MO)
if (class(dt2$TDPCODE_MO)!="factor") dt2$TDPCODE_MO<- as.factor(dt2$TDPCODE_MO)
if (class(dt2$PO4P_MO)=="factor") dt2$PO4P_MO <-as.numeric(levels(dt2$PO4P_MO))[as.integer(dt2$PO4P_MO) ]               
if (class(dt2$PO4P_MO)=="character") dt2$PO4P_MO <-as.numeric(dt2$PO4P_MO)
if (class(dt2$PO4PCODE_MO)!="factor") dt2$PO4PCODE_MO<- as.factor(dt2$PO4PCODE_MO)
if (class(dt2$PARTP_MO)=="factor") dt2$PARTP_MO <-as.numeric(levels(dt2$PARTP_MO))[as.integer(dt2$PARTP_MO) ]               
if (class(dt2$PARTP_MO)=="character") dt2$PARTP_MO <-as.numeric(dt2$PARTP_MO)
if (class(dt2$PARTPCODE_MO)!="factor") dt2$PARTPCODE_MO<- as.factor(dt2$PARTPCODE_MO)
if (class(dt2$UTN_MO)=="factor") dt2$UTN_MO <-as.numeric(levels(dt2$UTN_MO))[as.integer(dt2$UTN_MO) ]               
if (class(dt2$UTN_MO)=="character") dt2$UTN_MO <-as.numeric(dt2$UTN_MO)
if (class(dt2$UTNCODE_MO)!="factor") dt2$UTNCODE_MO<- as.factor(dt2$UTNCODE_MO)
if (class(dt2$TDN_MO)=="factor") dt2$TDN_MO <-as.numeric(levels(dt2$TDN_MO))[as.integer(dt2$TDN_MO) ]               
if (class(dt2$TDN_MO)=="character") dt2$TDN_MO <-as.numeric(dt2$TDN_MO)
if (class(dt2$TDNCODE_MO)!="factor") dt2$TDNCODE_MO<- as.factor(dt2$TDNCODE_MO)
if (class(dt2$DON_MO)=="factor") dt2$DON_MO <-as.numeric(levels(dt2$DON_MO))[as.integer(dt2$DON_MO) ]               
if (class(dt2$DON_MO)=="character") dt2$DON_MO <-as.numeric(dt2$DON_MO)
if (class(dt2$DONCODE_MO)!="factor") dt2$DONCODE_MO<- as.factor(dt2$DONCODE_MO)
if (class(dt2$PARTN_MO)=="factor") dt2$PARTN_MO <-as.numeric(levels(dt2$PARTN_MO))[as.integer(dt2$PARTN_MO) ]               
if (class(dt2$PARTN_MO)=="character") dt2$PARTN_MO <-as.numeric(dt2$PARTN_MO)
if (class(dt2$PARTNCODE_MO)!="factor") dt2$PARTNCODE_MO<- as.factor(dt2$PARTNCODE_MO)
if (class(dt2$UTKN_MO)=="factor") dt2$UTKN_MO <-as.numeric(levels(dt2$UTKN_MO))[as.integer(dt2$UTKN_MO) ]               
if (class(dt2$UTKN_MO)=="character") dt2$UTKN_MO <-as.numeric(dt2$UTKN_MO)
if (class(dt2$UTKNCODE_MO)!="factor") dt2$UTKNCODE_MO<- as.factor(dt2$UTKNCODE_MO)
if (class(dt2$TKN_MO)=="factor") dt2$TKN_MO <-as.numeric(levels(dt2$TKN_MO))[as.integer(dt2$TKN_MO) ]               
if (class(dt2$TKN_MO)=="character") dt2$TKN_MO <-as.numeric(dt2$TKN_MO)
if (class(dt2$TKNCODE_MO)!="factor") dt2$TKNCODE_MO<- as.factor(dt2$TKNCODE_MO)
if (class(dt2$NH3N_MO)=="factor") dt2$NH3N_MO <-as.numeric(levels(dt2$NH3N_MO))[as.integer(dt2$NH3N_MO) ]               
if (class(dt2$NH3N_MO)=="character") dt2$NH3N_MO <-as.numeric(dt2$NH3N_MO)
if (class(dt2$NH3NCODE_MO)!="factor") dt2$NH3NCODE_MO<- as.factor(dt2$NH3NCODE_MO)
if (class(dt2$NO3N_MO)=="factor") dt2$NO3N_MO <-as.numeric(levels(dt2$NO3N_MO))[as.integer(dt2$NO3N_MO) ]               
if (class(dt2$NO3N_MO)=="character") dt2$NO3N_MO <-as.numeric(dt2$NO3N_MO)
if (class(dt2$NO3NCODE_MO)!="factor") dt2$NO3NCODE_MO<- as.factor(dt2$NO3NCODE_MO)
if (class(dt2$NA_MO)=="factor") dt2$NA_MO <-as.numeric(levels(dt2$NA_MO))[as.integer(dt2$NA_MO) ]               
if (class(dt2$NA_MO)=="character") dt2$NA_MO <-as.numeric(dt2$NA_MO)
if (class(dt2$NACODE_MO)!="factor") dt2$NACODE_MO<- as.factor(dt2$NACODE_MO)
if (class(dt2$K_MO)=="factor") dt2$K_MO <-as.numeric(levels(dt2$K_MO))[as.integer(dt2$K_MO) ]               
if (class(dt2$K_MO)=="character") dt2$K_MO <-as.numeric(dt2$K_MO)
if (class(dt2$KCODE_MO)!="factor") dt2$KCODE_MO<- as.factor(dt2$KCODE_MO)
if (class(dt2$CA_MO)=="factor") dt2$CA_MO <-as.numeric(levels(dt2$CA_MO))[as.integer(dt2$CA_MO) ]               
if (class(dt2$CA_MO)=="character") dt2$CA_MO <-as.numeric(dt2$CA_MO)
if (class(dt2$CACODE_MO)!="factor") dt2$CACODE_MO<- as.factor(dt2$CACODE_MO)
if (class(dt2$MG_MO)=="factor") dt2$MG_MO <-as.numeric(levels(dt2$MG_MO))[as.integer(dt2$MG_MO) ]               
if (class(dt2$MG_MO)=="character") dt2$MG_MO <-as.numeric(dt2$MG_MO)
if (class(dt2$MGCODE_MO)!="factor") dt2$MGCODE_MO<- as.factor(dt2$MGCODE_MO)
if (class(dt2$SO4S_MO)=="factor") dt2$SO4S_MO <-as.numeric(levels(dt2$SO4S_MO))[as.integer(dt2$SO4S_MO) ]               
if (class(dt2$SO4S_MO)=="character") dt2$SO4S_MO <-as.numeric(dt2$SO4S_MO)
if (class(dt2$SO4SCODE_MO)!="factor") dt2$SO4SCODE_MO<- as.factor(dt2$SO4SCODE_MO)
if (class(dt2$CL_MO)=="factor") dt2$CL_MO <-as.numeric(levels(dt2$CL_MO))[as.integer(dt2$CL_MO) ]               
if (class(dt2$CL_MO)=="character") dt2$CL_MO <-as.numeric(dt2$CL_MO)
if (class(dt2$CLCODE_MO)!="factor") dt2$CLCODE_MO<- as.factor(dt2$CLCODE_MO)
if (class(dt2$DOC_MO)=="factor") dt2$DOC_MO <-as.numeric(levels(dt2$DOC_MO))[as.integer(dt2$DOC_MO) ]               
if (class(dt2$DOC_MO)=="character") dt2$DOC_MO <-as.numeric(dt2$DOC_MO)
if (class(dt2$DOCCODE_MO)!="factor") dt2$DOCCODE_MO<- as.factor(dt2$DOCCODE_MO)


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            

################################################################################
#Stream water nutrient mean annual concentrations (Oct 1 1968 - Oct 1 2018)
################################################################################

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4021/23/7c5da4e38e68c2f50c22a5a72a935ccb" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "STCODE",     
                 "ENTITY",     
                 "SITECODE",     
                 "WATERYEAR",     
                 "Q_AREA_YR",     
                 "QCODE_YR",     
                 "PH_YR",     
                 "PHCODE_YR",     
                 "COND_YR",     
                 "CONDCODE_YR",     
                 "ALK_YR",     
                 "ALKCODE_YR",     
                 "SSED_YR",     
                 "SSEDCODE_YR",     
                 "SI_YR",     
                 "SICODE_YR",     
                 "UTP_YR",     
                 "UTPCODE_YR",     
                 "TDP_YR",     
                 "TDPCODE_YR",     
                 "PARTP_YR",     
                 "PARTPCODE_YR",     
                 "PO4P_YR",     
                 "PO4PCODE_YR",     
                 "UTN_YR",     
                 "UTNCODE_YR",     
                 "TDN_YR",     
                 "TDNCODE_YR",     
                 "DON_YR",     
                 "DONCODE_YR",     
                 "PARTN_YR",     
                 "PARTNCODE_YR",     
                 "UTKN_YR",     
                 "UTKNCODE_YR",     
                 "TKN_YR",     
                 "TKNCODE_YR",     
                 "NH3N_YR",     
                 "NH3NCODE_YR",     
                 "NO3N_YR",     
                 "NO3NCODE_YR",     
                 "NA_YR",     
                 "NACODE_YR",     
                 "K_YR",     
                 "KCODE_YR",     
                 "CA_YR",     
                 "CACODE_YR",     
                 "MG_YR",     
                 "MGCODE_YR",     
                 "SO4S_YR",     
                 "SO4SCODE_YR",     
                 "CL_YR",     
                 "CLCODE_YR",     
                 "DOC_YR",     
                 "DOCCODE_YR"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$STCODE)!="factor") dt3$STCODE<- as.factor(dt3$STCODE)
if (class(dt3$ENTITY)=="factor") dt3$ENTITY <-as.numeric(levels(dt3$ENTITY))[as.integer(dt3$ENTITY) ]               
if (class(dt3$ENTITY)=="character") dt3$ENTITY <-as.numeric(dt3$ENTITY)
if (class(dt3$SITECODE)!="factor") dt3$SITECODE<- as.factor(dt3$SITECODE)
if (class(dt3$WATERYEAR)=="factor") dt3$WATERYEAR <-as.numeric(levels(dt3$WATERYEAR))[as.integer(dt3$WATERYEAR) ]               
if (class(dt3$WATERYEAR)=="character") dt3$WATERYEAR <-as.numeric(dt3$WATERYEAR)
if (class(dt3$Q_AREA_YR)=="factor") dt3$Q_AREA_YR <-as.numeric(levels(dt3$Q_AREA_YR))[as.integer(dt3$Q_AREA_YR) ]               
if (class(dt3$Q_AREA_YR)=="character") dt3$Q_AREA_YR <-as.numeric(dt3$Q_AREA_YR)
if (class(dt3$QCODE_YR)!="factor") dt3$QCODE_YR<- as.factor(dt3$QCODE_YR)
if (class(dt3$PH_YR)=="factor") dt3$PH_YR <-as.numeric(levels(dt3$PH_YR))[as.integer(dt3$PH_YR) ]               
if (class(dt3$PH_YR)=="character") dt3$PH_YR <-as.numeric(dt3$PH_YR)
if (class(dt3$PHCODE_YR)!="factor") dt3$PHCODE_YR<- as.factor(dt3$PHCODE_YR)
if (class(dt3$COND_YR)=="factor") dt3$COND_YR <-as.numeric(levels(dt3$COND_YR))[as.integer(dt3$COND_YR) ]               
if (class(dt3$COND_YR)=="character") dt3$COND_YR <-as.numeric(dt3$COND_YR)
if (class(dt3$CONDCODE_YR)!="factor") dt3$CONDCODE_YR<- as.factor(dt3$CONDCODE_YR)
if (class(dt3$ALK_YR)=="factor") dt3$ALK_YR <-as.numeric(levels(dt3$ALK_YR))[as.integer(dt3$ALK_YR) ]               
if (class(dt3$ALK_YR)=="character") dt3$ALK_YR <-as.numeric(dt3$ALK_YR)
if (class(dt3$ALKCODE_YR)!="factor") dt3$ALKCODE_YR<- as.factor(dt3$ALKCODE_YR)
if (class(dt3$SSED_YR)=="factor") dt3$SSED_YR <-as.numeric(levels(dt3$SSED_YR))[as.integer(dt3$SSED_YR) ]               
if (class(dt3$SSED_YR)=="character") dt3$SSED_YR <-as.numeric(dt3$SSED_YR)
if (class(dt3$SSEDCODE_YR)!="factor") dt3$SSEDCODE_YR<- as.factor(dt3$SSEDCODE_YR)
if (class(dt3$SI_YR)=="factor") dt3$SI_YR <-as.numeric(levels(dt3$SI_YR))[as.integer(dt3$SI_YR) ]               
if (class(dt3$SI_YR)=="character") dt3$SI_YR <-as.numeric(dt3$SI_YR)
if (class(dt3$SICODE_YR)!="factor") dt3$SICODE_YR<- as.factor(dt3$SICODE_YR)
if (class(dt3$UTP_YR)=="factor") dt3$UTP_YR <-as.numeric(levels(dt3$UTP_YR))[as.integer(dt3$UTP_YR) ]               
if (class(dt3$UTP_YR)=="character") dt3$UTP_YR <-as.numeric(dt3$UTP_YR)
if (class(dt3$UTPCODE_YR)!="factor") dt3$UTPCODE_YR<- as.factor(dt3$UTPCODE_YR)
if (class(dt3$TDP_YR)=="factor") dt3$TDP_YR <-as.numeric(levels(dt3$TDP_YR))[as.integer(dt3$TDP_YR) ]               
if (class(dt3$TDP_YR)=="character") dt3$TDP_YR <-as.numeric(dt3$TDP_YR)
if (class(dt3$TDPCODE_YR)!="factor") dt3$TDPCODE_YR<- as.factor(dt3$TDPCODE_YR)
if (class(dt3$PARTP_YR)=="factor") dt3$PARTP_YR <-as.numeric(levels(dt3$PARTP_YR))[as.integer(dt3$PARTP_YR) ]               
if (class(dt3$PARTP_YR)=="character") dt3$PARTP_YR <-as.numeric(dt3$PARTP_YR)
if (class(dt3$PARTPCODE_YR)!="factor") dt3$PARTPCODE_YR<- as.factor(dt3$PARTPCODE_YR)
if (class(dt3$PO4P_YR)=="factor") dt3$PO4P_YR <-as.numeric(levels(dt3$PO4P_YR))[as.integer(dt3$PO4P_YR) ]               
if (class(dt3$PO4P_YR)=="character") dt3$PO4P_YR <-as.numeric(dt3$PO4P_YR)
if (class(dt3$PO4PCODE_YR)!="factor") dt3$PO4PCODE_YR<- as.factor(dt3$PO4PCODE_YR)
if (class(dt3$UTN_YR)=="factor") dt3$UTN_YR <-as.numeric(levels(dt3$UTN_YR))[as.integer(dt3$UTN_YR) ]               
if (class(dt3$UTN_YR)=="character") dt3$UTN_YR <-as.numeric(dt3$UTN_YR)
if (class(dt3$UTNCODE_YR)!="factor") dt3$UTNCODE_YR<- as.factor(dt3$UTNCODE_YR)
if (class(dt3$TDN_YR)=="factor") dt3$TDN_YR <-as.numeric(levels(dt3$TDN_YR))[as.integer(dt3$TDN_YR) ]               
if (class(dt3$TDN_YR)=="character") dt3$TDN_YR <-as.numeric(dt3$TDN_YR)
if (class(dt3$TDNCODE_YR)!="factor") dt3$TDNCODE_YR<- as.factor(dt3$TDNCODE_YR)
if (class(dt3$DON_YR)=="factor") dt3$DON_YR <-as.numeric(levels(dt3$DON_YR))[as.integer(dt3$DON_YR) ]               
if (class(dt3$DON_YR)=="character") dt3$DON_YR <-as.numeric(dt3$DON_YR)
if (class(dt3$DONCODE_YR)!="factor") dt3$DONCODE_YR<- as.factor(dt3$DONCODE_YR)
if (class(dt3$PARTN_YR)=="factor") dt3$PARTN_YR <-as.numeric(levels(dt3$PARTN_YR))[as.integer(dt3$PARTN_YR) ]               
if (class(dt3$PARTN_YR)=="character") dt3$PARTN_YR <-as.numeric(dt3$PARTN_YR)
if (class(dt3$PARTNCODE_YR)!="factor") dt3$PARTNCODE_YR<- as.factor(dt3$PARTNCODE_YR)
if (class(dt3$UTKN_YR)=="factor") dt3$UTKN_YR <-as.numeric(levels(dt3$UTKN_YR))[as.integer(dt3$UTKN_YR) ]               
if (class(dt3$UTKN_YR)=="character") dt3$UTKN_YR <-as.numeric(dt3$UTKN_YR)
if (class(dt3$UTKNCODE_YR)!="factor") dt3$UTKNCODE_YR<- as.factor(dt3$UTKNCODE_YR)
if (class(dt3$TKN_YR)=="factor") dt3$TKN_YR <-as.numeric(levels(dt3$TKN_YR))[as.integer(dt3$TKN_YR) ]               
if (class(dt3$TKN_YR)=="character") dt3$TKN_YR <-as.numeric(dt3$TKN_YR)
if (class(dt3$TKNCODE_YR)!="factor") dt3$TKNCODE_YR<- as.factor(dt3$TKNCODE_YR)
if (class(dt3$NH3N_YR)=="factor") dt3$NH3N_YR <-as.numeric(levels(dt3$NH3N_YR))[as.integer(dt3$NH3N_YR) ]               
if (class(dt3$NH3N_YR)=="character") dt3$NH3N_YR <-as.numeric(dt3$NH3N_YR)
if (class(dt3$NH3NCODE_YR)!="factor") dt3$NH3NCODE_YR<- as.factor(dt3$NH3NCODE_YR)
if (class(dt3$NO3N_YR)=="factor") dt3$NO3N_YR <-as.numeric(levels(dt3$NO3N_YR))[as.integer(dt3$NO3N_YR) ]               
if (class(dt3$NO3N_YR)=="character") dt3$NO3N_YR <-as.numeric(dt3$NO3N_YR)
if (class(dt3$NO3NCODE_YR)!="factor") dt3$NO3NCODE_YR<- as.factor(dt3$NO3NCODE_YR)
if (class(dt3$NA_YR)=="factor") dt3$NA_YR <-as.numeric(levels(dt3$NA_YR))[as.integer(dt3$NA_YR) ]               
if (class(dt3$NA_YR)=="character") dt3$NA_YR <-as.numeric(dt3$NA_YR)
if (class(dt3$NACODE_YR)!="factor") dt3$NACODE_YR<- as.factor(dt3$NACODE_YR)
if (class(dt3$K_YR)=="factor") dt3$K_YR <-as.numeric(levels(dt3$K_YR))[as.integer(dt3$K_YR) ]               
if (class(dt3$K_YR)=="character") dt3$K_YR <-as.numeric(dt3$K_YR)
if (class(dt3$KCODE_YR)!="factor") dt3$KCODE_YR<- as.factor(dt3$KCODE_YR)
if (class(dt3$CA_YR)=="factor") dt3$CA_YR <-as.numeric(levels(dt3$CA_YR))[as.integer(dt3$CA_YR) ]               
if (class(dt3$CA_YR)=="character") dt3$CA_YR <-as.numeric(dt3$CA_YR)
if (class(dt3$CACODE_YR)!="factor") dt3$CACODE_YR<- as.factor(dt3$CACODE_YR)
if (class(dt3$MG_YR)=="factor") dt3$MG_YR <-as.numeric(levels(dt3$MG_YR))[as.integer(dt3$MG_YR) ]               
if (class(dt3$MG_YR)=="character") dt3$MG_YR <-as.numeric(dt3$MG_YR)
if (class(dt3$MGCODE_YR)!="factor") dt3$MGCODE_YR<- as.factor(dt3$MGCODE_YR)
if (class(dt3$SO4S_YR)=="factor") dt3$SO4S_YR <-as.numeric(levels(dt3$SO4S_YR))[as.integer(dt3$SO4S_YR) ]               
if (class(dt3$SO4S_YR)=="character") dt3$SO4S_YR <-as.numeric(dt3$SO4S_YR)
if (class(dt3$SO4SCODE_YR)!="factor") dt3$SO4SCODE_YR<- as.factor(dt3$SO4SCODE_YR)
if (class(dt3$CL_YR)=="factor") dt3$CL_YR <-as.numeric(levels(dt3$CL_YR))[as.integer(dt3$CL_YR) ]               
if (class(dt3$CL_YR)=="character") dt3$CL_YR <-as.numeric(dt3$CL_YR)
if (class(dt3$CLCODE_YR)!="factor") dt3$CLCODE_YR<- as.factor(dt3$CLCODE_YR)
if (class(dt3$DOC_YR)=="factor") dt3$DOC_YR <-as.numeric(levels(dt3$DOC_YR))[as.integer(dt3$DOC_YR) ]               
if (class(dt3$DOC_YR)=="character") dt3$DOC_YR <-as.numeric(dt3$DOC_YR)
if (class(dt3$DOCCODE_YR)!="factor") dt3$DOCCODE_YR<- as.factor(dt3$DOCCODE_YR)

# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
               
################################################################################
#Stream water nutrient monthly outflow (kg/ha) (Oct 1 1968 - Oct 1 2018)
################################################################################

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4021/23/281706bdca181568e1d1d0a88b6fb1cf" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "STCODE",     
                 "ENTITY",       
                 "SITECODE",     
                 "WATERYEAR",     
                 "YEAR",     
                 "MONTH",     
                 "TYPE",     
                 "Q_AREA_MO",     
                 "QCODE_MO",     
                 "ALK_OUT_MO",     
                 "ALKCODE_MO",     
                 "SSED_OUT_MO",     
                 "SSEDCODE_MO",     
                 "SI_OUT_MO",     
                 "SICODE_MO",     
                 "UTP_OUT_MO",     
                 "UTPCODE_MO",     
                 "TDP_OUT_MO",     
                 "TDPCODE_MO",     
                 "PO4P_OUT_MO",     
                 "PO4PCODE_MO",     
                 "PARTP_OUT_MO",     
                 "PARTPCODE_MO",     
                 "UTN_OUT_MO",     
                 "UTNCODE_MO",     
                 "TDN_OUT_MO",     
                 "TDNCODE_MO",     
                 "DON_OUT_MO",     
                 "DONCODE_MO",     
                 "PARTN_OUT_MO",     
                 "PARTNCODE_MO",     
                 "UTKN_OUT_MO",     
                 "UTKNCODE_MO",     
                 "TKN_OUT_MO",     
                 "TKNCODE_MO",     
                 "NH3N_OUT_MO",     
                 "NH3NCODE_MO",     
                 "NO3N_OUT_MO",     
                 "NO3NCODE_MO",     
                 "NA_OUT_MO",     
                 "NACODE_MO",     
                 "K_OUT_MO",     
                 "KCODE_MO",     
                 "CA_OUT_MO",     
                 "CACODE_MO",     
                 "MG_OUT_MO",     
                 "MGCODE_MO",     
                 "SO4S_OUT_MO",     
                 "SO4SCODE_MO",     
                 "CL_OUT_MO",     
                 "CLCODE_MO",     
                 "DOC_OUT_MO",     
                 "DOCCODE_MO"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$STCODE)!="factor") dt4$STCODE<- as.factor(dt4$STCODE)
if (class(dt4$ENTITY)=="factor") dt4$ENTITY <-as.numeric(levels(dt4$ENTITY))[as.integer(dt4$ENTITY) ]               
if (class(dt4$ENTITY)=="character") dt4$ENTITY <-as.numeric(dt4$ENTITY)
if (class(dt4$SITECODE)!="factor") dt4$SITECODE<- as.factor(dt4$SITECODE)
if (class(dt4$WATERYEAR)=="factor") dt4$WATERYEAR <-as.numeric(levels(dt4$WATERYEAR))[as.integer(dt4$WATERYEAR) ]               
if (class(dt4$WATERYEAR)=="character") dt4$WATERYEAR <-as.numeric(dt4$WATERYEAR)
if (class(dt4$YEAR)=="factor") dt4$YEAR <-as.numeric(levels(dt4$YEAR))[as.integer(dt4$YEAR) ]               
if (class(dt4$YEAR)=="character") dt4$YEAR <-as.numeric(dt4$YEAR)
if (class(dt4$MONTH)=="factor") dt4$MONTH <-as.numeric(levels(dt4$MONTH))[as.integer(dt4$MONTH) ]               
if (class(dt4$MONTH)=="character") dt4$MONTH <-as.numeric(dt4$MONTH)
if (class(dt4$TYPE)!="factor") dt4$TYPE<- as.factor(dt4$TYPE)
if (class(dt4$Q_AREA_MO)=="factor") dt4$Q_AREA_MO <-as.numeric(levels(dt4$Q_AREA_MO))[as.integer(dt4$Q_AREA_MO) ]               
if (class(dt4$Q_AREA_MO)=="character") dt4$Q_AREA_MO <-as.numeric(dt4$Q_AREA_MO)
if (class(dt4$QCODE_MO)!="factor") dt4$QCODE_MO<- as.factor(dt4$QCODE_MO)
if (class(dt4$ALK_OUT_MO)=="factor") dt4$ALK_OUT_MO <-as.numeric(levels(dt4$ALK_OUT_MO))[as.integer(dt4$ALK_OUT_MO) ]               
if (class(dt4$ALK_OUT_MO)=="character") dt4$ALK_OUT_MO <-as.numeric(dt4$ALK_OUT_MO)
if (class(dt4$ALKCODE_MO)!="factor") dt4$ALKCODE_MO<- as.factor(dt4$ALKCODE_MO)
if (class(dt4$SSED_OUT_MO)=="factor") dt4$SSED_OUT_MO <-as.numeric(levels(dt4$SSED_OUT_MO))[as.integer(dt4$SSED_OUT_MO) ]               
if (class(dt4$SSED_OUT_MO)=="character") dt4$SSED_OUT_MO <-as.numeric(dt4$SSED_OUT_MO)
if (class(dt4$SSEDCODE_MO)!="factor") dt4$SSEDCODE_MO<- as.factor(dt4$SSEDCODE_MO)
if (class(dt4$SI_OUT_MO)=="factor") dt4$SI_OUT_MO <-as.numeric(levels(dt4$SI_OUT_MO))[as.integer(dt4$SI_OUT_MO) ]               
if (class(dt4$SI_OUT_MO)=="character") dt4$SI_OUT_MO <-as.numeric(dt4$SI_OUT_MO)
if (class(dt4$SICODE_MO)!="factor") dt4$SICODE_MO<- as.factor(dt4$SICODE_MO)
if (class(dt4$UTP_OUT_MO)=="factor") dt4$UTP_OUT_MO <-as.numeric(levels(dt4$UTP_OUT_MO))[as.integer(dt4$UTP_OUT_MO) ]               
if (class(dt4$UTP_OUT_MO)=="character") dt4$UTP_OUT_MO <-as.numeric(dt4$UTP_OUT_MO)
if (class(dt4$UTPCODE_MO)!="factor") dt4$UTPCODE_MO<- as.factor(dt4$UTPCODE_MO)
if (class(dt4$TDP_OUT_MO)=="factor") dt4$TDP_OUT_MO <-as.numeric(levels(dt4$TDP_OUT_MO))[as.integer(dt4$TDP_OUT_MO) ]               
if (class(dt4$TDP_OUT_MO)=="character") dt4$TDP_OUT_MO <-as.numeric(dt4$TDP_OUT_MO)
if (class(dt4$TDPCODE_MO)!="factor") dt4$TDPCODE_MO<- as.factor(dt4$TDPCODE_MO)
if (class(dt4$PO4P_OUT_MO)=="factor") dt4$PO4P_OUT_MO <-as.numeric(levels(dt4$PO4P_OUT_MO))[as.integer(dt4$PO4P_OUT_MO) ]               
if (class(dt4$PO4P_OUT_MO)=="character") dt4$PO4P_OUT_MO <-as.numeric(dt4$PO4P_OUT_MO)
if (class(dt4$PO4PCODE_MO)!="factor") dt4$PO4PCODE_MO<- as.factor(dt4$PO4PCODE_MO)
if (class(dt4$PARTP_OUT_MO)=="factor") dt4$PARTP_OUT_MO <-as.numeric(levels(dt4$PARTP_OUT_MO))[as.integer(dt4$PARTP_OUT_MO) ]               
if (class(dt4$PARTP_OUT_MO)=="character") dt4$PARTP_OUT_MO <-as.numeric(dt4$PARTP_OUT_MO)
if (class(dt4$PARTPCODE_MO)!="factor") dt4$PARTPCODE_MO<- as.factor(dt4$PARTPCODE_MO)
if (class(dt4$UTN_OUT_MO)=="factor") dt4$UTN_OUT_MO <-as.numeric(levels(dt4$UTN_OUT_MO))[as.integer(dt4$UTN_OUT_MO) ]               
if (class(dt4$UTN_OUT_MO)=="character") dt4$UTN_OUT_MO <-as.numeric(dt4$UTN_OUT_MO)
if (class(dt4$UTNCODE_MO)!="factor") dt4$UTNCODE_MO<- as.factor(dt4$UTNCODE_MO)
if (class(dt4$TDN_OUT_MO)=="factor") dt4$TDN_OUT_MO <-as.numeric(levels(dt4$TDN_OUT_MO))[as.integer(dt4$TDN_OUT_MO) ]               
if (class(dt4$TDN_OUT_MO)=="character") dt4$TDN_OUT_MO <-as.numeric(dt4$TDN_OUT_MO)
if (class(dt4$TDNCODE_MO)!="factor") dt4$TDNCODE_MO<- as.factor(dt4$TDNCODE_MO)
if (class(dt4$DON_OUT_MO)=="factor") dt4$DON_OUT_MO <-as.numeric(levels(dt4$DON_OUT_MO))[as.integer(dt4$DON_OUT_MO) ]               
if (class(dt4$DON_OUT_MO)=="character") dt4$DON_OUT_MO <-as.numeric(dt4$DON_OUT_MO)
if (class(dt4$DONCODE_MO)!="factor") dt4$DONCODE_MO<- as.factor(dt4$DONCODE_MO)
if (class(dt4$PARTN_OUT_MO)=="factor") dt4$PARTN_OUT_MO <-as.numeric(levels(dt4$PARTN_OUT_MO))[as.integer(dt4$PARTN_OUT_MO) ]               
if (class(dt4$PARTN_OUT_MO)=="character") dt4$PARTN_OUT_MO <-as.numeric(dt4$PARTN_OUT_MO)
if (class(dt4$PARTNCODE_MO)!="factor") dt4$PARTNCODE_MO<- as.factor(dt4$PARTNCODE_MO)
if (class(dt4$UTKN_OUT_MO)=="factor") dt4$UTKN_OUT_MO <-as.numeric(levels(dt4$UTKN_OUT_MO))[as.integer(dt4$UTKN_OUT_MO) ]               
if (class(dt4$UTKN_OUT_MO)=="character") dt4$UTKN_OUT_MO <-as.numeric(dt4$UTKN_OUT_MO)
if (class(dt4$UTKNCODE_MO)!="factor") dt4$UTKNCODE_MO<- as.factor(dt4$UTKNCODE_MO)
if (class(dt4$TKN_OUT_MO)=="factor") dt4$TKN_OUT_MO <-as.numeric(levels(dt4$TKN_OUT_MO))[as.integer(dt4$TKN_OUT_MO) ]               
if (class(dt4$TKN_OUT_MO)=="character") dt4$TKN_OUT_MO <-as.numeric(dt4$TKN_OUT_MO)
if (class(dt4$TKNCODE_MO)!="factor") dt4$TKNCODE_MO<- as.factor(dt4$TKNCODE_MO)
if (class(dt4$NH3N_OUT_MO)=="factor") dt4$NH3N_OUT_MO <-as.numeric(levels(dt4$NH3N_OUT_MO))[as.integer(dt4$NH3N_OUT_MO) ]               
if (class(dt4$NH3N_OUT_MO)=="character") dt4$NH3N_OUT_MO <-as.numeric(dt4$NH3N_OUT_MO)
if (class(dt4$NH3NCODE_MO)!="factor") dt4$NH3NCODE_MO<- as.factor(dt4$NH3NCODE_MO)
if (class(dt4$NO3N_OUT_MO)=="factor") dt4$NO3N_OUT_MO <-as.numeric(levels(dt4$NO3N_OUT_MO))[as.integer(dt4$NO3N_OUT_MO) ]               
if (class(dt4$NO3N_OUT_MO)=="character") dt4$NO3N_OUT_MO <-as.numeric(dt4$NO3N_OUT_MO)
if (class(dt4$NO3NCODE_MO)!="factor") dt4$NO3NCODE_MO<- as.factor(dt4$NO3NCODE_MO)
if (class(dt4$NA_OUT_MO)=="factor") dt4$NA_OUT_MO <-as.numeric(levels(dt4$NA_OUT_MO))[as.integer(dt4$NA_OUT_MO) ]               
if (class(dt4$NA_OUT_MO)=="character") dt4$NA_OUT_MO <-as.numeric(dt4$NA_OUT_MO)
if (class(dt4$NACODE_MO)!="factor") dt4$NACODE_MO<- as.factor(dt4$NACODE_MO)
if (class(dt4$K_OUT_MO)=="factor") dt4$K_OUT_MO <-as.numeric(levels(dt4$K_OUT_MO))[as.integer(dt4$K_OUT_MO) ]               
if (class(dt4$K_OUT_MO)=="character") dt4$K_OUT_MO <-as.numeric(dt4$K_OUT_MO)
if (class(dt4$KCODE_MO)!="factor") dt4$KCODE_MO<- as.factor(dt4$KCODE_MO)
if (class(dt4$CA_OUT_MO)=="factor") dt4$CA_OUT_MO <-as.numeric(levels(dt4$CA_OUT_MO))[as.integer(dt4$CA_OUT_MO) ]               
if (class(dt4$CA_OUT_MO)=="character") dt4$CA_OUT_MO <-as.numeric(dt4$CA_OUT_MO)
if (class(dt4$CACODE_MO)!="factor") dt4$CACODE_MO<- as.factor(dt4$CACODE_MO)
if (class(dt4$MG_OUT_MO)=="factor") dt4$MG_OUT_MO <-as.numeric(levels(dt4$MG_OUT_MO))[as.integer(dt4$MG_OUT_MO) ]               
if (class(dt4$MG_OUT_MO)=="character") dt4$MG_OUT_MO <-as.numeric(dt4$MG_OUT_MO)
if (class(dt4$MGCODE_MO)!="factor") dt4$MGCODE_MO<- as.factor(dt4$MGCODE_MO)
if (class(dt4$SO4S_OUT_MO)=="factor") dt4$SO4S_OUT_MO <-as.numeric(levels(dt4$SO4S_OUT_MO))[as.integer(dt4$SO4S_OUT_MO) ]               
if (class(dt4$SO4S_OUT_MO)=="character") dt4$SO4S_OUT_MO <-as.numeric(dt4$SO4S_OUT_MO)
if (class(dt4$SO4SCODE_MO)!="factor") dt4$SO4SCODE_MO<- as.factor(dt4$SO4SCODE_MO)
if (class(dt4$CL_OUT_MO)=="factor") dt4$CL_OUT_MO <-as.numeric(levels(dt4$CL_OUT_MO))[as.integer(dt4$CL_OUT_MO) ]               
if (class(dt4$CL_OUT_MO)=="character") dt4$CL_OUT_MO <-as.numeric(dt4$CL_OUT_MO)
if (class(dt4$CLCODE_MO)!="factor") dt4$CLCODE_MO<- as.factor(dt4$CLCODE_MO)
if (class(dt4$DOC_OUT_MO)=="factor") dt4$DOC_OUT_MO <-as.numeric(levels(dt4$DOC_OUT_MO))[as.integer(dt4$DOC_OUT_MO) ]               
if (class(dt4$DOC_OUT_MO)=="character") dt4$DOC_OUT_MO <-as.numeric(dt4$DOC_OUT_MO)
if (class(dt4$DOCCODE_MO)!="factor") dt4$DOCCODE_MO<- as.factor(dt4$DOCCODE_MO)

# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            

################################################################################
#Stream water nutrient outflow (kg/ha) for each proportional sampling interval (Oct 1 1968 - Oct 1 2018)
################################################################################


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4021/23/a2cd7d661bd2920a18bd8bbd0f2bc463" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "STCODE",     
                 "ENTITY",     
                 "SITECODE",     
                 "WATERYEAR",     
                 "DATE_TIME",     
                 "LABNO",     
                 "TYPE",     
                 "Q_AREA_CM",     
                 "QCODE",     
                 "ALK_OUTPUT",     
                 "ALKCODE",     
                 "SSED_OUTPUT",     
                 "SSEDCODE",     
                 "SI_OUTPUT",     
                 "SICODE",     
                 "UTP_OUTPUT",     
                 "UTPCODE",     
                 "TDP_OUTPUT",     
                 "TDPCODE",     
                 "PARTP_OUTPUT",     
                 "PARTPCODE",     
                 "PO4P_OUTPUT",     
                 "PO4PCODE",     
                 "UTN_OUTPUT",     
                 "UTNCODE",     
                 "TDN_OUTPUT",     
                 "TDNCODE",     
                 "DON_OUTPUT",     
                 "DONCODE",     
                 "PARTN_OUTPUT",     
                 "PARTNCODE",     
                 "UTKN_OUTPUT",     
                 "UTKNCODE",     
                 "TKN_OUTPUT",     
                 "TKNCODE",     
                 "NH3N_OUTPUT",     
                 "NH3NCODE",     
                 "NO3N_OUTPUT",     
                 "NO3NCODE",     
                 "NA_OUTPUT",     
                 "NACODE",     
                 "K_OUTPUT",     
                 "KCODE",     
                 "CA_OUTPUT",     
                 "CACODE",     
                 "MG_OUTPUT",     
                 "MGCODE",     
                 "SO4S_OUTPUT",     
                 "SO4SCODE",     
                 "CL_OUTPUT",     
                 "CLCODE",     
                 "DOC_OUTPUT",     
                 "DOCCODE"    ), check.names=TRUE)

unlink(infile5)


# attempting to convert dt5$DATE_TIME dateTime string to R date structure (date or POSIXct)                                

tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp5DATE_TIME<-as.POSIXct(dt5$DATE_TIME,format=tmpDateFormat)

# Keep the new dates only if they all converted correctly
if(length(tmp5DATE_TIME) == length(tmp5DATE_TIME[!is.na(tmp5DATE_TIME)])){dt5$DATE_TIME <- tmp5DATE_TIME } else {print("Date conversion failed for dt5$DATE_TIME. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp5DATE_TIME) 


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$STCODE)!="factor") dt5$STCODE<- as.factor(dt5$STCODE)
if (class(dt5$ENTITY)=="factor") dt5$ENTITY <-as.numeric(levels(dt5$ENTITY))[as.integer(dt5$ENTITY) ]               
if (class(dt5$ENTITY)=="character") dt5$ENTITY <-as.numeric(dt5$ENTITY)
if (class(dt5$SITECODE)!="factor") dt5$SITECODE<- as.factor(dt5$SITECODE)
if (class(dt5$WATERYEAR)=="factor") dt5$WATERYEAR <-as.numeric(levels(dt5$WATERYEAR))[as.integer(dt5$WATERYEAR) ]               
if (class(dt5$WATERYEAR)=="character") dt5$WATERYEAR <-as.numeric(dt5$WATERYEAR)                                   
if (class(dt5$LABNO)!="factor") dt5$LABNO<- as.factor(dt5$LABNO)
if (class(dt5$TYPE)!="factor") dt5$TYPE<- as.factor(dt5$TYPE)
if (class(dt5$Q_AREA_CM)=="factor") dt5$Q_AREA_CM <-as.numeric(levels(dt5$Q_AREA_CM))[as.integer(dt5$Q_AREA_CM) ]               
if (class(dt5$Q_AREA_CM)=="character") dt5$Q_AREA_CM <-as.numeric(dt5$Q_AREA_CM)
if (class(dt5$QCODE)!="factor") dt5$QCODE<- as.factor(dt5$QCODE)
if (class(dt5$ALK_OUTPUT)=="factor") dt5$ALK_OUTPUT <-as.numeric(levels(dt5$ALK_OUTPUT))[as.integer(dt5$ALK_OUTPUT) ]               
if (class(dt5$ALK_OUTPUT)=="character") dt5$ALK_OUTPUT <-as.numeric(dt5$ALK_OUTPUT)
if (class(dt5$ALKCODE)!="factor") dt5$ALKCODE<- as.factor(dt5$ALKCODE)
if (class(dt5$SSED_OUTPUT)=="factor") dt5$SSED_OUTPUT <-as.numeric(levels(dt5$SSED_OUTPUT))[as.integer(dt5$SSED_OUTPUT) ]               
if (class(dt5$SSED_OUTPUT)=="character") dt5$SSED_OUTPUT <-as.numeric(dt5$SSED_OUTPUT)
if (class(dt5$SSEDCODE)!="factor") dt5$SSEDCODE<- as.factor(dt5$SSEDCODE)
if (class(dt5$SI_OUTPUT)=="factor") dt5$SI_OUTPUT <-as.numeric(levels(dt5$SI_OUTPUT))[as.integer(dt5$SI_OUTPUT) ]               
if (class(dt5$SI_OUTPUT)=="character") dt5$SI_OUTPUT <-as.numeric(dt5$SI_OUTPUT)
if (class(dt5$SICODE)!="factor") dt5$SICODE<- as.factor(dt5$SICODE)
if (class(dt5$UTP_OUTPUT)=="factor") dt5$UTP_OUTPUT <-as.numeric(levels(dt5$UTP_OUTPUT))[as.integer(dt5$UTP_OUTPUT) ]               
if (class(dt5$UTP_OUTPUT)=="character") dt5$UTP_OUTPUT <-as.numeric(dt5$UTP_OUTPUT)
if (class(dt5$UTPCODE)!="factor") dt5$UTPCODE<- as.factor(dt5$UTPCODE)
if (class(dt5$TDP_OUTPUT)=="factor") dt5$TDP_OUTPUT <-as.numeric(levels(dt5$TDP_OUTPUT))[as.integer(dt5$TDP_OUTPUT) ]               
if (class(dt5$TDP_OUTPUT)=="character") dt5$TDP_OUTPUT <-as.numeric(dt5$TDP_OUTPUT)
if (class(dt5$TDPCODE)!="factor") dt5$TDPCODE<- as.factor(dt5$TDPCODE)
if (class(dt5$PARTP_OUTPUT)=="factor") dt5$PARTP_OUTPUT <-as.numeric(levels(dt5$PARTP_OUTPUT))[as.integer(dt5$PARTP_OUTPUT) ]               
if (class(dt5$PARTP_OUTPUT)=="character") dt5$PARTP_OUTPUT <-as.numeric(dt5$PARTP_OUTPUT)
if (class(dt5$PARTPCODE)!="factor") dt5$PARTPCODE<- as.factor(dt5$PARTPCODE)
if (class(dt5$PO4P_OUTPUT)=="factor") dt5$PO4P_OUTPUT <-as.numeric(levels(dt5$PO4P_OUTPUT))[as.integer(dt5$PO4P_OUTPUT) ]               
if (class(dt5$PO4P_OUTPUT)=="character") dt5$PO4P_OUTPUT <-as.numeric(dt5$PO4P_OUTPUT)
if (class(dt5$PO4PCODE)!="factor") dt5$PO4PCODE<- as.factor(dt5$PO4PCODE)
if (class(dt5$UTN_OUTPUT)=="factor") dt5$UTN_OUTPUT <-as.numeric(levels(dt5$UTN_OUTPUT))[as.integer(dt5$UTN_OUTPUT) ]               
if (class(dt5$UTN_OUTPUT)=="character") dt5$UTN_OUTPUT <-as.numeric(dt5$UTN_OUTPUT)
if (class(dt5$UTNCODE)!="factor") dt5$UTNCODE<- as.factor(dt5$UTNCODE)
if (class(dt5$TDN_OUTPUT)=="factor") dt5$TDN_OUTPUT <-as.numeric(levels(dt5$TDN_OUTPUT))[as.integer(dt5$TDN_OUTPUT) ]               
if (class(dt5$TDN_OUTPUT)=="character") dt5$TDN_OUTPUT <-as.numeric(dt5$TDN_OUTPUT)
if (class(dt5$TDNCODE)!="factor") dt5$TDNCODE<- as.factor(dt5$TDNCODE)
if (class(dt5$DON_OUTPUT)=="factor") dt5$DON_OUTPUT <-as.numeric(levels(dt5$DON_OUTPUT))[as.integer(dt5$DON_OUTPUT) ]               
if (class(dt5$DON_OUTPUT)=="character") dt5$DON_OUTPUT <-as.numeric(dt5$DON_OUTPUT)
if (class(dt5$DONCODE)!="factor") dt5$DONCODE<- as.factor(dt5$DONCODE)
if (class(dt5$PARTN_OUTPUT)=="factor") dt5$PARTN_OUTPUT <-as.numeric(levels(dt5$PARTN_OUTPUT))[as.integer(dt5$PARTN_OUTPUT) ]               
if (class(dt5$PARTN_OUTPUT)=="character") dt5$PARTN_OUTPUT <-as.numeric(dt5$PARTN_OUTPUT)
if (class(dt5$PARTNCODE)!="factor") dt5$PARTNCODE<- as.factor(dt5$PARTNCODE)
if (class(dt5$UTKN_OUTPUT)=="factor") dt5$UTKN_OUTPUT <-as.numeric(levels(dt5$UTKN_OUTPUT))[as.integer(dt5$UTKN_OUTPUT) ]               
if (class(dt5$UTKN_OUTPUT)=="character") dt5$UTKN_OUTPUT <-as.numeric(dt5$UTKN_OUTPUT)
if (class(dt5$UTKNCODE)!="factor") dt5$UTKNCODE<- as.factor(dt5$UTKNCODE)
if (class(dt5$TKN_OUTPUT)=="factor") dt5$TKN_OUTPUT <-as.numeric(levels(dt5$TKN_OUTPUT))[as.integer(dt5$TKN_OUTPUT) ]               
if (class(dt5$TKN_OUTPUT)=="character") dt5$TKN_OUTPUT <-as.numeric(dt5$TKN_OUTPUT)
if (class(dt5$TKNCODE)!="factor") dt5$TKNCODE<- as.factor(dt5$TKNCODE)
if (class(dt5$NH3N_OUTPUT)=="factor") dt5$NH3N_OUTPUT <-as.numeric(levels(dt5$NH3N_OUTPUT))[as.integer(dt5$NH3N_OUTPUT) ]               
if (class(dt5$NH3N_OUTPUT)=="character") dt5$NH3N_OUTPUT <-as.numeric(dt5$NH3N_OUTPUT)
if (class(dt5$NH3NCODE)!="factor") dt5$NH3NCODE<- as.factor(dt5$NH3NCODE)
if (class(dt5$NO3N_OUTPUT)=="factor") dt5$NO3N_OUTPUT <-as.numeric(levels(dt5$NO3N_OUTPUT))[as.integer(dt5$NO3N_OUTPUT) ]               
if (class(dt5$NO3N_OUTPUT)=="character") dt5$NO3N_OUTPUT <-as.numeric(dt5$NO3N_OUTPUT)
if (class(dt5$NO3NCODE)!="factor") dt5$NO3NCODE<- as.factor(dt5$NO3NCODE)
if (class(dt5$NA_OUTPUT)=="factor") dt5$NA_OUTPUT <-as.numeric(levels(dt5$NA_OUTPUT))[as.integer(dt5$NA_OUTPUT) ]               
if (class(dt5$NA_OUTPUT)=="character") dt5$NA_OUTPUT <-as.numeric(dt5$NA_OUTPUT)
if (class(dt5$NACODE)!="factor") dt5$NACODE<- as.factor(dt5$NACODE)
if (class(dt5$K_OUTPUT)=="factor") dt5$K_OUTPUT <-as.numeric(levels(dt5$K_OUTPUT))[as.integer(dt5$K_OUTPUT) ]               
if (class(dt5$K_OUTPUT)=="character") dt5$K_OUTPUT <-as.numeric(dt5$K_OUTPUT)
if (class(dt5$KCODE)!="factor") dt5$KCODE<- as.factor(dt5$KCODE)
if (class(dt5$CA_OUTPUT)=="factor") dt5$CA_OUTPUT <-as.numeric(levels(dt5$CA_OUTPUT))[as.integer(dt5$CA_OUTPUT) ]               
if (class(dt5$CA_OUTPUT)=="character") dt5$CA_OUTPUT <-as.numeric(dt5$CA_OUTPUT)
if (class(dt5$CACODE)!="factor") dt5$CACODE<- as.factor(dt5$CACODE)
if (class(dt5$MG_OUTPUT)=="factor") dt5$MG_OUTPUT <-as.numeric(levels(dt5$MG_OUTPUT))[as.integer(dt5$MG_OUTPUT) ]               
if (class(dt5$MG_OUTPUT)=="character") dt5$MG_OUTPUT <-as.numeric(dt5$MG_OUTPUT)
if (class(dt5$MGCODE)!="factor") dt5$MGCODE<- as.factor(dt5$MGCODE)
if (class(dt5$SO4S_OUTPUT)=="factor") dt5$SO4S_OUTPUT <-as.numeric(levels(dt5$SO4S_OUTPUT))[as.integer(dt5$SO4S_OUTPUT) ]               
if (class(dt5$SO4S_OUTPUT)=="character") dt5$SO4S_OUTPUT <-as.numeric(dt5$SO4S_OUTPUT)
if (class(dt5$SO4SCODE)!="factor") dt5$SO4SCODE<- as.factor(dt5$SO4SCODE)
if (class(dt5$CL_OUTPUT)=="factor") dt5$CL_OUTPUT <-as.numeric(levels(dt5$CL_OUTPUT))[as.integer(dt5$CL_OUTPUT) ]               
if (class(dt5$CL_OUTPUT)=="character") dt5$CL_OUTPUT <-as.numeric(dt5$CL_OUTPUT)
if (class(dt5$CLCODE)!="factor") dt5$CLCODE<- as.factor(dt5$CLCODE)
if (class(dt5$DOC_OUTPUT)=="factor") dt5$DOC_OUTPUT <-as.numeric(levels(dt5$DOC_OUTPUT))[as.integer(dt5$DOC_OUTPUT) ]               
if (class(dt5$DOC_OUTPUT)=="character") dt5$DOC_OUTPUT <-as.numeric(dt5$DOC_OUTPUT)
if (class(dt5$DOCCODE)!="factor") dt5$DOCCODE<- as.factor(dt5$DOCCODE)

# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            

################################################################################
#	Stream water sample log sheet: field and laboratory comments (Oct 9 1968 - May 22 2019)
################################################################################

inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4021/23/8f95f84d1ef61a7074291e2670adc23c" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl"))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")


dt6 <-read.csv(infile6,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "STCODE",     
                 "ENTITY",     
                 "SITECODE",     
                 "WATERYEAR",     
                 "DATE_TIME",     
                 "LABNO",     
                 "TYPE",     
                 "QCCODE",     
                 "QA_SAMPLE",     
                 "SAMPLER_TYPE",     
                 "LOW_VOLUME",     
                 "FIELD_COMMENTS",     
                 "LAB_COMMENTS",     
                 "SEQ_INDEX"    ), check.names=TRUE)

unlink(infile6)

# attempting to convert dt6$DATE_TIME dateTime string to R date structure (date or POSIXct)  

tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp6DATE_TIME<-as.POSIXct(dt6$DATE_TIME,format=tmpDateFormat)

# Keep the new dates only if they all converted correctly

if(length(tmp6DATE_TIME) == length(tmp6DATE_TIME[!is.na(tmp6DATE_TIME)])){dt6$DATE_TIME <- tmp6DATE_TIME } else {print("Date conversion failed for dt6$DATE_TIME. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp6DATE_TIME) 



# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt6$STCODE)!="factor") dt6$STCODE<- as.factor(dt6$STCODE)
if (class(dt6$ENTITY)=="factor") dt6$ENTITY <-as.numeric(levels(dt6$ENTITY))[as.integer(dt6$ENTITY) ]               
if (class(dt6$ENTITY)=="character") dt6$ENTITY <-as.numeric(dt6$ENTITY)
if (class(dt6$SITECODE)!="factor") dt6$SITECODE<- as.factor(dt6$SITECODE)
if (class(dt6$WATERYEAR)=="factor") dt6$WATERYEAR <-as.numeric(levels(dt6$WATERYEAR))[as.integer(dt6$WATERYEAR) ]               
if (class(dt6$WATERYEAR)=="character") dt6$WATERYEAR <-as.numeric(dt6$WATERYEAR)                                   
if (class(dt6$LABNO)!="factor") dt6$LABNO<- as.factor(dt6$LABNO)
if (class(dt6$TYPE)!="factor") dt6$TYPE<- as.factor(dt6$TYPE)
if (class(dt6$QCCODE)!="factor") dt6$QCCODE<- as.factor(dt6$QCCODE)
if (class(dt6$QA_SAMPLE)!="factor") dt6$QA_SAMPLE<- as.factor(dt6$QA_SAMPLE)
if (class(dt6$SAMPLER_TYPE)!="factor") dt6$SAMPLER_TYPE<- as.factor(dt6$SAMPLER_TYPE)
if (class(dt6$LOW_VOLUME)!="factor") dt6$LOW_VOLUME<- as.factor(dt6$LOW_VOLUME)
if (class(dt6$FIELD_COMMENTS)!="factor") dt6$FIELD_COMMENTS<- as.factor(dt6$FIELD_COMMENTS)
if (class(dt6$LAB_COMMENTS)!="factor") dt6$LAB_COMMENTS<- as.factor(dt6$LAB_COMMENTS)
if (class(dt6$SEQ_INDEX)=="factor") dt6$SEQ_INDEX <-as.numeric(levels(dt6$SEQ_INDEX))[as.integer(dt6$SEQ_INDEX) ]               
if (class(dt6$SEQ_INDEX)=="character") dt6$SEQ_INDEX <-as.numeric(dt6$SEQ_INDEX)

# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            

