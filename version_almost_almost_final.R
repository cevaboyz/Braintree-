#----------------------------#
  #BRAINTREE SCRIPT v.001#
#----------------------------#


#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxml")
library(readxl)

library(ggplot2)

#install.packages("lubridate")
library(lubridate)

#install.packages("plyr")
library(plyr)

library(dplyr)

library(readxl)

library(countrycode)



#----------------------------#

getwd()

#files <- list.files(pattern = ".xlsx$")

#df_list <- lapply(files, read.excel)

#df_bind <- bind_rows(df_list)

#----------------------------#



transazioni_braintree_main <- read_excel("transazioni-braintree.xlsx", 
                                         col_types = c("text", "text", "text", 
                                                       "date", "date", "date", "text", "text", 
                                                       "numeric", "numeric", "text", "text", 
                                                       "text", "text", "date", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "numeric", "text", "text", "text", 
                                                       "text", "text", "text", "numeric", 
                                                       "text", "numeric", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "numeric", "text", "numeric", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "numeric", "text", "numeric", 
                                                       "text", "text", "numeric", "numeric", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "numeric", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text"))





brain_tree_for_manipulation <- transazioni_braintree_main %>% select("Transaction ID",
                                                                     "Transaction Type", 
                                                                     "Transaction Status",
                                                                     "Created Datetime",
                                                                     "Settlement Date",
                                                                     "Disbursement Date",
                                                                     "Order ID",
                                                                     "Payment Instrument Type",
                                                                     "Card Type",
                                                                     "Customer Email",
                                                                     "Customer Phone",
                                                                     "Customer First Name",
                                                                     "Customer Last Name",
                                                                     "Billing City (Locality)",
                                                                     "Processor Response Text",
                                                                     "Gateway Rejection Reason",
                                                                     "Fraud Detected",
                                                                     "Country of Issuance",
                                                                     "Issuing Bank",
                                                                     "Prepaid",
                                                                     "3DS - Status",
                                                                     "Risk ID",
                                                                     "Risk Decision",
                                                                     "Decision Reasons",
                                                                     "Settlement Amount",
                                                                     "Country of Issuance")



#-----------------------------#

refounds <- brain_tree_for_manipulation %>% filter(`Transaction Type`== "credit")

unique_refounds <- refounds[!duplicated(refounds$`Customer Email`), ]

refounds_vector <- refounds %>% pull(`Customer Email`)


#conteggio Credit Card/Paypal

payment_instrument_type <- unique_refounds %>% group_by(`Payment Instrument Type`) %>%
                           summarize(count = n())

payment_instrument_type <- payment_instrument_type %>% mutate(`Payment Instrument Type`=as.factor(`Payment Instrument Type`))

##ONLY CREDIT CARDS##

#Conteggio delle Carte Tipologie delle Carte di Credito

credit_card_type <- unique_refounds %>% filter(`Payment Instrument Type` == "Credit Card") %>% group_by(`Card Type`) %>% summarize(count = n())

credit_card_type <- credit_card_type %>% mutate(`Card Type`= as.factor(`Card Type`))

#Conteggio delle Tipologie di Istituti di Credito Emittenti carte utilizzate resi

issuing_bank_refound <- unique_refounds %>% group_by(`Issuing Bank`) %>% mutate(`Issuing Bank`= tolower(`Issuing Bank`)) %>% summarize(count = n())

issuing_bank_refound_normalizing <- issuing_bank_refound %>% mutate(`Issuing Bank`= gsub("\\.","", `Issuing Bank`))

issuing_bank_refound_normalizing <- issuing_bank_refound_normalizing %>% mutate(`Issuing Bank`= gsub("spa$","", `Issuing Bank`))

issuing_bank_refound_normalizing <- issuing_bank_refound_normalizing %>% mutate(`Issuing Bank`= gsub("spa","", `Issuing Bank`))

issuing_bank_refound_normalizing <- aggregate(issuing_bank_refound_normalizing$count, by= list(issuing_bank_refound_normalizing$`Issuing Bank`), FUN = sum)

issuing_bank_refound_normalizing <- issuing_bank_refound_normalizing %>% rename('Istituto di Credito Emittente' = Group.1, Frequenza = x)

issuing_bank_refound_normalizing <- issuing_bank_refound_normalizing %>% mutate(`Istituto di Credito Emittente`= as.factor(`Istituto di Credito Emittente`))

###issuing_bank_refound_normalizing <- issuing_bank_refound_normalizing %>% drop_na() %>% group_by(`Issuing Bank`) %>% summarize(count = n() ) 

#Tavola dei Comuni dove sono stati effettuati i resi

billing_city_refounds <- unique_refounds %>% select(`Billing City (Locality)`)

billing_city_refounds <- billing_city_refounds %>% drop_na()

billing_city_refounds <- billing_city_refounds %>% rename(Città = `Billing City (Locality)`) %>% mutate(Città = tolower(Città))

billing_city_refounds <- billing_city_refounds %>% group_by(Città) %>% summarize(count = n())

billing_city_refounds <- billing_city_refounds %>% mutate(Città = as.factor(Città))

billing_city_refounds_top_10 <- billing_city_refounds %>% arrange(desc(count)) %>% slice(1:10)

#Distribuzione tra carte prepagate e debit/credit card

prepaid_or_not <- unique_refounds %>% select(Prepaid) %>% group_by(Prepaid) %>% summarize(count = n())

prepaid_or_not <- drop_na(prepaid_or_not)

prepaid_or_not <- prepaid_or_not %>% mutate(Prepaid = as.factor(Prepaid))

#Distribuzione Geografica Emittenti delle Carte

country_of_issuance <- unique_refounds %>% select(`Country of Issuance`)

country_of_issuance <- country_of_issuance %>% group_by(`Country of Issuance`) %>% summarize(count = n())

country_of_issuance <- country_of_issuance %>% mutate(`Country of Issuance` = ifelse(is.na(`Country of Issuance`), "Non Disponibile", `Country of Issuance`))

country_of_issuance <- country_of_issuance %>% mutate(`Country of Issuance`= as.factor(`Country of Issuance`))

country_of_issuance$`Country of Issuance` <- countrycode(country_of_issuance$`Country of Issuance`, origin = "iso3c", destination = "country.name")

country_of_issuance <- country_of_issuance %>% mutate(`Country of Issuance`= ifelse(is.na(`Country of Issuance`), "Non Disponibile", `Country of Issuance`))

country_of_issuance <- country_of_issuance %>% mutate(`Country of Issuance`= as.factor(`Country of Issuance`))

#Tavola sullo status della transazione

transaction_status <- unique_refounds %>% select(`Processor Response Text`)

transaction_status <- transaction_status %>% group_by(`Processor Response Text`) %>% summarize(count = n())

transaction_status <- transaction_status %>% mutate(`Processor Response Text`= as.factor(`Processor Response Text`))

#Distribuzione Giornaliera dei Resi 

date_and_time <- unique_refounds %>% select(`Created Datetime`, `Disbursement Date`)

date_and_time <- date_and_time %>% mutate(`Created Datetime`=str_sub(`Created Datetime`,1,10), `Disbursement Date`=str_sub(`Disbursement Date`,1,10))

date_and_time$Processing_Time <- as.Date(as.character(date_and_time$`Disbursement Date`), format="%Y-%d-%m") -
  as.Date(as.character(date_and_time$`Created Datetime`), format="%Y-%d-%m")

date_and_time <- drop_na(date_and_time)

date_and_time_1 <- date_and_time %>% group_by(Processing_Time) %>% summarize(count = n())

date_and_time_1 <- date_and_time_1 %>% mutate(Processing_Time = as.factor(Processing_Time))

#####################################GRAFICI RESI##########################################

#Grafico Distribuzione Metodi di Pagamento

y <- ggplot(payment_instrument_type, aes(x = payment_instrument_type$`Payment Instrument Type`, y= payment_instrument_type$count, color = count, fill=count, width= count/100)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Metodi di Pagamento Utilizzati dai clienti che hanno effettuato il reso",x="Tipologia", y = "Frequenza")

y_1 <- y + theme(axis.text.x = element_text(angle = 40, hjust = 1))




#Grafico Tipologie Carte di Credito

a <- ggplot(credit_card_type, aes(x = credit_card_type$`Card Type`, y = count, color = count, fill = count, width=count/100)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Tipologie di Carte di Credito utilizzate da chi ha effettuato un reso",x="Tipologia", y = "Frequenza")

#Grafico Istituti di Credito Emittenti utenti che hanno effettuato il reso

b <- ggplot(issuing_bank_refound_normalizing, aes(x = issuing_bank_refound_normalizing$`Istituto di Credito Emittente`, y= issuing_bank_refound_normalizing$Frequenza, color = Frequenza, fill=Frequenza, width= issuing_bank_refound_normalizing$Frequenza/100)) +geom_bar(stat = "identity")+geom_text(aes(label = issuing_bank_refound_normalizing$Frequenza), vjust = -0.3)+ theme_pubclean()+ labs(title="Istituti di Credito Emittenti delle Carte utilizzate da chi ha effettuato un reso",x="Tipologia", y = "Frequenza")

b_1 <- b + theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico distribuzione geografica clienti che hanno fatto il reso 

c <- ggplot(billing_city_refounds_top_10, aes(x =billing_city_refounds_top_10$Città, y=billing_city_refounds_top_10$count, color=count, fill=count, width= billing_city_refounds_top_10$count/10))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione geografica degli utenti che hanno effettuato il reso",x="Città", y = "Frequenza")

c_1 <- c + theme(axis.text.x = element_text(angle = 40, hjust = 1))



#Grafico Distribuzione tra carte prepagate e non dei resi

d <- ggplot(prepaid_or_not, aes(x=prepaid_or_not$Prepaid, y=prepaid_or_not$count, color = count, fill = count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione tra carte prepagate e non utilizzate nelle procedure di reso",x="Tipologia Carta", y = "Frequenza")

d_1 <- d +theme(axis.text.x = element_text(angle = 0, hjust = 1))


#Grafico Distribuzione Geografica Emittenti Carte di Credito/Prepagate

e <- ggplot(country_of_issuance, aes(x=country_of_issuance$`Country of Issuance`, y=country_of_issuance$count, color = count, fill= count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title ="Nazione di Emissione della Carta di Credito/Debito degli utenti che hanno richiesto il reso", x="Nazione di Emissione", y="Frequenza")

e_1 <- e+theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico Distribuzione Status transazione

f <- ggplot(transaction_status, aes(x=transaction_status$`Processor Response Text`, y=count, color=count, fill=count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Status delle Transazioni", x= "Stato", y="Frequenza") 

f_1 <- f+theme(axis.text.x = element_text(angle = 360, hjust = 1))

#Grafico Distribuzione tempistiche di processazione ordine

g <- ggplot(date_and_time_1, aes(x=date_and_time_1$Processing_Time, y=date_and_time_1$count, color=count, fill=count, width = count/50))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Tempo Di Processazione degli Ordini (Resi)", x="Giorni Trascorsi dalla Data di Ricevimento dell'Ordine alla Data di Effettivo Accredito", y="Frequenza")

g_1 <- g+theme(axis.text.x = element_text(angle = 360, hjust = 1))

png("Grafico Distribuzione tempistiche di processazione ordine.png", height = 1000, width = 2000, units = "px", )
print(g_1)
dev.off()

#Eject all the plots

#invisible(mapply(ggsave, file=paste0("plot-", names(myplots), ".png"), plot=myplots))



################################DOUBLE CHECK ESCLUDERE I RESI DAGLI ORDINI###################

credit_from_main  <- brain_tree_for_manipulation %>% filter(`Transaction Type`== "credit")

credit_1 <- credit %>% select(`Order ID`)

credit_2_ID <- brain_tree_for_manipulation[! brain_tree_for_manipulation$`Order ID` %in% credit_1$`Order ID`, ]

sales_without_refounds <- credit_2_ID

#####################################STARTING SALES##################################################

#Numero di tentativi di acquisto per ordine

trials_of_sale <- sales_without_refounds %>% select(`Order ID`)

trials_of_sale <- trials_of_sale %>% group_by(`Order ID`) %>% summarize(count = n())

trials_of_sale_table_numbers_of_trial <- trials_of_sale %>% group_by(count) %>% summarize(count = n())

trials_of_sale_table_numbers_of_trial <- tibble::rownames_to_column(trials_of_sale_table_numbers_of_trial, "Valore")

trials_of_sale_table_numbers_of_trial <- trials_of_sale_table_numbers_of_trial %>% select(-value)

trials_of_sale_table_numbers_of_trial <- trials_of_sale_table_numbers_of_trial %>% mutate(Valore = as.factor(Valore))

trials_of_sale_top_15 <- trials_of_sale %>% arrange(desc(count)) %>% slice(1:15)

trials_of_sale_top_15_client_id <- merge(x= trials_of_sale_top_15, y=sales_without_refounds, by=c("Order ID"))


#conteggio Credit Card/Paypal

payment_instrument_type_sales <- sales_without_refounds %>% group_by(`Payment Instrument Type`) %>%
  summarize(count = n())

payment_instrument_type_sales <- payment_instrument_type_sales %>% mutate(`Payment Instrument Type`=as.factor(`Payment Instrument Type`))

##############################################ONLY CREDIT CARDS#################################

#Conteggio delle Carte Tipologie delle Carte di Credito

credit_card_type_sales <- sales_without_refounds %>% filter(`Payment Instrument Type` == "Credit Card") %>% group_by(`Card Type`) %>% summarize(count = n())

credit_card_type_sales <- credit_card_type_sales %>% mutate(`Card Type`= as.factor(`Card Type`))

#Conteggio delle Tipologie di Istituti di Credito Emittenti carte utilizzate per gli ordini

issuing_bank_sales <- sales_without_refounds %>% group_by(`Issuing Bank`) %>% mutate(`Issuing Bank`= tolower(`Issuing Bank`)) %>% summarize(count = n())

issuing_bank_sales_normalizing <- issuing_bank_sales %>% mutate(`Issuing Bank`= gsub("\\.","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("spa$","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("\\,","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("\\!","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("spa","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("sa","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("srl","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("plc","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("ltd","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("ag","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("sp","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("bnl$","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("\\-","", `Issuing Bank`))

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Issuing Bank`= gsub("^ *|(?<= ) | *$", "", `Issuing Bank`, perl=T))

#issuing_bank_sales_normalizing <- trimws(issuing_bank_refound_normalizing$`Istituto di Credito Emittente`, which = c("right"))

issuing_bank_sales_normalizing <- aggregate(issuing_bank_sales_normalizing$count, by= list(issuing_bank_sales_normalizing$`Issuing Bank`), FUN = sum)

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% rename('Istituto di Credito Emittente' = Group.1, Frequenza = x)

issuing_bank_sales_normalizing <- issuing_bank_sales_normalizing %>% mutate(`Istituto di Credito Emittente`= as.factor(`Istituto di Credito Emittente`))

issuing_bank_sales_normalizing_top_15 <- issuing_bank_sales_normalizing %>% arrange(desc(Frequenza)) %>% slice(1:15)

#Tavola dei Comuni dove sono stati effettuati gli ordini

billing_city_sales <- sales_without_refounds %>% select(`Billing City (Locality)`)

billing_city_sales <- billing_city_sales %>% drop_na()

billing_city_sales <- billing_city_sales %>% rename(Città = `Billing City (Locality)`) %>% mutate(Città = tolower(Città))

billing_city_sales <- billing_city_sales %>% group_by(Città) %>% summarize(count = n())

billing_city_sales <- billing_city_sales %>% mutate(Città = as.factor(Città))

billing_city_sales_top_15 <- billing_city_sales %>% arrange(desc(count)) %>% slice(1:15)

#Distribuzione tra carte prepagate e debit/credit card di chi ha effettuato un ordine

prepaid_or_not_sales <- sales_without_refounds %>% select(Prepaid) %>% group_by(Prepaid) %>% summarize(count = n())

prepaid_or_not_sales <- drop_na(prepaid_or_not_sales)

prepaid_or_not_sales <- prepaid_or_not_sales %>% mutate(Prepaid = as.factor(Prepaid))

#Distribuzione Geografica Emittenti delle Carte

country_of_issuance_sales <- sales_without_refounds %>% select(`Country of Issuance`)

country_of_issuance_sales <- country_of_issuance_sales %>% group_by(`Country of Issuance`) %>% summarize(count = n())

country_of_issuance_sales <- country_of_issuance_sales %>% mutate(`Country of Issuance` = ifelse(is.na(`Country of Issuance`), "Non Disponibile", `Country of Issuance`))

country_of_issuance_sales <- country_of_issuance_sales %>% mutate(`Country of Issuance`= as.factor(`Country of Issuance`))

country_of_issuance_sales$`Country of Issuance` <- countrycode(country_of_issuance_sales$`Country of Issuance`, origin = "iso3c", destination = "country.name")

country_of_issuance_sales <- country_of_issuance_sales %>% mutate(`Country of Issuance`= ifelse(is.na(`Country of Issuance`), "Non Disponibile", `Country of Issuance`))

country_of_issuance_sales <- country_of_issuance_sales %>% mutate(`Country of Issuance`= as.factor(`Country of Issuance`))

#Tavola sullo status della transazione degli utenti che hanno effettuato un ordine

transaction_status_sales <- sales_without_refounds %>% select(`Processor Response Text`)

transaction_status_sales <- transaction_status_sales %>% group_by(`Processor Response Text`) %>% summarize(count = n())

transaction_status_sales <- transaction_status_sales %>% mutate(`Processor Response Text`= as.factor(`Processor Response Text`))

transaction_status_sales <- transaction_status_sales %>% arrange(desc(count))

transaction_status_sales <- transaction_status_sales %>% mutate(`Processor Response Text`= str_replace_all(`Processor Response Text`, "Funding Instrument In The PayPal Account Was Declined By The Processor Or Bank, Or It Can't Be Used For This Payment", "Funding Instrument In The PayPal Account Was Declined"))

#Distribuzione Giornaliera degli Ordini  

date_and_time_sales <- sales_without_refounds %>% select(`Created Datetime`, `Disbursement Date`)

date_and_time_sales <- date_and_time_sales %>% mutate(`Created Datetime`=str_sub(`Created Datetime`,1,10), `Disbursement Date`=str_sub(`Disbursement Date`,1,10))

date_and_time_sales$Processing_Time <- as.Date(as.character(date_and_time_sales$`Disbursement Date`), format="%Y-%d-%m") -
  as.Date(as.character(date_and_time_sales$`Created Datetime`), format="%Y-%d-%m")

date_and_time_sales <- drop_na(date_and_time_sales)

date_and_time_sales_1 <- date_and_time_sales %>% group_by(Processing_Time) %>% summarize(count = n())

date_and_time_sales_1 <- date_and_time_sales_1 %>% mutate(Processing_Time = as.factor(Processing_Time))





########################################GRAFICI SALES############################################## 

#Grafico Distribuzione Tentativi di Pagamento/Ordine

aaa <- aa <- ggplot(trials_of_sale_table_numbers_of_trial , aes(x = trials_of_sale_table_numbers_of_trial$Valore, y= trials_of_sale_table_numbers_of_trial$count, color = count, fill=count, width= count/10000)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Numero di Tentativi per Ordine",x="Tipologia", y = "Frequenza")

aaa_1 <- aaa + theme(axis.text.x = element_text(angle = 360, hjust = 1))

aaa_1


#Grafico Distribuzione Metodi di Pagamento

aa <- ggplot(payment_instrument_type_sales, aes(x = payment_instrument_type_sales$`Payment Instrument Type`, y= payment_instrument_type_sales$count, color = count, fill=count, width= count/10000)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Metodi di Pagamento Utilizzati dai clienti",x="Tipologia", y = "Frequenza")

aa_1 <- aa + theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico Tipologie Carte di Credito

bb <- ggplot(credit_card_type_sales, aes(x = credit_card_type_sales$`Card Type`, y = count, color = count, fill = count, width=count/10000)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Tipologie di Carte di Credito utilizzate da chi ha effettuato un ordine",x="Tipologia", y = "Frequenza")

bb_1 <- bb + theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico Istituti di Credito Emittenti utenti che hanno effettuato un ordine

cc <- ggplot(issuing_bank_sales_normalizing_top_15, aes(x = issuing_bank_sales_normalizing_top_15$`Istituto di Credito Emittente`, y= issuing_bank_sales_normalizing_top_15$Frequenza, color = Frequenza, fill=Frequenza, width= issuing_bank_sales_normalizing_top_15$Frequenza/1000)) +geom_bar(stat = "identity")+geom_text(aes(label = issuing_bank_sales_normalizing_top_15$Frequenza), vjust = -0.3)+ theme_pubclean()+ labs(title="Istituti di Credito Emittenti delle Carte utilizzate da chi ha effettuato un ordine (TOP 15)",x="Tipologia", y = "Frequenza")

cc_1 <- cc + theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico distribuzione geografica clienti che hanno fatto un ordine

dd <- ggplot(billing_city_sales_top_15, aes(x =billing_city_sales_top_15$Città, y=billing_city_sales_top_15$count, color=count, fill=count, width= billing_city_sales_top_15$count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione geografica degli utenti che hanno effettuato un  (TOP 15)",x="Città", y = "Frequenza")

dd_1 <- dd + theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico Distribuzione tra carte prepagate e non dei resi

ee <- ggplot(prepaid_or_not_sales, aes(x=prepaid_or_not_sales$Prepaid, y=prepaid_or_not_sales$count, color = count, fill = count, width = count/10000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione tra carte prepagate e non utilizzate negli ordini",x="Tipologia Carta", y = "Frequenza")

ee_1 <- ee +theme(axis.text.x = element_text(angle = 0, hjust = 1))

#Grafico Distribuzione Geografica Emittenti Carte di Credito/Prepagate

ff <- ggplot(country_of_issuance_sales, aes(x=country_of_issuance_sales$`Country of Issuance`, y=country_of_issuance_sales$count, color = count, fill= count, width = count/10000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title ="Nazione di Emissione della Carta di Credito/Debito degli utenti che hanno effettuato un ordine", x="Nazione di Emissione", y="Frequenza")

ff_1 <- ff+theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico Distribuzione Status transazione

gg <- ggplot(transaction_status_sales, aes(x=transaction_status_sales$`Processor Response Text`, y=count, color=count, fill=count, width = count/10000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Status delle Transazioni degli Ordini", x= "Stato", y="Frequenza") 

gg_1 <- gg+theme(axis.text.x = element_text(angle = 40, hjust = 1))

#Grafico Distribuzione tempistiche di processazione ordine

ee <- ggplot(date_and_time_sales_1, aes(x=date_and_time_sales_1$Processing_Time, y=date_and_time_sales_1$count, color=count, fill=count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Tempo Di Processazione degli Ordini", x="Giorni Trascorsi dalla Data di Ricevimento dell'Ordine alla Data di Effettivo Addebito", y="Frequenza")

e_1 <- ee+theme(axis.text.x = element_text(angle = 360, hjust = 1))

e_1


##########################################################################################
########################################TENTATIVI DI ORDINI RIPETUTI######################


trials_of_sale_zoom <- merge(x= trials_of_sale, y=sales_without_refounds, by=c("Order ID"))

trials_of_sale_zoom <- trials_of_sale_zoom %>% select(`Order ID`,count, `Payment Instrument Type`, `Card Type`,`Customer Email`,`Customer First Name`, `Customer Last Name`, `Created Datetime`, `Billing City (Locality)`, `Processor Response Text`, `Fraud Detected`, `Issuing Bank`, Prepaid,`Risk Decision`,`Country of Issuance`)

trials_of_sale_zoom <- trials_of_sale_zoom %>% mutate(`Risk Decision`= ifelse(is.na(`Risk Decision`),0,`Risk Decision`))


trials_of_sale_zoom_no_frodi <- trials_of_sale_zoom %>% filter(!(`Risk Decision`== "Deny" ))##errore

trials_of_sale_zoom_no_frodi_top<- trials_of_sale_zoom_no_frodi %>% group_by(count) %>% summarize(count = n())

trials_of_sale_zoom_no_frodi_top <- tibble::rownames_to_column(trials_of_sale_zoom_no_frodi_top, "Valore")

#Numero di Tentativi per Ordine escludendo le frodi

trials_of_sale_zoom_no_frodi_top <- trials_of_sale_zoom_no_frodi_top %>% mutate( Valore = as.factor(Valore))



#Ordinamento Tentativi per Ordini Maggiori di 2 escludendo frodi 

trials_of_sale_zoom_maggiori_1 <- trials_of_sale_zoom_no_frodi %>% filter(count > 1)

#test per gli approved dopo quanti tentativi sono andati a buon fine

trials_of_sale_zoom_maggiori_2 <- trials_of_sale_zoom_no_frodi %>% filter(count > 1 & `Processor Response Text` =="Approved")

trials_of_sale_zoom_maggiori_2_table_count <- as.data.frame(table(trials_of_sale_zoom_maggiori_2$count))

trials_of_sale_zoom_maggiori_2_table_count <- trials_of_sale_zoom_maggiori_2_table_count %>% rename("Numero di Tentativi"=Var1, count = Freq)

trials_of_sale_zoom_maggiori_2_table_count <- trials_of_sale_zoom_maggiori_2_table_count %>% mutate(`Numero di Tentativi`=as.factor(`Numero di Tentativi`))
#################Verifica delle Motivazioni che hanno portato al non successo degli utenti che alla fine sono riusciti a fare il pagamento############################################################

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti <- trials_of_sale_zoom_maggiori_2 %>% select(`Customer Email`)

processor_response_for_merge <- brain_tree_for_manipulation %>% select(`Processor Response Text`, `Customer Email`)

########################################################################################################################
 
trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge <- merge(x=trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti, y= processor_response_for_merge, by="Customer Email")

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_minus_approved <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge %>% filter(!(`Processor Response Text`=="Approved"))

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table <- as.data.frame(table(trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_minus_approved$`Processor Response Text`))

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table %>% mutate(Var1 = as.factor(Var1))

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table %>% rename("Processor Response Text"=Var1, count=Freq)

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table %>% mutate(`Processor Response Text`= str_replace_all(`Processor Response Text`, "Funding Instrument In The PayPal Account Was Declined By The Processor Or Bank, Or It Can't Be Used For This Payment", "Funding Instrument In The PayPal Account Was Declined"))

#############################################################################

trials_of_sale_zoom_maggiori_1_unici <-trials_of_sale_zoom_maggiori_1  %>% distinct(trials_of_sale_zoom_maggiori_1$`Order ID`, .keep_all = TRUE)

trials_of_sale_zoom_maggiori_unici_top <- as.data.frame(table(trials_of_sale_zoom_maggiori_1_unici$count))

trials_of_sale_zoom_maggiori_unici_top <- trials_of_sale_zoom_maggiori_unici_top %>% rename("Numero di Tentativi" = Var1, count = Freq)

trials_of_sale_zoom_maggiori_unici_top <- trials_of_sale_zoom_maggiori_unici_top %>% mutate(`Numero di Tentativi`=as.factor(`Numero di Tentativi`))


trials_of_sale_zoom_top_top <- trials_of_sale_zoom_maggiori_1 %>% group_by(count) %>% summarize(count = n()) 

trials_of_sale_zoom_top_top <- tibble::rownames_to_column(trials_of_sale_zoom_top_top, "Valore")

#conteggio Credit Card/Paypal Ordini >1 

payment_instrument_type_sales_maj1 <- trials_of_sale_zoom_maggiori_1_unici %>% group_by(`Payment Instrument Type`) %>%
  summarize(count = n())

payment_instrument_type_sales_maj1 <- payment_instrument_type_sales_maj1 %>% mutate(`Payment Instrument Type`=as.factor(`Payment Instrument Type`))

#Conteggio delle Carte Tipologie delle Carte di Credito

credit_card_type_sales_trials_more_than_one <- trials_of_sale_zoom_maggiori_1_unici %>% filter(`Payment Instrument Type` == "Credit Card") %>% group_by(`Card Type`) %>% summarize(count = n())

credit_card_type_sales_trials_more_than_one <- credit_card_type_sales_trials_more_than_one %>% mutate(`Card Type`= as.factor(`Card Type`))

#Conteggio delle Tipologie di Istituti di Credito Emittenti carte utilizzate per gli ordini

issuing_bank_sales_more_than_one <- trials_of_sale_zoom_maggiori_1_unici %>% group_by(`Issuing Bank`) %>% mutate(`Issuing Bank`= tolower(`Issuing Bank`)) %>% summarize(count = n())

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_more_than_one %>% mutate(`Issuing Bank`= gsub("\\.","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("spa$","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("\\,","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("\\!","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("spa","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("sa","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("srl","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("plc","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("ltd","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("ag","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("sp","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("bnl$","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("\\-","", `Issuing Bank`))

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Issuing Bank`= gsub("^ *|(?<= ) | *$", "", `Issuing Bank`, perl=T))

#issuing_bank_sales_normalizing <- trimws(issuing_bank_refound_normalizing$`Istituto di Credito Emittente`, which = c("right"))

issuing_bank_sales_normalizing_more_than_one <- aggregate(issuing_bank_sales_normalizing_more_than_one$count, by= list(issuing_bank_sales_normalizing_more_than_one$`Issuing Bank`), FUN = sum)

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% rename('Istituto di Credito Emittente' = Group.1, Frequenza = x)

issuing_bank_sales_normalizing_more_than_one <- issuing_bank_sales_normalizing_more_than_one %>% mutate(`Istituto di Credito Emittente`= as.factor(`Istituto di Credito Emittente`))

issuing_bank_sales_normalizing_more_than_one_top_15 <- issuing_bank_sales_normalizing_more_than_one %>% arrange(desc(Frequenza)) %>% slice(1:15)

#Tavola dei Comuni dove è stato effettuato più di un tentativo per il pagamento degli ordini escluse le frodi

billing_city_sales_more_than_one_trial <- trials_of_sale_zoom_maggiori_1_unici %>% select(`Billing City (Locality)`)

billing_city_sales_more_than_one_trial <- billing_city_sales_more_than_one_trial %>% drop_na()

billing_city_sales_more_than_one_trial <- billing_city_sales_more_than_one_trial %>% rename(Città = `Billing City (Locality)`) %>% mutate(Città = tolower(Città))

billing_city_sales_more_than_one_trial <- billing_city_sales_more_than_one_trial %>% group_by(Città) %>% summarize(count = n())

billing_city_sales_more_than_one_trial <- billing_city_sales_more_than_one_trial %>% mutate(Città = as.factor(Città))

billing_city_sales_top_15_more_than_one_trial <- billing_city_sales_more_than_one_trial %>% arrange(desc(count)) %>% slice(1:15)


#Distribuzione tra carte prepagate e debit/credit card di chi ha tentato di effettuare il pagamento più di una volta esclusi i resi e le frodi

prepaid_or_not_sales_more_than_one <- trials_of_sale_zoom_maggiori_1_unici %>% select(Prepaid) %>% group_by(Prepaid) %>% summarize(count = n())

prepaid_or_not_sales_more_than_one <- drop_na(prepaid_or_not_sales_more_than_one)

prepaid_or_not_sales_more_than_one <- prepaid_or_not_sales_more_than_one %>% mutate(Prepaid = as.factor(Prepaid))

#Distribuzione Geografica Emittenti delle Carte

country_of_issuance_sales_more_than_one <- trials_of_sale_zoom_maggiori_1_unici %>% select(`Country of Issuance`)

country_of_issuance_sales_more_than_one <- country_of_issuance_sales_more_than_one %>% group_by(`Country of Issuance`) %>% summarize(count = n())

country_of_issuance_sales_more_than_one <- country_of_issuance_sales_more_than_one %>% mutate(`Country of Issuance` = ifelse(is.na(`Country of Issuance`), "Non Disponibile", `Country of Issuance`))

country_of_issuance_sales_more_than_one <- country_of_issuance_sales_more_than_one %>% mutate(`Country of Issuance`= as.factor(`Country of Issuance`))

country_of_issuance_sales_more_than_one$`Country of Issuance` <- countrycode(country_of_issuance_sales_more_than_one$`Country of Issuance`, origin = "iso3c", destination = "country.name")

country_of_issuance_sales_more_than_one <- country_of_issuance_sales_more_than_one %>% mutate(`Country of Issuance`= ifelse(is.na(`Country of Issuance`), "Non Disponibile", `Country of Issuance`))

country_of_issuance_sales_more_than_one <- country_of_issuance_sales_more_than_one %>% mutate(`Country of Issuance`= as.factor(`Country of Issuance`))

#Tavola sullo status della transazione degli utenti che hanno effettuato un ordine

transaction_status_sales_more_than_one <- trials_of_sale_zoom_maggiori_1_unici %>% select(`Processor Response Text`)

transaction_status_sales_more_than_one <- transaction_status_sales_more_than_one %>% group_by(`Processor Response Text`) %>% summarize(count = n())

transaction_status_sales_more_than_one <- transaction_status_sales_more_than_one %>% mutate(`Processor Response Text`= as.factor(`Processor Response Text`))

transaction_status_sales_more_than_one <- transaction_status_sales_more_than_one %>% arrange(desc(count))

transaction_status_sales_more_than_one <- transaction_status_sales_more_than_one %>% mutate(`Processor Response Text`= str_replace_all(`Processor Response Text`, "Funding Instrument In The PayPal Account Was Declined By The Processor Or Bank, Or It Can't Be Used For This Payment", "Funding Instrument In The PayPal Account Was Declined"))

#Tempistiche tra un tentativo ed un altro

processor_response_for_merge_2 <- brain_tree_for_manipulation %>% select(`Processor Response Text`, `Customer Email`, `Created Datetime`)

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge_date <- merge(x=trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti, y= processor_response_for_merge_2, by="Customer Email")

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge_date <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge_date %>% filter(!(`Processor Response Text`== "Approved"))

trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge_date <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge_date %>% mutate(`Created Datetime`=ifelse(is.na(`Created Datetime`), "Non Disponibile",`Created Datetime`))

#############################################Time tra i tentativi

Time_between_trials <- trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_test_precedenti_merge_date %>% arrange(`Created Datetime`) %>% group_by(`Customer Email`) %>% summarise(diff = last(`Created Datetime`) - first(`Created Datetime`))

Time_between_trials <- as.data.frame(table(Time_between_trials$diff))

Time_between_trials <- Time_between_trials %>% mutate(Var1= as.factor(Var1))

Time_between_trials <- Time_between_trials %>% rename(Secondi = Var1, count=Freq)

###############################################ERRORI CARTE PREPAGATE##################################

checking_prepaid <- sales_without_refounds %>% select(`Order ID`, `Customer Email`, `Processor Response Text`, Prepaid)

checking_prepaid <- checking_prepaid %>% filter(Prepaid=="Yes")

checking_prepaid <- checking_prepaid %>% group_by(`Processor Response Text`) %>% summarize(count= n())

checking_prepaid <- checking_prepaid %>% mutate(`Processor Response Text`=as.factor(`Processor Response Text`))

checking_prepaid_minus_approved <- checking_prepaid %>% filter(!(`Processor Response Text`== "Approved"))

##########################################Grafici Ordini Escludendo le frodi############

#Grafico Distribuzione Tentativi di Ordine Esclusi Frodi 

bbb <- ggplot(trials_of_sale_zoom_no_frodi_top, aes(x=trials_of_sale_zoom_no_frodi_top$Valore, y=trials_of_sale_zoom_no_frodi_top$count, color=count, fill=count, width = count/10000))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Numero di Ordini Senza Frodi", x="Numero di Tentativi", y="Frequenza")

bbb_1 <- bbb+theme(axis.text.x = element_text(angle = 360, hjust = 1))

bbb_1

#Grafico Distribuzione Tentativi di Ordine Maggiori di 1

ccc <- ggplot(trials_of_sale_zoom_maggiori_unici_top, aes(x=trials_of_sale_zoom_maggiori_unici_top$`Numero di Tentativi`, y=trials_of_sale_zoom_maggiori_unici_top$count, color=count, fill=count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Numero di Tentativi di Ordine Senza Frodi Maggiori di 1", x="Numero di Tentativi", y="Frequenza")

ccc_1 <- ccc+theme(axis.text.x = element_text(angle = 360, hjust = 1))

ccc_1

#Grafico Distribuzione su quanti tentativi ci hanno messo gli utenti che hanno provato a pagare più di una volta per avere successo

xxx <- ggplot(trials_of_sale_zoom_maggiori_2_table_count, aes(x=trials_of_sale_zoom_maggiori_2_table_count$`Numero di Tentativi`, y=trials_of_sale_zoom_maggiori_2_table_count$count, color=count, fill=count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Numero di Tentativi di pagamento effettuati dagli utenti prima di ottenere un esito positivo", x="Numero di Tentativi", y="Frequenza")

xxx_1 <- xxx+theme(axis.text.x = element_text(angle = 360, hjust = 1))

xxx_1

#Grafico Distribuzione delle motivazioni antecedenti il tentativo con buon esito per gli utenti che hanno tentato pagare più di una volta

zzz <- ggplot(trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table, aes(x=trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table$`Processor Response Text`, y=trials_of_sale_zoom_maggiori_2_con_successo_motivi_errori_table$count, color=count, fill=count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Motivazioni di errore antecedenti al tentativo ti pagamento con esito positivo", x= "Numero di Tentativi", y="Frequenza")

zzz_1 <- zzz+theme(axis.text.x = element_text(angle = 40, hjust = 1))

zzz_1

#Grafico Distribuzione Metodi di Pagamento dei Tentativi di Pagamento/Ordine Maggiori di 1

ddd  <- ggplot(payment_instrument_type_sales_maj1 , aes(x = payment_instrument_type_sales_maj1$`Payment Instrument Type`, y= payment_instrument_type_sales_maj1$count, color = count, fill=count, width= count/1000)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Metodi di Pagamento utilizzati da chi ha effettuato più di un tentativo per ordine (escluse le frodi ed i resi)",x="Tipologia", y = "Frequenza")

ddd_1 <- ddd + theme(axis.text.x = element_text(angle = 360, hjust = 1))

ddd_1

#Grafico Istituti di Credito Emittenti utenti che hanno tentato di effettuare il pagamento più di una volta escluse le frodi

eee <- ggplot(issuing_bank_sales_normalizing_more_than_one_top_15, aes(x = issuing_bank_sales_normalizing_more_than_one_top_15$`Istituto di Credito Emittente`, y= issuing_bank_sales_normalizing_more_than_one_top_15$Frequenza, color = Frequenza, fill=Frequenza, width= issuing_bank_sales_normalizing_more_than_one_top_15$Frequenza/1000)) +geom_bar(stat = "identity")+geom_text(aes(label = issuing_bank_sales_normalizing_more_than_one_top_15$Frequenza), vjust = -0.3)+ theme_pubclean()+ labs(title="Istituti di Credito Emittenti delle Carte utilizzate da chi ha tentato di effettuare un pagamento più di una volta escluse le frodi (TOP 15)",x="Tipologia", y = "Frequenza")

eee_1 <- eee + theme(axis.text.x = element_text(angle = 40, hjust = 1))

eee_1

#Grafico Distribuzione tra carte prepagate e non dei resi

fff <- ggplot(prepaid_or_not_sales_more_than_one, aes(x=prepaid_or_not_sales_more_than_one$Prepaid, y=prepaid_or_not_sales_more_than_one$count, color = count, fill = count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione tra carte prepagate e non utilizzate da chi ha effettuato più di un tentativo di pagamento",x="Tipologia Carta", y = "Frequenza")

fff_1 <- fff +theme(axis.text.x = element_text(angle = 0, hjust = 1))

fff_1

#Grafico Distribuzione Geografica Emittenti Carte di Credito/Prepagate

ggg <- ggplot(country_of_issuance_sales_more_than_one, aes(x=country_of_issuance_sales_more_than_one$`Country of Issuance`, y=country_of_issuance_sales_more_than_one$count, color = count, fill= count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title ="Nazione di Emissione della Carta di Credito/Debito degli utenti che hanno effettuato più di un tentativo di pagamento", x="Nazione di Emissione", y="Frequenza")

ggg_1 <- ggg+theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggg_1

#Grafico Distribuzione Status transazione

eeee <- ggplot(transaction_status_sales_more_than_one, aes(x=transaction_status_sales_more_than_one$`Processor Response Text`, y=count, color=count, fill=count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Status delle Transazioni degli utenti che hanno tentato di effettuare più di un pagamento", x= "Stato", y="Frequenza") 

eee_1 <- eeee+theme(axis.text.x = element_text(angle = 40, hjust = 1))

eee_1


#Grafico Distribuzione Status tempo tra tansazioni

jjjj <- ggplot(Time_between_trials, aes(x=Time_between_trials$Secondi, y=count, color=count, fill=count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Secondi tra un Tentativo ed un altro di pagamento degli utenti", x= "Secondi", y="Frequenza") 

jjj_1 <- jjjj+theme(axis.text.x = element_text(angle = 40, hjust = 1))

jjj_1

#Grafico Errori solo carte prepagate

kkkk <- ggplot(checking_prepaid, aes(x=checking_prepaid$`Processor Response Text`, y=count, color=count, fill=count, width = count/10000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Distribuzione dello Status delle operazioni effettuate con carte prepagate", x= "Status", y="Frequenza") 

kkk_1 <- kkkk+theme(axis.text.x = element_text(angle = 40, hjust = 1))

kkk_1

#Grafico Errori solo prepagate meno approved

www<- ggplot(checking_prepaid_minus_approved, aes(x=checking_prepaid_minus_approved$`Processor Response Text`, y=count, color=count, fill=count, width = count/100))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Distribuzione dello Status delle operazioni effettuate con carte prepagate meno le approvate", x= "Status", y="Frequenza") 

www_1 <- www+theme(axis.text.x = element_text(angle = 40, hjust = 1))

www_1

#transactions_without_refounds <- brain_tree_for_manipulation[! brain_tree_for_manipulation$`Customer Email` %in% refounds_vector, ]

#########################################POSTE PAY###############################
#Analisi degli utenti che hanno utilizzato/sono stati identificati come aventi una postepay

#selecting_only_postepay_issuing_bank_poste_vector <- sales_without_refounds %>% group_by(`Issuing Bank`) %>% pull(`Issuing Bank`)

#selectin_postepay <- grep("^P+.*", selecting_only_postepay_issuing_bank_poste_vector)

selecting_only_postepay_issuing_bank <- sales_without_refounds %>% select(`Issuing Bank`)

selecting_only_postepay_issuing_bank <- filter(sales_without_refounds, grepl("POSTEPAY S.P.A", sales_without_refounds$`Issuing Bank`) | grepl("PostePay S.p.A.",sales_without_refounds$`Issuing Bank`))


#conteggio Credit Card/Paypal PostePay 

payment_instrument_type_sales_poste <- selecting_only_postepay_issuing_bank %>% group_by(`Payment Instrument Type`) %>%
  summarize(count = n())

payment_instrument_type_sales_poste <- payment_instrument_type_sales_poste %>% mutate(`Payment Instrument Type`=as.factor(`Payment Instrument Type`))


#Conteggio delle Carte Tipologie delle Carte di Credito Postepay

credit_card_type_sales_trials_more_than_one_poste <- selecting_only_postepay_issuing_bank %>% filter(`Payment Instrument Type` == "Credit Card") %>% group_by(`Card Type`) %>% summarize(count = n())

credit_card_type_sales_trials_more_than_one_poste <- credit_card_type_sales_trials_more_than_one_poste %>% mutate(`Card Type`= as.factor(`Card Type`))

#Tavola dei Comuni dove sono stati effettuati pagamenti/tentativi di pagamento con le postepay

billing_city_sales_postepay <- selecting_only_postepay_issuing_bank %>% select(`Billing City (Locality)`)

billing_city_sales_postepay <- billing_city_sales_postepay %>% drop_na()

billing_city_sales_postepay <- billing_city_sales_postepay %>% rename(Città = `Billing City (Locality)`) %>% mutate(Città = tolower(Città))

billing_city_sales_postepay <- billing_city_sales_postepay %>% group_by(Città) %>% summarize(count = n())

billing_city_sales_postepay <- billing_city_sales_postepay %>% mutate(Città = as.factor(Città))

billing_city_sales_postepay_15 <- billing_city_sales_postepay %>% arrange(desc(count)) %>% slice(1:15)

#Distribuzione tra carte prepagate e debit/credit card poste pay

prepaid_or_not_sales_poste <- selecting_only_postepay_issuing_bank %>% select(Prepaid) %>% group_by(Prepaid) %>% summarize(count = n())

prepaid_or_not_sales_poste <- drop_na(prepaid_or_not_sales_poste)

prepaid_or_not_sales_poste <- prepaid_or_not_sales_poste %>% mutate(Prepaid = as.factor(Prepaid))

#Tavola sullo status della transazione degli utenti che hanno effettuato un ordine

transaction_status_sales_poste <- selecting_only_postepay_issuing_bank %>% select(`Processor Response Text`)

transaction_status_sales_poste <- transaction_status_sales_poste %>% group_by(`Processor Response Text`) %>% summarize(count = n())

transaction_status_sales_poste <- transaction_status_sales_poste %>% mutate(`Processor Response Text`= as.factor(`Processor Response Text`))

transaction_status_sales_poste <- transaction_status_sales_poste %>% arrange(desc(count))

transaction_status_sales_poste <- transaction_status_sales_poste %>% mutate(`Processor Response Text`= str_replace_all(`Processor Response Text`, "Funding Instrument In The PayPal Account Was Declined By The Processor Or Bank, Or It Can't Be Used For This Payment", "Funding Instrument In The PayPal Account Was Declined"))


#Distribuzione Giornaliera degli Ordini effettuati con una postepay

date_and_time_sales_poste <- selecting_only_postepay_issuing_bank %>% select(`Created Datetime`, `Disbursement Date`)

date_and_time_sales_poste <- date_and_time_sales_poste %>% mutate(`Created Datetime`=str_sub(`Created Datetime`,1,10), `Disbursement Date`=str_sub(`Disbursement Date`,1,10))

date_and_time_sales_poste$Processing_Time <- as.Date(as.character(date_and_time_sales_poste$`Disbursement Date`), format="%Y-%d-%m") -
  as.Date(as.character(date_and_time_sales_poste$`Created Datetime`), format="%Y-%d-%m")

date_and_time_sales_poste <- drop_na(date_and_time_sales_poste)

date_and_time_sales_poste_1 <- date_and_time_sales_poste %>% group_by(Processing_Time) %>% summarize(count = n())

date_and_time_sales_poste_1 <- date_and_time_sales_poste_1 %>% mutate(Processing_Time = as.factor(Processing_Time))







####################GRAFICI POSTEPAY######################

#Grafico Tipologie Carte di Credito Postepay

pppp <- ggplot(credit_card_type_sales_trials_more_than_one_poste, aes(x = credit_card_type_sales_trials_more_than_one_poste$`Card Type`, y = count, color = count, fill = count, width=count/10000)) +geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Tipologie di Carte PostePay",x="Tipologia", y = "Frequenza")

ppp_1 <- pppp + theme(axis.text.x = element_text(angle = 40, hjust = 1))

ppp_1


#Grafico distribuzione geografica clienti che hanno fatto un ordine con poste pay

ppppp <- ggplot(billing_city_sales_postepay_15, aes(x =billing_city_sales_postepay_15$Città, y=billing_city_sales_postepay_15$count, color=count, fill=count, width= billing_city_sales_postepay_15$count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione geografica degli utenti che hanno effettuato un ordine con postepay (TOP 15)",x="Città", y = "Frequenza")

pppp_1 <- ppppp + theme(axis.text.x = element_text(angle = 40, hjust = 1))

pppp_1


#Grafico Distribuzione tra Carte Postepay prepagate e non

pppppp <- ggplot(prepaid_or_not_sales_poste, aes(x=prepaid_or_not_sales_poste$Prepaid, y=prepaid_or_not_sales_poste$count, color = count, fill = count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+ theme_pubclean()+ labs(title="Distribuzione tra carte prepagate e non Postepay",x="Tipologia Carta", y = "Frequenza")

ppppp_1 <- pppppp +theme(axis.text.x = element_text(angle = 0, hjust = 1))

ppppp_1


#Grafico Distribuzione Status transazione postepay

ppppppp <- ggplot(transaction_status_sales_poste, aes(x=transaction_status_sales_poste$`Processor Response Text`, y=count, color=count, fill=count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label = count), vjust = -0.3)+theme_pubclean()+labs(title="Status delle Transazioni degli utenti che hanno tentato di effettuare più di un pagamento con Poste Pay", x= "Stato", y="Frequenza") 

pppppp_1 <- ppppppp+theme(axis.text.x = element_text(angle = 40, hjust = 1))

pppppp_1


#Grafico Distribuzione tempistiche di processazione ordine postepay

pppppppp<- ggplot(date_and_time_sales_poste_1, aes(x=date_and_time_sales_poste_1$Processing_Time, y=date_and_time_sales_poste_1$count, color=count, fill=count, width = count/1000))+geom_bar(stat = "identity")+geom_text(aes(label= count), vjust =-0.3)+theme_pubclean()+labs(title="Tempo Di Processazione degli Ordini Postepay", x="Giorni Trascorsi dalla Data di Ricevimento dell'Ordine alla Data di Effettivo Addebito", y="Frequenza")

ppppppp_1 <- pppppppp+theme(axis.text.x = element_text(angle = 360, hjust = 1))

ppppppp_1

##############Spesa Media Per Carta############################

sales_without_refounds_approved <- sales_without_refounds %>% filter(`Processor Response Text`=="Approved")

sales_without_refounds_approved <- sales_without_refounds_approved %>% mutate(`Settlement Amount`= ifelse(is.na(`Settlement Amount`),0, `Settlement Amount`))

sales_without_refounds_approved_credit_card_type_grouped_sum <- aggregate(sales_without_refounds_approved$`Settlement Amount`, by=list(sales_without_refounds_approved$`Card Type`), FUN=sum)

sales_without_refounds_approved_credit_card_type_grouped_mean <- aggregate(sales_without_refounds_approved$`Settlement Amount`, by=list(sales_without_refounds_approved$`Card Type`), FUN=mean)

sales_without_refounds_approved_credit_payment_instrument_grouped_sum <- aggregate(sales_without_refounds_approved$`Settlement Amount`, by=list(sales_without_refounds_approved$`Payment Instrument Type`), FUN=sum)

sales_without_refounds_approved_credit_payment_instrument_grouped_mean <- aggregate(sales_without_refounds_approved$`Settlement Amount`, by=list(sales_without_refounds_approved$`Payment Instrument Type`), FUN=mean)

sales_without_refounds_approved_credit_prepaid_grouped_sum <- aggregate(sales_without_refounds_approved$`Settlement Amount`, by=list(sales_without_refounds_approved$Prepaid), FUN=sum)

sales_without_refounds_approved_credit_prepaid_grouped_mean<- aggregate(sales_without_refounds_approved$`Settlement Amount`, by=list(sales_without_refounds_approved$Prepaid), FUN=mean)

#sales_without_refounds_approved_grouped_mean <- sales_without_refounds_approved %>% group_by(`Payment Instrument Type`, `Settlement Amount`) %>% summarize(mean=mean(`Settlement Amount`))

######GRAFICI SPESA MEDIA

spese<- ggplot(sales_without_refounds_approved_credit_card_type_grouped_sum, aes(x=sales_without_refounds_approved_credit_card_type_grouped_sum$Group.1 , y=sales_without_refounds_approved_credit_card_type_grouped_sum$x, color=x, fill=x, width = x/1000000))+geom_bar(stat = "identity")+geom_text(aes(label= x), vjust =-0.3)+theme_pubclean()+labs(title="Somma Spesa Totale per Carta", x="Carta", y="Somma")

spese_1 <- spese+theme(axis.text.x = element_text(angle = 360, hjust = 1))

spese_1

#mean

spese11<- ggplot(sales_without_refounds_approved_credit_card_type_grouped_mean, aes(x=sales_without_refounds_approved_credit_card_type_grouped_mean$Group.1 , y=sales_without_refounds_approved_credit_card_type_grouped_mean$x, color=x, fill=x, width = x/100))+geom_bar(stat = "identity")+geom_text(aes(label= x), vjust =-0.3)+theme_pubclean()+labs(title="Media Spesa Totale per Carta", x="Carta", y="Media")

spese_2 <- spese11+theme(axis.text.x = element_text(angle = 360, hjust = 1))

spese_2

#somma prepaid or not

spese22<- ggplot(sales_without_refounds_approved_credit_prepaid_grouped_sum, aes(x=sales_without_refounds_approved_credit_prepaid_grouped_sum$Group.1 , y=sales_without_refounds_approved_credit_prepaid_grouped_sum$x, color=x, fill=x, width = x/100000))+geom_bar(stat = "identity")+geom_text(aes(label= x), vjust =-0.3)+theme_pubclean()+labs(title="Somma Spesa Totale per Carta Prepagata o No", x="Carta", y="Somma")

spese_111 <- spese22+theme(axis.text.x = element_text(angle = 360, hjust = 1))

spese_111

#mean

spese33<- ggplot(sales_without_refounds_approved_credit_prepaid_grouped_mean, aes(x=sales_without_refounds_approved_credit_prepaid_grouped_mean$Group.1 , y=sales_without_refounds_approved_credit_prepaid_grouped_mean$x, color=x, fill=x, width = x/100))+geom_bar(stat = "identity")+geom_text(aes(label= x), vjust =-0.3)+theme_pubclean()+labs(title="Media Spesa Totale per Carta Prepagata o No", x="Carta", y="Somma")

spese_1111 <- spese33+theme(axis.text.x = element_text(angle = 360, hjust = 1))

spese_1111































#selecting_only_postepay_issuing_bank <- selecting_only_postepay_issuing_bank[selectin_postepay %in% selecting_only_postepay_issuing_bank$`Issuing Bank`, ]

#poste <- c("POSTEPAY S.P.A", 	"PostePay S.p.A.")

############################SALES###############################################







number_of_attempt_per_users <-as.data.frame(table(brain_tree_for_manipulation$`Customer Email`))

#######################################FRAUD####################################


my.summaries <- data.frame(customerID = unique(brain_tree_for_manipulation$`Customer Email`),
                           NumberofOrdersOfSpecificUser = sapply(unique(brain_tree_for_manipulation$`Customer Email`), 
                           function(customer) { length(unique(brain_tree_for_manipulation$`Created Datetime`[which(brain_tree_for_manipulation$`Customer Email` == customer)])) } ),
                           AverageValuePerOrder = tapply(tapply(brain_tree_for_manipulation$`Settlement Amount`, 
                           brain_tree_for_manipulation$`Order ID`, sum), brain_tree_for_manipulation$`Customer Email`[match(unique(brain_tree_for_manipulation$`Order ID`),
                           brain_tree_for_manipulation$`Order ID`)], mean))



names(my.summaries)[1]<- "Customer Email"


combinazione <- merge(x=my.summaries, y=brain_tree_for_manipulation, by=("Order ID"))


combinazione_con_status <- combinazione %>% select("Customer Email",
                                                   "NumberofOrdersOfSpecificUser",
                                                   "AverageValuePerOrder", 
                                                   "Transaction ID",
                                                   "Transaction Type",
                                                   "Transaction Status",
                                                   "Payment Instrument Type",
                                                   "Card Type" ,
                                                   "Processor Response Text",
                                                   "Gateway Rejection Reason",
                                                   "Fraud Detected",
                                                   "Country of Issuance"
                                                   ,"Issuing Bank", 
                                                   "Risk Decision")


fraud <- combinazione_con_status %>% mutate("Fraud Detected" =ifelse(is.na(`Fraud Detected`), 1,`Fraud Detected` ))

no_fraud <- fraud %>% filter(`Fraud Detected`== 1)


yes_fraud <- fraud %>% filter(`Fraud Detected`!= 1)


fraud_distinct <- fraud[!duplicated(fraud$`Customer Email`), ] 


fraud <- combinazione_con_status %>% mutate("Fraud Detected" =ifelse(is.na(`Fraud Detected`), 1,`Fraud Detected` ))

