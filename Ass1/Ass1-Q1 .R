#Norm <- rnorm(200, mean= 0.5, sd = 0.25)
Norm <-runif(200, min=0, max = 1)
library(readxl)
Data <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/Data.xlsx", col_types = "numeric")
DataTransform=transform(Data, cat = ifelse(RNG < 0.1,"0.1",
                                                ifelse(RNG < 0.2,"0.2",
                                                       ifelse(RNG < 0.3,"0.3",
                                                              ifelse(RNG < 0.4,"0.4",
                                                                     ifelse(RNG < 0.5,"0.5",
                                                                            ifelse(RNG < 0.6,"0.6",
                                                                                   ifelse(RNG < 0.7,"0.7",
                                                                                          ifelse(RNG < 0.8,"0.8",
                                                                                                 ifelse(RNG < 0.9,"0.9",
                                                                                                        ifelse(RNG < 1,"1","Inf")))))))))))


Rdataframe = data.frame(Norm)
RdataTransform=transform(Rdataframe, cat = ifelse(Norm < 0, "0",
                                            ifelse(Norm < 0.1,"0.1",
                                                  ifelse(Norm < 0.2,"0.2",
                                                        ifelse(Norm < 0.3,"0.3",
                                                         ifelse(Norm < 0.4,"0.4",
                                                                ifelse(Norm < 0.5,"0.5",
                                                                       ifelse(Norm < 0.6,"0.6",
                                                                              ifelse(Norm < 0.7,"0.7",
                                                                                     ifelse(Norm < 0.8,"0.8",
                                                                                            ifelse(Norm < 0.9,"0.9",
                                                                                                   ifelse(Norm < 1,"1","Inf"))))))))))))

taula_freq_data=as.data.frame(with(DataTransform, table(cat)))
taula_freq_rdata=as.data.frame(with(RdataTransform, table(cat)))
#To remove when using rnorm as we can get values unde 0 and over 1
#taula_freq_rdata = taula_freq_rdata[-1,]
#taula_freq_rdata = taula_freq_rdata[-11,]
taula_freq = data.frame(taula_freq_data[,2], taula_freq_rdata[,2])
Test = chisq.test(taula_freq, correct = FALSE)


