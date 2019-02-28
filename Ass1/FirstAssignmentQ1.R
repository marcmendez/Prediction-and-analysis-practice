#generation of a normal distribution.
v1=runif(200, 0,1)
summary(v1)

#Work with the data as a dataframe.
taula_v1=data.frame(x1=v1)

#Definition of the intervals, categories to be used.
taula_v1_cat=transform(taula_v1, cat = ifelse(x1 < 0.1,"0.1",
                                              ifelse(x1 < 0.2,"0.2",
                                                     ifelse(x1 < 0.3,"0.3",
                                                            ifelse(x1 < 0.4,"0.4",
                                                                   ifelse(x1 < 0.5,"0.5",
                                                                          ifelse(x1 < 0.6,"0.6",
                                                                                 ifelse(x1 < 0.7,"0.7",
                                                                                        ifelse(x1 < 0.8,"0.8",
                                                                                               ifelse(x1 < 0.9,"0.9",
                                                                                                      ifelse(x1 <1,"1","INF"
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                        )
)

                                                     

#Counting the amount of elements in each category “table” function.
taula_freq_v1=as.data.frame(with(taula_v1_cat, table(cat)))

