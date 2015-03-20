# using merged from merging_datasets

library(vegan)
library(ggplot2)
library(plyr)
library(reshape)

#rarefying data
merged$LP<-as.factor(merged$LP)
merged$BLK<-as.factor(merged$BLK)
merged$Year<-as.factor(merged$Year)
merged_rar<-cbind(merged[,c(1:18)],rrarefy(merged[,-c(1:18)],6678))


# doing multivariate tests

# first for tests of peak biomass with rarefied data
data.2011<-subset(merged_rar, Year=="2011")
names(data.2011[,1:20])
str(data.2011[,1:20])
adonis(decostand(data.2011[,-c(1:18)],"total")~data.2011$Month*data.2011$LP*data.2011$CS, strata=data.2011$BLK, permutations=9999)

                                          # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.2011$Month                            2    0.2613 0.13063 0.64093 0.02884 0.8087   
# data.2011$LP                               2    1.1371 0.56856 2.78952 0.12552 0.0032 **
# data.2011$CS                               1    0.1429 0.14290 0.70111 0.01577 0.5862   
# data.2011$Month:data.2011$LP               4    0.3635 0.09087 0.44582 0.04012 0.9999   
# data.2011$Month:data.2011$CS               2    0.1731 0.08654 0.42461 0.01911 0.9990   
# data.2011$LP:data.2011$CS                  2    0.4803 0.24014 1.17819 0.05301 0.2099   
# data.2011$Month:data.2011$LP:data.2011$CS  4    0.3867 0.09668 0.47433 0.04269 0.9992   
# Residuals                                 30    6.1146 0.20382         0.67494          
# Total                                     47    9.0594                 1.00000          

adonis(decostand(data.2011[,-c(1:18)],"pa")~data.2011$Month*data.2011$LP*data.2011$CS,method="jaccard", strata=data.2011$BLK, permutations=9999)

                                          # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.2011$Month                            2    0.4655 0.23274 0.84769 0.03696 0.7934   
# data.2011$LP                               2    0.9881 0.49403 1.79940 0.07845 0.0022 **
# data.2011$CS                               1    0.2635 0.26354 0.95989 0.02092 0.3570   
# data.2011$Month:data.2011$LP               4    0.8501 0.21254 0.77412 0.06750 0.9990   
# data.2011$Month:data.2011$CS               2    0.4079 0.20394 0.74283 0.03239 0.9991   
# data.2011$LP:data.2011$CS                  2    0.5541 0.27704 1.00906 0.04399 0.3171   
# data.2011$Month:data.2011$LP:data.2011$CS  4    0.8291 0.20727 0.75493 0.06583 1.0000   
# Residuals                                 30    8.2366 0.27455         0.65396          
# Total                                     47   12.5948                 1.00000          

# now with unrarefied data
data.2011<-subset(merged, Year=="2011")
names(data.2011[,1:20])
str(data.2011[,1:20])
adonis(decostand(data.2011[,-c(1:18)],"total")~data.2011$Month*data.2011$LP*data.2011$CS, strata=data.2011$BLK, permutations=9999)

                                          # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.2011$Month                            2    0.2428 0.12142 0.62806 0.02839 0.7901   
# data.2011$LP                               2    1.1070 0.55351 2.86313 0.12941 0.0029 **
# data.2011$CS                               1    0.1354 0.13544 0.70060 0.01583 0.5562   
# data.2011$Month:data.2011$LP               4    0.3257 0.08144 0.42124 0.03808 0.9999   
# data.2011$Month:data.2011$CS               2    0.1477 0.07386 0.38203 0.01727 0.9996   
# data.2011$LP:data.2011$CS                  2    0.4522 0.22612 1.16964 0.05287 0.2150   
# data.2011$Month:data.2011$LP:data.2011$CS  4    0.3438 0.08595 0.44458 0.04019 0.9995   
# Residuals                                 30    5.7997 0.19332         0.67797          
# Total                                     47    8.5545                 1.00000          
          

adonis(decostand(data.2011[,-c(1:18)],"pa")~data.2011$Month*data.2011$LP*data.2011$CS,method="jaccard", strata=data.2011$BLK, permutations=9999)

                                          # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.2011$Month                            2    0.4155 0.20776 0.83601 0.03641 0.7966   
# data.2011$LP                               2    0.9209 0.46045 1.85282 0.08070 0.0020 **
# data.2011$CS                               1    0.2442 0.24420 0.98263 0.02140 0.3194   
# data.2011$Month:data.2011$LP               4    0.7684 0.19210 0.77301 0.06733 0.9953   
# data.2011$Month:data.2011$CS               2    0.3619 0.18095 0.72811 0.03171 0.9989   
# data.2011$LP:data.2011$CS                  2    0.5078 0.25388 1.02157 0.04449 0.2878   
# data.2011$Month:data.2011$LP:data.2011$CS  4    0.7377 0.18444 0.74215 0.06465 0.9999   
# Residuals                                 30    7.4554 0.24851         0.65331          
# Total                                     47   11.4119                 1.00000          

# note there were no differences in unrarefied or rarefied, might as well just present rarefied

# now differences between peak biomasses for rarefied
data.peak<-subset(merged_rar, Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012"))

adonis(decostand(data.peak[,-c(1:18)],"total")~data.peak$Year*data.peak$LP*data.peak$CS,strata=data.peak$BLK, permutations=9999)
                                         # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.peak$Year                            1    0.1438 0.14384 0.69448 0.02197 0.6191   
# data.peak$LP                              2    1.0507 0.52537 2.53651 0.16050 0.0092 **
# data.peak$CS                              1    0.1545 0.15451 0.74598 0.02360 0.5535   
# data.peak$Year:data.peak$LP               2    0.1936 0.09682 0.46743 0.02958 0.9884   
# data.peak$Year:data.peak$CS               1    0.1065 0.10654 0.51440 0.01627 0.8847   
# data.peak$LP:data.peak$CS                 2    0.3530 0.17651 0.85220 0.05392 0.5175   
# data.peak$Year:data.peak$LP:data.peak$CS  2    0.1949 0.09746 0.47053 0.02977 0.9879   
# Residuals                                21    4.3495 0.20712         0.66438          
# Total                                    32    6.5467                 1.00000          

adonis(decostand(data.peak[,-c(1:18)],"pa")~data.peak$Year*data.peak$LP*data.peak$CS,method="jaccard", strata=data.peak$BLK,permutations=9999)
                                         # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.peak$Year                            1    0.2496 0.24962 0.90767 0.02829 0.4928   
# data.peak$LP                              2    0.9545 0.47725 1.73534 0.10819 0.0081 **
# data.peak$CS                              1    0.2814 0.28136 1.02306 0.03189 0.3072   
# data.peak$Year:data.peak$LP               2    0.4253 0.21264 0.77319 0.04820 0.9532   
# data.peak$Year:data.peak$CS               1    0.2242 0.22418 0.81517 0.02541 0.7519   
# data.peak$LP:data.peak$CS                 2    0.5067 0.25336 0.92127 0.05744 0.5497   
# data.peak$Year:data.peak$LP:data.peak$CS  2    0.4053 0.20267 0.73696 0.04595 0.9920   
# Residuals                                21    5.7753 0.27502         0.65463          
# Total                                    32    8.8223                 1.00000          

# now differences between peak biomasses for nonrarefied
data.peak<-subset(merged, Experiment=="LB" & ((Year=="2011" & Month=="July") | Year=="2012"))

adonis(decostand(data.peak[,-c(1:18)],"total")~data.peak$Year*data.peak$LP*data.peak$CS,strata=data.peak$BLK, permutations=9999)
                                         # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
# data.peak$Year                            1    0.1358 0.13584 0.70195 0.02220 0.5780  
# data.peak$LP                              2    1.0247 0.51233 2.64743 0.16745 0.0125 *
# data.peak$CS                              1    0.1429 0.14293 0.73859 0.02336 0.5283  
# data.peak$Year:data.peak$LP               2    0.1670 0.08349 0.43144 0.02729 0.9873  
# data.peak$Year:data.peak$CS               1    0.0934 0.09343 0.48281 0.01527 0.8759  
# data.peak$LP:data.peak$CS                 2    0.3190 0.15952 0.82428 0.05214 0.5415  
# data.peak$Year:data.peak$LP:data.peak$CS  2    0.1724 0.08620 0.44546 0.02818 0.9786  
# Residuals                                21    4.0639 0.19352         0.66412         
# Total                                    32    6.1192                 1.00000         

adonis(decostand(data.peak[,-c(1:18)],"pa")~data.peak$Year*data.peak$LP*data.peak$CS,method="jaccard", strata=data.peak$BLK,permutations=9999)

                                         # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
# data.peak$Year                            1    0.2421 0.24209 1.00091 0.03105 0.3329   
# data.peak$LP                              2    0.8811 0.44055 1.82142 0.11302 0.0075 **
# data.peak$CS                              1    0.2420 0.24198 1.00045 0.03104 0.3363   
# data.peak$Year:data.peak$LP               2    0.3658 0.18288 0.75611 0.04692 0.9446   
# data.peak$Year:data.peak$CS               1    0.1935 0.19347 0.79990 0.02482 0.7457   
# data.peak$LP:data.peak$CS                 2    0.4408 0.22038 0.91115 0.05654 0.5601   
# data.peak$Year:data.peak$LP:data.peak$CS  2    0.3518 0.17589 0.72719 0.04512 0.9824   
# Residuals                                21    5.0793 0.24187         0.65151          
# Total                                    32    7.7962                 1.00000          

# also no differences here, present rarefied data

# now doing the same for rhizosphere data with rarefied

data.rhizo<-subset(merged_rar, Year=="2012" & LP != "3")
adonis(decostand(data.rhizo[,-c(1:18)],"total")~data.rhizo$Experiment*data.rhizo$LP*data.rhizo$CS,strata=data.rhizo$BLK, permutations=9999)
                                                 # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
# data.rhizo$Experiment                              1    0.3053 0.30527  1.3506 0.05504 0.2006  
# data.rhizo$LP                                      1    0.7857 0.78568  3.4760 0.14165 0.0118 *
# data.rhizo$CS                                      1    0.2528 0.25275  1.1182 0.04557 0.2817  
# data.rhizo$Experiment:data.rhizo$LP                1    0.1286 0.12863  0.5691 0.02319 0.8495  
# data.rhizo$Experiment:data.rhizo$CS                1    0.1240 0.12402  0.5487 0.02236 0.8713  
# data.rhizo$LP:data.rhizo$CS                        1    0.2406 0.24056  1.0643 0.04337 0.3207  
# data.rhizo$Experiment:data.rhizo$LP:data.rhizo$CS  1    0.0934 0.09338  0.4131 0.01684 0.9846  
# Residuals                                         16    3.6165 0.22603         0.65200         
# Total                                             23    5.5468                 1.00000         

adonis(decostand(data.rhizo[,-c(1:18)],"pa")~data.rhizo$Experiment*data.rhizo$LP*data.rhizo$CS,method="jaccard",strata=data.rhizo$BLK, permutations=9999)
                                                 # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
# data.rhizo$Experiment                              1    0.3120 0.31198 1.09402 0.04601 0.2569  
# data.rhizo$LP                                      1    0.6496 0.64958 2.27786 0.09580 0.0114 *
# data.rhizo$CS                                      1    0.3389 0.33894 1.18856 0.04999 0.1953  
# data.rhizo$Experiment:data.rhizo$LP                1    0.2119 0.21192 0.74312 0.03125 0.9030  
# data.rhizo$Experiment:data.rhizo$CS                1    0.2141 0.21406 0.75064 0.03157 0.8765  
# data.rhizo$LP:data.rhizo$CS                        1    0.2940 0.29397 1.03087 0.04336 0.3088  
# data.rhizo$Experiment:data.rhizo$LP:data.rhizo$CS  1    0.1972 0.19716 0.69138 0.02908 0.9763  
# Residuals                                         16    4.5627 0.28517         0.67293         
# Total                                             23    6.7803                 1.00000         

# now doing the same for rhizosphere data with unrarefied

data.rhizo<-subset(merged, Year=="2012" & LP != "3")
adonis(decostand(data.rhizo[,-c(1:18)],"total")~data.rhizo$Experiment*data.rhizo$LP*data.rhizo$CS,strata=data.rhizo$BLK, permutations=9999)
                                                  # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
# data.rhizo$Experiment                              1    0.3070 0.30699  1.4246 0.05780 0.1878  
# data.rhizo$LP                                      1    0.7791 0.77906  3.6151 0.14668 0.0130 *
# data.rhizo$CS                                      1    0.2466 0.24656  1.1441 0.04642 0.2736  
# data.rhizo$Experiment:data.rhizo$LP                1    0.1132 0.11318  0.5252 0.02131 0.8647  
# data.rhizo$Experiment:data.rhizo$CS                1    0.1090 0.10897  0.5056 0.02052 0.8867  
# data.rhizo$LP:data.rhizo$CS                        1    0.2280 0.22801  1.0581 0.04293 0.3112  
# data.rhizo$Experiment:data.rhizo$LP:data.rhizo$CS  1    0.0806 0.08063  0.3742 0.01518 0.9876  
# Residuals                                         16    3.4480 0.21550         0.64917         
# Total                                             23    5.3114                 1.00000 
adonis(decostand(data.rhizo[,-c(1:18)],"pa")~data.rhizo$Experiment*data.rhizo$LP*data.rhizo$CS,method="jaccard",strata=data.rhizo$BLK, permutations=9999)
                                                  # Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)  
# data.rhizo$Experiment                              1    0.3447 0.34468 1.31782 0.05504 0.1516  
# data.rhizo$LP                                      1    0.6093 0.60929 2.32949 0.09730 0.0126 *
# data.rhizo$CS                                      1    0.2993 0.29933 1.14442 0.04780 0.2316  
# data.rhizo$Experiment:data.rhizo$LP                1    0.1866 0.18658 0.71334 0.02980 0.9177  
# data.rhizo$Experiment:data.rhizo$CS                1    0.1929 0.19290 0.73751 0.03080 0.8709  
# data.rhizo$LP:data.rhizo$CS                        1    0.2677 0.26768 1.02344 0.04275 0.3244  
# data.rhizo$Experiment:data.rhizo$LP:data.rhizo$CS  1    0.1767 0.17672 0.67566 0.02822 0.9652  
# Residuals                                         16    4.1849 0.26155         0.66829         
# Total                                             23    6.2620                 1.00000     