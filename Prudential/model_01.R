# 26-11-2015
# Modeling prudential data
library(Hmisc)

setwd("D:/D Drive/247NETWORKBACKUP/BackupOn21May/Kaggle/Prudential")
jdata <- read.csv(file="train.csv", header=TRUE, stringsAsFactors=FALSE)

# [1] "Id"                  "Product_Info_1"      "Product_Info_2"     
# [4] "Product_Info_3"      "Product_Info_4"      "Product_Info_5"     
# [7] "Product_Info_6"      "Product_Info_7"      "Ins_Age"            
# [10] "Ht"                  "Wt"                  "BMI"                
# [13] "Employment_Info_1"   "Employment_Info_2"   "Employment_Info_3"  
# [16] "Employment_Info_4"   "Employment_Info_5"   "Employment_Info_6"  
# [19] "InsuredInfo_1"       "InsuredInfo_2"       "InsuredInfo_3"      
# [22] "InsuredInfo_4"       "InsuredInfo_5"       "InsuredInfo_6"      
# [25] "InsuredInfo_7"       "Insurance_History_1" "Insurance_History_2"
# [28] "Insurance_History_3" "Insurance_History_4" "Insurance_History_5"
# [31] "Insurance_History_7" "Insurance_History_8" "Insurance_History_9"
# [34] "Family_Hist_1"       "Family_Hist_2"       "Family_Hist_3"      
# [37] "Family_Hist_4"       "Family_Hist_5"       "Medical_History_1"  
# [40] "Medical_History_2"   "Medical_History_3"   "Medical_History_4"  
# [43] "Medical_History_5"   "Medical_History_6"   "Medical_History_7"  
# [46] "Medical_History_8"   "Medical_History_9"   "Medical_History_10" 
# [49] "Medical_History_11"  "Medical_History_12"  "Medical_History_13" 
# [52] "Medical_History_14"  "Medical_History_15"  "Medical_History_16" 
# [55] "Medical_History_17"  "Medical_History_18"  "Medical_History_19" 
# [58] "Medical_History_20"  "Medical_History_21"  "Medical_History_22" 
# [61] "Medical_History_23"  "Medical_History_24"  "Medical_History_25" 
# [64] "Medical_History_26"  "Medical_History_27"  "Medical_History_28" 
# [67] "Medical_History_29"  "Medical_History_30"  "Medical_History_31" 
# [70] "Medical_History_32"  "Medical_History_33"  "Medical_History_34" 
# [73] "Medical_History_35"  "Medical_History_36"  "Medical_History_37" 
# [76] "Medical_History_38"  "Medical_History_39"  "Medical_History_40" 
# [79] "Medical_History_41"  "Medical_Keyword_1"   "Medical_Keyword_2"  
# [82] "Medical_Keyword_3"   "Medical_Keyword_4"   "Medical_Keyword_5"  
# [85] "Medical_Keyword_6"   "Medical_Keyword_7"   "Medical_Keyword_8"  
# [88] "Medical_Keyword_9"   "Medical_Keyword_10"  "Medical_Keyword_11" 
# [91] "Medical_Keyword_12"  "Medical_Keyword_13"  "Medical_Keyword_14" 
# [94] "Medical_Keyword_15"  "Medical_Keyword_16"  "Medical_Keyword_17" 
# [97] "Medical_Keyword_18"  "Medical_Keyword_19"  "Medical_Keyword_20" 
# [100] "Medical_Keyword_21"  "Medical_Keyword_22"  "Medical_Keyword_23" 
# [103] "Medical_Keyword_24"  "Medical_Keyword_25"  "Medical_Keyword_26" 
# [106] "Medical_Keyword_27"  "Medical_Keyword_28"  "Medical_Keyword_29" 
# [109] "Medical_Keyword_30"  "Medical_Keyword_31"  "Medical_Keyword_32" 
# [112] "Medical_Keyword_33"  "Medical_Keyword_34"  "Medical_Keyword_35" 
# [115] "Medical_Keyword_36"  "Medical_Keyword_37"  "Medical_Keyword_38" 
# [118] "Medical_Keyword_39"  "Medical_Keyword_40"  "Medical_Keyword_41" 
# [121] "Medical_Keyword_42"  "Medical_Keyword_43"  "Medical_Keyword_44" 
# [124] "Medical_Keyword_45"  "Medical_Keyword_46"  "Medical_Keyword_47" 
# [127] "Medical_Keyword_48"  "Response"

categorical <- c("Product_Info_1", "Product_Info_2", "Product_Info_3", 
                 "Product_Info_5", "Product_Info_6", "Product_Info_7", 
                 "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", 
                 "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", 
                 "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", 
                 "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", 
                 "Insurance_History_3", "Insurance_History_4", 
                 "Insurance_History_7", "Insurance_History_8", 
                 "Insurance_History_9", "Family_Hist_1", "Medical_History_2", 
                 "Medical_History_3", "Medical_History_4", "Medical_History_5", 
                 "Medical_History_6", "Medical_History_7", "Medical_History_8", 
                 "Medical_History_9", "Medical_History_10", "Medical_History_11", 
                 "Medical_History_12", "Medical_History_13", "Medical_History_14", 
                 "Medical_History_16", "Medical_History_17", "Medical_History_18", 
                 "Medical_History_19", "Medical_History_20", "Medical_History_21", 
                 "Medical_History_22", "Medical_History_23", "Medical_History_25", 
                 "Medical_History_26", "Medical_History_27", "Medical_History_28", 
                 "Medical_History_29", "Medical_History_30", "Medical_History_31", 
                 "Medical_History_33", "Medical_History_34", "Medical_History_35", 
                 "Medical_History_36", "Medical_History_37", "Medical_History_38", 
                 "Medical_History_39", "Medical_History_40", "Medical_History_41")

discrete <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", 
              "Medical_History_32")

continuous <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", 
                "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", 
                "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", 
                "Family_Hist_4", "Family_Hist_5")

remove_variables <- c("Id", paste0("Medical_Keyword_", 1:48))

for (v in categorical){
  jdata[[v]] <- as.factor(jdata[[v]])
}

for (v in discrete){
  jdata[[v]] <- as.factor(jdata[[v]])
}

# Convert numerical to categorical
# chisq.test(table(cut2(jdata$Product_Info_4, g = 8), jdata$Response))
jdata$Product_Info_4 <- cut2(jdata$Product_Info_4, g = 10)


