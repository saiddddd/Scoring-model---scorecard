setwd("C:/Users/DELL-POJ/Documents/Iykra/SaidML/PelatihanStatistik/pgd")

library(scorecard)
library(stringr)
library(ggpubr)
library(ROCit)
# Preprocessing
## Membaca data dan melihat sekilas isinya
data <- #membaca data di sini
  #mohon diperhatikan %training dan testing data, point dan pdo, dan penentuan threshold  
  
  colnames(data)
nrow(data)
ncol(data)
data1<-data

## membagi data training testing, 70%-30%
library(caret)
set.seed(NULL)
index_train <- createDataPartition(data1$status,
                                   p = 0.75,list = FALSE)

data.train <- data1[index_train,]
data.test <- data1[-index_train,]


## membuat binning dan woe
binning_bank  <- woebin(data.train, y = "status",
                        positive = "GOOD")

WOE.train <-  woebin_ply(data.train, binning_bank)

#menghitung bobot, menggunakan regresi logistik
model.train <- glm(status=="BAD" ~ ., 
                   family = binomial, 
                   data = WOE.train)

#menyusun scorecard
kartu_score <- scorecard(bins = binning_bank,
                         model.train, points0 = 500,
                         odds0 =1/20, pdo = 25,
                         basepoints_eq0 = TRUE)

## Menghitung dan Menampilkan Information Value
kartu_score_df <- do.call(rbind,kartu_score[-1])
kartu_score_fin <- kartu_score_df[,c("variable","bin","woe",
                                     "bin_iv","total_iv", "points")]
kartu_score_fin$bin <- str_replace_all(kartu_score_fin$bin,"\\[", "Dari ")
kartu_score_fin$bin <- str_replace_all(kartu_score_fin$bin,"\\,", " ke ")
kartu_score_fin$bin <- str_replace_all(kartu_score_fin$bin,"\\)",", ")
colnames(kartu_score_fin) <- c("Predictor", "Group", "WOE",
                               "Bin IV", "Total IV", "Points")

IV_data_plot <- data.frame(tapply(kartu_score_fin$`Total IV`,
                                  kartu_score_fin$Predictor,mean))
IV_data_plot$Predictor <- rownames(IV_data_plot)
rownames(IV_data_plot) <- NULL
colnames(IV_data_plot)[1] <- "IV"
IV_data_plot <- IV_data_plot[,c("Predictor","IV")]
urutan_predictor <- IV_data_plot$Predictor[order(IV_data_plot$IV)]

ggbarplot(IV_data_plot,x = "Predictor",y="IV",fill = "steelblue",
          orientation = "horiz",order = urutan_predictor)+
  geom_hline(yintercept =0.02,color="red",size=1.24)

# Menampilkan Scorecard
#View(kartu_score_fin)
kartu_score_fin[,c("Predictor", "Group", "Points")]



## Memberikan Skor pada Nasabah di Data Training
score.nasabah.train <- as.data.frame(scorecard_ply(data.train,kartu_score))
data.train.score <- cbind(data.train, score.nasabah.train)


# approved bad = rugi  20
# approved good = untung 7

threshold <- 440
approved <- ifelse(data.train.score$score > 440, 1, 0)
profit <- ifelse(approved==1 & data.train.score$status == "GOOD", 10,
                 ifelse(approved==1 & data.train.score$status == "BAD", -25,0))
totalprofit <- sum(profit)

#penentuan threshold
threshold <- 300:600 
for (i in seq_along(threshold)){
  approved <- ifelse(data.train.score$score > threshold[i], 1, 0)
  profit <- ifelse(approved==1 & data.train.score$status == "GOOD", 10,
                   ifelse(approved==1 & data.train.score$status == "BAD", -25,0))
  totalprofit[i] <- sum(profit)
}
plot(threshold, totalprofit, type="b")

#zooming
plot(threshold[threshold>420 & threshold<480], 
     totalprofit[threshold>420 & threshold<480], 
     type="b", col="coral")

