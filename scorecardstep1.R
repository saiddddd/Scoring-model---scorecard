setwd("C:/Users/DELL-POJ/Documents/Iykra/SaidML/PelatihanStatistik/pgd")
library(scorecard)
library(stringr)
library(ggpubr)
library(ROCit)
# Preprocessing
## Membaca data dan melihat sekilas isinya
#perhatikan % data training dan testing, padanan point dan pdo, dan variabel2 yang digunakan 
data <- #pembacaan data ada di sini
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



#EVALUASI pada data TRAINING

## Memberikan Skor pada Nasabah di Data Training
score.nasabah.train <- as.data.frame(scorecard_ply(data.train,kartu_score))

## Plot distribusi Scorecard Data Training
data.train.score <- cbind(data.train, score.nasabah.train)
ggdensity(data.train.score,x = "score",fill = "status")

## KS Statistic Data Training
KS.train <- ks.test(x=data.train.score$score[data.train.score$status=="BAD"],
                    y=data.train.score$score[data.train.score$status=="GOOD"],
                    alternative = "two.sided"
)
KS.train <- KS.train$statistic

## Akurasi Data Training
prediksi.train <- 1-predict(model.train, newdata = WOE.train,
                            type = "response")
kelas.train <- as.factor(ifelse(prediksi.train > 0.5,
                                "GOOD","BAD"))
conf.train <- confusionMatrix(WOE.train$status, kelas.train, positive="GOOD")
akurasi.train <- conf.train$overall[1]
akurasi.train

# ROC dan AUC Data Training
ROC.train <- rocit(score=prediksi.train, class=WOE.train$status)
plot(ROC.train)
AUC.train <- ROC.train$AUC
AUC.train


# Evaluasi Model berdasarkan data testing
# mengkonversi data test ke dalam bentuk WoE
WOE.test <-  woebin_ply(data.test, binning_bank)
prediksi.test <- 1-predict(model.train, newdata = WOE.test,
                           type = "response")

# Evaluasi prediksi pada data test
kelas.test <- as.factor(ifelse(prediksi.test > 0.5,
                               "GOOD","BAD"))
conf.test <- confusionMatrix(WOE.test$status, 
                             kelas.test, positive="GOOD")
akurasi.test <- conf.test$overall[1]
akurasi.test

ROC.test <- rocit(score=prediksi.test, class=WOE.test$status)
plot(ROC.test)
AUC.test <- ROC.test$AUC
AUC.test

## KS Statistic Data Training
score.nasabah.test <- as.data.frame(scorecard_ply(data.test,kartu_score))

data.test.score <- cbind(data.test, score.nasabah.test)

KS.test <- ks.test(x=data.test.score$score[data.test.score$status=="BAD"],
                   y=data.test.score$score[data.test.score$status=="GOOD"],
                   alternative = "two.sided"
)
KS.test <- KS.test$statistic


#rekap
hasil.training <- c(akurasi.train, AUC.train, KS.train)
hasil.testing <- c(akurasi.test, AUC.test, KS.test)
hasil <- rbind(hasil.training, hasil.testing)
colnames(hasil) <- c("Akurasi", "AUC", "KS")
rownames(hasil) <- c("Data Training", "Data Testing")
hasil


