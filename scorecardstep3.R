setwd("C:/Users/DELL-POJ/Documents/Iykra/SaidML/PelatihanStatistik/Pekan4")

library(scorecard)
library(stringr)
library(ggpubr)
library(ROCit)
# Preprocessing
## Membaca data dan melihat sekilas isinya
data <- read.csv("C:/Users/DELL-POJ/Documents/Iykra/SaidML/PelatihanStatistik/pgd/p4_latihan.csv",stringsAsFactors = TRUE)
colnames(data)
nrow(data)
ncol(data)
data1 <- data

## membagi data training testing, 70%-30%
library(caret)
set.seed(NULL)
index_train <- createDataPartition(data1$status,
                                   p = 0.7,list = FALSE)

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
                         odds0 =0.1, pdo = 20,
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

#menghitung acceptance rate dan bad rate 
threshold <- 400
acceptance_rate <- mean(data.train.score$score > threshold)
acceptance_rate
status.accepted <- data.train.score[which(data.train.score$score > threshold),"status"]
bad_rate <- ifelse(length(status.accepted) == 0, 0,
                   mean(status.accepted=="BAD"))
bad_rate

#menghitung acceptance rate dan bad rate untuk berbagai
#kemungkinan nilai threshold
acceptance_rate <- NULL
bad_rate <- NULL
threshold <- 300:600
for (i in seq_along(threshold)){
  acceptance_rate[i] <- mean(data.train.score$score > threshold[i])
  status.accepted <- data.train.score[which(data.train.score$score > threshold[i]),"status"]
  bad_rate[i] <- ifelse(length(status.accepted) == 0, 0,
                        mean(status.accepted=="BAD"))
}

hasil <- data.frame(threshold, acceptance_rate, bad_rate)
head(hasil, n=20)

plot(threshold, acceptance_rate, type="b")
points(threshold, bad_rate, type="b")


par(mar = c(5, 4, 4, 4) + 0.3)             
plot(threshold, acceptance_rate, type="b", col="coral")  
par(new = TRUE)                             
plot(threshold, bad_rate, type="b", col="green",              
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = pretty(range(bad_rate)))      
mtext("bad rate", side = 4, line = 3)             

#zooming
par(mar = c(5, 4, 4, 4) + 0.3)             
plot(threshold[threshold>420 & threshold<480], 
     acceptance_rate[threshold>420 & threshold<480], 
     type="b", col="coral")  
par(new = TRUE)                             
plot(threshold[threshold>420 & threshold<480], 
     bad_rate[threshold>420 & threshold<480], 
     type="b", col="green",              
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = pretty(range(bad_rate[threshold>420 & threshold<480])))      
mtext("bad rate", side = 4, line = 3)   


library(plotly)
plot_ly(data = hasil,x=~threshold,y=~acceptance_rate,type = "scatter",
        mode="markers",name = "acceptance rate")%>%
  add_markers(x=~threshold,y=~bad_rate,name="bad rate")%>%
  layout(yaxis=list(title="Rate"))






ggplotly(
  ggbarplot(IV_data_plot,x = "Predictor",y="IV",fill = "steelblue",
            orientation = "horiz",order = urutan_predictor)+
    geom_hline(yintercept =0.02,color="red",size=1.24)
)

















