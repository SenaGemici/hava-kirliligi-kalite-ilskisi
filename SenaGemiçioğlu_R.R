
rm(list=ls())
install.packages("tidyverse")  # Veri manip??lasyonu ve g??rselle??tirme
install.packages("caret")      # Model olu??turma ve de??erlendirme
install.packages("car")
install.packages("nnet") # Multinomial lojistik regresyon i??in
library("nnet")
library("tidyverse")
library("caret")
library(readxl)
library("car")  # VIF hesaplamak i??in


veri<-read_excel("C:\\Users\\USER\\Desktop\\havakalitesiii.xlsx")
veri$hava_kalitesi <-as.factor(veri$hava_kalitesi)
head(veri)
dim(veri)
colnames(veri)
str(veri)
table(veri$hava_kalitesi)
bagimsiz_degiskenler <- colnames(veri[, -which(names(veri)=="hava_kalitesi")])
########
formul <- as.formula(paste("hava_kalitesi ~",paste(bagimsiz_degiskenler,collapse = "+")))
model<-multinom(formul,data=veri)                    
linear_model<-lm(nem+C+PM2.5+PM10+azotdioksit+kukurtdioksit+karbonmonoksit+enyakin_sanayiye_uzaklik+kmbasi_kisisayisi,data=veri)
#vif hesaplama
vif_values<-vif(model)
print(vif_values)
print(formul)
linear_model<-lm(,data=veri)
vif(model)
#########

cor_matrix <- cor(veri[,bagimsiz_degiskenler], method = "pearson")
print(cor_matrix)


# ????kartmak istedi??iniz de??i??kenlerin isimlerini belirleyin

degiskenler_to_remove <- c("PM2.5", "PM10", "karbonmonoksit", "enyakin_sanayiye_uzaklik")
#Bu de??i??kenleri veri setinden ????kar??n
veri_new <- veri[, !(names(veri) %in% degiskenler_to_remove)]

# Yeni veri setini kontrol edelim
head(veri_new)
new_bagimsiz_degiskenler <- colnames(veri_new[, -which(names(veri_new)=="hava_kalitesi")])
print(new_bagimsiz_degiskenler)

##yeni veri seti korelasyonu
cor_matrix2<-cor(veri_new[,new_bagimsiz_degiskenler], method = "pearson")
print(cor_matrix2)

## yeni model
formul <- as.formula(paste("hava_kalitesi ~",paste(new_bagimsiz_degiskenler,collapse = "+")))
model_new<-multinom(formul,data=veri_new)
print(model_new)


###likelihood ratio test
null_model<-multinom(hava_kalitesi ~ 1 ,data=veri_new)
summary(null_model)
null_model_deviance<-deviance(null_model)

# Verilen modelin residual deviance de??eri
tam_model_deviance <- 3979.519

# Null modelin residual deviance de??eri (bu de??eri kendi null modelinizden alman??z gerekir)
# Bu ??rnekte, null modelin residual deviance de??eri varsay??m olarak verilmektedir
null_model_deviance <- 12798.54  # ??rnek bir de??er, kendi null modelinizin residual deviance'??n?? kullanmal??s??n??z

# Likelihood ratio test istatisti??ini hesaplay??n
likelihood_ratio_statistic <- 2 * (null_model_deviance - tam_model_deviance)

# Serbestlik derecesi (ba????ms??z de??i??kenlerin say??s?? kadar)
# Modeldeki ba????ms??z de??i??ken say??s??n?? kontrol edin
df <- 5  # Ba????ms??z de??i??ken say??s?? (ba????ms??z de??i??kenler: C, nem, azotdioksit, kukurtdioksit, kmbasi_kisisayisi)

# p-de??erini hesaplay??n (chi-squared da????l??m??na g??re)
p_value <- 1 - pchisq(likelihood_ratio_statistic, df)

# Sonu??lar?? yazd??r??n
cat("Likelihood Ratio Test Statistic:", likelihood_ratio_statistic, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

# E??er p-de??eri 0.05'ten k??????kse, modelin anlaml?? oldu??u sonucuna var??l??r.
 
###McFadden???s R-squared


# Tam modelin log-likelihood de??eri
logLik_full_model <- logLik(model)  # Tam model ad??: full_model

# Null modelin log-likelihood de??eri
logLik_null_model <- logLik(null_model)  # Null model ad??: null_model

# McFadden's R-squared hesaplama
mcfadden_r2 <- 1 - (logLik_full_model / logLik_null_model)

# McFadden's R-squared de??erini yazd??rma
cat("McFadden's R-squared:", mcfadden_r2, "\n")


##modelin Tahmin g??c?? 


# Veri setini e??itim ve test verisi olarak ay??rma
set.seed(123)  # Sonu??lar??n tekrarlanabilir olmas?? i??in
trainIndex <- createDataPartition(veri_new$hava_kalitesi, p = 0.7, list = FALSE)  # %70 e??itim verisi
veri_train <- veri_new[trainIndex, ]  # E??itim verisi
veri_test <- veri_new[-trainIndex, ]   # Test verisi

# Test verisini kontrol etme
head(veri_test)
predicted_values <- predict(model_new, newdata = veri_test, type = "class")

# Test verisinin ger??ek de??erlerini al
true_values <- veri_test$hava_kalitesi

# Kar??????kl??k matrisini olu??turma
conf_matrix <- confusionMatrix(predicted_values, true_values)

# Sonu??lar?? yazd??rma
print(conf_matrix)



coef(model_new)

summary(model_new)

# Ba????ms??z de??i??kenlerin isimlerini tan??mlay??n


# Kolmogorov-Smirnov testi ba????ms??z de??i??kenler i??in
for (degisken in bagimsiz_degiskenler) {  # Ba????ms??z de??i??kenler ??zerinde d??n
  # Veri setinde de??i??ken olup olmad??????n?? kontrol et
  if (!(degisken %in% colnames(veri))) {  # De??i??ken veri setinde yoksa uyar?? ver
    cat("Hata: De??i??ken", degisken, "veri setinde bulunamad??.\n")
    next
  }
  
  # De??i??kenin say??sal olup olmad??????n?? kontrol et
  if (!is.numeric(veri[[degisken]])) {
    cat("Hata: De??i??ken", degisken, "say??sal bir de??i??ken de??il.\n")
    next
  }
  
  # Kolmogorov-Smirnov testi uygulama
  ks_test <- ks.test(veri[[degisken]], "pnorm", 
                     mean = mean(veri[[degisken]], na.rm = TRUE), 
                     sd = sd(veri[[degisken]], na.rm = TRUE))
  
  # Sonu??lar?? yazd??rma
  cat("De??i??ken:", degisken, "\n")
  cat("Kolmogorov-Smirnov Test ??statistigi:", ks_test$statistic, "\n")
  cat("p-degeri:", ks_test$p.value, "\n")
  
  if (ks_test$p.value > 0.05) {
    cat("Sonu??: Veri normal dag??l??ma uygundur (H0 reddedilmez).\n\n")
  } else {
    cat("Sonu??: Veri normal dag??l??ma uygun de??ildir (H0 reddedilir).\n\n")
  }
}

### art??k de??erlerin grafi??i 

artiklar <- residuals(model, type = "pearson")
summary(artiklar)
boxplot(artiklar, main = "Art??klar??n Boxplot'u")







