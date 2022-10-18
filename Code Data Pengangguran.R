# Kita load terlebih dahulu packages yang dibutuhkan
library(tidyverse)
library(readr)
library(reshape2)
library(reshape)
library(data.table)
library(dplyr)
library(GGally)
library(corrplot)

# Kita load dataset yang ingin digunakan
Data_PN <- read_csv("https://raw.githubusercontent.com/Iqbalpr/Tugas-Kuliah--UIN/main/Pengangguran%20-%20Sheet1%20(1).csv")
View(Data_PN)

# Ukuran Data
nrow(Data_PN)
ncol(Data_PN)

##----Transformasi Data----##
# Transformasi Data menjadi bentuk Data Longitudinal
Data_PN2 <- melt(Data_PN, id = c("Negara", "Status"))
View(Data_PN2) # Data berbentuk dalam format LONG

# Mengubah nama Kolom Variable menjadi tahun dan value menjadi pengangguran
names(Data_PN2)
Nama_lama <- c("variable", "value")
nama_baru <- c("Tahun", "Pengangguran")
setnames(Data_PN2, old = Nama_lama, new = nama_baru)
names(Data_PN2)

##----Ringkasan Data----##
summary(Data_PN2)
str(Data_PN2)
# Setelah melihat tipe data pada kolom "Pengangguran(%)" masih kurang tepat, maka kita ubah terlebih dahulu
Data_PN2$Pengangguran <- as.numeric(Data_PN2$Pengangguran)
summary(Data_PN2) # Kita periksa kembali tipe data yang telah kita ubah
str(Data_PN2) # Kita periksa kembali tipe data yang telah kita ubah

##----Plot Profil----##

# Plot individu
p <- ggplot(Data_PN2) + geom_point(aes(x = Tahun, y = Pengangguran))
q <- p + geom_line(aes(x = Tahun, y = Pengangguran, group = Negara, color = Negara)) + facet_wrap(~ Status)
q

# Plot Mean
tabel <- Data_PN2 %>% group_by(Status, Tahun) %>%
  summarise(mean = mean(Pengangguran, na.rm = TRUE),
            var = var(Pengangguran, na.rm = TRUE))
view(tabel)

#Lanjutan Plot Mean (versi 1)
ggplot() + geom_point(data = Data_PN2, aes(x = Tahun, y = Pengangguran,
                                      group = Status, color = Status)) +
  geom_line(data = tabel, aes(x = Tahun, y = mean,
                               group = Status, color = Status))

#Lanjutan Plot Mean (versi 2)
ggplot() + geom_point(data = Data_PN2, aes(x = Tahun, y = Pengangguran,
                                          group = Status, color = Status)) +
  geom_line(data = tabel, aes(x = Tahun, y = mean,
                              group = Status, color = Status)) + 
  facet_wrap(~ Status)

# Plot Variansi
ggplot(tabel) + geom_point(aes(x = Tahun, y = var,
                                group = Status, color = Status)) +
  geom_line(aes(x = Tahun, y = var,
                group = Status, color = Status))

##----Matrix Korelasi dan Colleogram----##
Data_PN2.wide <- spread(data = Data_PN2, key = Tahun, value = Pengangguran)
View(Data_PN2.wide)

# Matreix Korelasi
cor(Data_PN2.wide[,3:14])
ggpairs(Data_PN2.wide[,3:14])

# Colleogram
corrplot(cor(Data_PN2.wide[,3:14]), method="pie")
corrplot(cor(Data_PN2.wide[,3:14]), method="number")
