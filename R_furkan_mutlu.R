library(tuber)  # YOUTUBE API ILE BAGLANTI KURMAK ICIN KULLANILDI
library(httpuv)  # YOUTUBE DAN VERI CEKMEK ICIN KULLANILDI
library(tidytext)  # METIN MADENCILIGI ICIN KULLANILAN KUTUPHANEDIR
library(dplyr)  # VERI MANIPILASYONU ICIN KULLANILAN KUTUPHANEDIR
library(tm)  # METIN MADENCILIGI ICIN GEREKLI OLAN KUTUPHANEDIR
library(stringr)  # KARAKTER YAPILI METINLER ICIN KULLANILIR
library(tibble)  #VERI CERCEVESI OLUSTURMAK ICIN KULLANILMISTIR
library(magrittr)   #  %>% KOMUTU ILE ZINCIRLEME YAPMAK ICIN KULLANILAN KUTUPHANEDIR
library(ggthemes)  # GRAFIK OLUSTURMAK ICIN KULLANILAN KUTUPHANEDIR
library(ggplot2)  # VERILERI GORSELLESTIRMEK ICIN KULLANILAN KUTUPHANEDIR
library(wordcloud2)  # KELIME BULUTU ICIN KULLANILAN KUTUPHANEDIR
library(RColorBrewer) # KELIME BULUTUNUN RENKLENDIRILMESI ICIN KULLANILAN PAKETTIR
library(sentimentr) # DUYGU ANALIZI ICIN KULLANILAN KUTUPHANEDIR
library(pander)  # VERI ANALIZI SONUCLARINI GOSTERMEK ICIN KULLANILAN KUTUPHANEDIR
library(pastecs) # TABLOLASTIRMA ICIN KULLANILAN KUTUPHANEDIR
library(stringi) # METIN VERILERI ILE CALISMAYI KOLAYLASTIRIR
library(tidyverse) # VERI MANIPULASYONU GORSELLESTIRME MODELLEME
library(janitor) # VERI TEMIZLEME VE DUZENLEME
library(emoji) # EMOJILER?? KULLANMA VE GORUNTULEME
library(readr)  # R DILINDE VERI OKUMA ISLEMLERINI KOLAY HALE GETIRIR
library(openxlsx) # XLSX OLARAK YAZDIRMAK ICIN KULLANILIR
library(ggtext) # GGPLOT2 I??IN METIN OGELERINI ZENGINLESTIRIR

# YOUTUBE DAN BULDUGUM VIDEONUN YORUMLARINI CEKTIM

app_id <- "1057915472699-i1c4u51400l2hgu34sggad1qa59gm7rv.apps.googleusercontent.com"
app_secret <- "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"
yt_oauth(app_id, app_secret, token ="")
get_all_comments(video_id = "zhEWqfP6V_w")    
comments1 <- get_all_comments(video_id ="zhEWqfP6V_w")
write.xlsx(comments1, file = "youtubecomments.xlsx")
verim <- read.xlsx("youtubecomments.xlsx")   # csv DOSYASI CAGIRMA


# ---------EMOJI ANALIZI----------

verim <- verim %>%
  mutate(X = row_number())

verim <- verim %>%       # verim setinin i??erisinde x ve textoriginal olarak ay??rd??m
  select(X, textOriginal)

verim$textOriginal <- tolower(verim$textOriginal) # k??????k harfe ??evirdim


emojiler <- stri_extract_all_regex(verim$textOriginal, "\\p{So}")  # verim setinin i??inden emojileri ??ektim
emoji_tablosu <- table(unlist(emojiler)) # ??ekilen emojileri tablo haline getirdim


emoji_setim <- data.frame(emoji = names(emoji_tablosu),
                       tekrar_sayisi = as.numeric(emoji_tablosu)) %>%   # ka?? tane emoji tekrar etti??ini yazd??rd??m
  # Tekrar say??s??na g??re azalan s??rada d??zenler
  arrange(desc(tekrar_sayisi)) %>%
  # En s??k tekrar eden ilk 100 emojiyi se??er
  slice_head(n = 100)


# burada hatal?? emoji sat??rlar??n?? temizledim

silinecek_satirlar <- c(2, 3, 5, 10, 12, 17, 18, 21, 22, 23, 30, 31, 33, 36, 37, 38, 50, 52, 53, 60, 64, 66, 68, 70, 72, 82, 86, 88, 96, 98)
emoji_setim <- emoji_setim[!(1:nrow(emoji_setim) %in% silinecek_satirlar), ]


# emojiler i??in kelime bulutu olu??turdum

wordcloud2(data = emoji_setim, size = 1.5, color = "random-light", backgroundColor = "white")


# bar ??emas?? olu??turmak i??in temizlenip 70e d????en listenin i??inden lk 20 tanesini ald??m

ilk_20_emoji <- emoji_setim %>% 
  slice_head(n = 20)


# emojilerin grafi??ini olu??turdum

ggplot(ilk_20_emoji, aes(x = reorder(emoji, tekrar_sayisi), y = tekrar_sayisi)) +
  geom_bar(stat = "identity", aes(fill = emoji), show.legend = FALSE) +
  coord_flip() +
  labs(title = "En S??k Kullan??lan Emojiler",
       x = "Emoji",
       y = "Tekrar Say??s??") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_markdown(size = 12)  
  )

# ----------KELIME ANALIZI-----------

# kelime olarak par??alad??m

verim <- verim %>%
  unnest_tokens(word, textOriginal)


# latin alfabesi haricindeki harfleri ????kard??m

verim$word <- as.character(verim$word)
verim <- verim[str_detect(verim$word, "^[A-Za-z0-9 ]"), ]


# temizlik kodlar??

verim<-verim %>% mutate(word=removePunctuation(word)) #METINDEN NOKTLAMA ISARETLERINI KALDIRDIM
verim<- verim %>% mutate(word=str_squish(word))     #SAYILARLA YAZILARIN BIRLESTIGI KELIMELERIKALDIRDIM
verim<-verim %>% mutate(word=removeNumbers(word))   #RAKAM VE SAYILARI KALDIRDIM
verim<-verim %>% filter(str_length(word)>3)


data("stop_words")
verim <- verim %>%
  anti_join(stop_words, by = c("word"))


# kelimelerin tekrar say??s??n?? olu??turdum

count_ettim <- verim%>%
  count(word, sort = TRUE)

# tekrar eden kelimelerin frekans grafi??ini olu??turdum

count_ettim%>%
  head(30)%>%
  ggplot(aes(reorder(word,n),n))+
  geom_col()+
  coord_flip() + theme_minimal()+
  labs(x="KELIMELER",
       y="FREKANS DEGERI",
       title="TEKRAR EDEN KELIME GRAFIGI")

# frekans de??erli olan kelimelerin kelime bulutunu olu??turdum

wordcloud2(count_ettim)


# count edilen kelimelerin pozitif ve negatif de??erlerini olu??turdum

poz_neg<-count_ettim%>% inner_join(get_sentiments("bing"),by="word")
poz_neg_ozet <- poz_neg %>%
  group_by(sentiment) %>%
  summarise(word_count = n(), .groups = "drop")

# grafiklerini olu??turdum

poz_neg %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  head(50) %>%
  ggplot(aes(x=reorder(word,n), y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Kelime",x = "yogunluk") +
  coord_flip() +
  theme_hc()+
  labs(caption = "Veri Kaynagi: Youtube'da FIFA kanalinda WORLD CUP videosundan analiz edilmistir.")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))


# duygu tonu analizi yapt??m

polarite<-sentiment(verim$word)

stat.desc(polarite$sentiment, basic=T) %>% pander()


polarite<-sentiment(verim$word)
tablo<-cbind(verim$word, polarite[,c(3,4)])

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Kelimelerin Frekansi") +
  theme_gray()+
  labs(caption = "Veri Kaynagi: Youtube'da FIFA kanalinda WORLD CUP videosundan analiz edilmistir.")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))


# # DEGERLERE GORE POZITIF, NEGATIF VE NOTR BASLIGI ATADIM
bar_grafik <- polarite$sentiment  
bar_grafik <- as.data.frame(bar_grafik)
bar_grafik$bar_grafik[bar_grafik$bar_grafik > 0] <- 'Pozitif'   
bar_grafik$bar_grafik[bar_grafik$bar_grafik == 0] <- 'Notr'      # DEGERLERE GORE POZITIF, NEGATIF VE NOTR BASLIGI ATADIM
bar_grafik$bar_grafik[bar_grafik$bar_grafik < 0] <- 'Negatif'
bar_grafik$bar_grafik <- as.factor(bar_grafik$bar_grafik)
degerler <- table(bar_grafik$bar_grafik)
barplot(degerler, main='Duygu Dagilimi', xlab='Kategori', ylab='Sayi', col=c("red","blue", "green" ))
