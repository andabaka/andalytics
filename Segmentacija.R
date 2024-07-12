# Andalytics, obrt za obradu podataka


# SEGMENTACIJA KUPACA PRIMJENOM K-MEANS KLASTERIRANJA

# Segmentacija kupaca je proces dijeljenja tržišta na manje, homogenije grupe kupaca sličnih 
# karakteristika. 
# K-means je popularna metoda klaster analize koja se koristi za segmentaciju podataka u skupine (klastere) temeljem 
# njihove sličnosti.



# 1. Preuzimanje podataka
# Preuzmite podatke sa linka i spremite ih u datoteku data unutar radnog direktorija
# https://www.kaggle.com/datasets/shwetabh123/mall-customers?resource=download


# 2. Instaliranje i učitavanje potrebnih paketa
# Funkcija install.packages() instalira pakete, a library() ih učitava u našu R sesiju. 
# tidyverse je kolekcija R paketa za obradu podataka, cluster sadrži funkcije za klasteriranje, 
# a factoextra nam pomaže u vizualizaciji rezultata klasteriranja.

install.packages("tidyverse")
install.packages("factoextra")
install.packages("cluster")

library(tidyverse)
library(cluster)
library(factoextra)



# 3. Učitavanje podataka
# read.csv() učitava naš CSV file. 
# Koristimo glimpse() za brzi pregled strukture podataka

mall_data <- read.csv("Mall_Customers-1.csv")

mall_data %>% glimpse()
mall_data

# 4. Priprema podataka
# Za našu analizu, fokusirat ćemo se na godišnji prihod i ocjenu potrošnje kupaca.
# Koristimo select() da izdvojimo relevantne varijable. Zatim koristimo scale() za standardizaciju 
# podataka. Standardizacija je važna jer osigurava da varijable s različitim rasponima vrijednosti 
# jednako doprinose analizi.

segmentation_data <- mall_data %>%
  select(Annual.Income..k.., Spending.Score..1.100.)

segmentation_data_scaled <- scale(segmentation_data)



# 4. Određivanje optimalnog broja klastera
# Prije samog klasteriranja, moramo odrediti optimalan broj klastera. 
# Za to koristimo metodu lakta (elbow method). Elbow metoda (ili metoda lakta) je tehnika koja se koristi 
# za određivanje optimalnog broja klastera (K) u klaster analizi. Cilj elbow metode je pronaći 
# točku gdje dodavanje dodatnog klastera više ne donosi značajno poboljšanje u 
# smanjenju ukupne sume kvadrata unutar klastera (WSS - within-cluster sum of squares).

# Izračunavamo wss za različite brojeve klastera pomoću funkcije
wss <- sapply(1:10, function(k){kmeans(segmentation_data_scaled, k, nstart=25)$tot.withinss})

# Prikaz elbow grafa
# Točka na grafu gdje se nagib krivulje značajno smanjuje (tzv. 'lakat') sugerira optimalan broj klastera
plot(1:10, wss, type="b", xlab="Broj klastera", ylab="Ukupna suma kvadrata unutar klastera")



# 5. Primjena k-means algoritma
# Sada ćemo provesti K-means klasteriranje s 5 klastera, što smo odredili metodom lakta.
# Funkcija set.seed() osigurava reproducibilnost naših rezultata.
# Funkcija kmeans() provodi K-means algoritam. Parametar centers određuje broj klastera, a nstart 
# definira broj različitih početnih centroida koje algoritam treba isprobati
set.seed(123)
kmeans_result <- kmeans(segmentation_data_scaled, centers = 5, nstart = 25)


# 6. Vizualizacija rezultata
# Ova funkcija stvara scatter plot naših klastera. Svaka točka predstavlja kupca, a boje označavaju 
# različite klastere. Elipse oko klastera pomažu u vizualnom razgraničenju grupa."

fviz_cluster(kmeans_result, data = segmentation_data_scaled,
             
             geom = "point",
             
             ellipse.type = "convex",
             
             palette = "jco",
             
             ggtheme = theme_minimal())



# 7. Analiza rezultata
# Dodajemo oznake klastera natrag u naš originalni dataframe i zatim računamo prosječne 
# vrijednosti za svaki klaster. Ovo nam daje uvid u karakteristike svakog segmenta kupaca

mall_data$cluster <- kmeans_result$cluster

cluster_summary <- mall_data %>%
  group_by(cluster) %>%
  summarise(
    avg_income = mean(Annual.Income..k..),
    avg_spending = mean(Spending.Score..1.100.),
    avg_age = mean(Age),
    count = n(),
    male_percentage = mean(Genre == "Male") * 100
  )

cluster_summary

# 8. Interpretacija klastera

# 1. Umjereni kupci srednjih primanja
#    - srednja primanja, umjerena potrošnja
#    - strategija: ponuditi mix proizvoda različitih cjenovnih rangova
# 2. Štedljivi kupci niskih primanja
#    - niska primanja, niska potrošnja
#    - strategija: ponuditi popuste i programe vjernosti za poticanje češće kupnje
# 3. Impulzivni kupci niskih primanja
#    - niska primanja, visoka potrošnja
#    - strategija: fokusirati se na privlačne, ali cjenovno pristupačne proizvode
# 4. Konzervativni kupci visokih primanja
#    - visoka primanja, niska potrošnja
#    - strategija: naglasiti kvalitetu i ekskluzivnost proizvoda za poticanje veće potrošnje
# 5. Luksuzni kupci visokih primanja
#    - visoka primanja, visoka potrošnja
#    - strategija: ponuditi premium proizvode i personalizirane usluge





