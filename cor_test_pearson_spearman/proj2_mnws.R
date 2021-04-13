library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(ggpubr)
library("mvtnorm")
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(patchwork)

set.seed(160230)

sigma <- matrix(c(1,0,0,1),2,2) #kowariancja = 0, wariancja = 1
mu <- c(0,1)
#bivariate normal data under H0
sim <- replicate(n=1000, rmvnorm(10, mean=mu, sigma = sigma))
sim
# Test the actual level under H0

result <- sapply(1:1000, function(i) 
  { 
  cor.test(sim[,1,i],sim[,2,i],method = c("pearson"))$p.value
  
  })

mc<-mean(result < 0.05)
mc

#Moc testu w zależności od  kowariancji

rho <- seq(0, .9, .1)
pwr <- sapply(rho, function(r) {
  sigma <- matrix(c(1,r,r,1),2,2) 
  mu <- c(0,0)
  
  sim <- replicate(n=1000, rmvnorm(10, mean=mu, sigma = sigma))
  

  result <- sapply(1:1000, function(i) { 
    cor.test(sim[,1,i],sim[,2,i],method = c("pearson"))$p.value})
  
  mean(result < 0.05)
})

plot(rho, pwr, type="l", xlab=expression(rho), ylab="Moc", main="Moc testu w zależności od rho")
#---------------------
# TEST ISTOTNOŚCI KORELACJI PEARSONA
#---------------------

#ilość symulacji
N <- 1000

#srednia
mus <- c(1,7,10,20,30,40)
sigmas <-c(1,30,50,90)
#dfs2 <- seq(15,50, by = 5)
#dfs <- c(dfs1, dfs2)
#dfs
#poziomy istotności
alphas <- c(0.02, 0.05, 0.1, 0.2)
#długość próby
observations <-c(500, 1000, 5000)


#expand.grid tworzy ramkę danych wszystkich kombinacji o nazwie ‘params’ z każdą możliwą trójką z tych wartości.

params <- expand.grid(mu = mus, sigma = sigmas, alpha = alphas, observation = observations)
set.seed(42)


percentage_corr <- sapply(1:nrow(params), 
                          function(i){
                          
                          mu <- params[i, 1]
                          alpha <- params[i, 3]
                          sigma <- params[i,2]
                          n <- params[i, 4]
                          p_vector <- sapply(rep(mu, N), function(x)
                            {
                            my_sample <- rnorm(n, mu, sigma)
                            my_sample2<- rnorm(n,mu, sigma)
                            cor.test(my_sample, my_sample2, method = c("pearson"))$p.value})
                          
                          
                          mean(p_vector < alpha)
                          
                          })

corr_df <- bind_cols(params, p_value = percentage_corr )
#mean(corr_df$p_value)

# mniejsze <- corr_df %>% filter (p_value < (alpha))
# odsetek <- count(mniejsze)/nrow(corr_df)
# odsetek

corr_df %>%
  ggplot(aes(x = mu, y = p_value, color = factor(observation))) +
  labs(title = "Test istotności korelacji") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  facet_wrap(~ alpha, nrow = 2)    

corr_df %>%
  ggplot(aes(x = sigma, y = p_value, color = factor(observation))) +
  labs(title = "Test istotności korelacji") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  facet_wrap(~ alpha, nrow = 2)   

 

value_wide <-
  corr_df %>%
  spread(observation, p_value)
View(value_wide)

value_wide %>%
  ggplot(aes(x = mu, y = `1000`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 1000 obserwacji") +
  geom_point()

value_wide %>%
  ggplot(aes(x = mu, y = `5000`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 5000 obserwacji") +
  geom_point()








#--- zmienia sie srednia, odchylenie stałe
pvector<- c()
mvector3<-c()
for(i in c(seq(from=1, to=100, by=2))){
  for(j in 1:1000){
    cor_test2<-cor.test(rnorm(1000, i,1), rnorm(1000, i,1),method = c("pearson"))
    pvector<-append(pvector, cor_test2$p.value)
  }
  mvector3<-append(mvector3, mean(pvector<0.05))
  pvector<-c()
}



plot1<-ggplot( data.frame("Srednia"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector3),aes(Srednia,Odsetek_odrzucen))+
  labs(title = "Odsetek odrzuceń dla 1000 symulacji w zależnosci od sredniej w teście Pearsona",
       x="Srednia",
       y = "Odsetek odrzuceń" )+geom_point() + geom_smooth(method = "loess")
plot1

box1<- ggplot(data.frame("Srednia"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector3),aes(x=Srednia, y=Odsetek_odrzucen)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Odsetek odrzuceń dla 1000 symulacji w zależnosci od średniej w teście Pearsona") +
  xlab("")
box1


#--- zmienia sie odchylenie, a srednia stała
pvector<- c()
mvector4<-c()
for(i in c(seq(from=1, to=100, by=2))){
  for(j in 1:1000){
    cor_test2<-cor.test(rnorm(1000, 1,i), rnorm(1000, 1,i),method = c("pearson"))
    pvector<-append(pvector, cor_test2$p.value)
  }
  mvector4<-append(mvector4, mean(pvector<0.05))
  pvector<-c()
}

boxplot.default(mvector4)

plot2<-ggplot( data.frame("Odchylenie"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector4),aes(Odchylenie,Odsetek_odrzucen))+
  labs(title = "Odsetek odrzuceń dla 1000 symulacji w zależnosci od odchylenia standardowego w teście Pearsona",
       x="Sigma",
       y = "Odsetek odrzuceń" )+geom_point() + geom_smooth(method = "loess")
plot2


 box2<- ggplot(data.frame("Odchylenie"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector4),aes(x=Odchylenie, y=Odsetek_odrzucen)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Odsetek odrzuceń dla 1000 symulacji w zależnosci od odchylenia standardowego w teście Pearsona") +
  xlab("")
box2

#---------------------
#TEST KORELACJI SPEARMANA
#---------------------


#ilość symulacji
N <- 100

#srednia
mus <- c(1,7,10,20,30,40)
sigmas <-c(1,30,50,90)
#poziomy istotności
alphas <- c(0.02, 0.05, 0.1, 0.2)
#długość próby
observations <-c(500, 1000, 5000)


#expand.grid tworzy ramkę danych wszystkich kombinacji o nazwie ‘params’ z każdą możliwą trójką z tych wartości.

params <- expand.grid(mu = mus, sigma = sigmas, alpha = alphas, observation = observations)
set.seed(42)


percentage_corr_spearman <- sapply(1:nrow(params), 
                          function(i){
                            
                            mu <- params[i, 1]
                            alpha <- params[i, 3]
                            sigma <- params[i,2]
                            n <- params[i, 4]
                            p_vector <- sapply(rep(mu, N), function(x)
                            {
                              my_sample <- rnorm(n, mu, sigma)
                              my_sample2<- rnorm(n,mu, sigma)
                              cor.test(my_sample, my_sample2, method = c("spearman"))$p.value})
                            
                            
                            mean(p_vector < alpha)
                            
                          })

spearman_corr_df <- bind_cols(params, p_value = percentage_corr_spearman )
#mean(corr_df$p_value)

# mniejsze <- corr_df %>% filter (p_value < (alpha))
# odsetek <- count(mniejsze)/nrow(corr_df)
# odsetek

spearman_corr_df %>%
  ggplot(aes(x = mu, y = p_value, color = factor(observation))) +
  labs(title = "Test istotności korelacji Spearmana") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point() +
  facet_wrap(~ alpha, nrow = 2)    

spearman_corr_df %>%
  ggplot(aes(x = sigma, y = p_value, color = factor(observation))) +
  labs(title = "Test istotności korelacji Spearmana") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  facet_wrap(~ alpha, nrow = 2)   



value_wide1 <-
  corr_df %>%
  spread(observation, p_value)
View(value_wide1)

value_wide1 %>%
  ggplot(aes(x = mu, y = `1000`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 1000 obserwacji") +
  geom_point()

value_wide1 %>%
  ggplot(aes(x = mu, y = `5000`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 5000 obserwacji") +
  geom_point()






#--- zmienia sie srednia, odchylenie stałe
pvector<- c()
mvector5<-c()
for(i in c(seq(from=1, to=100, by=2))){
  for(j in 1:1000){
    cor_test2<-cor.test(rnorm(1000, i,1), rnorm(1000, i,1),method = c("spearman"))
    pvector<-append(pvector, cor_test2$p.value)
  }
  mvector5<-append(mvector5, mean(pvector<0.05))
  pvector<-c()
}



plot3<-ggplot( data.frame("Srednia"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector5),aes(Srednia,Odsetek_odrzucen))+
  labs(title = "Odsetek odrzuceń dla 1000 symulacji w zależnosci od sredniej w teście Spearmana",
       x="Srednia",
       y = "Odsetek odrzuceń" )+geom_point() + geom_smooth(method = "loess")
plot3

box3<- ggplot(data.frame("Srednia"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector5),aes(x=Srednia, y=Odsetek_odrzucen)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Odsetek odrzuceń dla 1000 symulacji w zależnosci od średniej teście Spearmana") +
  xlab("")
box3


#--- zmienia sie odchylenie, a srednia stała
pvector<- c()
mvector6<-c()
for(i in c(seq(from=1, to=100, by=2))){
  for(j in 1:1000){
    cor_test2<-cor.test(rnorm(1000, 1,i), rnorm(1000, 1,i),method = c("spearman"))
    pvector<-append(pvector, cor_test2$p.value)
  }
  mvector6<-append(mvector6, mean(pvector<0.05))
  pvector<-c()
}

boxplot.default(mvector6)

plot4<-ggplot( data.frame("Odchylenie"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector6),aes(Odchylenie,Odsetek_odrzucen))+
  labs(title = "Odsetek odrzuceń dla 1000 symulacji w zależnosci od odchylenia standardowego w teście Spearmana",
       x="Sigma",
       y = "Odsetek odrzuceń" )+geom_point() + geom_smooth(method = "loess")
plot4


box4<- ggplot(data.frame("Odchylenie"=(seq(from=1, to=100, by=2)),"Odsetek_odrzucen"=mvector6),aes(x=Odchylenie, y=Odsetek_odrzucen)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Odsetek odrzuceń dla 1000 symulacji w zależnosci od odchylenia standardowego") +
  xlab("")
box4



#---------------------
# PORÓWNANIE PEARSONA I SPEARMANA
#---------------------

# GDY PEARSON JEST RANGOWY

# Parametry symulacji
PERMUTATIONS = 1:200
Ns = c(seq(from=6, to=20, by=2), 30, 50, 80)  # Wielkość sampli
rs = c(0, 0.5, 0.95)  # Współczynniki korelacji


D = expand.grid(set=PERMUTATIONS, r=rs, N=Ns) %>%  
  mutate(
    # Używamy parametrów do utworzenia nieskorelowanych danych pochodzących z rozkładu normalnego
    data = map2(N, r, function(N, r) MASS::mvrnorm(N, mu=c(0, 4), Sigma=cbind(c(1, r), c(r, 1)))),
    
    # Testy
    pearson_raw = map(data, ~cor.test(rank(.x[, 1]), rank(.x[, 2]), method='pearson')),
    spearman_raw = map(data, ~cor.test(.x[, 1], .x[, 2], method = 'spearman')),
    
    # Porządkujemy dane - rozbijamy testy na poszczególne informacje
    pearson = map(pearson_raw, broom::tidy),
    spearman = map(spearman_raw, broom::tidy),
  ) %>%
  
  # Wyjmujemy dane za pomocą unnest
  unnest(pearson, spearman, .sep='_') %>%
  select(-data, -pearson_raw, -spearman_raw)


head(D)
summary(D$pearson_estimate - D$spearman_estimate) #roznica miedzy wspolczynnikami


# GDY PEARSON NIERANGOWY

# Parametry symulacji
PERMUTATIONS = 1:200
Ns = c(seq(from=6, to=10, by=2), 30, 50, 80)  # Wielkość sampli
vars = c(1, 5, 10)  # Wektor wariancji


D1 = expand.grid(set=PERMUTATIONS, var=vars, N=Ns) %>% 
  mutate(
    # Używamy parametrów do utworzenia nieskorelowanych danych pochodzących z rozkładu normalnego
    data = map2(N, var, function(N, var) MASS::mvrnorm(N, mu=c(0, 4), Sigma=cbind(c(var, 0), c(0, var)))),
    
    # Testy
    pearson_raw = map(data, ~cor.test(.x[, 1], .x[, 2], method='pearson')),
    #pearson_raw = map(data, ~cor.test(rank(.x[, 1]), rank(.x[, 2]), method='pearson')),
    spearman_raw = map(data, ~cor.test(.x[, 1], .x[, 2], method = 'spearman')),
    
    # Porządkujemy dane - rozbijamy testy na poszczególne informacje
    pearson = map(pearson_raw, broom::tidy),
    spearman = map(spearman_raw, broom::tidy),
  ) %>%
  
  # Wyjmujemy dane za pomocą unnest
  unnest(pearson, spearman, .sep='_') %>%
  select(-data, -pearson_raw, -spearman_raw)



head(D1)
summary(D1$pearson_estimate - D1$spearman_estimate) #roznica miedzy wspolczynnikami

#***

#WYKRESY:

#***

# GDY PEARSON RANGOWY


# Zestawienie p-value
p_relative = ggplot(D, aes(x=spearman_p.value, y=pearson_p.value, color=N)) + 
  geom_line() + 
  geom_vline(xintercept=0.05, lty=2) +
  geom_hline(yintercept=0.05, lty=2) +
  
  labs(title='Zestawienie p-value testu Pearsona (z rangami) oraz Spearmana', x = 'Spearman p-value', y = 'Pearson p-value') + 
  #coord_cartesian(xlim=c(0, 0.10), ylim=c(0, 0.1)) + 
  theme_gray(13) 


p_relative 

# Różnica (błąd) między p-value Pearsona (z rangami) i Spearmana
p_error_all = ggplot(D, aes(x=spearman_p.value, y=pearson_p.value-spearman_p.value, color=factor(N))) + 
  geom_line() + 
  geom_vline(xintercept=0.05, lty=2) +
  
  labs(title='Różnica (błąd) między p-value Pearsona (z rangami) i Spearmana', x='Spearman p-value', y='Różnica') + 
  coord_cartesian(ylim=c(-0.025, 0.005)) + 
  theme_gray(13) 

p_error_all

# Też różnica p value, ale w okolicy p-value=0.05
p_error_zoom = ggplot(D, aes(x=spearman_p.value, y=pearson_p.value-spearman_p.value, color=factor(N))) + 
  geom_line() + 
  geom_vline(xintercept=0.05, lty=2) +
  
  labs(title='Błąd w okolicy p-value=0.05', x='Spearman p-value', y='Różnica') + 
  coord_cartesian(ylim=c(-0.02, 0.005), xlim=c(0, 0.10)) + 
  theme_gray(13)

p_error_zoom





#GDY PEARSON NIERANGOWY



# Zestawienie p-value
p_relative1 = ggplot(D1, aes(x=spearman_p.value, y=pearson_p.value, color=N)) + 
  #geom_point(alpha = 0.1) 
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +       
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE) +
  geom_point(shape = '.', col = 'white')+   
  #geom_vline(xintercept=0.05, lty=2) +
  #geom_hline(yintercept=0.05, lty=2) +
  
  labs(title='Zestawienie p-value testu Pearsona oraz Spearmana', x = 'Spearman p-value', y = 'Pearson p-value') + 
  #coord_cartesian(xlim=c(0, 0.10), ylim=c(0, 0.11)) + 
  theme_gray(13) 

p_relative1

# Różnica (błąd) między p-value Pearsona i Spearmana
p_error_all1 = ggplot(D1, aes(x=spearman_p.value, y=pearson_p.value-spearman_p.value, color=factor(N))) + 
  geom_point(alpha=0.2) +
  geom_vline(xintercept=0.05, lty=2) +
  
  labs(title='Różnica (błąd) między p-value Pearsona i Spearmana', x='Spearman p-value', y='Różnica') + 
  
  theme_gray(13) 

p_error_all1

# Też różnica p value, ale w okolicy p-value=0.05
p_error_zoom1 = ggplot(D1, aes(x=spearman_p.value, y=pearson_p.value-spearman_p.value, color=factor(N))) + 
  geom_point() + 
  geom_vline(xintercept=0.05, lty=2) +
  
  labs(title='Błąd w okolicy p-value=0.05', x='Spearman p-value', y='Różnica') + 
  coord_cartesian( xlim=c(0.02, 0.08)) + 
  theme_gray(13)

p_error_zoom1

