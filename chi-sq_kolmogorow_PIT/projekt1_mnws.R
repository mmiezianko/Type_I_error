library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)

#---------------------
# TEST KOLMOGOROWA-SMIRNOWA
#---------------------

#ilość symulacji
N <- 2000

#stopnie swobody
dfs <- c(1, 2, 3, 5, 10, 15, 20, 25, 30, 40, 50, 60, 80, 100)
#dfs2 <- seq(15,50, by = 5)
#dfs <- c(dfs1, dfs2)
#dfs
#poziomy istotności
alphas <- c(0.02, 0.05, 0.1, 0.2)
#długość próby
observations <-c(5,100,1000)


#expand.grid tworzy ramkę danych wszystkich kombinacji o nazwie ‘params’ z każdą możliwą trójką z tych wartości.

params <- expand.grid(df = dfs, alpha = alphas, observation = observations)
set.seed(42)

percentage_ks <- sapply(1:nrow(params), 
    function(i){
    df <- params[i, 1]
    alpha <- params[i, 2]
    n <- params[i, 3]
    p_vector <- sapply(rep(df, N), function(x){
    my_sample <- rt(n, df)
    ks.test(my_sample, y="pt", df)$p.value
    })

    mean(p_vector < alpha)
    
    })

kolmogorow_df <- bind_cols(params, p_value = percentage_ks )
mean(kolmogorow_df$p_value)

kolmogorow_df %>%
  ggplot(aes(x = df, y = p_value, color = factor(observation))) +
  labs(title = "Test Kołmogorowa-Smirnowa") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  facet_wrap(~ alpha, nrow = 2)    


 
value_wide <-
  kolmogorow_df %>%
  spread(observation, p_value)
View(value_wide)

value_wide %>%
  ggplot(aes(x = df, y = `100`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 100 obserwacji") +
  geom_line()

value_wide %>%
  ggplot(aes(y = `1000`, x = df, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 1000 obserwacji") +
  geom_line()

#---------------------
# CHI KWADRAT
#---------------------

#PRZYPOMNIENIE:

#ilość symulacji
#N <- 1000
#stopnie swobody
#dfs <- seq(1, 10, by = 1)


# data frame z prawdopodobienstwami - (poczatek klasy, koniec klasy]

classes5 <- as.data.frame(matrix(data = NA, nrow = 5, ncol = 6))
colnames(classes5) = c("Poczatek klasy", "Koniec klasy", "F dla pocz. klasy", "F dla konca klasy", "Prawd.", "Liczebnosc")

classes5[,1] <- seq(0,0.8, by=0.2) #dzielimy na 5 klas
classes5[,2] <- seq(0.2,1, by=0.2) #dzielimy na 5 klas
classes5[,5] <- 0.2 #prawdopodobieństwo

df_chisq <- 40 # stopnie swobody
for (i in 1:nrow(classes5))
{
  classes5[i,3] <- qt(classes5[i,1],df_chisq)
  classes5[i,4] <- qt(classes5[i,2],df_chisq)
}

# przeszukanie i zliczenie liczebnosci generowanych liczb
chi_distr <- rt(100,4)

classes5[,6]<-table(cut(chi_distr,breaks=c(classes5[,3],Inf))) # liczebnosc generowanych liczba
#cut = break up a continuous variable such 
#table = uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.

temp_chisq <- chisq.test(classes5[,6], p = classes5[,5]) #pierwszy argument to liczebnosci, drygi to prawdopodobienstwo dla kazdej z klas
temp_chisq


#długość próby
observations1 <- c(50, 100,1000)


#expand.grid tworzy ramkę danych wszystkich kombinacji o nazwie ‘params’ z każdą możliwą trójką z tych wartości.

params1 <- expand.grid(df = dfs, alpha = alphas, observation = observations1)


  

percentage_chisq <- sapply(1:nrow(params1), 
                     function(i){
                     df1 <- params1[i, 1]
                     alpha1 <- params1[i, 2]
                     n1 <- params1[i, 3]
                     p_vector_chisq <- sapply(rep(df1, N), function(x){
                       pom <- rt(n1, df1)
                       
                       classes5[,6]<-table(cut(pom,breaks=c(classes5[,3],Inf)))
                       chisq.test(classes5[,6], p = classes5[,5])$p.value
                       
                     })
                     
                     mean(p_vector_chisq < alpha1)
                     
                   })

chisq_df <- bind_cols(params1, p_value = percentage_chisq)
mean(chisq_df$p_value)

chisq_df %>%
  ggplot(aes(x = df, y = p_value, color = factor(observation))) +
  labs(title = "Test Chi-kwadrat") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  facet_wrap(~ alpha, nrow = 2)   

value_wide_chisq <-
  chisq_df %>%
  spread(observation, p_value)
View(value_wide)


value_wide_chisq %>%
  ggplot(aes(x = df, y = `100`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 100obserwacji") +
  geom_line()

value_wide_chisq %>%
  ggplot(aes(x = df,y = `1000`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 1000 obserwacji") +
  geom_line()
  
#---------------------
# CHI KWADRAT 2
#---------------------



x=0
chisq_degree_frdm<-seq(1,20,1)
observ<-seq(100,120,4)
params<-expand.grid(df=chisq_degree_frdm,length=observ)
prob<-seq(0.1,1,0.1)
min=0

classes<-data.frame(seq(1,120,1))
pr=rep(0.1,10)

tmp<-0
error<-0
counter<-1
pvalue<-0
repeat{
  for(i in 1:nrow(params)){
    df<-params[i,1]
    n<-params[i,2]
    dane<-rt(n,df)
    max<-qt(prob,df)
    min[1]<- -Inf 
    for(j in 1:9){
      min[j+1]<-max[j]
    }
    result<-data.frame(min,max,prob)
    for(z in 1:10){
      for(k in 1:100){
        if(dane[k]<result$max[z]&&dane[k]>result$min[z]){
          x=x+1
        }
        
      }
      tmp[z]<-x
      x=0
      
    }
    result$observ<-tmp
    
    pvalue[i]<-chisq.test(result$observ,p=pr)$p.value
    
    
  }
  classes[,counter]<-pvalue
  counter=counter+1
  if(counter==1000)
    break
}


for(i in 1:120){
  error[i]<-mean(as.numeric(classes[i,]<0.05))
}

chi_df<-bind_cols(params, error_1 =error )


chi_df %>%
  ggplot(aes(x = df, y = error_1, color = factor(length))) +
  geom_line()


v=0
for(i in 1:120){
  for (z in 1:999) {
    if(as.numeric(classes[i,z])<0.05)
      v=v+1
  }
}
stats<-v/(120*999)

#---------------------
# PIT
#---------------------


###exercise
x_pit <- seq(- 10, 10, by = 0.01)   
pit_rt <- rt(x_pit, df=3)
pit_pt <- pt(pit_rt, df = 3)
F_y <- cumsum(table(pit_pt))/sum(table(pit_pt))
par(mfrow=c(1,3))
hist(pit_rt, col='blue')
plot(F_y, col='green', pch=19, xlab='y', ylab=expression('F'['Y']), main=expression(paste('Y=1-e'^ paste('-',lambda,'X'))))
plot(ecdf(pit_pt), col='red', pch=19, xlab='y', ylab='ECDF(y)')
###exercise


dfss <- seq(1, 10, by = 1)
#dfs2 <- seq(15,50, by = 5)
#dfs <- c(dfs1, dfs2)
#dfs
#poziomy istotności
alphass <- c(0.02, 0.05, 0.1, 0.2)
#długość próby
observationss <- c(5,20,100)#seq(10, 1010, by = 50)


#expand.grid tworzy ramkę danych wszystkich kombinacji o nazwie ‘params’ z każdą możliwą trójką z tych wartości.

params_pit <- expand.grid(df = dfss, alpha = alphass, observation = observationss)

percentage_pit <- sapply(1:nrow(params_pit), 
                         function(i){
                           df2 <- params_pit[i, 1]
                           alpha2 <- params_pit[i, 2]
                           n2 <- params_pit[i, 3]
                           p_vector <- sapply(rep(df2, N), function(x){
                             my_sample_pit <- rt(n2, df2)
                             pt_pit<-pt(my_sample_pit,df2)
                             ks.test(pt_pit, y="punif", min = 0, max = 1)$p.value
                             
                           })
                           
                           mean(p_vector<alpha2)
                           #hist(p_vector)
                         })

pit0 <- bind_cols(params_pit, p_value = percentage_pit)
mean(pit0$p_value)

pit0 %>%
  ggplot(aes(x = df, y = p_value, color = factor(observation))) +
  labs(title = "PIT") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line() +
  facet_wrap(~ alpha, nrow = 2) 


value_wide_pit <-
  pit0  %>%
  spread(observation, p_value)
View(value_wide)


value_wide_pit %>%
  ggplot(aes(x = df, y = `20`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 20 obserwacji") +
  geom_line()

value_wide_pit %>%
  ggplot(aes(x = df,y = `100`, col = factor(alpha))) +
  labs(title = "Poziomy alpha", subtitle = "dla 100 obserwacji") +
  geom_line()








