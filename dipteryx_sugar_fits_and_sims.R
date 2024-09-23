library(janitor)

#https://github.com/rmcelreath/rethinking/issues/174 
# install.packages(c("mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
# options(repos=c(getOption('repos'), rethinking='http://xcelab.net/R'))
# install.packages('rethinking',type='source')
library(rethinking)
library(RColorBrewer)
#round color pallete
round_pal <- brewer.pal(3,"Dark2") # color pallete for rounds

#load and clean data
df <- read.csv("SugarAug282024_treeinformation.csv")
df <- clean_names(df)
df <- df[complete.cases(df$x_sugar), ]#drop cases with no sugar, there are missing sample numbers
str(df)
dens(df$x_sugar)
range(df$x_sugar)
str(df$x_sugar)
df$sugar_prop = df$x_sugar/100 # convert to percent scale for beta regression
df$sugar_prop_logodds = logit(df$sugar_prop) # log odds scale

#table of summary stats per tree per seaseon
tree_seas_stats <- cbind(
  aggregate(sugar_prop_logodds ~ tree_rnd, data = df, FUN = mean) ,
  aggregate(sugar_prop_logodds ~ tree_rnd, data = df, FUN = sd)[2] ,  
  aggregate(round_num ~ tree_rnd, data = df, FUN = max)[2] 
  )
colnames(tree_seas_stats) <- c("tree_rnd" , "mean_lodds" , "sd_lodds" , "round_num")

tree_seas_stats$tree <- substr(tree_seas_stats$tree_rnd, start = 1, stop = 4)
tree_seas_stats$tree_index <- as.integer(as.factor(tree_seas_stats$tree))
tree_seas_stats$mean_sugar_prop <- logistic(tree_seas_stats$mean_lodds)
str(tree_seas_stats)



#plot per tree of mean sugar info on log odds scale
plot(mean_lodds~round_num , data=tree_seas_stats , col=round_pal[tree_seas_stats$round_num] , pch=1)
for(i in 1:max(tree_seas_stats$tree_index) ){
  points(mean_lodds~round_num , data=tree_seas_stats[tree_seas_stats$tree_index==i,] , col=1 , pch=19 , type='l')
}
points(mean_lodds~round_num , data=tree_seas_stats , col=round_pal[tree_seas_stats$round_num] , pch=19)

## plot per tree of mean sugar on real scale
plot(mean_sugar_prop ~round_num , data=tree_seas_stats , col=round_pal[tree_seas_stats$round_num] , pch=1)
for(i in 1:max(tree_seas_stats$tree_index) ){
  points(mean_sugar_prop ~round_num , data=tree_seas_stats[tree_seas_stats$tree_index==i,] , col=1 , pch=19 , type='l')
}
points(mean_sugar_prop ~round_num , data=tree_seas_stats , col=round_pal[tree_seas_stats$round_num] , pch=19)

##plot sd i dunno why
plot(sd_lodds~round_num , data=tree_seas_stats , col=round_pal[tree_seas_stats$round_num] , pch=1)
for(i in 1:max(tree_seas_stats$tree_index) ){
  points(sd_lodds~round_num , data=tree_seas_stats[tree_seas_stats$tree_index==i,] , col=1 , pch=19 , type='l')
}
#jitter <- seq(from=-.2, to=.2 , length=max(tree_seas_stats$tree_index) )
points(sd_lodds~round_num , data=tree_seas_stats , col=round_pal[tree_seas_stats$round_num] , pch=19)


#look at summary stats
tr_sug_mean_lodds_r1 <- mean(tree_seas_stats$mean_lodds[tree_seas_stats$round_num==1])
tr_sug_sd_lodds_r1 <- mean(tree_seas_stats$sd_lodds[tree_seas_stats$round_num==1])
tr_sug_mean_lodds_r2 <-mean(tree_seas_stats$mean_lodds[tree_seas_stats$round_num==2])
tr_sug_sd_lodds_r2 <- mean(tree_seas_stats$sd_lodds[tree_seas_stats$round_num==2])
tr_sug_mean_lodds_r3 <- mean(tree_seas_stats$mean_lodds[tree_seas_stats$round_num==3])
tr_sug_sd_lodds_r3 <- mean(tree_seas_stats$sd_lodds[tree_seas_stats$round_num==3])
# mean sd across seasons does not change much but highest in transition, mean sugar does climb

# initial conditions to vary
# tree_num  <- 100 # number of trees
# tree_sample_num <- 100 # number of samples per tree
# seasons_num <- 3 # number of sampling efforts-- fixed, but could be made fancier
# mean_lodds_vector <- c(tr_sug_mean_lodds_r1,tr_sug_mean_lodds_r2,tr_sug_mean_lodds_r3) #mean across rounds in Lodds
# sd_lodds_vector <- c(tr_sug_sd_lodds_r1,tr_sug_sd_lodds_r2,tr_sug_sd_lodds_r3) #sd across rounds in lodds
# sim_mean_lodds_sugar <- rep(NA , tree_num*tree_sample_num*seasons_num)

# for (i in 1:seasons_num){
#   for(j in 1:tree_num){
#     for(k in 1:tree_sample_num){
#       rnorm(tree_sample_num , mean=mean_lodds_vector[seasons_num] , sd=)
#     }
#   }
# }
# rnorm(tree_sample_num , mean=tree_seas_stats$mean_lodds[tr_sug_mean_lodds_r1] , sd=tree_seas_stats$sd_lodds[tr])

### FIT MODEL TO EMPIRICAL DATA################
#list for model
dlist <- list(
  sugar = log(df$x_sugar) ,
  tree_id = as.integer(as.factor(df$tree_num)),
  round_num = df$round_num,
  sugar_prop = df$sugar_prop
)

#plot densities of raw data
dens(df$sugar_prop[df$round_num==1], col=round_pal[1] , xlim=c(0,.2))
dens(df$sugar_prop[df$round_num==2],col=round_pal[2] , add=TRUE)
dens(df$sugar_prop[df$round_num==3] ,col=round_pal[3],add=TRUE)
legend("topright", c("r1","r2","r3") , fill = round_pal)

# dens(df$sugar_prop[df$round_num==1], col=round_pal[1] , xlim=c(0,.2))
# fabio <- which(df$round_num==1)
# for (i in fabio){
#   dens(df$sugar_prop[i] , add=TRUE)
# }

#sd per tree per rouns


# 
# m0 <- ulam(
#   alist(
#     sugar ~ dnorm( mu , sigma ) ,
#     mu <- a ,
#     a ~ dnorm( 5 , 1 ) ,
#     sigma ~ dexp(1)
#   ) , data=dlist , chains=4 , cmdstan = TRUE , cores=4 , iter=3000)
# 
# 
# m1 <- ulam(
#   alist(
#     sugar ~ dnorm( mu , sigma ) ,
#     mu <-  at[tree_id] ,
#     a_bar ~ dnorm( 5 , 1 ) ,
#     at[tree_id] ~ dnorm( a_bar , sigma_tree ),
#     c(sigma,sigma_tree) ~ dexp(1)
#   ) , data=dlist , chains=4 , cmdstan = TRUE , cores=4 , iter=3000)
# precis(m1)
# 
# precis(m1)
# plot(precis(m1 , pars="at" , depth=3))
# 
# 
# m2 <- ulam(
#   alist(
#     sugar ~ dnorm( mu , sigma ) ,
#     mu <-  at[tree_id] + ar[round_num],
#     a_bar ~ dnorm( 2 , 0.7 ) ,
#     at[tree_id] ~ dnorm( 0, sigma_tree ),
#     ar[round_num] ~ dnorm( a_bar, sigma_round ),
#     c(sigma,sigma_tree,sigma_round) ~ dexp(1)
#   ) , data=dlist , chains=4 , cmdstan = TRUE , cores=4 , iter=3000)
# precis(m2)


ms_beta <- ulam(
  alist(
    sugar_prop ~ dbeta2( p , theta) ,
    logit(p) <- a_bar + at[tree_id] + ar[round_num],
    a_bar ~ dnorm( -1.5,1) ,
    at[tree_id] ~ dnorm( 0, sigma_tree ),
    ar[round_num] ~ dnorm( 0, sigma_round ),
    c(sigma_tree,sigma_round) ~ dexp(1),
    theta ~ dexp(0.05)
  ) , data=dlist , chains=4 , #cmdstan = TRUE ,
  cores=4 , iter=1500)
precis(ms_beta)

###start with beta
#Visualize prior preds of mean sugar dist
xx <- rbeta2(n=6000, prob=logistic(rnorm(-1.5 , 1, n=6000)) , theta = rexp(6000, 0.05) )
dens(xx)
#picks some extreme values, but lots of less than .4
precis(ms_beta) # overall params
plot(precis(ms_beta , pars="at" , depth=3)) # offsets of mean tree
plot(precis(ms_beta , pars="ar" , depth=3)) # offsets of mean round
post <- extract.samples(ms_beta) #extract posterior samples

#simple simulation of mean sugar value distribution from model
sugar_sim_mean <- rbeta2(n=6000, prob=mean(logistic(post$a_bar)) , theta = mean(post$theta) )
sim_post_dist <- rbeta2(n=6000, prob=logistic(post$a_bar) , theta = post$theta ) # from whence one should simulate data, theta variation
dens(sugar_sim_mean)
dens(sim_post_dist, add= TRUE, col="blue") #better for describing pop variation, gets highs and lowa
points(dlist$sugar_prop , rep(0,846) , pch="|")

#plot of model preds
par(mfrow = c(7,6) , mar=c(2,2,0,0) )
dens(logistic(post$a_bar) , xlim=c(0,.25))
points(dlist$sugar_prop ,  rep(0,846) , pch="|" , col=round_pal[dlist$round])

for(i in 1:max(dlist$tree_id)) {
  dens(logistic(post$a_bar + post$at[,i]) , col="darkgreen" , xlim=c(0,.25))
  pandafart <- which(dlist$tree_id==i)
  points(dlist$sugar_prop[ pandafart] ,  rep(0,length(pandafart)) , pch="|" , col=round_pal[dlist$round[ pandafart]])
}

for(i in 1:3) {
  dens(logistic(post$a_bar + post$ar[,i]) , col=round_pal[i] , xlim=c(0,.25))
  pandafart <- which(dlist$round_num==i)
  points(dlist$sugar_prop[ pandafart] ,  rep(0,length(pandafart)) , pch="|" , col=round_pal[dlist$round[ pandafart]])
}

dev.off() # run to kill multipanel hell

#samples per tree from posterior
# n_samp_tree <- 100
# for (i in 1:max(35)){
#   for (j in 1:max(df$round_num)){
#     store <- rbeta2(n=n_samp_tree, prob=logistic(post$a_bar + post$at[,i] + post$ar[,j]) , theta = post$theta )
#     store2 <- cbind(store , rep(i,n_samp_tree) ,rep(j,n_samp_tree))
#    if(i==1 & j==1) store3 <- store2
#    if(i!=1 & j!=1) store3 <- rbind(store3,store2)
#   }
# }
# sim_tree_data <- as.data.frame(store3)
# colnames(sim_tree_data) <- c("sugar_prop" , "tree_id" , "round_num")
# str(sim_tree_data)
# c("sugar_prop" , "tree_id" , "round_num")
# sim_list <- as.list(
#   
# )
###########DATA SIMULATION############

# There are two sims, that should be written in a function
# cahnge the number of trees and number of samples per tree
# the rounds is fixed at 3, but draws from model mean, can be changed if needed
# i would tweak the variation in trees (tree_sigma_sim and tree_std), to make it harder.more robust, values are based on empirical estimates
#generalizable samples for data simulation
## init values to vary
n_samp_tree <- 20
n_trees <- 20
tree_sigma_sim <- mean(post$sigma_tree)
tree_std <- mean(tree_seas_stats$sd_lodds)

#round_sigma_sim <- mean(post$sigma_round)
#fixed things
post_length <- length(post$theta)

#blank data frame to populates
df_2_pop <- data.frame(sugar_prop=rep(0,3*n_samp_tree*n_trees) ,
                       tree_id=rep(0,3*n_samp_tree*n_trees) ,
                       round_num=rep(0,3*n_samp_tree*n_trees))
#init values and storeage
the_rows <- 1:n_samp_tree
# full bayesian data imputation
for (j in 1:max(df$round_num)){
  for (i in 1:n_trees){
      n_samp_tree_post <- sample.int(post_length , n_samp_tree , replace=FALSE) # assume a min of 2000 samples
      n_tree_post <- sample.int(post_length , n_trees , replace=FALSE) # assume a min of 2000 samples, get random integers
      fruit_offsets <- rnorm(n=n_samp_tree, mean=0 , sd=tree_std) # gets offsets from a fixed distribution of fruits
      tree_offsets <- rnorm(n_trees, mean=0 , sd=post$sigma_tree[n_tree_post] ) # per tree offsets, w/ variable sd informed by posterior
      store <- rbeta2(n=n_samp_tree, prob=logistic(post$a_bar[n_samp_tree_post] +
              tree_offsets[i] + post$ar[n_samp_tree_post,j] + fruit_offsets) ,
              theta = post$theta[n_samp_tree_post] )
      df_2_pop[the_rows,] <- cbind(store , rep(i,n_samp_tree) , rep(j,n_samp_tree))
      the_rows <- the_rows + n_samp_tree
  }
}

#####below is normaler, but less variation, more traditional data imputation

## initial values to vary  ###Kate starts here, above is more cautious 

n_samp_tree <- 20
n_trees <- 40
tree_sigma_sim <- mean(post$sigma_tree)
tree_std <- mean(tree_seas_stats$sd_lodds)
##fixed values
post_length <- length(post$theta)
tree_offsets <- rnorm(n_trees, mean=0 , sd=tree_sigma_sim )#var ef per tree
a_bar <- mean(post$a_bar)
round_means <- apply( post$ar , 2 , mean)
theta_sim <- mean(post$theta)

the_rows <- 1:n_samp_tree
tree_offsets <- rnorm(n_trees, mean=0 , sd=tree_sigma_sim ) # per tree offsets, w/ variable sd informed by posterior
for (j in 1:max(df$round_num)){
  for (i in 1:n_trees){
    fruit_offsets <- rnorm(n=n_samp_tree, mean=0 , sd=tree_std)
    store <- rbeta2(n=n_samp_tree, prob=logistic(mean(post$a_bar) +
                    tree_offsets[i] + mean(post$ar[,j]) + fruit_offsets) ,
                    theta = mean(post$theta) )
    df_2_pop[the_rows,] <- cbind(store , rep(i,n_samp_tree) , rep(j,n_samp_tree))
    the_rows <- the_rows + n_samp_tree
  }
}

#look at simulated data structre
str(df_2_pop)
#fit model to simulated data
ms_beta_sim <- ulam(
  alist(
    sugar_prop ~ dbeta2( p , theta) ,
    logit(p) <- a_bar + at[tree_id] + ar[round_num],
    a_bar ~ dnorm( -1.5,1) ,
    at[tree_id] ~ dnorm( 0, sigma_tree ),
    ar[round_num] ~ dnorm( 0, sigma_round ),
    c(sigma_tree,sigma_round) ~ dexp(1),
    theta ~ dexp(0.05)
  ) , data=df_2_pop , chains=4 , #cmdstan = TRUE ,
  cores=4 , iter=1500)





####Function to loop through this to change variation per tree 
#B suggests to start w empirical variation and go up 

varTreeList <- c(kjdkd, lksjdf)
varSeasonList <- c( )


#fit_params <- unlist(as.vector(precis(ms_beta_sim , depth=2)[1:3]), use.names = FALSE)
#####plot of per simulation validation
fit_params <- as.data.frame(precis(ms_beta_sim , depth=2)[1:4])
str(fit_params)
simulated_params <- c(a_bar,tree_offsets,round_means,round_sigma_sim, tree_sigma_sim , theta_sim)
col_vector_sims <- c("black" , rep("green", n_trees), round_pal, "darkblue","darkgreen","darkviolet")
plot(fit_params$mean , simulated_params , xlim=c(-3,3) , ylim=c(-3,3) , col=col_vector_sims , pch=19)
abline(a=0 , b=1, lty=3)
#hpdi segments
for(i in 1:nrow(fit_params)){
  segments(x0=fit_params[i,3] , x1=fit_params[i,4] , 
           y0=simulated_params[i], y1=simulated_params[i] , col=col_vector_sims[i]) 
}


### where to now
#1. simulate about 10- 20 times over a range of trees and samples per tree, and biologically realiztic conditions of variation
#2. save model outputs (PME and HPDI are fine) and simulation conditions IDs to a list
#3. devise come comparable metric (i.e. loss, distance of posterior means to true value, % of HPDIs including true value to inform sampling effort)
###start with beta
precis(ms_beta_sim) # overall params
plot(precis(ms_beta_sim , pars="at" , depth=3)) # offsets of mean tree
plot(precis(ms_beta_sim , pars="ar" , depth=3)) # offsets of mean round
post_sim <- extract.samples(ms_beta_sim)




###### Write a big loop or function to sample from 


#Sample from posteriors to simulate the data then fit models 






#######SIMULATION OF VISITS

#this can be made fancier but it is an initial pass, you pass throught the samples generated above to this model, and we can add variation in tree visits
### simulate visits to a patch based on sugar
p_visit_logit <- logit(.2) # intercept visitation rate at mean sugar
beta_s_sim <- 1 # slope effect

#simulate choice
n_samples <- 100 # number of samples overall
pr_vist <- rep(NA,100)
sim_uniform <- standardize(runif(n_samples, 
                                 min = min(dlist$sugar_prop), 
                                 max = max(dlist$sugar_prop) ) )
range(dlist$sugar_prop)
pr_visit <- logistic( p_visit_logit + beta_s_sim*sim_uniform)
prob_visit <- rbinom(n_samples,1,pr_visit)

plot(pr_visit~sim_uniform , col="darkgreen", pch=1 , ylim=c(0,1))
points(sim_uniform,prob_visit, pch="|")
sim_data <- 
  list(
    visit=prob_visit ,
    sugar_sim = sim_uniform 
    )

mprobvis_sim <- ulam(
  alist(
    visit ~ dbinom( 1,p) ,
    # logit(p) <- a_bar + at[tree_id] + ar[round_num],
    logit(p) <- a_bar + beta_s*sugar_sim,
    c(a_bar,beta_s) ~ dnorm( 0,1) 
    #at[tree_id] ~ dnorm( 0, sigma_tree ),
    #ar[round_num] ~ dnorm( 0, sigma_round ),
    #c(sigma_tree,sigma_round) ~ dexp(1),
  ) , data=sim_data , chains=4 , #cmdstan = TRUE ,
  cores=4 , iter=3000)

post_sim <- extract.samples(mprobvis_sim)

dens(post_sim$beta_s , xlim=c(-4,4) , col="red")
abline(v=beta_s_sim, col="red")
dens(post_sim$a_bar , xlim=c(-4,4) , col="blue")
abline(v=p_visit_logit, col="blue")
plot(post_sim$beta_s[1:100]~ rep(beta_s_sim,100) , col="red" , xlim=c(-2,2) , ylim=c(-2,2))
points(post_sim$a_bar[1:100]~ rep(p_visit_logit,100) , col="blue")
abline(a=0,b=1,lty=3)

#round_index <- 
## ok, now lets simulate variation across trees





