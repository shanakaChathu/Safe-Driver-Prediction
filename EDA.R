# general visualisation
library('ggplot2') # visualisation
library('ggplot')
library('scales') # visualisation
library('grid') # visualisation
library('ggthemes') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('rlang') # data manipulation

# specific visualisation
library('alluvial') # visualisation
library('ggfortify') # visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('VIM') # NAs
library('plotly') # interactive
library('ggforce') # visualisation

# modelling
library('xgboost') # modelling
library('caret') # modelling
library('MLmetrics') # gini metric

#Loading data 

train =as.tibble(fread('train.csv', na.strings=c("-1","-1.0")))
test =as.tibble(fread('test.csv', na.strings=c("-1","-1.0")))
sample_submit = as.tibble(fread('sample_submission.csv'))

glimpse(train)
glimpse(test)
attach(train)

#count no of missing values 
sum(is.na(train))
sum(is.na(test))

#Convert bin and cat var into factor format 

train = train %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
  mutate(target = as.factor(target))

test = test %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))


#Combine train and test data frames 

combine <- bind_rows(train %>% mutate(dset = "train"), 
                     test %>% mutate(dset = "test",
                                     target = NA))
combine <- combine %>% mutate(dset = factor(dset))

#Visualizing binary variables 
p1 <- train %>%
  ggplot(aes(ps_ind_06_bin, fill = ps_ind_06_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_ind_07_bin, fill = ps_ind_07_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_ind_08_bin, fill = ps_ind_08_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_ind_09_bin, fill = ps_ind_09_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_ind_10_bin, fill = ps_ind_10_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_ind_11_bin, fill = ps_ind_11_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train %>%
  ggplot(aes(ps_ind_12_bin, fill = ps_ind_12_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_ind_13_bin, fill = ps_ind_13_bin)) +
  geom_bar() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout=layout)


#visualizatiton of categorical variables 
p1 <- train %>%
  ggplot(aes(ps_ind_02_cat, fill = ps_ind_02_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_ind_04_cat, fill = ps_ind_04_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_ind_05_cat, fill = ps_ind_05_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_car_01_cat, fill = ps_car_01_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_car_02_cat, fill = ps_car_02_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_car_03_cat, fill = ps_car_03_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

layout = matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)

#Plottting intiger variables but with lower scale

p1 <- train %>%
  mutate(ps_ind_01 = as.factor(ps_ind_01)) %>%
  ggplot(aes(ps_ind_01, fill = ps_ind_01)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  mutate(ps_ind_03 = as.factor(ps_ind_03)) %>%
  ggplot(aes(ps_ind_03, fill = ps_ind_03)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  mutate(ps_ind_14 = as.factor(ps_ind_14)) %>%
  ggplot(aes(ps_ind_14, fill = ps_ind_14)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>%
  mutate(ps_ind_15 = as.factor(ps_ind_15)) %>%
  ggplot(aes(ps_ind_15, fill = ps_ind_15)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  mutate(ps_car_11 = as.factor(ps_car_11)) %>%
  ggplot(aes(ps_car_11, fill = ps_car_11)) +
  geom_bar() +
  theme(legend.position = "none")


layout <- matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)



#Other int variables 

p1 <- train %>%
  ggplot(aes(ps_calc_10, fill = ps_calc_10)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_calc_11, fill = ps_calc_11)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)


#Target variables 

train %>%
  group_by(target) %>%
  summarise(percentage = n()/nrow(train)*100)


#Missing values 

train %>%
  select(which(colMeans(is.na(.)) > 0)) %>%
  aggr(prop = FALSE, combined = TRUE, numbers = TRUE, bars = FALSE, cex.axis = 0.7)



# function to extract binomial confidence levels
get_binCI= function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))


#Error bars  bin variables

p1 <- train %>%
  group_by(ps_ind_06_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_06_bin, frac_claim, fill = ps_ind_06_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

#Error bard for integer variable 

p1 <- train %>%
  group_by(ps_ind_01, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
  ) %>%
  ggplot(aes(ps_ind_01, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_01", y = "Claims [%]")

p1


#corelation plot 


train %>%
  select(-starts_with("ps_calc"), -ps_ind_10_bin, -ps_ind_11_bin, -ps_car_10_cat, -id) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  mutate(target = as.integer(target)) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", tl.col = "black",  diag=FALSE)



#alluva 

allu_train <- train %>%
  filter(!is.na(ps_car_02_cat)) %>%
  filter(ps_car_04_cat %in% c("0","1","2","8","9")) %>%
  group_by(target, ps_ind_17_bin, ps_ind_18_bin, ps_car_02_cat, ps_car_04_cat) %>%
  count() %>%
  ungroup

alluvial(allu_train %>% select(-n),
         freq=allu_train$n, border=NA,
         col=ifelse(allu_train$target == 0, "red", "blue"),
         cex=0.75,
         hide = allu_train$n < 200,
         ordering = list(
           order(allu_train$target==1),
           NULL,
           NULL,
           NULL,
           NULL))