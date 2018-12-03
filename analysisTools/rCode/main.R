
tz <- "Asia/Taipei"
Sys.setenv(TZ = tz)
source("global.R")
source(file.path("rCode", "functions.R"))

# Generate data -----
set.seed(1)
A <- paste0("a", 1:36)
B <- paste("b", rep(1:2, each= 18), sep= "")
C <- rep(paste("c", rep(1:3, each= 6), sep= ""), 2)

X11 <- rnorm(36, 100, 3)
X12 <- 30.3 + 3.71 * X11 + rnorm(36, 0, 5)
X11[c(3, 8, 19)] <- NA
X11[5] <- X11[5] /10; X11[29] <- 9999
X12[21] <- NA; X12[3] <- 9999; X12[9] <- -408.7
# two sample, subgroup B
X2L <- c(rnorm(18, 26.7, 1.2), rnorm(18, 25.9, 1.1))
# paried sample
X2LP <- c(X2L[1:18], X2L[1:18] + rnorm(18, 3, 1))
# ANOVA ---
l31 <- 75 + 4.75 * 1:3
l32 <- 61 + rnorm(3)
# one-way anova with 3 levels, subgroup C
X3L <- c(rnorm(6, l31[1], 2), rnorm(6, l31[2], 2), rnorm(6, l31[3], 2),
         rnorm(6, l31[1], 2), rnorm(6, l31[2], 2), rnorm(6, l31[3], 2))
# two-way anova with 3 levels, subgroup B & C
X3L2 <- c(rnorm(6, l31[1], 2), rnorm(6, l31[2], 2), rnorm(6, l31[3], 2),
         rnorm(6, l32[1], 2), rnorm(6, l32[2], 2), rnorm(6, l32[3], 2)) 

df <- data.frame(A= A, B= B, C= C, X11= X11, X12= X12, X2L= X2L, X2LP= X2LP,
                 X3L= X3L, X3L2= X3L2)

oneSampleMean <- data.frame(sample_id= 1:36, value= rnorm(36, 100, 3))
twoSampleMean <- data.frame(sample_id= 1:36, group= B, value= X2L)
pairSampleMean <- data.frame(sample_id= rep(1:18, 2), 
                             group= rep(c("before", "after"), each= 18), 
                             value= X2LP)
sampleOneWayAOV <- data.frame(sample_id= 1:36, factor= C, response= X3L)
sampleTwoWayAOV <- data.frame(sample_id= 1:36, factor_1= B, factor_2= C, response= X3L2)
oneSampleProp <- data.frame(sample_id= 1:3, size= 100, pass= rbinom(3, 100, 0.97))
twoSampleProp <- data.frame(sample_id= 1:36, group= B, size= 100, 
                            pass= c(rbinom(18, 100, 0.78), rbinom(18, 100, 0.85)))
chiSample <- data.frame()

for (i in c("oneSampleMean", "twoSampleMean", "pairSampleMean", "sampleOneWayAOV",
            "sampleTwoWayAOV", "oneSampleProp", "twoSampleProp")) {
    write.csv(get(i), 
              file.path("Raw", paste0(i, ".csv")), row.names = FALSE)
}
# * Data distribution ==================================================================


#-------------------------------------------------------------------------------------
df <- read_csv("./Raw/data_dupli.csv")
df <- df %>% select(V2, X1, X2)


df <- read_csv("./Raw/testMeanData.csv") 

# Clean data
missing_rows <- which(! complete.cases(df))
df_na <- df[missing_rows, ]
df_clean <- df[- missing_rows, ]
idCol <- "No"
ftrCols <- c("B", "C")
numCols <- c("X11", "X12", "X2L", "X3L", "X3L2")
li <- Outlier_IQR(df, var = numCols, threshold = 10)
anyNA(li$cleanData)


# * one sample mean test -----
dfo <- read.csv("./Raw/oneSampleMean.csv")

colSel <- "value"
hypoMean <- 99.8


df <- data_frame(value= dfo[[colSel]])

# Dealing wiht missing data
df <- df[complete.cases(df), ]

# Dealing with extreme value
li <- Outlier_IQR(df)
df <- li$cleanData
# Test outlier
alpha <- 0.05
out_test <- Grubbs_test(df[[1]], alpha = alpha)

paste0("p-value = ", out_test$p_value, "; alpha = ", alpha)
paste0("Outlier test result: ", out_test$comment)
if (! out_test$H0) {
    "Be careful of outler, especially the extreme value. 
    Outlier may cause data not normally distributed and result of t-test."
}

# Normality test
alpha <- 0.05
norm_test <- Normal_test(df[[1]], alpha = alpha)
if (norm_test$sample_size >= 30) {
    info <- "Sample size >= 30, t-test is applicable"
} else {
    if (norm_test$H0) {
        info <- "Data set is normally distributed; t-tes is applicable."
    } else {
        info <- "Data set is NOT normally distributed and sample size < 30; 
        Wilcoxon is applicable."
    }
}

# t-test
testMethod <- "auto"
nullHypo <- "equal"
mu= 97 #hypoMean
alpha <- 0.05

alt <- switch (nullHypo,
               "equal" = "two.sided",
               "greater" = "less",
               "less" = "greater"
)

# mean test
x = df[[1]]
if (testMethod == "auto") {
    if (norm_test$H0 | norm_test$sample_size >= 30) testMethod <- "t-test"
    else testMethod <- "Wilcoxon-test"
}

if (testMethod == "t-test") {
    sample_size <- length(x[! is.na(x)])
    mean_test <- t.test(x, mu = mu, alternative = alt, conf.level = 1 - alpha)
    
    # effect size
    sd <- sd(x, na.rm = TRUE)
    delta = as.numeric(abs(mean_test$estimate - mean_test$null.value))
    effect_size <- delta / sd
    
    # C.I. plot
    plot_ci <- Plotly_ci_one(DF_ci(mean_test), sd,
                             title = paste0((1 - alpha) * 100, "% confidence interval of sample mean"), 
                             legend_name = c("sample mean", "hypothesized mean"))
    
    # power test and infomation of alarming effect size
    power_test <- pwr.t.test(n= sample_size, d= effect_size, sig.level = alpha)
    info_effect <- Info_effect(mean_test$p.value, alpha, effect_size)
} else {
    mean_test <- wilcox.test(x, mu = mu, alternative = alt, conf.level = 1 - alpha)
    chart_title <- NULL
    legend_name <- NULL
    power_test <- list(n = length(x[! is.na(x)]),
                       d = NULL,
                       sig.level = alpha,
                       power = NULL,
                       alternative = alt)
    info_effect <- NULL
    plot_ci <- NULL
}


# Parse test result
result <- Parse_test(mean_test, power_test)
# plot
Plotly_box(data.frame(x= "", y= df$value), name_vector = c("", ""), 
            width = NULL, height = NULL, autosize = TRUE
        )
dfx1 <- data.frame(x= "1", y= df$value)
Plotly_box(dfx1)

ggplot(df, aes(x= value)) + 
    geom_histogram(color= "white", fill= "steelblue4", aes(y= ..density..)) + 
    geom_density(alpha= 0.5, fill= "grey") +
    geom_hline(yintercept = 0, color= "#708090", size= 1) +
    labs(x= "", y= "") +
    theme_min(base_size = 16, border_color = NA, yText = F)

QQ_plot(df$value)

# ci
chart_title <- paste0((1 - alpha) * 100, "% confidence interval of sample mean")
df_ci = data.frame(estimate= mean_test$estimate, low_ci= mean_test$conf.int[1],
                   up_ci= mean_test$conf.int[2], fidu= mean_test$null.value)
Plotly_ci_one(df_ci, title = chart_title)

# effect size
df_eff <- data.frame(x= round(c(delta, sd), 3), 
                     y=  c("effect", "std. deviation"))
markers <- sd * c(0.2, 0.5, 0.8)
title = paste0("<b>Effect size = ", round(effect_size, 2), "</b>")
p <- plot_ly(df_eff, x = ~x, y = ~y, type = "bar") %>%
    add_annotations(x= ~x, y= ~y, text = ~x, showarrow= F, xshift= 20) %>%
    plotly::layout(xaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
                   yaxis= list(zeroline= F, showticklabels= T, title= F, showgrid= F),
                   title= title, titlefont= list(size= 12),
                   font= list(family= "Courier", size= 12))
if (sd >= delta * 0.8) {
    p <- p %>%
        add_annotations(x= markers , y= 1, text= c("20%", "50%", "80%"), 
                        font= list(color= "white"),
                        showarrow= F, xshift= -15) %>%
        plotly::layout(shapes= list(vline(markers[1], color = "white"), 
                                    vline(markers[2], color= "white"), 
                                    vline(markers[3], color= "white")))
}
p

# * two sample mean test -----
dfo <- read_csv("./Raw/twoSampleMean.csv")
colId = "sample_id"; colGroup = "group"; colData = "value"
df <- data_frame(sample_id= dfo[[colId]],  group= dfo[[colGroup]], value= dfo[[colData]])
# Dealing wiht missing data
df <- df[complete.cases(df), ]

# Dealing with extreme value
li <- Outlier_IQR(df, var = "value")
df <- li$cleanData

# Test outlier
alpha <- 0.05
out_test <- Grubbs_test(df$value, df$group, alpha = alpha)
if (prod(out_test$H0) == 1) {
    info <- "Both two groups are no outlier"
} else {
    info <- "Outler is observed."
}
tb_out_test <- out_test %>%
    mutate(alpha= alpha) %>%
    select(group, p_value, alpha, H0)

# tb_out_test %>% Table_DT()

# Normality test
norm_test <- Normal_test(df$value, df$group, alpha = alpha)

if (prod(norm_test$H0) == 1) {
    info <- "Data set is normally distributed; t-tes is applicable."
} else {
    if (prod(norm_test$sample_size >= 30) ) {
        info <- "Data set is NOT normally distributed but sample size >= 30; 
        t-test is applicable"
    } else {
        info <- "Data set is NOT normally distributed and sample size < 30; 
        Wilcoxon is applicable."
    }
}

norm_test %>%
    mutate(alpha= alpha,
           normality= ifelse(H0, "Yes", "No")) %>%
    select(group, sample_size, p_value, alpha, normality) %>%
    Table_DT(c("group", "sample size", "p value", "alpha", "normality"))

# equal variances
if (prod(norm_test$H0) == 1) {
    var_test <- var.test(value ~ group, data = df)
} else {
    var_test <- fligner.test(value ~ group, data = df)
}

equal_variance <- var_test$p.value >= 0.05
 
# t-test
alpha= 0.05
if (prod(norm_test$H0) == 1) {
    mean_test <- t.test(value ~ group, data = df, var.equal= F, conf.level= 1 - alpha)
} else {
    mean_test <- wilcox.test(value ~ group, data = df, exact= FALSE)
}

# plot
Plotly_box(select(df, group, value), xcolor = df$group, 
           colors = c("steelblue", "orangered3"),
           name_vector = c("group", ""))
plot_ly(df, x= ~group, y= ~value, color= ~group, type= "box", 
        colors= c("steelblue", "orangered3")) %>%
    plotly::style(box= list(visible= T), meanline= list(visible= T),
                            marker= list(symbol= 4))

ggplot(df, aes(x= value, fill= group, color= group)) + geom_density(alpha= 0.3) + 
    geom_hline(yintercept = 0, color= "#708090", size= 1) +
    scale_fill_manual(values = color_set) + 
    scale_color_manual(values = color_set) +
    labs(x= "", y= "") +
    theme_min(base_size = 16, border_color = NA, yText = F)

ggplot(df, aes(sample= value, color= group)) + 
    stat_qq_point(alpha= 0.7, size= 2) + 
    stat_qq_line() +
    scale_color_manual(values = color_set) +
    theme_min(base_size = 16, border_color = NA)

QQ_plot(df$value, group = df$group, df_info = df$sample_id,  
        info_names = c("sample id"),
        colors = c("steelblue", "orangered3"))



ggpairs(dfx,
        lower = list(continuous= wrap("points", color= "steelblue")),
        diag= list(continuous= wrap("densityDiag", color= "steelblue")))  +
    theme_min(xGrid_major = F, yGrid_major = F, tick = T, strip_fill = NA)

# * Paired t-test ----
dfo <- read_csv("./Raw/pairSampleMean.csv")
colId = "sample_id"; colGroup = "group"; colValue = "value"
col_names <- c(colId, colGroup, colValue)
df <- data_frame(sample_id= dfo[[colId]], group= dfo[[colGroup]], value= dfo[[colValue]])

groups <- unique(df$group)
df$group <- factor(df$group, levels = groups)
df %>% group_by(group) %>%
    summarise(count= n(),
              mean= mean(value), na.rm= TRUE,
              stDev = sd(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE),
              IQR = IQR(value, na.rm = TRUE))
df_wide <- df %>%
    spread(group, value) %>%
    mutate(diff= .[[groups[2]]] - .[[groups[1]]])

Grubbs_test(df_wide$diff, alpha = 0.05)
Normal_test(df_wide$diff, alpha = 0.05)

# plot
ggplot(df, aes(x= group, y= value, group= sample_id)) + 
    geom_line(color= "steelblue") + geom_point(color= "steelblue", size= 2)+
    theme_min(base_size = 16, border_color = NA)

# paired mean test
alt = "two.sided"; alpha= 0.05

# * One-way ANOVA ----
# Null hypothesis: the means of different groups are all equal
# Alternative hypothesis: the means of different groups are NOT all equal

dfo <- read_csv("./Raw/sampleOneWayAOV.csv")
colId = "sample_id"; colGroup = "factor"; colValue = "response"
df <- data_frame(sample_id= dfo[[colId]], 
                 group= dfo[[colGroup]], 
                 value= dfo[[colValue]])
groups <- unique(df$group)
df$group <- factor(df$group, levels = groups)

#--- Initialize aov
aov <- aov(value ~ group, data= df)


#--- Cook's distance
x <- df$value
group <- df$group
info= data.frame(sample_id= df$sample_id)
xcolor= df$group
text= NULL
name_vector = c("sequence", "Cook's distance", "Sample id")
thresCook= 4
threshold = thresCook
colors= color_set
title= ""
showLegend = TRUE

cooksd <- Cook_dist(df$value, df$group, info= select(df, sample_id), xcolor= df$group,
    name_vector = c("sequence", "Cook's distance", "Sample id"), threshold = thresCook,
    showLegend = TRUE)

cooksd$df %>% filter(influential) %>%
    select(sample_id, group, x, cooksd) %>%
    Table_DT(colnames = c("Sample id", "Group", "Value", "Cook's distance")) %>%
    DT::formatRound(columns= c("x", "cooksd"), digits = 3)

#--- normality
aov <- aov(value ~ group, data= df)
residual <- residuals(object = aov)
p_value <- shapiro.test(x = residual)$p.value

QQ_plot(aov$residuals, df_info = df$sample_id,
        info_names = "Sample id")

#---



#--- homogeneity of  variance
# normally distribution
bartlett.test(value ~ group, data= df)
# less sensitive to normality
library(car)
test <- leveneTest(value ~ group, data= df)
# non-parametric test
fligner.test(value ~ group, data= df)

df_resi <- data.frame(fit= aov$fitted.values, residual= aov$residuals, value= df$value,
                      sample_id= df$sample_id, group= df$group) 
Plotly_scatter(select(df_resi, fit, residual), info= select(df_resi, sample_id), 
               name_vector = c("fitted value", "residual", "sample id"), mode= "markers")

Plotly_box(select(df_resi, group, residual), info= select(df_resi, sample_id), 
               name_vector = c("group", "residual", "sample id"))

# plot
Plotly_box(select(df, group, value), xGrid = F,
           width = NULL, height = NULL, autosize = TRUE)

df_ci <- group.CI(value ~ group, data= df, ci= 0.95) %>%
    mutate(error = value.upper - value.mean)

plot_ly(df, x= ~group, y= ~value, type = "scatter", mode= "markers", alpha = 0.5,
        color = I(color_set[1])) %>%
    add_trace(data= df_ci, x= ~group, y= ~value.mean, type= "scatter", 
              mode= "markers+lines", color= I(color_set[1]),
              marker= list(size= 10),
              error_y= ~list(type= 'data', array= error, color= color_set[1])) %>%
    plotly::layout(showlegend= FALSE)

#--- Comparing mean
ract_oneFactor_meanTest <- function() {
    #... setting
    normality <- TRUE
    homoVar <- F
    method <- "auto"
    alpha <- 0.05
    df <- df
    
    #... functions
    Aov <- function(df, aov) {
        test <- summary(aov)
        p_value <- aov_test[[1]][["Pr(>F)"]][[1]]
        title <- "One-way ANOVA"
        body <- test
        
        # Tukey multiple pairwis-comparisons
        multiComp <- pairwise.t.test(df$value, df$group, p.adjust.method = "BH")
        
        # Return
        list(test= test, p_value= p_value, title= title, body= body, multiComp= multiComp)
    }
    
    Oneway <- function(df, aov) {
        test <- oneway.test(value ~ group, data= df, subset = NULL, na.action = "na.omit")
        p_value <-   test$p.value
        title <- test$method
        body <- paste0(names(test$statistic), " = ", round(test$statistic, 3), 
                       ", denom df = ", round(test$parameter[2], 3), 
                       ", p-value = ", formatC(p_value, format= "e", digits= 3))
        # Pairwise t-tests with no assumption of equal variances
        multiComp <- pairwise.t.test(df$value, df$group, 
                                     p.adjust.method = "BH", pool.sd = FALSE)
        # Return
        list(test= test, p_value= p_value, title= title, body= body, multiComp= multiComp)
    }
    
    Kruskal <- function(df, aov) {
        test <- kruskal.test(df$value, df$group)
        p_value <- test$p.value
        title <- test$method
        body <- paste0(names(test$statistic), " = ", round(test$statistic, 3), 
                       ", df = ", test$parameter, 
                       ", p-value = ", formatC(p_value, format= "e", digits= 3))
        # Pairwise Wilcoxon rank sum tests
        multiComp <- pairwise.wilcox.test(df$value, df$group, p.adjust.method = "BH")
        
        # Return
        list(test= test, p_value= p_value, title= title, body= body, multiComp= multiComp)
    }
    
    #... Perform mean test
    Fun_test <- switch(method,
                       "auto" = ifelse(normality, ifelse(homoVar, Aov, Oneway), Kruskal),
                       "one-way ANOVA" = Aov,
                       "Kruskal-Wallis Test" = Kruskal
    )
    aovResult <- Fun_test(df, aov)
    
    mean_test_result <- ifelse(aovResult$p_value >= alpha,
                               "Means of differnt groups are all equal.",
                               "Means of differnt groups are NOT all equal.")
    aovResult$mean_test_result <- mean_test_result
    
    # Return list of (1) ANOVA test, (2) p-value of ANOVA, (3) title for output, 
    #   (4) body, which is content of ANOVA, for output, 
    #   (5) result of multiple comparison, (6) result of ANOVA test, accept or rejct H0
    aovResult
}

#--- Multiple comparison
Reform_multiComp <- function(multiComp) {
    title <- multiComp$method
    adj_method <- paste0("P value adjustment method: ", multiComp$p.adjust.method)
    tb <- as.data.frame(multiComp$p.value)
    tb <- data.frame(p_value= row.names(tb), tb)
    
    # Return
    list(title= title, adj_method= adj_method, tb= tb)
}
tb1 <- ifelse(tb >= alpha, "equal", "different")
Table_DT(tb, rownames = TRUE) %>%
    DT::formatRound(1:length(tb), 4)
Table_DT(tb1, rownames = TRUE, cellClass = "dt-center")

# * One-proportion test ----
#... Input by summarized data
event_c= 40; trial_c= 100; hypoProp= 0.03

#... Input by raw data <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
set.seed(1)
df <- data.frame(batch_id= paste0("B", 1:10),
                  event_count= rbinom(10, 100, 0.025),
                  sample_size= 100)

names(df) <- c("batch_id", "value", "size")
df[c(2, 7), "Value"] <- c(18, 13)
df[c(4, 9), "Size"] <- c(25, 58)

#--- Statistics
base_size <- round(mean(df$size, na.rm = TRUE))

df <- df %>%
    mutate(perDefect = value / size)

p_chart(df, "batch_id", "value", "size", xlab= "batch", ylab = "proportion")

cl <- cl_p(df$value, df$size)
ooc <- df$batch_id[which(df$perDefect > cl["UCL"] | df$perDefect < cl["LCL"])]

if (length(ooc) >= 1) {
    info <- "Proportions across batches are NOT equal. Refer to p-chart and 
    make sure there is no mistaken data."
} else {
    info <- "Proportions across batches are equal"
}

#..............................................................

#... Check whether is large group
nullHypo <- c("equal", "less", "greater")[1]
alt <- switch (nullHypo,
               "equal" = "two.sided",
               "greater" = "less",
               "less" = "greater"
)
po <- event_c / trial_c
large_sample <- ifelse(trial_c * po >= 5 & trial_c * (1 - po) >= 5, TRUE, FALSE)

#... Perform one-poroportion test
alpha <- 0.05
test_method <- ifelse(large_sample, prop.test, binom.test)
test <- binom.test(event_c, trial_c, hypoProp, alternative = alt, conf.level = 1-alpha)
prop_test <- test_method(event_c, trial_c, p= hypoProp, correct = FALSE, 
                    alternative = alt, conf.level = 1-alpha)
prop_test$p.value

# power test
h <- ES.h(po, hypoProp)
pwr.p.test(h, n= trial_c, sig.level = alpha, alternative = alt)

# test result
title <- prop_test$method

if (prop_test$p.value >= alpha) {
    h0 <- switch (nullHypo,
                  "equal" = "equal to",
                  "greater" = "greater than or equal to",
                  "less" = "less than or equal to"
    )
} else {
    h0 <- switch (nullHypo,
                  "equal" = "NOT equal to",
                  "greater" = "less than",
                  "less" = "greater than"
    )
}
icon <- icon("check-circle", "fa-2x")
result <- paste0("True proportion is ",  h0, " ", hypoProp, ".")
result <- paste0("<p><b>", span(icon, result), "</b></p>")

# plot
dfx <- data.frame(estimate= prop_test$estimate,
                       ci_low= prop_test$conf.int[1],
                       ci_up= prop_test$conf.int[2],
                       hypoProp= hypoProp)
dfx1 <- gather(dfx, key= "item", value= "value") %>%
    mutate(value= round(value, 4))

plot_ly(dfx, y= 0, height = 200) %>%
    add_markers(x= ~estimate, color= I("steelblue"), marker= list(size= 15), 
                name= "sample proportion",
                error_x= ~list(type= "data", symmetric= FALSE, 
                               array= c(ci_up - estimate),
                               arrayminus= c(estimate - ci_low))) %>%
    add_markers(x= ~hypoProp, color= I("red"), marker= list(size= 15, symbol = 25), 
                name= "hypothesized proportion") %>%
    add_annotations(data= dfx1[1, ], x= ~value, text = ~value, showarrow= F, 
                    yshift= 20) %>%
    add_annotations(data= dfx1[2, ], x= ~value, text = ~value, showarrow= F, 
                    xshift= -25) %>%
    add_annotations(data= dfx1[3, ], x= ~value, text = ~value, showarrow= F, 
                    xshift= 25) %>%
    add_annotations(data= dfx1[4, ], x= ~value, text = ~value, showarrow= F, 
                    yshift= -15) %>%
    plotly::layout(xaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
                   yaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F))
    
# * Two-proportion test -----
event1 = 6; trial1 = 100; event2 = 8; trial2 = 97
event1 = 1; trial1 = 100; event2 = 3; trial2 = 97

groups <- c("group 1", "group 2")

# input raw data
set.seed(1)
df <- data.frame(batch_id= paste0("B", 1:20),
                 group= rep(c("control", "experiment"), each= 10),
                 event_count= c(rbinom(10, 100, 0.085), rbinom(10, 100, 0.056)),
                 sample_size= 100)

names(df) <- c("batch_id", "group", "value", "size")
df$batch_id <- factor(df$batch_id, levels= unique(df$batch_id))

groups <- unique(df$group)
event1 <- df %>% filter(group == groups[1]) %>% .$value %>% sum()
event2 <- df %>% filter(group == groups[2]) %>% .$value %>% sum()
trial1 <- df %>% filter(group == groups[1]) %>% .$size %>% sum()
trial2 <- df %>% filter(group == groups[2]) %>% .$size %>% sum()

df <- df %>%
    mutate(perDefect = value / size)
plotx = list()
oocx = list()
for (gp in groups) {
    df1 <- filter(df, group == gp)
    plotx[[gp]] <- p_chart(df1, "batch_id", "value", "size", 
                           xlab= "batch", ylab = "proportion")
    
    cl <- cl_p(df1$value, df1$size)
    ooc <- df1$batch_id[which(df1$perDefect > cl["UCL"] | df1$perDefect < cl["LCL"])]
    if (length(ooc) >= 1) {
        info <- "Proportions across batches are NOT steady. Refer to p-chart and 
            make sure there is no mistaken data."
    } else {
        info <- "Proportions across batches are steady"
    }
    oocx[[gp]] <- info
}

 # perform two-proportion test
alt <- "two.sided"
alpha <- 0.05
large_sample <- ifelse(event1 >= 5 & (trial1 - event1) >= 5 &
                           event2 >=5 & (trial2 - event2) >=5,
                       TRUE, FALSE)

if (large_sample) {
    prop_test <- prop.test(x= c(event1, event2), 
                           n= c(trial1, trial2), alternative = alt,
                           conf.level = 1 - alpha)
    chart_title <- paste0((1 - alpha) * 100, 
                          "% confidence interval of proportion dfference, ",
                          groups[1], " - ", groups[2])
    legendx <- "p1 - p2"
    
    dfx <- data.frame(row.names = 1,
        estimate= prop_test$estimate[1] - prop_test$estimate[2],
        ci_low= prop_test$conf.int[1],
        ci_up= prop_test$conf.int[2],
        fidu= 0)
} else {
    matx <- matrix(c(event1, event2, trial1 - event1, trial2 - event2), nrow= 2)
    prop_test <- fisher.test(matx, alternative = alt, conf.level = 1 - alpha)
    equal_value <- 1
    chart_title <- paste0((1 - alpha) * 100, 
                          "% confidence interval of odds ratio, ",
                          groups[1], " / ", groups[2])
    legendx <- "odds ratio"

    dfx <- data.frame(row.names= 1,
        estimate= prop_test$estimate,
        ci_low= prop_test$conf.int[1],
        ci_up= prop_test$conf.int[2],
        fidu= 1)
}

alpha= 0.05
nullHypo= "equal"
prop_test$data.name <- ""
test_title <- prop_test$method

if (prop_test$p.value >= alpha) {
    h0 <- switch (nullHypo,
                  "equal" = "=",
                  "greater" = "&ge;",
                  "less" = "&le;"
    )
} else {
    h0 <- switch (nullHypo,
                  "equal" = "&ne;",
                  "greater" = "<",
                  "less" = ">"
    )
}
icon <- icon("check-circle", "fa-2x")
result <- paste("Comparing two proportions:", groups[1], h0, groups[2], sep= " ")
result <- paste0("<b>", icon, "&nbsp;&nbsp;",  result, "</b>")


# plot
dfx1 <- gather(dfx, key= "item", value= "value") %>%
    mutate(value= round(value, 4))

plot_ly(dfx, y= 0, height = 200) %>%
    add_markers(x= ~estimate, color= I("steelblue"), marker= list(size= 15), 
                name= legendx,
                error_x= ~list(type= "data", symmetric= FALSE, 
                               array= c(ci_up - estimate),
                               arrayminus= c(estimate - ci_low))) %>%
    add_markers(x= ~fidu, color= I("red"), marker= list(size= 15, symbol = 25), 
                name= ~fidu) %>%
    add_annotations(data= dfx1[1, ], x= ~value, text = ~value, showarrow= F, 
                    yshift= 20) %>%
    add_annotations(data= dfx1[2, ], x= ~value, text = ~value, showarrow= F, 
                    xshift= -25) %>%
    add_annotations(data= dfx1[3, ], x= ~value, text = ~value, showarrow= F, 
                    xshift= 25) %>%
    add_annotations(data= dfx1[4, ], x= ~value, text = ~value, showarrow= F, 
                    yshift= -15) %>%
    plotly::layout(xaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
                   yaxis= list(zeroline= F, showticklabels= F, title= F, showgrid= F),
                   title= chart_title)


# * Paired proportion test -----
ntn = 1146; nty = 37; ytn = 234; yty = 17
alphaTest = 0.05; nullHyop= "equal"

mat <- matrix(c(ntn, ytn, nty, yty), nrow= 2)
mcnemar.test(mat, correct = FALSE)
binom.test(c(nty, ytn), p= 0.5)
alt = "two.sided"; alpha = 0.05
prop_test <- binom.test(c(nty, ytn), p= 0.5, alternative = "two.sided", conf.level = 1-alpha)



