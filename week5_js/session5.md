# LMER workshop Session 5
Justin Sulik  
justin.sulik@gmail.com  
github/justinsulik  
November 7, 2017  



# Load packages

```r
library(tidyverse)
library(lme4)
library(gridExtra)
library(magrittr)
library(lattice)
library(sjPlot)
library(sjmisc)
```



```r
theme_set(theme_bw()) 
```

## Learning outcomes

- Understand why/how to use a glmer instead of an lmer

- How to report (g)lmers (esp. p-values, CIs)

- How to graph model effects

# 1. GLMs/GLMERs

## Assumptions

- The relationship between variables is linear

- Variance of residuals is homogeneous 
    
- Residuals are normally distributed

Violations of these in an LMER aren't always very serious, ...

BUT we may as well use a more robust method since it exists and is easy to use!

## Assumptions

- These are all things that glmer can help with

- **Sometimes** transformation is an alternative, but glmers are helpful more generally 

- We're just going to run through a couple examples to see what it **looks** like when the assumptions do/don't hold

    - I'll discuss the distributions (e.g. Poisson, binomial) soon
    
 
## Random data


```r
set.seed(1234)
x <- 1:40

data.norm <- data.frame(outcome=rnorm(40,x, 2),
                        predictor=x)

data.pois <- data.frame(outcome=rpois(40,x),
                           predictor=x)

data.binom <- data.frame(outcome=rbinom(40,1,x/40),
                         predictor=x)
```

## First look at the distribution of the outcome

![](session5_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Density plots can make it easier to see skewness

![](session5_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Linear relationship?

![](session5_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Linear relationship?

![](session5_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Homogeneity of variance?

![](session5_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Homogeneity of variance?

![](session5_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

## Residuals normally distributed? {.smaller}

See [https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot](https://stats.stackexchange.com/questions/101274/how-to-interpret-a-qq-plot)
![](session5_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Is it just about assumptions?

I think focusing on assumptions is *partly* missing the point: these are the symptoms of a problem, not the problem itself

One general theme in this workshop has been to explore the data, try understand the data, let the data speak for itself

Sometimes a linear model is wrong just because it mischaracterizes your data

## Elements of a generalized linear model

A glm/glmer adds two things to an lm/lmer:

- you specify what distribution family your outcome variable belongs to

    - see `?family` in RStudio

    - normal (gaussian), binomial, Gamma, inverse.gaussian, poisson, quasibinomial, quasipoisson
    
    - [https://blog.cloudera.com/blog/2015/12/common-probability-distributions-the-data-scientists-crib-sheet/](https://blog.cloudera.com/blog/2015/12/common-probability-distributions-the-data-scientists-crib-sheet/)
    
- you (optionally) specify how the predictor and outcome are related

    - links: "identity", "log", "logit", "inverse", ...

## Elements of a generalized linear model

Specifying the distribution family involves explicitly modeling:

- The **shape** of the data 
    - e.g. skewed RT data
    
- The **range** of the data 
    - e.g. positive (for RT data or count data) 
    - between 0 and 1 (for a ratio)

## Elements of a generalized linear model

- `lmer(y ~ x + (1|subject),data)`

same as lmer: 

- `glmer(y ~ x + (1|subject),data,family=gaussian(link = "identity"))`

Change family and link

- `glmer(y ~ x + (1|subject),data,family=poisson(link = "log"))`

Or (since log is the "canonical" link for a Poisson regression)

- `glmer(y ~ x + (1|subject),data,family=poisson)`

## Elements of a generalized linear model

- So the `glmer` allows you to control how your model fits the data in two ways: by specifying a *family* AND by specifying a *link* function

- a transformation (e.g. log(x)) is really just an attempt to handle these in one go:

    - link="log" and *hopefully* the transformed data is normal-ish
    - Often doesn't manage to do **both** jobs well
    - Some distributions can't be transformed to normal
    
## Check understanding

- The following creates some data where the link is log and the outcome is skewed

- Try model with 

    - lm
    - lm with log transformation
    - glm with log link and gaussian family
    - glm with log link and gamma family


```r
data <- data.frame(outcome=rgamma(1000,2),
                   noise=rnorm(1000,0,2)) %>% 
  mutate(predictor = log(outcome)+noise) 
```

## Check understanding

![](session5_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Check understanding

![](session5_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## Types of outcome variable

- Don't start out by worrying what a Poisson/Gamma/negative binomial are

- Start by thinking about your data and the processes that gave rise to it

- Imagine you're counting the number of adjectives in a sentence

- How will this be different from a normal distribution?

## Poisson distributions with different means

![](session5_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## Types of outcome variable

Eventually you can build up a sense of what to try 

- google + crossvalidated

- literature, e.g. 
    - Lo & Andrews, 2015, To transform or not to transform, Frontiers in Psych.
    - Coxe, West & Aiken, 2009, The analysis of count data, J. Pers. Assess.

- [https://www.johndcook.com/blog/distribution_chart](https://www.johndcook.com/blog/distribution_chart)
- [http://www.math.wm.edu/~leemis/chart/UDR/UDR.html](http://www.math.wm.edu/~leemis/chart/UDR/UDR.html)

## Types of outcome variable

Some rules of thumb:

- count data: try Poisson or negative-binomial

- success/failures: try binomial

- reaction time: try inverse Gaussian, ex-Gaussian or Gamma (see Lo & Andrews, previous slide)

## Types of outcome variable

Ultimately, you just explore, simulate, and compare. 

It's not just about model fits, but also about justifying the distribution/the processes that gave rise to the data

- E.g. # students walking through main doors of library every 15 minutes -> Poisson
- How is your count data like/unlike this? 
    - Does it have a maximum upper bound?
    - Does it have a constant interval?
    - Is it only about hits, or failures too?

## Types of outcome variable

- Or is it more about waiting until something happens? 
    - E.g. length of hospital stay -> negative binomial
    - If so, number of failures until success? 
    - Time until response? 

## Types of outcome variable

You'll have to learn the details by googling (wikipedia's pretty good for this!)
  
- e.g. Poisson regression assumes constant intervals (e.g. counts / minute)
    - You can include weights if intervals not constant    
- no upper limit
- variance=mean
    - Switching to negative-binomial has extra parameter for variance

- ...

## Check understanding

What model should be used to for the following data? Poisson or binomial? 

(Let's ignore other possibilities -- e.g. negative binomial -- to keep things simple)

20 Participants are given a cue (e.g. 'bank'). Each participant has to try coordinate by producing the same response as other participants. E.g. if 15 people respond "money" and 2 respond "vault", then the former have coordinated better. 

The outcome is the count of participants producing each word. The predictor is the associative strength from cue to word. Data on github (data_session5).

## Check understanding: data-generating process

First: look back to the examples of Poisson/binomial mentioned previously 

(or check out the wikipedia pages or other links provided above)

How is this design similar/different to the typical *data-generating processes* described there?

What reasons do we have, *initially*, to think these are two plausible distributions?

## Check understanding: shape

Explore the data distribution:

- Use ggplot+geom_histogram to graph `match`

- How are the data distributed?

- Are there any minima/maxima?

## Check understanding: relationship between predictor and outcome

Do a dot plot of the relationship. Does it look linear? 

If not, what?

## Check understanding: finally, model the data

Try model the data and see which is best

## Summary for section 1:

Specifying `family` (and, optionally, `link`) is a more general, robust alternative to transformation in cases when a vanilla lm/lmer is problematic

It involves trying to model the data as it is, rather than trying to shoehorn the data into normality

Using a glmer (instead of glm) means you have multiple ways to deal robustly with small and large departures from normality

    - Fitting the data distribution+link
    - Fitting random effects

# 2. Reporting g(lmer) models

## lme4 and p-values

By default, `lme4` doesn't output p values. That's ok!

The main problem is that it's hard to count degrees of freedom.

The authors of lme4 have some detailed explanations:

- type `help("pvalues",package="lme4")` in RStudio
- [https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html](https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html)
- [https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html](https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html)
- [https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html](https://stat.ethz.ch/pipermail/r-sig-mixed-models/2008q2/000904.html)

## Rule of thumb

If your t-value is above 2, you're probably ok

But don't leave it there ...

## Method 1: anova {.smaller}

Create two models, one missing the predictor you're interested in. In the `cake` dataset, is `temperature` a significant predictor of `angle`?


```r
#REML=FALSE when comparing models with different fixed effects. 
#Just here for completeness sake. anova will refit in any case
mod <- lmer(Reaction ~ Days + (1+Days|Subject), sleepstudy, REML=FALSE) 
mod0 <- lmer(Reaction ~ 1 + (1+Days|Subject), sleepstudy, REML=FALSE)
anova(mod,mod0)
```

```
## Data: sleepstudy
## Models:
## ..1: Reaction ~ 1 + (1 + Days | Subject)
## object: Reaction ~ Days + (1 + Days | Subject)
##        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
## ..1     5 1785.5 1801.4 -887.74   1775.5                             
## object  6 1763.9 1783.1 -875.97   1751.9 23.537      1  1.226e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Method 2: lmerTest

You can use package `lmerTest`

Just load it, and it updates the output of the lmer function

BUT be aware that it's using approximations that are not known to be robust

## Method 2: lmerTest {.smaller}


```r
library(lmerTest)
mod <- lmer(Reaction ~ Days + (1+Days|Subject), sleepstudy, REML=FALSE)
summary(mod)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: Reaction ~ Days + (1 + Days | Subject)
##    Data: sleepstudy
## 
##      AIC      BIC   logLik deviance df.resid 
##   1763.9   1783.1   -876.0   1751.9      174 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9416 -0.4656  0.0289  0.4636  5.1793 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  Subject  (Intercept) 565.52   23.781       
##           Days         32.68    5.717   0.08
##  Residual             654.94   25.592       
## Number of obs: 180, groups:  Subject, 18
## 
## Fixed effects:
##             Estimate Std. Error      df t value Pr(>|t|)    
## (Intercept)  251.405      6.632  18.000  37.906  < 2e-16 ***
## Days          10.467      1.502  18.000   6.968 1.65e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##      (Intr)
## Days -0.138
```

## Method 3: Markov chain Montecarlo

You might come across reference to `mcmcsamp` or `pvals.fnc` while googling (or in one of the links above)

This is no longer supported by `lme4`

## Confidence intervals

Instead of (or as well as) providing p-values, you can provide bootstrapped confidence intervals

(See my course on github. Basically, bootstrapping is a very robust way to provide confidence intervals. It's not just a different way to get p-values -- CIs are conceptually different. But this isn't the time for getting into that)

Just report the beta along with the bootstrapped CIs. If they don't include 0, you're golden!

## Confidence intervals: shortcut


```r
a <- confint(mod,method="boot")
a
```

```
##                   2.5 %     97.5 %
## .sig01       10.0012401  33.903601
## .sig02       -0.4677134   1.000000
## .sig03        3.0307754   7.651423
## .sigma       22.5015638  28.358434
## (Intercept) 238.7365753 264.004054
## Days          7.4772076  13.470208
```

## Confidence intervals: more explicit


```r
library(boot)
b <- bootMer(mod,fixef,nsim=500)
boot.ci(b,type="basic",index=2) #The second row of fixef(mod)
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 500 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = b, type = "basic", index = 2)
## 
## Intervals : 
## Level      Basic         
## 95%   ( 7.42, 13.22 )  
## Calculations and Intervals on Original Scale
```

## Reporting an effect

There is a significant negative effect of turn on perimetric complexity $(\beta=−0.199,SE=0.033,t=−6.029,p<0.001$, bootstrapped 95% CIs [-0.265, -0.133])

## What else to report?

- Random effects structure
- How you got p-values
- Family if you specified one (and link if you didn't use the canonical one)

- IDEALLY include supplementary material that presents your working 
    - distribution of outcome variable
    - models tried 
    - especially if your random effects structure is a little complicated 
    - or you're using some Byzantine regression

## Summary for section 2:

Get comfortable with bootstrapping confidence intervals here - these really are the most important part

Otherwise, the anova function is a good bet

Using both the above means no reviewer will be able to complain (at least about this part...)

I haven't seen many articles reporting p-values from `lmerTest`

# Graphing models

## Model parameters

See github for some code that produces the following graph:

<img src="modelParams.pdf" width=500>

## Model fit (plain LM/GLM) {.smaller}

If you're using a plain lm you can graph the model fit using ggplot


```r
ggplot(data.pois, aes(x=predictor,y=outcome)) + 
  geom_point() + 
  stat_smooth(method=lm,
              se=F)
```

![](session5_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

## Model fit (plain LM/GLM) {.smaller}

If you're using a plain glm you just need to tell stat_smooth the `list` of args it should pass to `glm`


```r
ggplot(data.pois, aes(x=predictor,y=outcome)) + 
  geom_point() + 
  stat_smooth(method=glm,
              method.args=list(family='poisson'),
              se=F)
```

![](session5_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

## Model fit lmer

If you are using a (g)lmer there are are packages, or manual ways to do this

## Model fit: packages {.smaller}

Strengejacke is a pretty good package for plotting. See what else it can do at [http://www.strengejacke.de/sjPlot/sjp.lmer/](http://www.strengejacke.de/sjPlot/sjp.lmer/)


```r
sjp.lmer(mod, type = "fe.slope", vars = c("Days"))
```

![](session5_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

## Model fit: manual

These packages aren't infinitely flexible. 

If you need more control, you can do it yourself, manually

This also means you'll gain a deeper understanding of the model output

## Model fit: manual (1)

One way is to just add in the formula for the regression line 

  - (i.e. they're really just an intercept and slope so you can use them as such!)
  

```r
mod.effects <- fixef(mod)
```

## Model fit: manual (1) {.smaller}



```r
sleepstudy %>% ggplot(aes(x=Days,y=Reaction))+
  geom_point(alpha=0.4)+
  geom_abline(aes(intercept=mod.effects[[1]],slope=mod.effects[[2]]),
              color="blue")
```

![](session5_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

## Model fit: manual (1)

This is pretty easy for lmers, but for glmers you'd have to transform the model output. 

This can get complicated: 

- [https://stats.stackexchange.com/questions/86351/interpretation-of-rs-output-for-binomial-regression](https://stats.stackexchange.com/questions/86351/interpretation-of-rs-output-for-binomial-regression)
- [https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/)  
  
## Model fit: manual (2)

Another way is to use `predict()` to create a bunch of predictions, and then use `stat_smooth()` to fit a curve to those predictions

First, create a list of predictions.

You *could* get predictions straight out of the model itself and add this as a column to your dataframe:


```r
data <- sleepstudy 
data$prediction = predict(mod)
```

## Model fit (2) {.smaller}

However, you can see this won't help with plotting a regression curve. 

Why? It's a prediction based on each line of data, so reflects everything in the model (random effects, any other fixed effects)


```r
p1 <- data %>% ggplot(aes(x=Days,y=Reaction)) + geom_point()
p2 <- data %>% ggplot(aes(x=Days,y=prediction)) + geom_point() 
grid.arrange(p1,p2,ncol=2)
```

![](session5_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

## Model fit: manual (2)

We need to create a new dataframe that has just the info we need (e.g. 100 values of Days from 0 to 9). 

The following will give you an error:


```r
newData <- data.frame(Days=(1:90)/10)
data$prediction <- predict(mod,newdata=newData)
```

## Model fit: manual (2) {.smaller}

We need to tell `predict` to ignore random effects


```r
newData <- data.frame(Days=(1:90)/10)
newData$prediction <- predict(mod,newdata=newData, re.form=NA)
newData %>% ggplot(aes(x=Days,y=prediction)) + geom_point() 
```

![](session5_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

## Model fit: manual (2) {.smaller}

Why go such a roundabout way? Well the nice thing here is that we can just smooth the prediction dots to get a nice curve! But why do we get a straight line still?!


```r
mod.pois <- glm(outcome~predictor,data.pois,family=poisson)
newData <- data.frame(predictor=1:100*0.4)
newData$prediction = predict(mod.pois,newdata=newData)
ggplot(newData,aes(x=predictor,y=prediction))+geom_point()
```

![](session5_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

## Model fit: manual (2) {.smaller}

It's because `predict` by default gives us values on the transformed y-scale (and the whole point of that was to produce a linear relationship)

To get a value on the untransformed scale, `type="response"`


```r
newData$prediction = predict(mod.pois,newdata=newData, type="response") 
ggplot(newData,aes(x=predictor,y=prediction))+geom_point()
```

![](session5_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

## Model fit: manual (2) {.smaller}

Still a series of many black points. Smooth to a curve


```r
ggplot(newData,aes(x=predictor,y=prediction))+stat_smooth()
```

![](session5_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

## Model fit: manual (2) {.smaller}

Include original data


```r
ggplot(data.pois,aes(x=predictor,y=outcome))+
  geom_point(alpha=0.6)+
  stat_smooth(data=newData,
              aes(y=prediction))
```

![](session5_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

## Summary for section 3

It might sometimes take a bit extra work to graph lmers and glmers, but 

- For anything fancier than an lm/glm, it's useful to be able to do it manually

- A lot of packages are not extremely flexible, or assume you know what you're doing with `predict` anyway

- Watch out for `type` of prediction

- Watch out for random effects

- If your model has any other fixed effects, they need to be in your newdata frame

    - just add a column with the appropriate name(s) set to all 0s

## Learning outcomes

- Understand why/how to use a glmer instead of an lmer

- How to report (g)lmers (esp. p-values, CIs)

- How to graph model effects
