## Welcome!
---

This site demonstrates the impact of missing data on treatment effect estimates in randomized controlled trials.

I simulated a data generating process where an intervention increases test scores by 3 points on average. To estimate the treatment effect, test scores are regressed onto a treatment indicator and a baseline covariate. There are two different modeling scenarios:

- Model 1: the covariate is family income ("No baseline outcome in model" tab)
- Model 2: the covariate is baseline test score ("Baseline outcome in model" tab)

In each scenario, we've measured only the covariate included in the model, and the other is an unobserved confounder.

## Data can be missing in different ways, for different reasons
---

I've generated missing data under 10 scenarios, each of which has unique implications for our treatment effect estimates. Data can be missing on either a) the outcome or b) the covariate, and there are five different combinations of variables associated with the probability that data are missing:

1. Completely at random (ie unrelated to all variables in the model)
2. The covariate
3. The covariate and treatment assignment
4. The outcome
5. The outcome and treatment assignment

For example, where the probability of missing data is a function of the covariate, we might imagine that people with lower incomes faced more travel barriers and were therefore more likely to miss their data collection appointments.

There are two important missingness processes that may arise in RCTs that are a bit unintuitive, so I want to discuss them here.

### When missing baseline data is related to the outcome

An important scenario to consider is when the probability of missing the baseline covariate is related to the outcome. It is not possible for the outcome itself to cause people's data to be missing at baseline, since it is measured after baseline data collection. However, there may be confounders that cause the missingness. That would mean the variables that cause missing baseline data also influence the outcome, and we haven't measured them. So the outcome would be related to the missingness in our model, even though it's not the cause.

A particularly relevant scenario is when people's baseline "ability," the thing that the test score is measuring, influences the probability of missing baseline data. For example, one might have high test anxiety and therefore be more likely to miss their baseline data collection appointment. Or, in a more likely scenario, confounders related to "ability" may drive the missingess. Because baseline ability is so strongly related to our test score outcome, it will explain much of the variation in the probability of missing data that is related to the outcome, even if it is not causing the missingness per se. This helps illustrate why having a baseline test score measure is such a powerful addition to our treatment effect estimate model and why it warrants having its own separate modeling demonstration.

### When missing baseline data is related to treatment assignment

The beauty of an RCT is that treatment assignment is random and therefore unrelated to all other baseline variables. If baseline data are collected before treatment assignment, the probability of missing that data should also be unrelated to treatment assignment. In this case, missing data in covariates shouldn't pose much threat to our ability to estimate the treatment effect.

However, in practice, we must often collect baseline data after the treatment has been assigned. This opens up the possibility that treatment assignment could influence the probability of missing baseline data, which could bias our treatment effect esimates.

## The best approach to handle missing data depends on the situation
---

With all this context in mind, the final component of the site is a demonstration of the four common approaches for dealing with missing data under each of these missingness scenarios. The four strategies are:

1. Listwise Deletion
2. Mean Imputation (with a missing indicator added to the model)
3. Full Information Maximum Likelihood
4. Multiple Imputation

The page shows a plot of the "true" data, along with the covariate regression slopes for the treatment and control groups and the gap between them representing the treatment effect estimate. You can click on each of the missing data handling approaches to see how the data used for modeling and the estimates change. The page also shows a table comparing the average treatment effect (ATE) estimate and its standard error under each approach. The bottom of the screen shows an equation representing the missingness function and a DAG of the whole causal process.

Finally, I include a summary table comparing the performance of each approach. I hope that this information can aid the process of deciding how to handle missing data. Keep in mind that this is a highly simplistic simulation, and that this page is not intended to provide any definitive evidence or recommendations. Rather, I want to help illuminate the many different missing data scenarios and provide some indication as to what may or may not work well in each.

<br>