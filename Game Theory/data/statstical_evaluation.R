# The code of the statistical evaluation
library(ggplot2)

##
## First we conducted two experiments to check the validity of
## our implementation.
## The first one runs 50 experiments with benefit 1 and cost 1.5.
## According to the paper this should result in a fast domination of
## defectors.
##
## The second experiment used benefit 1 and cost .75. This results in
## a stable distribution of defectors and cooperators.
##
## Both results could be achieved.
##


##
## Load data for the "plausability" experiments
##
exp_instable <- read.csv("experiment_instable_coexistence-table_for_R.csv")
exp_stable <- read.csv("experiment_stable_coexistence-table_for_R.csv")

## Plot the distribution of steps required until defectors dominate
ggplot(exp_instable, aes(x = step, fill="red", colour="red")) + geom_density(alpha=.5)

## This is the histogram for the above density
ggplot(exp_instable, aes(x = step, fill="red", colour="red")) + geom_histogram(alpha=.5, binwidth=1) + scale_x_continuous(limits=c(8, 21)) + ylab("Number of experiments") + xlab("Number of steps")+theme(legend.position="none")

## The equilibrium after 10.000 steps, we have a sd of the steps of 0.006 and a mean of 0.247256
ggplot(exp_stable, aes(x=run, y=propC_end))+geom_point(color="red") + ylab("Proportion of cooperators after 10.000 steps") + xlab("Run") + ylim(0,0.5)

## Some basic statistics
sd(exp_stable$propC_end)
mean(exp_stable$propC_end)

## Next we try to determine whether 1.000 or 500 steps are sufficient for reaching
## a rather stable state.

## The hypothesis is that exp1000 and exp10000 have the same distribution of propC_end
exp_stable_1000 <-  read.csv("experiment_stable_coexistence-table_1000steps.csv")
exp_stable_500 <-  read.csv("experiment_stable_coexistence-table_500steps.csv")

## Wir prüfen, ob wir davon ausgehen können, dass die Verteilung gleich bleibt.
## Zuerst ein F-Test und dann t.test zweiseitig
var.test(exp_stable$propC_end, exp_stable_1000$propC_end)
t.test(exp_stable$propC_end, exp_stable_1000$propC_end)

var.test(exp_stable$propC_end, exp_stable_500$propC_end)
t.test(exp_stable$propC_end, exp_stable_500$propC_end)


## This is true for both, so we assume there is no difference in simulation
## results between the 10.000 and the 500 steps. So we do futher evaluation
## with 500 steps only. Otherwise we will not be finish anytime soon ;-)

## Next we check whether the initial proportion of defectors has an impact
## on the propC_end value.

exp04 <- read.csv("experiment_stable_coexistence-table_initial_0.4.csv")
exp05 <- read.csv("experiment_stable_coexistence-table_500steps.csv")
exp06 <- read.csv("experiment_stable_coexistence-table_initial_0.6.csv")

## Wir prüfen, ob wir davon ausgehen können, dass die Verteilung gleich bleibt.
## Zuerst ein F-Test und dann t.test zweiseitig
var.test(exp06$propC_end, exp04$propC_end)
t.test(exp06$propC_end, exp04$propC_end)

## Auch die stimmen überein. So können wir davon ausgehen, dass bei gegebener
## benefit und cost struktur immer ein festes verhältnis von propC_end entsteht

## Experiments changing cost from 0 to 1 (= cost-benefit-ratio in paper)


## Experiments cost-benefit-ratio with mixed strategy


## Task 3: This is the nash equilibrium after many steps:
nash <- read.csv("./extended_pd_mixed_strategy Frequency of strategies.csv")

## Plot the results of the simulation
ggplot(nash, aes(x = x, y=C, fill="red", colour="Cooperators")) + geom_line() + geom_line(aes(y=D, colour="Defectors"))+ labs(colour=NULL) + xlab("Ticks") + ylab("Proportion of Strategy")

## The values after the equil has been reached
nash_eq <- nash[10000:41796 , ]

## Get the mean; theoretical result is 0.60 and the actual mean
## is 0.596 which is the same ;-)
mean(nash_eq$D)

## Test the standard deviation, just to be sure that this is not
## too large; it is 0.01 that is rather small
sd(nash_eq$D)

## Assuming normal distribution (= random in netlogo hopefully
## gives normal distributed random values)

## Calculate the 1% confidende interval for the real value:
error <- qnorm(.995)*sd(nash_eq$D)/sqrt(31797)

## the left and the right "boundaries"
l <- m - error
r <- m + error

## The real value of the equilibrium is with 99% probability between
##  0.596188 and 0.5965448 Both will be rounded to 0.60
## This is a god result!

summary(nash_eq$D)

##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.5420  0.5884  0.5964  0.5964  0.6048  0.6468 
