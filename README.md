# Elbe oxygen prediction

Here we present an empirical model implemented in `R` to predict oxygen hypoxia in the river Elbe.
This repository includes:
  - `get_data.sh` to download air temperature data from German Weather Service (DWD; https://opendata.dwd.de), and water temperature and dissolved oxygen concentration from several stations of the Wasserstraßen- und Schifffahrtsverwaltung des Bundes (WSV; https://kuestendaten.de).
  - `lagged_model.R` to fit and run an auto-regressive model to predict dissolved oxygen concentrations and hypoxia.   

## Requirements

`get_data.sh` requires a system with working `wget`, `unzip`, and `sed` functions.

`lagged_model.R` is an `R` implementation that depends on the following libraries: 
  - dplyr https://dplyr.tidyverse.org/
  - tidyr https://tidyr.tidyverse.org/
  - caret [https://cran.r-project.org/package=caret](https://cran.r-project.org/package=caret)
  - lubridate https://lubridate.tidyverse.org/
  - TeachingDemos [https://CRAN.R-project.org/package=TeachingDemos](https://cran.r-project.org/package=TeachingDemos)


## About the method: Auto-regressive model for hypoxia prediction

An auto-regressive model (ARM) is used to simulate a response variable (either oxygen concentration the i-th day $O_i$ or the odds ratios of observing an hypoxic event, i.e., when $O_i<4$ mg L $^{-1}$
as function of the values of oxygen, water- and air-temperature ($O$, $T_w$, and $T_a$, respectively) recorded from the day $i-n-m$ to the day $i-n$:

$$
 \text{ARM} = \sum_{j=i-n-m}^{i-n} \left[ \alpha_{O,j} O_j + \alpha_{w,j} {T_w}_j + \alpha_{T_a,j} {T_a}_j \right] + \beta.
$$

The summation indexes in the later indicate an observation window of $m$ days counted backwards from the forecast horizon, which is $n$ days before the present (i.e., the $i$-th day).
The coefficients $\alpha$ are regression coefficients indicating the relative importance of each lagged variable, and $\beta$ is a constant. 
Oxygen concentration was modeled with linear regression, and hypoxia probability with logistic regression, using ARM as a binary predictor. 
Logistic regression is well-suited for binary outcome data, where the target variable follows a Bernoulli distribution---taking the value 1 with probability $p$ and 0 with probability $1 - p$. 
Here, $p$ represents the probability of observing an hypoxic event (DO $4<$ mg L $^{-1}$). 
Logistic models assume that the log-odds of $p$ are a linear function of driving variables, which follow the ARM, thus

$$
    \log\left(\frac{p}{1-p}\right) = \text{ARM}.
$$

## License
CC-BY-4.0
