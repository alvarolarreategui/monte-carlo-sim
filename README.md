# monte-carlo-simulation
## Using Monte Carlo Simulation to develop a News Vendor Problem Application

*This project was developed for Gerogia Tech class ISyE 6644 Simulation in 2025. The file "Project Report Group 208.pdf" contains full details and conclusions*

**Monte Carlo simulation** (MCS) was primarily developed by S. Ulam in the 1940s by realizing “that electronic computers made it practical to apply statistical methods to functions without known solutions”. With current computational power, MCS can draw thousands of simulated samples that can be used to compute many statistics about each sample. Then, thanks to the Law of Large Numbers, it is possible to compute probabilities and confidence intervals for the expected values with precision that increases as the number of simulations increases.

The **News Vendor Problem** (NVP) is a classic problem in inventory management, that appeared in 1888 in the context of banking [3]. A newsboy must determine the number of newspapers to order for the next day, considering that if he orders too many he will incur in overage costs (papers left unsold), and if he orders too few, he will face underage costs (lost sales). The decision must be made daily, prior to the realization of the demand for the day.

The specific problem of **Optimizing the Daily Production of a Bakery** is approached from the perspective of the NVP, i.e. find the production amount Q that maximizes the expected profit. The approach here is to consider a **Compound Demand**, which is a general demand model that models the number of customers, e.g. N as a random variable and the demand of a specific item, e.g. X as another random variable (see for example [4]. The demand of a single item $k$ for a single day can be expressed as:

$$D_k = \sum_{i=1}^{N} X_{ik}$$

Contrary to the Single Period Problem (SPP), the model above has no easy analytical solution in the general case [4]. An alternative is to find Q by simulation, which is the approach here. By generating many realizations of the random variables that compose the demand, we will compute the daily profits for a business cycle, say a quarter or year, then average them. By doing this for many replications, the Law of Large Numbers says the average profit is normally distributed. This allows us to have a point estimator of the expected profit as well as a confidence interval.

Shiny App:

<img width="2366" height="1297" alt="image" src="https://github.com/user-attachments/assets/1f5d6b5e-f412-42f6-b27b-a572def1d730" />

Shiny APP INSTRUCTIONS:

- To run the app,

	- you will need an R installation

	- make sure you have both files app.Rand helpers.R in the shiny folder

	- run app.R
