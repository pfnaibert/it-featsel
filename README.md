# Description

This repository contains the files of my paper (jointly with João Frois Caldeira) Index Tracking with Feature Selection.

# File

The pdf file is in [here](./it-featsel.pdf)

# Abstract

We compare the performance of passive investment portfolio with a small number of assets (sparse index-tracking portfolios) using different feature selection algorithms.
To isolate the effect of the selection methods, we separate the asset selection and the asset allocation phase.
In the asset allocation phase, we follow \cite{liu-2009}, and select minimum tracking error volatility portfolios.
The selection methods used are the backward stepwise selection, forward stepwise selection and the lasso.
Our results show that the lasso selection method outperforms the other methods, in the brazilian case.
It presents similar tracking error volatility and higher mean return, which leads to a better risk-adjusted performance.
In the american case, the lasso presents better risk-adjusted performance, but this is due to higher mean returns, not lower volatility.
This is undesirable in our case.
One highlight of this paper is that the forward and backward iteration algorithms (simple methods that receive little attention in the literature) perform well in selecting assets for index tracking.

