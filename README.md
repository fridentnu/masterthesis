# masterthesis
This is the code used in the master thesis "Prediction Models of Systolic Blood Pressure Based on HUNT Study Data" by Fride Nordstrand Nilsen.
This repository mainly contains the code used to clean, explore and transform the data as described in Chapter 2, and to implement, inspect and evaluate the prediction models as described in this chapter. In addition there is some code used to create illustrative figures to explain the PIT diagrams in Chapter 3.5 in the file Illustrations.R. In general, we have used base R functions and functions from the ggplot2 and DataExplorer R-packages to create the figures in this thesis.

All of the code is self-written, except the code used to calculate the PAI-level, PAI.R, and the MVPA- score, MVPA.R. These files are written by Emma Ingström, a PhD student who also works with HUNT study data. The code in PAI.R and MVPA.R is based, respectively, on the papers http://www.sciencedirect.com/science/article/pii/S0033062018301890 and http://www.sciencedirect.com/science/article/pii/S000293431500786X. 

The code files used to clean and explore the data are named DataCleaning.R and EDA.R, respectively. The data transformation and implementation of the GLM models is located in Models.R. The modified Framingham model is implemented in Framingham.R. The code creating the figures of the residuals is located in the file Residuals, while the code exploring the prediction distributions of individual participants is in the file EvalParticipant.R. All the evaluation methods are implemented and applied to the prediction models in Evaluation.R.

Note that due to privacy reasons the data used in this thesis is not available on the github-webpage. However, the format of the original data is described in DataCleaning.R. 
