# ARFIMA-Time-Series-Analysis-in-R

## Goal:
This study forecasts the direction that Brent spot prices will take during the course of the year following the 2022 Ukraininan conflict,  making use of ARFIMA parametric and semiparametric methods. According to the long memory of the series, prices are to fall steeply during the next few months, reaching up to 80 or 75 dollars by the end of March 2023.

## Data and Method
The data used  are the real monthly spot prices of Brent crude oil for the period 1988.01 - 2022.03, obtained by deflation with the consumer price index of the US dollar, base year 2015. Data on nominal prices and CPI were taken from the Federal Reserve economic data website based on their availability (Federal Reserve Bank of St. Louis, 2022).

![image](https://user-images.githubusercontent.com/101098099/222784267-637822f3-fd55-4f51-be25-3954f8ca493a.png)

Fractal studies of Brent prices have been conducted before with favorable results (Abdollahi & Ebrahimi, 2020; Al Gounmeein & Ismail, 2021; Jibrin & al, 2015). The model to be applied is the fractional ARIMA, which differentiates with a positive fractal number between 0 and 0.5 in order to achieve stationarity yet avoid overdifferentiation, as some series with long-term memory that are still differentiated at lag one may lose information useful for prediction (Granger & Joyeux, 1980; Hosking, 1981). In order to identify whether a series has long memory the Hurst exponent can be calculated via R/S analysis, where values of  H between 0.5 and 1 indicate long memory, while the ACF graph of the series shoud show a slow hiperbolic decrease. Then, the non-stationary long memory process can then be reduced to stationarity by applying the ARFIMA model.


## Results

The long term memory of the series is visible through the hiperbolic decrease of coefficients in ACF, as well as by looking at the values of Hurst exponent, between 0.5 and 1.
![image](https://user-images.githubusercontent.com/101098099/222784760-87cdb8f3-2651-4da0-8a9c-500e849fd3c3.png)
<div align="center">
    <img src="https://user-images.githubusercontent.com/101098099/222784890-add6d607-cb26-472a-bed2-5d7a42ba5998.png">
</div>

The non-stationary long memory process can then be reduced to stationarity by applying the ARFIMA model. For training the models, both a sample with data up to 2019 and the entire set of data were used. The parametric method requieres a first estimation of d, which is 0.499 for the training set consisting of all data. Applying the difference and visualizing the ACF and PACF diagrams led to the identification of the three potential models in the table below. All parameters are estimated simultaneously while running the models.
![image](https://user-images.githubusercontent.com/101098099/222830612-354450a0-f351-4eb8-8b2b-e893fab0c490.png)
The optimal ARFIMA model (3, 0.448, 0) with the lowest AIC was chosen. The model’s residuals pass the Ljung-Box test with p-value > 0.1, meaning they do not show autocorrelation, and don’t present heteroscedasticity according to ARCH-LM. However, they aren’t normally distributed according to Jarque-Berra test. The model can be written as: 
![image](https://user-images.githubusercontent.com/101098099/222830782-8a1a434b-ffe7-40d2-948a-db6e8736c136.png)

The semiparametric method involves calculating the mean of estimators like Gweke dan Porte-Hudak (GPH)Smoothed, GPH (Sperio)and R/S, and using this value to differenciate the series.Like this, I obtained the degree of fractal differentiation of 0.197. Similarly to the parametric method, I identified the best model by fitting the AR and MA parameters and looking at AIC, BIC as well as the tests on residuals. The best fit was ARFIMA(3,0.197,0) .

The same proceedures have been applied to a training set ending in 2019, with the results being visible in figure 3 alongside the forecasts resulted from the whole data analysis.

The RMSE favors the model built by the parametric method. This method indicates a downward trend of price in the next year, with a less steeper slope than that associated with the semiparametric method for the first six months forecasted. After august, the semiparametric method suggests a decrease in the rate of price decline, which will lead to a stabilization of the price around $ 83. The forecast for March 2023, one year after the outbreak of the armed conflict in Ukraine, is $ 80.72. The parametric method, however, suggests a price decline up to  $ 75 in March.

![image](https://user-images.githubusercontent.com/101098099/222831589-32adfde5-fb35-4210-bb0f-ad1858ff4abf.png)








