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

