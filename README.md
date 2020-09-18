[![Project Status: usable - The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)

# TFM

Al estar alojado en la versión gratuita de shinyapps.io no se debería procesar datos de más de 20k filas x 40 columnas.
Es recomendable usar train y test de competiciones de Kaggle, por ejemplo, House Prices o Titanic.


# Tiempos

Con el dataset de House Prices (1461 x 81): 
  1. **Carga de datos:** Instantánea.
  2. **Estadísiticos básicos:** Instantánea.
  3. **EDA:** 12 Segundos para 85 gráficos.
  4. **Importancia de variables + Outliers:** 2 segundos.
  5. **ML:** 
  * Preprocesado básico: Yeojohnson, nuevas variables + Vtreat: 7.3 segundos.
  * Preporcesado con imputación Random Forest: 1 minuto 40 segundos. (Hay que tener en cuenta que hay 20 variables con NA).
  * Comparación de todos los modelos: 45 segundos. | Comparación base de Random Forest + Elastic Net: 10 segundos.
  * Tunear el XGB ~ 5 minutos y 10 segundos (Se entrenan 7 hyperparámetros). | Elastic Net 30 segundos (2) | Rando Forest: 2 minutos 4 hyperparámetors ...
  * Queda la implementación en paralelo que no se puede hacer en la versión gratuita de shiny.
    





![](/img/nutTFM.jpeg)

Top 12% en la competición de Kaggle: House Prices: Advanced Regression Techniques.
https://www.kaggle.com/c/house-prices-advanced-regression-techniques/overview

![](/img/kagglepng.png)

![](/img/prep.png)

![](/img/eda.png)

![](/img/fi.png)

![](/img/ml.png)

-
