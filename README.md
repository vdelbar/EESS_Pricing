### EESS_Pricing

Repositorio que contiene el TFM relativo a un sistema de ayuda en la fijación de precios de las estaciones de servicio.

https://vdelbar.shinyapps.io/_PFM/


## INTRODUCCION

La finalidad de todo trabajo de fin de master que se precie debiera ser el poder asentar, el tratar de llevar a la practica todo lo aprendido durante el mismo con un ejemplo de negocio real.

En el caso que nos ocupa, el problema a resolver sería el de definir una herramienta que permita a las estaciones de servicio el estudio de los precios circundantes en aras de poder discernir cual de las 5 estaciones más cercanas influye de forma más clara en el rendimiento de la propia estación.


## DATOS DE ENTRADA

La fuente principal de los datos de entrada del proceso son los extraídos desde la página http://geoportalgasolineras.es/#/Descargas y consiste en un listado diario de todas las gasolineras existentes así como su localización y precios.

Además de esta descarga existen otras descargas (semanales) en formato PDF con los litros de las gasolineras EasyFuel.

Y por último, otra hoja excel de caracter privado donde se han podido encontrar los precios históricos del primer trimestre aunque la calidad de los mismos haya dejado mucho que desear, dado el caracter discontinuo de los mismos.


## METODOLOGÍA

El procesamiento de datos consiste basicamente en importar las estaciones de servicio de la descarga de Geoportal, el importar los datos de precios históricos y por último, importar el archivo que recoge las cercanías de todas las EESS.

Despues de estas cargas, se procede a rellenar el Front-End para su visionado con Shiny.


## RESUMEN DEL RESULTADO

Dada la poca calidad de los datos obtenidos y sobre todo la imposibilidad de generar una regresión lineal en base a los litros de una gasolinera en concreto, se ha optado por relacionar unicamente los precios obteniendose resultados prometedores como puede verse a continuación,

Call:
lm(formula = original ~ p1 + p2 + p3, data = selected)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.026573 -0.003715  0.001270  0.003082  0.014253 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.34998    0.09718   3.601 0.000613 * * *
p1           0.40558    0.15566   2.606 0.011356 *  
p2           0.18112    0.11917   1.520 0.133410    
p3           0.02886    0.17345   0.166 0.868389    
---
Signif. codes:  0 ‘* * *’ 0.001 ‘* *’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.006854 on 65 degrees of freedom
Multiple R-squared:  0.5454,	Adjusted R-squared:  0.5244 
F-statistic: 25.99 on 3 and 65 DF,  p-value: 3.635e-11


## FRONT END

Si hay algo que distingue y aporta valor al proyecto es su facilidad de uso gracias a Shiny. 

Como apuntaba en el apartado anterior, y gracias precisamente a esa facilidad de uso, el proyecto ha sentado las bases de una herramienta muy útil para la gestión de precios de las EESS, siendo mi intención el seguir trabajando en ella a nivel profesional para poder mejorar el histórico de precios y poder incorporar los litros de cada estación de servicio usuaria de la aplicación.


Madrid a 7 de julio de 2017
