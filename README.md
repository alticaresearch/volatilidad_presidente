Volatilidad seccional 2018-2024, elección presidencial. 

Este repositorio contiene los siguientes archivos:

1). Código en R para calcular la volatilidad electoral en cada sección con base en 
	a. Reglas de distribución de votos en coalición:https://portalanterior.ine.mx/2015/PREP/CentroDeAyuda/Normatividad/partidos_coaligados.html
	b. Volatilidad electoral partidos estables, partidos salientes y entrantes (Powell y Tucker, 2013)

	El código permite desagregar el voto de las diferentes combinaciones de una coalición según el Artículo 311, inciso C. LEGIPE. El porcentaje de voto calculado por partido es el efectivo (sin nulos ni registrados). Posteriormente genera el cálculo de la volatilidad tipo A (partidos nuevos y salientes) y de la volatilidad tipo B (partidos estables). La suma de ambas volatilidades es igual a la volatilidad total o índice de Pedersen (IP). El IP va de 0 a 100, donde 0 indica que no hubo cambio de votantes y 100 implica que todos los votantes de una sección cambiaron el sentido de su voto. 

2). Artículo "Revisiting Electoral Volatility in PostCommunist Countries: New Data, New Results and New Approaches"
