# Red-de-comercio-mundial
Caracterización de la red de comercio mundial y sus determinantes mediante el análisis de las características estructurales, un modelo de grafos aleatorios exponenciales (ERGM) y un modelo de Bloques estocásticos (EBM).

## La red de comercio internacional y sus determinantes: Un análisis del impacto de la pandemia de COVID-19 desde la perspectiva de datos relacionales.

El archivo ZIP contiene las bases de datos y los códigos de rStudio necesarios para reproducir o replicar los resultados obtenidos en el artículo.
El archivo, se compone por dos carpetas, tituladas: 

	1). Datos: Esta carpera contiene cuatro bases de datos: 
		- Initial_data_all_countrys: Contiene los datos de las importaciones y exportaciones bilaterales para los años 2018, 2020 y 2022 extraídas del
		  WITS.
		- P_Data_Extract_From_World_Development_Indicators: Contiene los datos de los atributos nodales empleados en el estudio para los años 2018		  2020 y 2022 extraídos de la base de datos de Indicadores de Desarrollo del Banco Mundial.
		- Cualitative_variables: Contiene los datos de los atributos nodales empleados en el estudio para los años 2018, 2020 y 2022 extraídos de la 
		  base de datos del CEPII.
		- country_codes: Contiene los códigos ISO3 de los países reconocidos de por la ONU.

	2). Códigos: Esta carpeta contiene los códigos de rStudio empleados en el artículo; para reproducción y réplica de los resultados, ejecútese los 	    scripts en el orden expuesto a continuación: 
		(i)   Datos_estadisticas_redes_2018_2020_2022: Este script realiza la depuración y manipulación de datos necesaria para la posterior ejecución 
		      de los modelos y metodologías. Adicionalmente, este script contiene los códigos empleados para el cálculo de las estadísticas de la red 
		      y la elaboración de los grafos.
		(ii)  Modelo_ERGM_redes_2018_2020_2022: Contiene el código empleado para el ajuste del modelo ERGM. De igual forma, este script almacena los
		      códigos empleados para las simulaciones y el cálculo de la bondad de ajuste.
		(iii) Modelo_Bloques_estocasticos_2018_2020_2022: Contiene el código empleado para el ajuste del modelo de bloques estocásticos (SBM).
iales (ERGM) y un modelo de Bloques estocásticos (EBM).
