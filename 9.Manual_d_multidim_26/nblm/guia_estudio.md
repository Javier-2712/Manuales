# Guía de Estudio: Diversidad Multidimensional de la Biodiversidad (Marco A1-A2-B1-B2)

Esta guía sintetiza los conceptos fundamentales, metodologías y aplicaciones prácticas para el estudio de la biodiversidad en sus dimensiones taxonómica (TD), funcional (FD) y filogenética (PD). El documento se organiza siguiendo el marco analítico **A1-A2-B1-B2**, estructurado en fundamentos compartidos, diversidad funcional y diversidad filogenética.

---

## Sección 0: Fundamentos Compartidos
Estos conceptos transversales sostienen la arquitectura del análisis multiescalar y multidimensional de la biodiversidad.

### 1. Multidimensionalidad y Multiescalaridad
*   **Multidimensionalidad:** Define la dimensión biológica bajo estudio: taxonómica, funcional o filogenética.
*   **Multiescalaridad:** Define la escala espacial de la diversidad:
    *   **Diversidad Alfa ($\alpha$):** Diversidad promedio en comunidades locales.
    *   **Diversidad Gamma ($\gamma$):** Diversidad total acumulada en una región o pool de especies.
    *   **Diversidad Beta ($\beta$):** Grado de variación o recambio en la composición entre comunidades locales. Según la descomposición multiplicativa de Whittaker: $\gamma = \alpha \times \beta$. Para que esta partición sea útil, los componentes deben ser independientes entre sí.

### 2. Números de Hill y Marco Unificado (Chao et al. 2021)
Representan el "número efectivo de especies" (o linajes/grupos) igualmente abundantes. El parámetro **q** controla la sensibilidad a la abundancia:
*   **q = 0 (Riqueza):** Sensible a especies raras; ignora abundancias.
*   **q = 1 (Shannon):** Pondera especies proporcionalmente a su abundancia.
*   **q = 2 (Simpson):** Pondera especies dominantes; insensible a las raras.

### 3. Estandarización y Cobertura de la Muestra (SC)
La **Cobertura de la Muestra** es una medida estadística de la completitud del inventario basada en la fracción de abundancia detectada.
*   **Rarefacción/Extrapolación:** Permite comparar comunidades con diferente esfuerzo de muestreo. La extrapolación se considera confiable hasta el doble del tamaño de la muestra de referencia ($2n$).
*   **Comparación por Cobertura:** A diferencia de la comparación por tamaño físico, esta asegura una representatividad biológica equitativa entre sitios.

---

## Sección 1: Diversidad Funcional (FD) - Semanas 8–10 [LISTO]

### A1: Estimadores Clásicos de FD (Villéger et al. 2008)
Se basan en la distribución de las especies en un espacio funcional multidimensional.
*   **Riqueza Funcional (FRic):** Volumen del espacio funcional (convex hull) ocupado por los rasgos de las especies.
*   **Equidad Funcional (FEve):** Regularidad de la distribución de abundancias en el volumen funcional; utiliza el árbol de expansión mínima (MST).
*   **Divergencia Funcional (FDiv):** Distancia de las especies más abundantes respecto al centro de gravedad del espacio funcional.
*   **Identidad Funcional (CWM):** Composición funcional promedio de un rasgo en la comunidad.

### A2: Entropía Cuadrática de Rao (RaoQ)
Mide la disimilitud promedio esperada entre dos individuos seleccionados al azar. Integra abundancia y distancia funcional, sirviendo de base para una partición aditiva ($\alpha$, $\beta$, $\gamma$) coherente entre dimensiones.

### B1: Hill-Chao FD Alfa (iNEXT.3D)
Introduce el concepto de **"especie virtual"** o grupo funcional definido por un umbral de similitud de rasgos ($\tau$). El marco resuelve la arbitrariedad de seleccionar un solo umbral calculando el área bajo la curva (AUC) del perfil de diversidad funcional.

### B2: Hill-Chao FD Beta (iNEXT.beta3D)
Permite medir el recambio neto de atributos biológicos:
*   **Cq (Sørensen):** Proporción efectiva de atributos ausentes respecto al promedio local.
*   **Uq (Jaccard):** Fracción de atributos no compartidos respecto al pool regional.

---

## Sección 2: Diversidad Filogenética (PD) - Semana 11+ [BORRADOR]

### A1: Estimadores Clásicos Candidatos
*   **PD de Faith (1992):** Suma de las longitudes de las ramas de un árbol filogenético que conectan a un conjunto de especies.
*   **Señal Filogenética (Estadístico K):** Grado en que la filogenia predice la similitud ecológica; justifica el uso de PD como proxy de FD.
*   **EDGE:** Combina distintividad evolutiva (ED) y estado de amenaza global (GE) de la IUCN para priorizar la conservación.
*   **Endemismo Ponderado (WE):** Riqueza donde cada especie se pondera inversamente por su rango geográfico.

### B1: Hill-Chao PD Alfa Candidato
Utiliza el **tiempo de corte (T)** o profundidad de referencia del árbol filogenético.
*   **qPD(T):** Número efectivo de linajes igualmente divergentes.
*   **Diversidad Filogenética Media ($qPD\text{-bar}(T)$):** Resultado de dividir $qPD(T)$ por el tiempo $T$, permitiendo comparaciones con la escala taxonómica.

### Temas Avanzados y Puente FD-PD
*   **Trait Evolutionary History (TEH):** Integra el camino evolutivo de los rasgos mediante el **Tempo** (tasa de cambio acumulado) y el **Modo** (patrón de distribución de cambios).
*   **Phylomorphospace:** Proyección de una filogenia sobre un espacio de ordenación de rasgos funcionales.

---

## Práctica: Quiz de Respuestas Cortas

1.  **¿Qué mide la Entropía Cuadrática de Rao (Q)?**
    *Respuesta:* La disimilitud promedio esperada entre dos individuos seleccionados al azar de una comunidad.
2.  **En iNEXT.3D, ¿hasta qué límite es confiable la extrapolación para la riqueza (q=0)?**
    *Respuesta:* Hasta el doble del tamaño de la muestra de referencia ($2n$).
3.  **¿Qué técnica geométrica utiliza el índice FRic?**
    *Respuesta:* El volumen del casco convexo (convex hull).
4.  **¿Qué representa la unidad "equivalentes de especies/linajes"?**
    *Respuesta:* Una unidad común que permite comparar directamente las dimensiones taxonómica, filogenética y funcional.
5.  **¿Cuál es la diferencia entre curvas de rarefacción basadas en tamaño y en cobertura?**
    *Respuesta:* Las de tamaño comparan por esfuerzo físico (n), mientras que las de cobertura comparan por representatividad biológica.
6.  **¿Qué indica el índice de Turnover de Sørensen ($V_q$) en iNEXT.beta3D?**
    *Respuesta:* El recambio neto de atributos generado puramente por reemplazo geográfico, controlado por la riqueza local.

---

## Preguntas de Ensayo para Deeper Exploration

1.  **Análisis de Estandarización:** Explique por qué comparar la riqueza de especies observada entre dos comunidades con diferente esfuerzo de muestreo puede llevar a conclusiones erróneas. Proponga cómo el marco iNEXT.3D soluciona este problema mediante la cobertura de muestra.
2.  **Degradación en la Amazonia:** Discuta el hallazgo donde la diversidad taxonómica permanece estable mientras la funcional y filogenética declinan ante la degradación antrópica. ¿Qué implicaciones tiene esto para las estrategias de conservación basadas únicamente en conteos de especies?
3.  **Sucesión Ecológica:** Basándose en el estudio de sucesión en Costa Rica, analice el concepto de "limitación de fuente". ¿Cómo puede la ecología de la restauración utilizar este conocimiento para acelerar la recuperación de la diversidad funcional?

---

## Glosario de Términos Importantes

| Término | Bloque | Definición |
| :--- | :--- | :--- |
| **Convex Hull** | A1-FD | Polígono convexo mínimo que encierra todos los puntos en un espacio funcional. |
| **Especie Virtual** | B1-FD/PD | Categoría formada por especies agrupadas bajo un umbral de similitud de rasgos o distancia filogenética. |
| **EDGE** | A1-PD | Índice que combina la distintividad evolutiva y el estado de amenaza IUCN. |
| **Extrapolación** | Fundamentos | Predicción estadística de la diversidad para un tamaño de muestra mayor al observado. |
| **Números de Hill** | Fundamentos | Familia de índices de diversidad expresados en unidades de "número efectivo de especies". |
| **qPD-bar(T)** | B1-PD | Diversidad filogenética de Hill dividida por el tiempo de corte T; permite comparar con la escala taxonómica. |
| **Rarefacción** | Fundamentos | Submuestreo analítico para estandarizar comunidades a un tamaño o cobertura común. |
| **Matrices Dispersas** | A1-PD | Estructuras de datos (usadas en `phyloregion`) que almacenan solo valores distintos a cero para optimizar memoria. |
| **Distancia de Gower** | A1-FD | Coeficiente de disimilitud que permite combinar rasgos cualitativos, cuantitativos y binarios. |