# shinybatch

Package R facilitant la configuration, le lancement, et la récupération de résultats de *longs calculs*  lancés en mode *batch* depuis **shiny**

## Idées / composants principaux : 

1. une fonction générant un *.yml* de configuration avec toutes les informations nécessaire au lancement d'un calcul

2. une fonction permettant de lancer le calcul, qui se base sur le *.yml*

3. un module **shiny** pour configurer un lancement

4. un module **shiny** pour voir l'état d'avancement des calculs

