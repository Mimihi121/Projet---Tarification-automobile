# Projet---Tarification-automobile
Projet de tarification automobile


Projet de tarification automobile-Machine Learning sous Python
Projet de tarification automobile Présentation Ce projet vise à modéliser la fréquence et le coût moyen des sinistres à l’aide de trois modèles de machine learning : Random Forest Regressor, Gradient Boosting Regressor et HistGradientBoosting Regressor.
Les données, issues du package CASdatasets, permettent d’analyser séparément la fréquence des sinistres (nombre de sinistres par contrat) et leur coût moyen. La fréquence présente une distribution relativement stable, tandis que le coût moyen est très dispersé et asymétrique, en raison de sinistres exceptionnellement élevés.
Ces valeurs extrêmes influencent fortement les indicateurs de performance (comme le MSE) et peuvent conduire à un R² négatif, traduisant la difficulté du modèle à prédire correctement les sinistres rares mais coûteux.
Pour améliorer la précision, nous proposons une segmentation des variables quantitatives et qualitatives en trois catégories de risque — petit, moyen et grand risque — afin de constituer trois sous-bases de données distinctes. Sur chacune d’elles, la prime pure est ensuite recalculée (sous R).
