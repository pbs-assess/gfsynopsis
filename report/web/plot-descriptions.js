"use strict";

const TEXT = {
  en: {
    title: "Plot descriptions · BC Groundfish Data Synopsis",
    description: "Descriptions of the figures in the British Columbia groundfish data synopsis.",
    skip: "Skip to plot descriptions",
    eyebrow: "Data synopsis",
    siteTitle: "British Columbia groundfish",
    navigation: "Site navigation",
    back: "← Species synopsis",
    language: "Page language",
    heading: "Plot descriptions",
    intro: "This section provides captions for each of the visualizations that form the species-by-species pages. Petrale Sole is used as an example species for all plots except for commercial catch per unit effort maps where Pacific Cod is used."
  },
  fr: {
    title: "Description des graphiques · Synopsis des données sur les poissons de fond de la C.-B.",
    description: "Description des figures du synopsis des données sur les poissons de fond de la Colombie-Britannique.",
    skip: "Passer à la description des graphiques",
    eyebrow: "Synopsis des données",
    siteTitle: "Poissons de fond de la Colombie-Britannique",
    navigation: "Navigation du site",
    back: "← Synopsis des espèces",
    language: "Langue de la page",
    heading: "Description des graphiques",
    intro: "Cette section fournit les légendes de chacune des visualisations qui composent les pages par espèce. La sole de Petrale sert d’espèce d’exemple pour tous les graphiques, sauf les cartes des captures commerciales par unité d’effort, qui utilisent la morue du Pacifique."
  }
};

const FRENCH_HEADINGS = [
  "Tendances des indices de biomasse relative provenant des relevés",
  "Cartes de la biomasse relative provenant des relevés",
  "Captures de la pêche commerciale",
  "Indices de capture par unité d’effort du chalutage de fond commercial",
  "Cartes des captures commerciales par unité d’effort",
  "Échantillons biologiques disponibles",
  "Données de composition selon la longueur",
  "Données de composition selon l’âge",
  "Ajustements des modèles longueur-âge et longueur-poids",
  "Fréquence de maturité par mois",
  "Ogives de maturité"
];

const FRENCH_ALTS = [
  "Exemple de tendances des indices de biomasse relative provenant des relevés pour la sole de Petrale",
  "Exemple de cartes de la biomasse relative provenant des relevés pour la sole de Petrale",
  "Exemple de graphique des captures commerciales pour la sole de Petrale",
  "Exemple d’indice de capture par unité d’effort du chalutage de fond commercial pour la sole de Petrale",
  "Exemple de cartes des captures par unité d’effort du chalut et de la ligne pour la morue du Pacifique",
  "Exemple du nombre de spécimens biologiques provenant des relevés et des pêches commerciales pour la sole de Petrale",
  "Exemple de données de composition selon la longueur pour la sole de Petrale",
  "Exemple de données de composition selon l’âge pour la sole de Petrale",
  "Exemple d’ajustements des modèles longueur-âge et longueur-poids pour la sole de Petrale",
  "Exemple de fréquence de maturité par mois pour la sole de Petrale",
  "Exemple d’ogives de maturité selon l’âge et la longueur pour la sole de Petrale"
];

const FRENCH_CAPTIONS = [
  "Exemple de tendances des indices de biomasse relative provenant de relevés au chalut et à la palangre pour la sole de Petrale. Les points représentent les estimations moyennes fondées sur le plan de relevé et les lignes verticales, leurs intervalles de confiance bootstrap à 95 %. Une ligne pleine et un ruban ombré indiquent un indice standardisé au moyen d’un modèle spatiotemporel. Ces indices sont omis si moins de 5 % des traits contiennent l’espèce ou si aucun modèle ne converge. Le « CV moyen » est la moyenne des coefficients de variation annuels; les « traits positifs moyens » comparent le nombre moyen de traits ayant capturé l’espèce au nombre moyen de traits. Les axes verticaux vont de zéro à la limite supérieure maximale de l’intervalle de confiance. Lorsque les deux indices sont présentés, ils sont ramenés à la même moyenne géométrique. Pour le relevé MSSM WCVI, les années antérieures à 2003 sont grisées parce que les captures sont moins fiables. Les valeurs HBLL OUT, HBLL INS et IPHC FISS représentent l’abondance plutôt que la biomasse. Des indices couvrant toute la côte ou tout le relevé sont aussi présentés pour les séries synoptiques, HBLL OUT et HBLL INS.",
  "Exemples de cartes de biomasse relative, ou de taux de capture, tirées des plus récentes années disponibles des relevés au chalut et à la palangre pour la sole de Petrale. Les relevés synoptiques figurent à gauche, les relevés à la palangre sur fond dur extérieur au centre et le relevé IPHC FISS à droite. Les croix pâles représentent les traits sans capture; la superficie des cercles est proportionnelle à la densité observée. Sauf pour l’IPHC, les couleurs montrent les prédictions d’un modèle spatial comprenant la profondeur, son carré et des effets spatiaux aléatoires. L’échelle de couleurs, transformée par racine quatrième, va de zéro au maximum de chaque carte; le gris foncé signale les zones où le modèle n’a pu être ajusté. Les cartes synoptiques et HBLL montrent la biomasse prédite dans le domaine du relevé, tandis que la carte IPHC présente les données brutes aux stations fixes. Les relevés comptant moins de 2 % de traits positifs ne sont pas modélisés. Les moyennes au bas des cartes proviennent des données brutes de toute la côte. Pour l’IPHC, l’unité est le poisson par raie effective, soit 100 hameçons circulaires espacés de 18 pieds. La côte est tournée de 40°; les isobathes indiquent 100, 200 et 500 m.",
  "Exemples de captures de la pêche commerciale pour la sole de Petrale. Les couleurs distinguent les types d’engins et les captures correspondent au poids total des débarquements par année. Les rejets déclarés de toutes les pêches sont inclus, sauf ceux jugés moins fiables avant la couverture complète des observateurs du chalut en 1996 et avant l’intégration des autres pêches en 2006. Ces périodes sont grisées. Les estimations des flottilles de chalut dans les eaux extérieures sont moins certaines avant 1996, tout comme celles des secteurs de la palangre et du casier avant la surveillance électronique instaurée en 2006. Depuis l’interruption du programme d’observateurs en mer en 2020, la flotte de chalut est surveillée électroniquement, avec un échantillonnage biologique au port pour certaines espèces. Les zones de gestion apparaissent dans le coin supérieur gauche de chaque panneau.",
  "Exemple de tendances des captures commerciales par unité d’effort (CPUE) du chalutage de fond, l’effort étant mesuré en heures de chalutage, pour la sole de Petrale. Les CPUE sont standardisées au moyen d’un modèle spatiotemporel. La ligne et la zone ombrée représentent respectivement la moyenne et son intervalle de confiance à 95 %. Les séries standardisées sont mises à l’échelle pour partager la même valeur maximale de l’intervalle de confiance. Les zones de gestion sont indiquées dans le coin supérieur gauche de chaque panneau.",
  "Exemples de cartes des CPUE commerciales au chalut et à la ligne pour la morue du Pacifique; cette espèce remplace la sole de Petrale, dont le panneau de pêche à la ligne serait presque vide. Une teinte plus claire indique une moyenne géométrique plus élevée des CPUE dans une cellule hexagonale. L’échelle est transformée par racine quatrième. Les cellules, larges de 7 km, ne sont affichées que si au moins trois navires distincts y ont pêché. Pour le chalut de fond, la CPUE est le poids des prises, débarquements et rejets compris, divisé par les heures de pêche des traits positifs. Les données depuis 2013 sont remplies; celles de 2007 à 2012 sont représentées par des hexagones gris pâle en contour. Pour la ligne, la CPUE est le nombre de poissons débarqués ou rejetés par trait et les données commencent en 2008. La côte est tournée de 40°; les isobathes indiquent 100, 200 et 500 m.",
  "Exemple de disponibilité des spécimens pour la sole de Petrale. Les cellules indiquent le nombre de poissons dont la longueur, le poids, la maturité ou l’âge ont été mesurés, ainsi que les structures disponibles pour la détermination de l’âge. Le panneau supérieur regroupe tous les relevés et le panneau inférieur toutes les flottilles commerciales. Une cellule vide signifie qu’aucune donnée n’est disponible pour la combinaison année-mesure. L’intensité de la teinte reflète le nombre relatif de spécimens et la cellule affiche ce nombre sous forme arrondie.",
  "Exemple de fréquences de longueur pour la sole de Petrale. Les femelles sont représentées par des barres colorées ou noires et les mâles, à l’arrière-plan, par des barres gris clair. Le nombre de poissons mesurés pour chaque relevé et chaque année figure dans le coin supérieur gauche du panneau. Un histogramme n’est affiché que si plus de 20 poissons ont été mesurés. Les mâles et les femelles des prises commerciales sont regroupés, car de nombreux poissons ne sont pas sexés. Voir la figure 3 pour les abréviations des relevés.",
  "Exemple de fréquences d’âge pour la sole de Petrale. Les femelles sont représentées par des cercles colorés ou noirs et les mâles, à l’arrière-plan, par des cercles gris clair. Le nombre de poissons dont l’âge a été déterminé figure en haut des panneaux. Des diagonales espacées de cinq ans facilitent le suivi des cohortes. La fenêtre montre les 15 années les plus récentes pour lesquelles des données d’âge existent. Voir la figure 3 pour les abréviations des relevés. Des comparaisons de la précision des lectures d’âge sont fournies à l’annexe A d’Anderson et coll. (2019).",
  "Exemple d’ajustements des modèles longueur-âge et longueur-poids pour la sole de Petrale. La croissance longueur-âge suit un modèle de von Bertalanffy avec erreur log-normale; la relation longueur-poids est ajustée sur l’échelle logarithmique avec une loi t de Student à trois degrés de liberté, robuste aux valeurs aberrantes. Les lignes noires pleines représentent les femelles et les lignes grises tiretées les mâles. Le texte présente les paramètres estimés et les cercles gris ouverts, les poissons individuels utilisés pour l’ajustement. Toutes les données de relevé sont incluses. Voir l’annexe H d’Anderson et coll. (2019) pour les détails des modèles.",
  "Exemple de fréquence de maturité par mois pour la sole de Petrale. Les catégories vont du stade le moins mature, en haut, au plus mature, en bas; après leur maturation, les poissons parcourent les différents stades matures. La superficie d’un cercle correspond au nombre de spécimens dans une catégorie et un mois donnés. Les femelles sont indiquées en noir et les mâles en gris clair à l’arrière-plan. Le nombre total de spécimens par mois figure au-dessus du graphique. Les échantillons commerciaux et ceux des relevés sont inclus.",
  "Exemples d’ogives de maturité selon l’âge et la longueur pour la sole de Petrale. Des régressions logistiques sont ajustées aux poissons classés comme matures ou immatures. Les lignes noires pleines représentent les femelles et les lignes grises tiretées les mâles; les lignes verticales indiquent l’âge ou la longueur à 50 % de maturité. Le texte donne les valeurs estimées à 5, 50 et 95 % pour les femelles et les mâles. Un modèle n’est affiché que si les données comprennent au moins 20 poissons matures et 20 immatures de chaque sexe. Les petits traits en haut et en bas représentent jusqu’à 1 500 poissons choisis au hasard. Tous les échantillons de relevé sont utilisés, quelle que soit la saison. Voir l’annexe H d’Anderson et coll. (2019)."
];

const englishHeadings = [...document.querySelectorAll(".plot-description h3")].map((element) => element.textContent);
const englishAlts = [...document.querySelectorAll(".plot-description img")].map((element) => element.alt);
const englishCaptions = [...document.querySelectorAll(".plot-description__caption")].map((element) => element.textContent);

function requestedLanguage() {
  return new URL(window.location.href).searchParams.get("lang") === "fr" ? "fr" : "en";
}

function render(language, historyMode = "replace") {
  const copy = TEXT[language];
  document.documentElement.lang = language;
  document.title = copy.title;
  document.querySelector('meta[name="description"]').content = copy.description;
  document.querySelector(".skip-link").textContent = copy.skip;
  document.querySelector(".eyebrow").textContent = copy.eyebrow;
  document.querySelector(".site-header h1").textContent = copy.siteTitle;
  document.querySelector("#descriptions-navigation").setAttribute("aria-label", copy.navigation);
  document.querySelector("#species-synopsis-link").textContent = copy.back;
  document.querySelector("#descriptions-language").setAttribute("aria-label", copy.language);
  document.querySelector(".descriptions-page__header h2").textContent = copy.heading;
  document.querySelector(".descriptions-page__header p").textContent = copy.intro;

  document.querySelectorAll(".plot-description h3").forEach((element, index) => {
    element.textContent = language === "fr" ? FRENCH_HEADINGS[index] : englishHeadings[index];
  });
  document.querySelectorAll(".plot-description img").forEach((element, index) => {
    element.alt = language === "fr" ? FRENCH_ALTS[index] : englishAlts[index];
    element.src = element.src.replace(/\/plot-descriptions\/(?:en|fr)\//, `/plot-descriptions/${language}/`);
  });
  document.querySelectorAll(".plot-description__caption").forEach((element, index) => {
    element.textContent = language === "fr" ? FRENCH_CAPTIONS[index] : englishCaptions[index];
  });

  document.querySelector("#descriptions-english").setAttribute("aria-pressed", String(language === "en"));
  document.querySelector("#descriptions-french").setAttribute("aria-pressed", String(language === "fr"));

  const url = new URL(window.location.href);
  if (language === "fr") url.searchParams.set("lang", "fr");
  else url.searchParams.delete("lang");
  if (historyMode === "push") window.history.pushState({}, "", url);
  else if (historyMode === "replace") window.history.replaceState({}, "", url);

  const backUrl = new URL("index.html", window.location.href);
  const species = url.searchParams.get("species");
  if (species) backUrl.searchParams.set("species", species);
  if (language === "fr") backUrl.searchParams.set("lang", "fr");
  document.querySelector("#species-synopsis-link").href = backUrl;
}

document.querySelector("#descriptions-english").addEventListener("click", () => render("en", "push"));
document.querySelector("#descriptions-french").addEventListener("click", () => render("fr", "push"));
window.addEventListener("popstate", () => render(requestedLanguage(), "none"));
render(requestedLanguage());
