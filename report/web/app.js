"use strict";

const DEFAULT_LANGUAGE = "en";
const BASE_TITLE = "BC Groundfish Data Synopsis";
const UI_TEXT = {
  en: {
    skipLink: "Skip to species synopsis",
    eyebrow: "Data synopsis",
    siteTitle: "British Columbia groundfish",
    speciesNavigation: "Species navigation",
    species: "Species",
    searchSpecies: "Search species…",
    previousOrNextSpecies: "Previous or next species",
    previous: "Previous",
    next: "Next",
    howToReadPlots: "How to read these plots",
    howToReadThisPage: "How to read this page",
    switchToFrench: "Français",
    switchToEnglish: "English",
    loadingSpeciesData: "Loading species data…",
    conservationStatus: "Conservation status",
    speciesDetails: "Species details",
    order: "Order",
    family: "Family",
    externalSpeciesRecords: "External species records",
    reportsAndReferences: "Latest reports",
    notes: "Notes",
    synopsisFigures: "Synopsis figures",
    landingHeading: "Explore the data, species by species",
    figurePageTitle: (number) => number === 1
      ? "Surveys, catch, and CPUE"
      : "Biological sampling, growth, and maturity",
    landingLede: (count) => `Standardized, reproducible visualizations of population and fishing trends, distribution, growth, and maturity for ${count} species—primarily groundfish—off Canada’s Pacific coast.`,
    suggested: "Suggested",
    allSpecies: "All species",
    footerSynopsis: "British Columbia groundfish data synopsis",
    sourceCode: "Source code",
    issues: "Issues",
    contact: "Contact",
    unofficialUpdateOf: "An unofficial update of ",
    annualUpdateOf: ", an annual update of ",
    asDescribedIn: " as described in ",
    cosewicStatus: "COSEWIC status",
    saraStatus: "SARA status",
    loadingPage: (number) => `Loading page ${number}…`,
    pageCouldNotLoad: (number) => `Page ${number} could not be loaded.`,
    imagesCouldNotLoad: (name) => `One or more images for ${name} could not be loaded.`,
    figureAriaLabel: (name, number) => `${name} synopsis page ${number}, full resolution`,
    figureAlt: (name, number) => `${name} synopsis, page ${number} of 2`,
    loadingFigures: (name) => `Loading synopsis figures for ${name}…`,
    noMatchingSpecies: "No matching species",
    matchingSpecies: (number) => `${number} matching species`,
    dataVersion: (edition) => `Data Version ${edition}`,
    generated: (date) => `Generated ${date}`,
    dataCouldNotLoad: "The species data could not be loaded. Please refresh the page or try again later."
  },
  fr: {
    skipLink: "Passer au synopsis de l’espèce",
    eyebrow: "Synopsis des données",
    siteTitle: "Poissons de fond de la Colombie-Britannique",
    speciesNavigation: "Navigation entre les espèces",
    species: "Espèce",
    searchSpecies: "Rechercher une espèce…",
    previousOrNextSpecies: "Espèce précédente ou suivante",
    previous: "Précédente",
    next: "Suivante",
    howToReadPlots: "Comment lire ces graphiques",
    howToReadThisPage: "Comment lire cette page",
    switchToFrench: "Français",
    switchToEnglish: "English",
    loadingSpeciesData: "Chargement des données sur les espèces…",
    conservationStatus: "État de conservation",
    speciesDetails: "Détails sur l’espèce",
    order: "Ordre",
    family: "Famille",
    externalSpeciesRecords: "Dossiers externes sur l’espèce",
    reportsAndReferences: "Rapports récents",
    notes: "Notes",
    synopsisFigures: "Figures du synopsis",
    landingHeading: "Explorez les données, espèce par espèce",
    figurePageTitle: (number) => number === 1
      ? "Relevés, captures et CPUE"
      : "Échantillonnage biologique, croissance et maturité",
    landingLede: (count) => `Des visualisations standardisées et reproductibles illustrant les tendances des populations et de la pêche, la répartition, la croissance et la maturité de ${count} espèces — principalement des poissons de fond — au large de la côte canadienne du Pacifique.`,
    suggested: "Suggestions",
    allSpecies: "Toutes les espèces",
    footerSynopsis: "Synopsis des données sur les poissons de fond de la Colombie-Britannique",
    sourceCode: "Code source",
    issues: "Problèmes",
    contact: "Contact",
    unofficialUpdateOf: "Mise à jour non officielle de ",
    annualUpdateOf: ", mise à jour annuelle de ",
    asDescribedIn: " telle que décrite dans ",
    cosewicStatus: "Statut du COSEPAC",
    saraStatus: "Statut de la LEP",
    loadingPage: (number) => `Chargement de la page ${number}…`,
    pageCouldNotLoad: (number) => `La page ${number} n’a pas pu être chargée.`,
    imagesCouldNotLoad: (name) => `Une ou plusieurs images pour ${name} n’ont pas pu être chargées.`,
    figureAriaLabel: (name, number) => `${name}, page ${number} du synopsis, pleine résolution`,
    figureAlt: (name, number) => `${name}, synopsis, page ${number} sur 2`,
    loadingFigures: (name) => `Chargement des figures du synopsis pour ${name}…`,
    noMatchingSpecies: "Aucune espèce correspondante",
    matchingSpecies: (number) => `${number} espèces correspondantes`,
    dataVersion: (edition) => `Version des données ${edition}`,
    generated: (date) => `Généré le ${date}`,
    dataCouldNotLoad: "Les données sur les espèces n’ont pas pu être chargées. Veuillez actualiser la page ou réessayer plus tard."
  }
};

const elements = {
  picker: document.querySelector(".species-picker"),
  pickerSentinel: document.querySelector("#species-picker-sentinel"),
  pickerPlaceholder: document.querySelector("#species-picker-placeholder"),
  search: document.querySelector("#species-search"),
  options: document.querySelector("#species-options"),
  matchCount: document.querySelector("#species-match-count"),
  landing: document.querySelector("#landing"),
  landingLede: document.querySelector("#landing-lede"),
  homeLink: document.querySelector("#home-link"),
  previous: document.querySelector("#previous-species"),
  next: document.querySelector("#next-species"),
  languageLink: document.querySelector("#page-language-link"),
  dfoLogoLink: document.querySelector("#dfo-logo-link"),
  plotDescriptions: document.querySelector("#plot-descriptions-link"),
  status: document.querySelector("#app-status"),
  error: document.querySelector("#app-error"),
  figureStatus: document.querySelector("#figure-status"),
  content: document.querySelector("#species-content"),
  commonName: document.querySelector("#common-name"),
  scientificName: document.querySelector("#scientific-name"),
  silhouette: document.querySelector("#species-silhouette"),
  silhouetteImage: document.querySelector("#species-silhouette-image"),
  silhouetteLabel: document.querySelector("#species-silhouette-label"),
  silhouetteAttribution: document.querySelector("#species-silhouette-attribution"),
  order: document.querySelector("#species-order"),
  family: document.querySelector("#species-family"),
  conservationStatus: document.querySelector("#conservation-status"),
  conservationStatusList: document.querySelector("#conservation-status-list"),
  links: document.querySelector("#external-links"),
  referencesSection: document.querySelector("#references-section"),
  references: document.querySelector("#references-list"),
  notesSection: document.querySelector("#notes-section"),
  notes: document.querySelector("#notes-list"),
  figures: document.querySelector("#figures"),
  buildDetails: document.querySelector("#build-details")
};

let species = [];
let metadata = {};
let selectedIndex = -1;
let figureLanguage = DEFAULT_LANGUAGE;
let renderVersion = 0;

const toolbarPicker = {
  search: elements.search,
  options: elements.options,
  matchCount: elements.matchCount,
  idPrefix: "species-option",
  filteredIndices: [],
  activeOption: -1
};

const landingPicker = {
  search: document.querySelector("#landing-search"),
  options: document.querySelector("#landing-options"),
  matchCount: document.querySelector("#landing-match-count"),
  idPrefix: "landing-option",
  suggestFeatured: true,
  filteredIndices: [],
  activeOption: -1
};

const FEATURED_SLUGS = [
  "arrowtooth-flounder",
  "bocaccio",
  "canary-rockfish",
  "dover-sole",
  "lingcod",
  "north-pacific-spiny-dogfish",
  "pacific-cod",
  "petrale-sole",
  "quillback-rockfish",
  "redstripe-rockfish",
  "rougheye-blackspotted-rockfish-complex",
  "silvergray-rockfish",
  "yelloweye-rockfish",
  "yellowtail-rockfish"
];

function t(key, ...args) {
  const value = UI_TEXT[figureLanguage][key];
  return typeof value === "function" ? value(...args) : value;
}

function localizedPage(page) {
  return figureLanguage === "fr" ? { ...page, ...page.translations.fr } : page;
}

function updateLanguageLink() {
  const nextLanguage = figureLanguage === "fr" ? "en" : "fr";
  const languageUrl = new URL(window.location.href);
  if (selectedIndex < 0) {
    languageUrl.searchParams.delete("species");
  }
  if (nextLanguage === DEFAULT_LANGUAGE) {
    languageUrl.searchParams.delete("lang");
  } else {
    languageUrl.searchParams.set("lang", nextLanguage);
  }
  elements.languageLink.textContent = nextLanguage === "fr"
    ? t("switchToFrench")
    : t("switchToEnglish");
  elements.languageLink.lang = nextLanguage;
  elements.languageLink.hreflang = nextLanguage;
  elements.languageLink.href = languageUrl;

  const descriptionsUrl = new URL("plot-descriptions.html", window.location.href);
  if (selectedIndex >= 0 && species[selectedIndex]) {
    descriptionsUrl.searchParams.set("species", species[selectedIndex].slug);
  }
  if (figureLanguage === DEFAULT_LANGUAGE) {
    descriptionsUrl.searchParams.delete("lang");
  } else {
    descriptionsUrl.searchParams.set("lang", figureLanguage);
  }
  elements.plotDescriptions.href = descriptionsUrl;

  const dfoLanguage = figureLanguage === "fr" ? "fr" : "en";
  const dfoUrl = dfoLanguage === "fr"
    ? "https://www.dfo-mpo.gc.ca/index-fra.html"
    : "https://www.dfo-mpo.gc.ca/index-eng.html";
  elements.dfoLogoLink.href = dfoUrl;
  elements.dfoLogoLink.lang = dfoLanguage;
  elements.dfoLogoLink.hreflang = dfoLanguage;

  const homeUrl = new URL(window.location.href);
  homeUrl.searchParams.delete("species");
  if (figureLanguage === DEFAULT_LANGUAGE) {
    homeUrl.searchParams.delete("lang");
  } else {
    homeUrl.searchParams.set("lang", figureLanguage);
  }
  elements.homeLink.href = homeUrl;
}

function translateStatus(value) {
  if (figureLanguage !== "fr" || !value) return value;
  return value
    .replaceAll("Data Deficient", "Données insuffisantes")
    .replaceAll("Endangered", "En voie de disparition")
    .replaceAll("Special Concern", "Préoccupante")
    .replaceAll("Not at Risk", "Non en péril")
    .replaceAll("Threatened", "Menacée")
    .replaceAll("No Status", "Aucun statut");
}

function renderInterface() {
  document.documentElement.lang = figureLanguage;
  document.querySelectorAll("[data-i18n]").forEach((element) => {
    element.textContent = t(element.dataset.i18n);
  });
  document.querySelectorAll("[data-i18n-placeholder]").forEach((element) => {
    element.placeholder = t(element.dataset.i18nPlaceholder);
  });
  document.querySelectorAll("[data-i18n-aria-label]").forEach((element) => {
    element.setAttribute("aria-label", t(element.dataset.i18nAriaLabel));
  });

  updateLanguageLink();

  if (species.length > 0) {
    elements.landingLede.textContent = t("landingLede", species.length);
  }
}

function showMessage(message, isError = false) {
  elements.status.hidden = isError || !message;
  elements.error.hidden = !isError;
  if (isError) {
    elements.error.textContent = message;
  } else {
    elements.status.textContent = message;
  }
}

function showFigureMessage(message, isError = false) {
  elements.figureStatus.hidden = !message;
  elements.figureStatus.classList.toggle("app-message--error", isError);
  elements.figureStatus.textContent = message;
}

function setupToolbarShadow() {
  const toolbar = document.querySelector(".species-toolbar");
  if (!toolbar) return;
  const update = () => {
    toolbar.classList.toggle("is-scrolled", window.scrollY > 0);
  };
  window.addEventListener("scroll", update, { passive: true });
  update();
}

function setupMobileSpeciesPicker() {
  if (!elements.picker || !elements.pickerSentinel ||
      !elements.pickerPlaceholder) {
    return;
  }

  const mobileQuery = window.matchMedia("(max-width: 700px)");
  let latestEntry;

  function updatePickerPosition() {
    const sentinelTop = latestEntry
      ? latestEntry.boundingClientRect.top
      : elements.pickerSentinel.getBoundingClientRect().top;
    const shouldFix = mobileQuery.matches && sentinelTop < 0;
    elements.picker.classList.toggle("is-fixed", shouldFix);
    if (shouldFix) {
      elements.pickerPlaceholder.style.height =
        `${elements.picker.getBoundingClientRect().height}px`;
    } else {
      elements.pickerPlaceholder.style.height = "0px";
    }
  }

  if ("IntersectionObserver" in window) {
    const observer = new IntersectionObserver(([entry]) => {
      latestEntry = entry;
      updatePickerPosition();
    });
    observer.observe(elements.pickerSentinel);
  } else {
    let framePending = false;
    const scheduleUpdate = () => {
      if (framePending) return;
      framePending = true;
      window.requestAnimationFrame(() => {
        framePending = false;
        updatePickerPosition();
      });
    };
    window.addEventListener("scroll", scheduleUpdate, { passive: true });
    window.addEventListener("resize", scheduleUpdate);
  }
  if (typeof mobileQuery.addEventListener === "function") {
    mobileQuery.addEventListener("change", updatePickerPosition);
  } else {
    mobileQuery.addListener(updatePickerPosition);
  }
  updatePickerPosition();
}

function addTextWithLinks(container, text) {
  const markupPattern = /\*\*([^*]+)\*\*|\[([^\]]+)]\((https:\/\/[^)]+)\)/g;
  let position = 0;
  let match;

  while ((match = markupPattern.exec(text)) !== null) {
    container.append(document.createTextNode(text.slice(position, match.index)));
    if (match[1] !== undefined) {
      const emphasis = document.createElement("strong");
      emphasis.textContent = match[1];
      container.append(emphasis);
    } else {
      const link = document.createElement("a");
      link.href = match[3];
      link.textContent = match[2];
      link.target = "_blank";
      link.rel = "noopener noreferrer";
      container.append(link);
    }
    position = markupPattern.lastIndex;
  }
  container.append(document.createTextNode(text.slice(position)));
}

function renderConservationStatus(page) {
  elements.conservationStatusList.replaceChildren();
  const statuses = [
    [t("cosewicStatus"), translateStatus(page.cosewic_status)],
    [t("saraStatus"), translateStatus(page.sara_status)]
  ].filter(([, value]) => value);

  statuses.forEach(([label, value]) => {
    const line = document.createElement("div");
    line.textContent = `${label}: ${value}`;
    elements.conservationStatusList.append(line);
  });
  elements.conservationStatus.hidden = statuses.length === 0;
}

function renderLinks(links) {
  elements.links.replaceChildren();
  for (const item of links) {
    const link = document.createElement("a");
    link.href = item.url;
    link.textContent = item.label;
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    elements.links.append(link);
  }
}

function appendCitationText(container, text) {
  // Citation markup is generated from trusted BibTeX, but append text nodes
  // explicitly so bibliography content can never become arbitrary HTML.
  const emphPattern = /<em>([^<]*)<\/em>/g;
  let position = 0;
  let match;
  while ((match = emphPattern.exec(text)) !== null) {
    container.append(document.createTextNode(text.slice(position, match.index)));
    const emphasis = document.createElement("em");
    emphasis.textContent = match[1];
    container.append(emphasis);
    position = emphPattern.lastIndex;
  }
  container.append(document.createTextNode(text.slice(position)));
}

function appendReferenceCitation(container, reference) {
  const title = reference.title || "";
  const titleIndex = reference.url && title
    ? reference.citation.indexOf(title)
    : -1;

  if (titleIndex === -1) {
    appendCitationText(container, reference.citation);
    return;
  }

  appendCitationText(container, reference.citation.slice(0, titleIndex));
  const titleLink = document.createElement("a");
  titleLink.href = reference.url;
  titleLink.target = "_blank";
  titleLink.rel = "noopener noreferrer";
  appendCitationText(titleLink, title);
  container.append(titleLink);
  appendCitationText(container, reference.citation.slice(titleIndex + title.length));
}

function renderReferences(references) {
  elements.references.replaceChildren();
  elements.referencesSection.hidden = references.length === 0;

  for (const reference of references) {
    const item = document.createElement("li");
    item.className = "reference-citation";
    appendReferenceCitation(item, reference);
    elements.references.append(item);
  }
}

function renderNotes(notes) {
  elements.notes.replaceChildren();
  elements.notesSection.hidden = notes.length === 0;

  for (const note of notes) {
    const paragraph = document.createElement("p");
    addTextWithLinks(paragraph, note);
    elements.notes.append(paragraph);
  }
}

function renderSilhouette(silhouette) {
  if (!silhouette) {
    elements.silhouette.hidden = true;
    elements.silhouetteImage.removeAttribute("src");
    return;
  }

  elements.silhouetteImage.src = silhouette.image;
  elements.silhouetteImage.alt = silhouette.alt;
  elements.silhouetteLabel.replaceChildren();
  const scientificName = document.createElement("em");
  const matchedName = silhouette.matched_name || silhouette.label || "Scientific name";
  if (silhouette.level === "genus") {
    scientificName.textContent = matchedName.split(/\s+/)[0];
    elements.silhouetteLabel.append(scientificName, " sp.");
  } else {
    scientificName.textContent = matchedName;
    elements.silhouetteLabel.append(scientificName);
  }
  elements.silhouetteAttribution.replaceChildren();
  if (silhouette.credit) {
    elements.silhouetteAttribution.append(" · ");
    if (silhouette.source_url) {
      const credit = document.createElement("a");
      credit.href = silhouette.source_url;
      credit.target = "_blank";
      credit.rel = "noopener noreferrer";
      credit.textContent = silhouette.credit;
      elements.silhouetteAttribution.append(credit);
    } else {
      elements.silhouetteAttribution.append(silhouette.credit);
    }
  }
  if (silhouette.license) {
    elements.silhouetteAttribution.append(" · ");
    if (silhouette.license_url) {
      const license = document.createElement("a");
      license.href = silhouette.license_url;
      license.target = "_blank";
      license.rel = "noopener noreferrer";
      license.textContent = silhouette.license;
      elements.silhouetteAttribution.append(license);
    } else {
      elements.silhouetteAttribution.append(silhouette.license);
    }
  }
  elements.silhouette.hidden = false;
}

function createFigure(page, imagePath, pageNumber, version) {
  const figure = document.createElement("figure");
  figure.className = "synopsis-figure";

  const title = document.createElement("figcaption");
  title.className = "synopsis-figure__title";
  const titleText = document.createElement("span");
  titleText.textContent = t("figurePageTitle", pageNumber);

  const helpUrl = new URL("plot-descriptions.html", window.location.href);
  helpUrl.hash = pageNumber === 1 ? "survey-index-heading" : "samples-heading";
  if (selectedIndex >= 0 && species[selectedIndex]) {
    helpUrl.searchParams.set("species", species[selectedIndex].slug);
  }
  if (figureLanguage !== DEFAULT_LANGUAGE) {
    helpUrl.searchParams.set("lang", figureLanguage);
  }
  const helpLink = document.createElement("a");
  helpLink.className = "synopsis-figure__help-link";
  helpLink.href = helpUrl;
  helpLink.textContent = t("howToReadThisPage");

  title.append(titleText, helpLink);

  const frame = document.createElement("div");
  frame.className = "figure-frame is-loading";
  const loading = document.createElement("p");
  loading.className = "figure-loading";
  loading.textContent = t("loadingPage", pageNumber);

  const link = document.createElement("a");
  link.href = imagePath;
  link.target = "_blank";
  link.rel = "noopener noreferrer";
  link.setAttribute("aria-label", t("figureAriaLabel", page.common_name, pageNumber));

  const image = document.createElement("img");
  image.alt = t("figureAlt", page.common_name, pageNumber);
  image.decoding = "async";
  image.loading = pageNumber === 1 ? "eager" : "lazy";
  image.addEventListener("load", () => {
    if (version !== renderVersion) return;
    frame.classList.remove("is-loading");
    loading.remove();
    if (pageNumber === 1) showFigureMessage("");
  });
  image.addEventListener("error", () => {
    if (version !== renderVersion) return;
    frame.classList.remove("is-loading");
    loading.className = "figure-error";
    loading.textContent = t("pageCouldNotLoad", pageNumber);
    showFigureMessage(t("imagesCouldNotLoad", page.common_name), true);
  });
  image.src = imagePath;

  link.append(image);
  frame.append(loading, link);
  figure.append(title, frame);
  return figure;
}

function updateAddress(slug, language, mode) {
  if (mode === "none") return;
  const url = new URL(window.location.href);
  url.searchParams.set("species", slug);
  if (language === DEFAULT_LANGUAGE) {
    url.searchParams.delete("lang");
  } else {
    url.searchParams.set("lang", language);
  }
  const method = mode === "push" ? "pushState" : "replaceState";
  window.history[method]({ species: slug }, "", url);
}

function updateLandingAddress(language, mode) {
  if (mode === "none") return;
  const url = new URL(window.location.href);
  url.searchParams.delete("species");
  if (language === DEFAULT_LANGUAGE) {
    url.searchParams.delete("lang");
  } else {
    url.searchParams.set("lang", language);
  }
  const method = mode === "push" ? "pushState" : "replaceState";
  window.history[method]({}, "", url);
}

function scrollToPageTop() {
  // Wait until the selection's default touch/click behavior and the landing
  // layout change have completed. Otherwise mobile scroll anchoring can undo
  // an immediate reset.
  window.requestAnimationFrame(() => {
    // The site uses smooth scrolling globally, but a route change should be
    // immediate.
    const root = document.documentElement;
    const previousBehavior = root.style.scrollBehavior;
    root.style.scrollBehavior = "auto";
    window.scrollTo(0, 0);
    root.style.scrollBehavior = previousBehavior;
  });
}

function showLanding(historyMode = "none") {
  selectedIndex = -1;
  renderVersion += 1;
  document.body.classList.add("is-landing");
  elements.content.hidden = true;
  elements.landing.hidden = false;
  showMessage("");
  showFigureMessage("");
  renderInterface();
  renderBuildDetails(metadata);
  document.title = BASE_TITLE;
  landingPicker.search.value = "";
  closeSpeciesOptions(landingPicker);
  updateLandingAddress(figureLanguage, historyMode);
}

function renderSpecies(index, historyMode = "none", language = figureLanguage) {
  if (index < 0 || index >= species.length) return;
  selectedIndex = index;
  figureLanguage = language;
  renderInterface();
  renderBuildDetails(metadata);
  renderVersion += 1;
  const version = renderVersion;
  document.body.classList.remove("is-landing");
  elements.landing.hidden = true;
  const page = species[index];
  const displayPage = localizedPage(page);

  elements.search.value = displayPage.common_name;
  closeSpeciesOptions(toolbarPicker);
  elements.previous.disabled = index === 0;
  elements.next.disabled = index === species.length - 1;
  elements.commonName.textContent = displayPage.common_name;
  elements.scientificName.textContent = displayPage.scientific_name;
  renderSilhouette(page.silhouette);
  elements.order.textContent = displayPage.order;
  elements.family.textContent = displayPage.family;
  renderConservationStatus(displayPage);
  renderLinks(displayPage.links);
  renderReferences(displayPage.references);
  renderNotes(displayPage.notes);

  elements.figures.replaceChildren();
  const imagePaths = page.images[figureLanguage] || page.images[DEFAULT_LANGUAGE];
  imagePaths.forEach((imagePath, imageIndex) => {
    elements.figures.append(createFigure(
      displayPage,
      imagePath,
      imageIndex + 1,
      version
    ));
  });

  elements.content.hidden = false;
  document.title = `${displayPage.common_name} · ${t("siteTitle")}`;
  showMessage("");
  showFigureMessage(t("loadingFigures", displayPage.common_name));
  updateAddress(page.slug, figureLanguage, historyMode);
  if (historyMode === "push") scrollToPageTop();
  updateLanguageLink();
}

function requestedSpeciesIndex() {
  const slug = new URL(window.location.href).searchParams.get("species");
  return species.findIndex((page) => page.slug === slug);
}

function requestedFigureLanguage() {
  return new URL(window.location.href).searchParams.get("lang") === "fr"
    ? "fr"
    : DEFAULT_LANGUAGE;
}

function setActiveOption(picker, position) {
  const options = picker.options.querySelectorAll(".species-option");
  picker.activeOption = position >= 0 && position < options.length ? position : -1;

  options.forEach((option, index) => {
    const isActive = index === picker.activeOption;
    option.classList.toggle("is-active", isActive);
    option.setAttribute("aria-selected", String(isActive));
  });

  if (picker.activeOption >= 0) {
    const option = options[picker.activeOption];
    picker.search.setAttribute("aria-activedescendant", option.id);
    option.scrollIntoView({ block: "nearest" });
  } else {
    picker.search.removeAttribute("aria-activedescendant");
  }
}

function renderSpeciesOptions(picker, query = "", preferredIndex = -1) {
  const needle = query.trim().toLocaleLowerCase(figureLanguage);
  picker.filteredIndices = species
    .map((page, index) => ({ page, index }))
    .filter(({ page }) =>
      localizedPage(page).common_name.toLocaleLowerCase(figureLanguage).includes(needle)
    )
    .map(({ index }) => index);

  const showFeatured = picker.suggestFeatured && needle === "";
  let featuredCount = 0;
  if (showFeatured) {
    const featured = FEATURED_SLUGS
      .map((slug) => species.findIndex((page) => page.slug === slug))
      .filter((index) => index >= 0 && picker.filteredIndices.includes(index))
      .sort((left, right) => localizedPage(species[left]).common_name.localeCompare(
        localizedPage(species[right]).common_name,
        figureLanguage
      ));
    picker.filteredIndices = [
      ...featured,
      ...picker.filteredIndices.filter((index) => !featured.includes(index))
    ];
    featuredCount = featured.length;
  }

  const options = picker.filteredIndices.map((speciesIndex, optionIndex) => {
    const option = document.createElement("li");
    option.id = `${picker.idPrefix}-${optionIndex}`;
    option.className = "species-option";
    option.dataset.speciesIndex = String(speciesIndex);
    option.setAttribute("role", "option");
    option.setAttribute("aria-selected", "false");
    option.textContent = localizedPage(species[speciesIndex]).common_name;
    return option;
  });

  if (options.length === 0) {
    const empty = document.createElement("li");
    empty.className = "species-no-results";
    empty.textContent = t("noMatchingSpecies");
    picker.options.replaceChildren(empty);
  } else if (featuredCount > 0 && options.length > featuredCount) {
    const suggestedLabel = document.createElement("li");
    suggestedLabel.className = "species-options-group";
    suggestedLabel.setAttribute("role", "presentation");
    suggestedLabel.textContent = t("suggested");
    const allLabel = document.createElement("li");
    allLabel.className = "species-options-group";
    allLabel.setAttribute("role", "presentation");
    allLabel.textContent = t("allSpecies");
    picker.options.replaceChildren(
      suggestedLabel,
      ...options.slice(0, featuredCount),
      allLabel,
      ...options.slice(featuredCount)
    );
  } else {
    picker.options.replaceChildren(...options);
  }

  picker.matchCount.textContent = t("matchingSpecies", options.length);
  const preferredPosition = picker.filteredIndices.indexOf(preferredIndex);
  setActiveOption(picker, preferredPosition >= 0 ? preferredPosition : (options.length ? 0 : -1));
}

function openSpeciesOptions(picker) {
  picker.options.hidden = false;
  picker.search.setAttribute("aria-expanded", "true");
}

function closeSpeciesOptions(picker, restoreValue = false) {
  picker.options.hidden = true;
  picker.search.setAttribute("aria-expanded", "false");
  picker.search.removeAttribute("aria-activedescendant");
  picker.activeOption = -1;
  if (restoreValue) {
    picker.search.value = picker === toolbarPicker && selectedIndex >= 0
      ? localizedPage(species[selectedIndex]).common_name
      : "";
  }
}

function enableSpeciesSearch() {
  toolbarPicker.search.disabled = false;
  landingPicker.search.disabled = false;
  elements.previous.disabled = false;
  elements.next.disabled = false;
}

function renderBuildDetails(metadata) {
  const details = [];
  if (metadata.edition) details.push(t("dataVersion", metadata.edition));
  if (metadata.generated_at) {
    const date = new Date(metadata.generated_at);
    if (!Number.isNaN(date.valueOf())) {
      const dateText = new Intl.DateTimeFormat(
        figureLanguage === "fr" ? "fr-CA" : "en-CA", {
        dateStyle: "long",
        timeZone: "UTC"
      }).format(date);
      details.push(t("generated", dateText));
    }
  }
  elements.buildDetails.textContent = details.join(" · ");
}

async function initialize() {
  try {
    const response = await fetch("species.json", { cache: "no-cache" });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    if (!Array.isArray(data.species) || data.species.length === 0) {
      throw new Error("No species records were found");
    }

    species = [...data.species].sort((a, b) =>
      a.common_name.localeCompare(b.common_name, "en", { sensitivity: "base" })
    );
    metadata = data.metadata || {};
    figureLanguage = requestedFigureLanguage();
    enableSpeciesSearch();
    const index = requestedSpeciesIndex();
    const requestedSlug = new URL(window.location.href).searchParams.get("species");
    if (index >= 0) {
      renderSpecies(index, "none", figureLanguage);
    } else {
      showLanding(requestedSlug ? "replace" : "none");
    }
  } catch (error) {
    console.error(error);
    showMessage(t("dataCouldNotLoad"), true);
  }
}

function attachPickerHandlers(picker) {
  const touchDragThreshold = 10;

  const selectOption = (option) => {
    renderSpecies(Number(option.dataset.speciesIndex), "push");
    if (picker === toolbarPicker) picker.search.focus();
  };

  picker.search.addEventListener("focus", () => {
    picker.search.select();
    renderSpeciesOptions(picker, "", picker === toolbarPicker ? selectedIndex : -1);
    openSpeciesOptions(picker);
  });

  picker.search.addEventListener("input", () => {
    renderSpeciesOptions(picker, picker.search.value);
    openSpeciesOptions(picker);
  });

  picker.search.addEventListener("keydown", (event) => {
    if (event.key === "Escape") {
      closeSpeciesOptions(picker, true);
      return;
    }
    if (event.key === "Tab") {
      closeSpeciesOptions(picker, true);
      return;
    }
    if (event.key === "Enter" && !picker.options.hidden && picker.activeOption >= 0) {
      event.preventDefault();
      renderSpecies(picker.filteredIndices[picker.activeOption], "push");
      return;
    }
    if (event.key !== "ArrowDown" && event.key !== "ArrowUp") return;

    event.preventDefault();
    if (picker.options.hidden) {
      renderSpeciesOptions(picker, "", picker === toolbarPicker ? selectedIndex : -1);
      openSpeciesOptions(picker);
    }
    const direction = event.key === "ArrowDown" ? 1 : -1;
    const nextOption = Math.max(
      0,
      Math.min(picker.filteredIndices.length - 1, picker.activeOption + direction)
    );
    setActiveOption(picker, nextOption);
  });

  picker.search.addEventListener("blur", () => {
    // A touch starts by moving focus away from the input. Keep the list open
    // while that touch is still being classified as a tap or a scroll.
    if (picker.touchSelection) return;
    closeSpeciesOptions(picker, true);
  });

  picker.options.addEventListener("pointerdown", (event) => {
    const option = event.target.closest(".species-option");
    if (!option) return;

    // Select touch options on release, not initial contact: `pointerdown` is
    // also the first event of a scroll gesture on iOS.
    if (event.pointerType === "touch") {
      picker.touchSelection = {
        option,
        pointerId: event.pointerId,
        startX: event.clientX,
        startY: event.clientY,
        moved: false
      };
      return;
    }

    // Keep the search input focused for mouse and pen selection so its blur
    // handler does not hide the option before the click is delivered.
    event.preventDefault();
    picker.pointerSelection = option;
    selectOption(option);
  });

  picker.options.addEventListener("pointermove", (event) => {
    const selection = picker.touchSelection;
    if (!selection || selection.pointerId !== event.pointerId) return;
    if (Math.hypot(event.clientX - selection.startX, event.clientY - selection.startY) >= touchDragThreshold) {
      selection.moved = true;
    }
  });

  picker.options.addEventListener("pointerup", (event) => {
    const selection = picker.touchSelection;
    if (!selection || selection.pointerId !== event.pointerId) return;
    picker.touchSelection = null;
    if (!selection.moved) selectOption(selection.option);
  });

  picker.options.addEventListener("pointercancel", (event) => {
    if (picker.touchSelection?.pointerId === event.pointerId) {
      picker.touchSelection = null;
    }
  });

  picker.options.addEventListener("click", (event) => {
    const option = event.target.closest(".species-option");
    if (!option) return;
    if (picker.pointerSelection === option) {
      picker.pointerSelection = null;
      return;
    }
    if (event.detail !== 0) return;
    selectOption(option);
  });
}

attachPickerHandlers(toolbarPicker);
attachPickerHandlers(landingPicker);

elements.homeLink.addEventListener("click", (event) => {
  if (species.length === 0) return;
  event.preventDefault();
  showLanding("push");
});

elements.previous.addEventListener("click", () => {
  renderSpecies(selectedIndex - 1, "push");
});

elements.next.addEventListener("click", () => {
  renderSpecies(selectedIndex + 1, "push");
});

window.addEventListener("popstate", () => {
  figureLanguage = requestedFigureLanguage();
  const index = requestedSpeciesIndex();
  if (index >= 0) {
    renderSpecies(index, "none", figureLanguage);
  } else {
    showLanding("none");
  }
});

setupMobileSpeciesPicker();
setupToolbarShadow();
initialize();
