"use strict";

const DEFAULT_SPECIES = "pacific-cod";
const DEFAULT_LANGUAGE = "en";
const BASE_TITLE = "BC Groundfish Data Synopsis";
const REFERENCE_LABELS = {
  research_documents: "Research document",
  science_advisory_reports: "Science advisory report",
  other: "Related document",
  cosewic_status_report: "COSEWIC status report"
};
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
    figureLanguage: "Page language",
    figures: "Pages",
    english: "English",
    loadingSpeciesData: "Loading species data…",
    conservationStatus: "Conservation status",
    speciesDetails: "Species details",
    order: "Order",
    family: "Family",
    externalSpeciesRecords: "External species records",
    reportsAndReferences: "Reports and references",
    notes: "Notes",
    synopsisFigures: "Synopsis figures",
    openFigure: "Open either image to view it at full resolution.",
    footerSynopsis: "British Columbia groundfish data synopsis",
    sourceCode: "Source code ↗",
    webVersionOf: "This is a web version of:",
    citeTechnicalReport: "Please cite the Technical Report if referencing material.",
    speciesCode: (code) => `Species code ${code}`,
    cosewicStatus: "COSEWIC status",
    saraStatus: "SARA status",
    reference: "Reference",
    fullCitation: "Full citation",
    loadingPage: (number) => `Loading page ${number}…`,
    pageCouldNotLoad: (number) => `Page ${number} could not be loaded.`,
    imagesCouldNotLoad: (name) => `One or more images for ${name} could not be loaded.`,
    figureAriaLabel: (name, number) => `${name} synopsis page ${number}, full resolution`,
    figureAlt: (name, number) => `${name} synopsis, page ${number} of 2`,
    pageOf: (number) => `Page ${number} of 2`,
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
    figureLanguage: "Langue de la page",
    figures: "Pages",
    english: "Anglais",
    loadingSpeciesData: "Chargement des données sur les espèces…",
    conservationStatus: "État de conservation",
    speciesDetails: "Détails sur l’espèce",
    order: "Ordre",
    family: "Famille",
    externalSpeciesRecords: "Dossiers externes sur l’espèce",
    reportsAndReferences: "Rapports et références",
    notes: "Notes",
    synopsisFigures: "Figures du synopsis",
    openFigure: "Ouvrez une image pour l’afficher en pleine résolution.",
    footerSynopsis: "Synopsis des données sur les poissons de fond de la Colombie-Britannique",
    sourceCode: "Code source ↗",
    webVersionOf: "Il s’agit d’une version Web de :",
    citeTechnicalReport: "Veuillez citer le rapport technique lorsque vous faites référence à ce contenu.",
    speciesCode: (code) => `Code d’espèce ${code}`,
    cosewicStatus: "Statut du COSEPAC",
    saraStatus: "Statut de la LEP",
    reference: "Référence",
    fullCitation: "Citation complète",
    loadingPage: (number) => `Chargement de la page ${number}…`,
    pageCouldNotLoad: (number) => `La page ${number} n’a pas pu être chargée.`,
    imagesCouldNotLoad: (name) => `Une ou plusieurs images pour ${name} n’ont pas pu être chargées.`,
    figureAriaLabel: (name, number) => `${name}, page ${number} du synopsis, pleine résolution`,
    figureAlt: (name, number) => `${name}, synopsis, page ${number} sur 2`,
    pageOf: (number) => `Page ${number} sur 2`,
    loadingFigures: (name) => `Chargement des figures du synopsis pour ${name}…`,
    noMatchingSpecies: "Aucune espèce correspondante",
    matchingSpecies: (number) => `${number} espèces correspondantes`,
    dataVersion: (edition) => `Version des données ${edition}`,
    generated: (date) => `Généré le ${date}`,
    dataCouldNotLoad: "Les données sur les espèces n’ont pas pu être chargées. Veuillez actualiser la page ou réessayer plus tard."
  }
};

const elements = {
  search: document.querySelector("#species-search"),
  options: document.querySelector("#species-options"),
  matchCount: document.querySelector("#species-match-count"),
  previous: document.querySelector("#previous-species"),
  next: document.querySelector("#next-species"),
  englishFigures: document.querySelector("#figures-english"),
  frenchFigures: document.querySelector("#figures-french"),
  status: document.querySelector("#app-status"),
  error: document.querySelector("#app-error"),
  content: document.querySelector("#species-content"),
  code: document.querySelector("#species-code"),
  commonName: document.querySelector("#common-name"),
  scientificName: document.querySelector("#scientific-name"),
  order: document.querySelector("#species-order"),
  family: document.querySelector("#species-family"),
  badges: document.querySelector("#status-badges"),
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
let filteredIndices = [];
let activeOption = -1;

function t(key, ...args) {
  const value = UI_TEXT[figureLanguage][key];
  return typeof value === "function" ? value(...args) : value;
}

function localizedPage(page) {
  return figureLanguage === "fr" ? { ...page, ...page.translations.fr } : page;
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

function addTextWithLinks(container, text) {
  const linkPattern = /\[([^\]]+)]\((https:\/\/[^)]+)\)/g;
  let position = 0;
  let match;

  while ((match = linkPattern.exec(text)) !== null) {
    container.append(document.createTextNode(text.slice(position, match.index)));
    const link = document.createElement("a");
    link.href = match[2];
    link.textContent = match[1];
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    container.append(link);
    position = linkPattern.lastIndex;
  }
  container.append(document.createTextNode(text.slice(position)));
}

function renderBadges(page) {
  elements.badges.replaceChildren();
  const statuses = [
    [t("cosewicStatus"), translateStatus(page.cosewic_status)],
    [t("saraStatus"), translateStatus(page.sara_status)]
  ];

  for (const [label, value] of statuses) {
    if (!value) continue;
    const badge = document.createElement("span");
    badge.className = "status-badge";
    const heading = document.createElement("strong");
    heading.textContent = label;
    badge.append(heading, document.createTextNode(value));
    elements.badges.append(badge);
  }
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

function renderReferences(references) {
  elements.references.replaceChildren();
  elements.referencesSection.hidden = references.length === 0;

  for (const reference of references) {
    const item = document.createElement("li");
    const heading = document.createElement("div");
    heading.className = "reference-heading";
    const group = document.createElement("span");
    group.className = "reference-label";
    group.textContent = `${reference.group || REFERENCE_LABELS[reference.type] || t("reference")}: `;
    const label = reference.url
      ? document.createElement("a")
      : document.createElement("span");
    label.className = "reference-link";
    label.textContent = reference.label;
    if (reference.url) {
      label.href = reference.url;
      label.target = "_blank";
      label.rel = "noopener noreferrer";
    }
    heading.append(group, label);
    const details = document.createElement("details");
    const summary = document.createElement("summary");
    summary.textContent = t("fullCitation");
    const citation = document.createElement("span");
    citation.className = "reference-text";
    appendCitationText(citation, reference.citation);
    details.append(summary, citation);
    item.append(heading, details);
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

function createFigure(page, imagePath, pageNumber, version) {
  const figure = document.createElement("figure");
  figure.className = "synopsis-figure";

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
    if (pageNumber === 1) showMessage("");
  });
  image.addEventListener("error", () => {
    if (version !== renderVersion) return;
    frame.classList.remove("is-loading");
    loading.className = "figure-error";
    loading.textContent = t("pageCouldNotLoad", pageNumber);
    showMessage(t("imagesCouldNotLoad", page.common_name), true);
  });
  image.src = imagePath;

  link.append(image);
  frame.append(loading, link);
  const caption = document.createElement("figcaption");
  caption.textContent = t("pageOf", pageNumber);
  figure.append(frame, caption);
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

function renderSpecies(index, historyMode = "none", language = figureLanguage) {
  if (index < 0 || index >= species.length) return;
  selectedIndex = index;
  figureLanguage = language;
  renderInterface();
  renderBuildDetails(metadata);
  renderVersion += 1;
  const version = renderVersion;
  const page = species[index];
  const displayPage = localizedPage(page);

  elements.search.value = displayPage.common_name;
  closeSpeciesOptions();
  elements.previous.disabled = index === 0;
  elements.next.disabled = index === species.length - 1;
  elements.code.textContent = t("speciesCode", displayPage.species_code);
  elements.commonName.textContent = displayPage.common_name;
  elements.scientificName.textContent = displayPage.scientific_name;
  elements.order.textContent = displayPage.order;
  elements.family.textContent = displayPage.family;
  renderBadges(displayPage);
  renderLinks(displayPage.links);
  renderReferences(displayPage.references);
  renderNotes(displayPage.notes);

  elements.figures.replaceChildren();
  const imagePaths = page.images[figureLanguage] || page.images[DEFAULT_LANGUAGE];
  elements.englishFigures.setAttribute("aria-pressed", String(figureLanguage === "en"));
  elements.frenchFigures.setAttribute("aria-pressed", String(figureLanguage === "fr"));
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
  showMessage(t("loadingFigures", displayPage.common_name));
  updateAddress(page.slug, figureLanguage, historyMode);
}

function requestedSpeciesIndex() {
  const slug = new URL(window.location.href).searchParams.get("species");
  const index = species.findIndex((page) => page.slug === slug);
  if (index >= 0) return index;
  const defaultIndex = species.findIndex((page) => page.slug === DEFAULT_SPECIES);
  return defaultIndex >= 0 ? defaultIndex : 0;
}

function requestedFigureLanguage() {
  return new URL(window.location.href).searchParams.get("lang") === "fr"
    ? "fr"
    : DEFAULT_LANGUAGE;
}

function setActiveOption(position) {
  const options = elements.options.querySelectorAll(".species-option");
  activeOption = position >= 0 && position < options.length ? position : -1;

  options.forEach((option, index) => {
    const isActive = index === activeOption;
    option.classList.toggle("is-active", isActive);
    option.setAttribute("aria-selected", String(isActive));
  });

  if (activeOption >= 0) {
    const option = options[activeOption];
    elements.search.setAttribute("aria-activedescendant", option.id);
    option.scrollIntoView({ block: "nearest" });
  } else {
    elements.search.removeAttribute("aria-activedescendant");
  }
}

function renderSpeciesOptions(query = "", preferredIndex = -1) {
  const needle = query.trim().toLocaleLowerCase(figureLanguage);
  filteredIndices = species
    .map((page, index) => ({ page, index }))
    .filter(({ page }) =>
      localizedPage(page).common_name.toLocaleLowerCase(figureLanguage).includes(needle)
    )
    .map(({ index }) => index);

  const options = filteredIndices.map((speciesIndex, optionIndex) => {
    const option = document.createElement("li");
    option.id = `species-option-${optionIndex}`;
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
    elements.options.replaceChildren(empty);
  } else {
    elements.options.replaceChildren(...options);
  }

  elements.matchCount.textContent = t("matchingSpecies", options.length);
  const preferredPosition = filteredIndices.indexOf(preferredIndex);
  setActiveOption(preferredPosition >= 0 ? preferredPosition : (options.length ? 0 : -1));
}

function openSpeciesOptions() {
  elements.options.hidden = false;
  elements.search.setAttribute("aria-expanded", "true");
}

function closeSpeciesOptions(restoreValue = false) {
  elements.options.hidden = true;
  elements.search.setAttribute("aria-expanded", "false");
  elements.search.removeAttribute("aria-activedescendant");
  activeOption = -1;
  if (restoreValue && selectedIndex >= 0) {
    elements.search.value = localizedPage(species[selectedIndex]).common_name;
  }
}

function enableSpeciesSearch() {
  elements.search.disabled = false;
  elements.previous.disabled = false;
  elements.next.disabled = false;
  elements.englishFigures.disabled = false;
  elements.frenchFigures.disabled = false;
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
    const validRequest = species.some((page) => page.slug === requestedSlug);
    renderSpecies(index, validRequest ? "none" : "replace", figureLanguage);
  } catch (error) {
    console.error(error);
    showMessage(t("dataCouldNotLoad"), true);
  }
}

elements.search.addEventListener("focus", () => {
  elements.search.select();
  renderSpeciesOptions("", selectedIndex);
  openSpeciesOptions();
});

elements.search.addEventListener("input", () => {
  renderSpeciesOptions(elements.search.value);
  openSpeciesOptions();
});

elements.search.addEventListener("keydown", (event) => {
  if (event.key === "Escape") {
    closeSpeciesOptions(true);
    return;
  }
  if (event.key === "Tab") {
    closeSpeciesOptions(true);
    return;
  }
  if (event.key === "Enter" && !elements.options.hidden && activeOption >= 0) {
    event.preventDefault();
    renderSpecies(filteredIndices[activeOption], "push");
    return;
  }
  if (event.key !== "ArrowDown" && event.key !== "ArrowUp") return;

  event.preventDefault();
  if (elements.options.hidden) {
    renderSpeciesOptions("", selectedIndex);
    openSpeciesOptions();
  }
  const direction = event.key === "ArrowDown" ? 1 : -1;
  const nextOption = Math.max(
    0,
    Math.min(filteredIndices.length - 1, activeOption + direction)
  );
  setActiveOption(nextOption);
});

elements.search.addEventListener("blur", () => {
  closeSpeciesOptions(true);
});

elements.options.addEventListener("pointerdown", (event) => {
  if (event.target.closest(".species-option")) event.preventDefault();
});

elements.options.addEventListener("click", (event) => {
  const option = event.target.closest(".species-option");
  if (!option) return;
  renderSpecies(Number(option.dataset.speciesIndex), "push");
  elements.search.focus();
});

elements.previous.addEventListener("click", () => {
  renderSpecies(selectedIndex - 1, "push");
});

elements.next.addEventListener("click", () => {
  renderSpecies(selectedIndex + 1, "push");
});

elements.englishFigures.addEventListener("click", () => {
  if (figureLanguage !== "en") renderSpecies(selectedIndex, "push", "en");
});

elements.frenchFigures.addEventListener("click", () => {
  if (figureLanguage !== "fr") renderSpecies(selectedIndex, "push", "fr");
});

window.addEventListener("popstate", () => {
  renderSpecies(requestedSpeciesIndex(), "none", requestedFigureLanguage());
});

initialize();
