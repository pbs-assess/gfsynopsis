"use strict";

const DEFAULT_SPECIES = "pacific-cod";
const BASE_TITLE = "BC Groundfish Data Synopsis";
const REFERENCE_LABELS = {
  research_documents: "Research document",
  science_advisory_reports: "Science advisory report",
  other: "Related document",
  cosewic_status_report: "COSEWIC status report"
};

const elements = {
  search: document.querySelector("#species-search"),
  options: document.querySelector("#species-options"),
  matchCount: document.querySelector("#species-match-count"),
  previous: document.querySelector("#previous-species"),
  next: document.querySelector("#next-species"),
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
let selectedIndex = -1;
let renderVersion = 0;
let filteredIndices = [];
let activeOption = -1;

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
    ["COSEWIC status", page.cosewic_status],
    ["SARA status", page.sara_status]
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

function renderReferences(references) {
  elements.references.replaceChildren();
  elements.referencesSection.hidden = references.length === 0;

  for (const reference of references) {
    const item = document.createElement("li");
    const heading = document.createElement("div");
    heading.className = "reference-heading";
    const group = document.createElement("span");
    group.className = "reference-label";
    group.textContent = `${reference.group || REFERENCE_LABELS[reference.type] || "Reference"}: `;
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
    summary.textContent = "Full citation";
    const citation = document.createElement("span");
    citation.className = "reference-text";
    citation.textContent = reference.citation;
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
  loading.textContent = `Loading page ${pageNumber}…`;

  const link = document.createElement("a");
  link.href = imagePath;
  link.target = "_blank";
  link.rel = "noopener noreferrer";
  link.setAttribute("aria-label", `${page.common_name} synopsis page ${pageNumber}, full resolution`);

  const image = document.createElement("img");
  image.alt = `${page.common_name} synopsis, page ${pageNumber} of 2`;
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
    loading.textContent = `Page ${pageNumber} could not be loaded.`;
    showMessage(`One or more images for ${page.common_name} could not be loaded.`, true);
  });
  image.src = imagePath;

  link.append(image);
  frame.append(loading, link);
  const caption = document.createElement("figcaption");
  caption.textContent = `Page ${pageNumber} of 2`;
  figure.append(frame, caption);
  return figure;
}

function updateAddress(slug, mode) {
  if (mode === "none") return;
  const url = new URL(window.location.href);
  url.searchParams.set("species", slug);
  const method = mode === "push" ? "pushState" : "replaceState";
  window.history[method]({ species: slug }, "", url);
}

function renderSpecies(index, historyMode = "none") {
  if (index < 0 || index >= species.length) return;
  selectedIndex = index;
  renderVersion += 1;
  const version = renderVersion;
  const page = species[index];

  elements.search.value = page.common_name;
  closeSpeciesOptions();
  elements.previous.disabled = index === 0;
  elements.next.disabled = index === species.length - 1;
  elements.code.textContent = `Species code ${page.species_code}`;
  elements.commonName.textContent = page.common_name;
  elements.scientificName.textContent = page.scientific_name;
  elements.order.textContent = page.order;
  elements.family.textContent = page.family;
  renderBadges(page);
  renderLinks(page.links);
  renderReferences(page.references);
  renderNotes(page.notes);

  elements.figures.replaceChildren();
  page.images.forEach((imagePath, imageIndex) => {
    elements.figures.append(createFigure(
      page,
      imagePath,
      imageIndex + 1,
      version
    ));
  });

  elements.content.hidden = false;
  document.title = `${page.common_name} · ${BASE_TITLE}`;
  showMessage(`Loading synopsis figures for ${page.common_name}…`);
  updateAddress(page.slug, historyMode);
}

function requestedSpeciesIndex() {
  const slug = new URL(window.location.href).searchParams.get("species");
  const index = species.findIndex((page) => page.slug === slug);
  if (index >= 0) return index;
  const defaultIndex = species.findIndex((page) => page.slug === DEFAULT_SPECIES);
  return defaultIndex >= 0 ? defaultIndex : 0;
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
  const needle = query.trim().toLocaleLowerCase("en");
  filteredIndices = species
    .map((page, index) => ({ page, index }))
    .filter(({ page }) =>
      page.common_name.toLocaleLowerCase("en").includes(needle)
    )
    .map(({ index }) => index);

  const options = filteredIndices.map((speciesIndex, optionIndex) => {
    const option = document.createElement("li");
    option.id = `species-option-${optionIndex}`;
    option.className = "species-option";
    option.dataset.speciesIndex = String(speciesIndex);
    option.setAttribute("role", "option");
    option.setAttribute("aria-selected", "false");
    option.textContent = species[speciesIndex].common_name;
    return option;
  });

  if (options.length === 0) {
    const empty = document.createElement("li");
    empty.className = "species-no-results";
    empty.textContent = "No matching species";
    elements.options.replaceChildren(empty);
  } else {
    elements.options.replaceChildren(...options);
  }

  elements.matchCount.textContent = `${options.length} matching species`;
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
    elements.search.value = species[selectedIndex].common_name;
  }
}

function enableSpeciesSearch() {
  elements.search.disabled = false;
  elements.previous.disabled = false;
  elements.next.disabled = false;
}

function renderBuildDetails(metadata) {
  const details = [];
  if (metadata.edition) details.push(`Data Version ${metadata.edition}`);
  if (metadata.generated_at) {
    const date = new Date(metadata.generated_at);
    if (!Number.isNaN(date.valueOf())) {
      details.push(`Generated ${new Intl.DateTimeFormat("en-CA", {
        dateStyle: "long",
        timeZone: "UTC"
      }).format(date)}`);
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
    enableSpeciesSearch();
    renderBuildDetails(data.metadata || {});
    const index = requestedSpeciesIndex();
    const requestedSlug = new URL(window.location.href).searchParams.get("species");
    const validRequest = species.some((page) => page.slug === requestedSlug);
    renderSpecies(index, validRequest ? "none" : "replace");
  } catch (error) {
    console.error(error);
    showMessage(
      "The species data could not be loaded. Please refresh the page or try again later.",
      true
    );
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

window.addEventListener("popstate", () => {
  renderSpecies(requestedSpeciesIndex(), "none");
});

initialize();
