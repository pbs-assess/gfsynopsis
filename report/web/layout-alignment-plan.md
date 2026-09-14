# Web layout alignment plan

## Goal

Create one consistent horizontal content rail for the species page so that the
metadata text, the `Plot descriptions` link, and the synopsis figures all share
the same left and right alignment at desktop and mobile widths.

Keep `Plot descriptions` above the figures as a lightweight text link. It should
remain right-aligned on wider screens and move to the left on narrow screens.

## Current problem

The page currently mixes three horizontal alignments:

1. The page and toolbar use the outer content width.
2. The species metadata panel has its own inner horizontal padding.
3. The figures grid starts closer to the outer edge than the metadata text.

The plot link was aligned to the metadata panel's inner padding, but the figures
below it still use a wider rail. This makes the link look detached from the
content it describes.

## Proposed design

Use two deliberate layers:

- An outer species content width shared by the species header, metadata panel,
  and figures section.
- A shared inner gutter used by metadata text, the plot link, and the figure
  grid.

The visual relationship should be:

```text
species panel edge
  └─ shared inner gutter: metadata text / plot link / figure grid
species panel edge
```

The link should be a normal underlined link, without a button border or filled
background. The hidden `Synopsis figures` heading should remain in the markup as
the accessible label for the figures section.

## Implementation steps

1. In `report/web/app.css`, define or reuse a single custom property for the
   shared horizontal gutter, matching the metadata panel's current
   `clamp(1.25rem, 3vw, 2rem)` padding.
2. Give `.figures-section` the same maximum width and centering behavior as the
   species header and metadata panel.
3. Apply the shared gutter consistently to `.section-heading` and `.figures`.
   Remove any width or padding combination that makes the figure grid wider
   than the metadata text rail.
4. Keep `.section-heading .plot-descriptions-link` as a text link. On desktop,
   align it to the right edge of the shared inner rail; at the mobile breakpoint,
   align it to the left edge of that same rail.
5. Preserve the existing `#plot-descriptions-link` ID and JavaScript URL logic
   so species selection and English/French navigation continue to update the
   link correctly.
6. Keep the `h3#figures-heading` visually hidden rather than deleting it, so
   `aria-labelledby="figures-heading"` remains meaningful to screen readers.

## Acceptance criteria

- On desktop, the right edge of `Plot descriptions` aligns with the right edge
  of the metadata's inner content, not the metadata box itself.
- On desktop, the left and right edges of the figure grid align with the same
  inner content rail.
- At 360 px and 390 px widths, the link and figures align with the metadata
  text and do not cause horizontal scrolling.
- At widths near 768 px, the layout transitions without a visible alignment
  jump or clipped content.
- The link remains visible and keyboard-focusable with a clear focus outline.
- English pages show `Plot descriptions`; French pages show
  `Description des graphiques`.
- The hidden figures heading remains present in the accessibility tree.

## Verification

1. Run `node --check report/web/app.js` and
   `node --check report/web/plot-descriptions.js`.
2. Run `git diff --check`.
3. Rebuild the web output with `Rscript report/R/10-build-web.R`.
4. Preview the generated site locally and inspect an ordinary species plus an
   exceptional species such as Pacific Cod and Rougheye/Blackspotted Rockfish
   Complex.
5. Check desktop, 768 px, 390 px, and 360 px screenshots for alignment and
   horizontal overflow.
6. Verify English/French link text, URLs, browser navigation, keyboard focus,
   and a clean browser console.

## Scope

This change should affect layout and presentation only. Do not change species
data, figure generation, figure assets, or the language-routing behavior.
