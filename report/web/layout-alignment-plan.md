# Web layout alignment plan

## Outcome

Give the species synopsis one intentional horizontal layout system. The species
header and metadata retain their outer panel edges, while the metadata content,
`Plot descriptions` link, and synopsis figures share one inner content rail at
all viewport widths.

Keep `Plot descriptions` as a lightweight, underlined text link above the
figures. Align it to the rail's right edge on wider screens and its left edge on
narrow screens.

## Review of the current layout

The proposed direction is appropriate, but the implementation needs one clear
owner for width and one for padding. At present:

- `main` can grow to `--content-width` (1500 px);
- `.species-header` and `.metadata-panel` stop at 72 rem and include their own
  horizontal padding;
- `.section-heading` also stops at 72 rem and has matching padding; and
- `.figures` has neither the 72 rem limit nor the matching padding, so it grows
  to the full width of `main`.

This explains the mismatch. Adding limits independently to both
`.figures-section` and its children could introduce nested width constraints or
double gutters. The revised implementation below avoids that ambiguity.

## Layout contract

Define these semantic properties in `:root` in `report/web/app.css`:

```css
--species-content-width: 72rem;
--species-gutter: clamp(1.25rem, 3vw, 2rem);
```

Use the following ownership model:

```text
main (site-wide maximum width)
└─ species component (species maximum width, centred)
   ├─ panel edge
   └─ inner rail (species gutter on each side)
      ├─ header content
      ├─ metadata content
      ├─ Plot descriptions
      └─ synopsis figures
```

- `.species-header`, `.metadata-panel`, and `.figures-section` own the same
  centred outer width: `min(100%, var(--species-content-width))`.
- The header and metadata panel keep their visible backgrounds/borders and use
  `--species-gutter` internally.
- `.figures-section` has no panel treatment and no horizontal padding of its
  own. Its direct layout children, `.section-heading` and `.figures`, each use
  `padding-inline: var(--species-gutter)`.
- Once `.figures-section` owns the width, remove the independent width and
  centring rules from `.section-heading`; otherwise the layout has two
  competing containers.
- Do not add horizontal margins to individual figures. Their grid tracks should
  fill the shared inner rail exactly.

This makes the aligned edges a consequence of the container model rather than
several repeated pixel values.

## Responsive behaviour

- Keep the current site-wide outer margin change at 700 px; it is separate from
  the species gutter and should not require a species-specific override.
- Above 700 px, right-align the plot-description link within the shared rail.
- At 700 px and below, left-align it. Preserve the existing compact vertical
  spacing and ensure its focus outline is not clipped.
- Retain `minmax(0, 1fr)` grid tracks to prevent intrinsic image width from
  causing horizontal overflow.
- Reassess the existing two-column breakpoint after constraining the figures.
  A 72 rem container with two gutters leaves roughly 32–33 rem per figure. If
  labels in the rendered synopsis pages are not comfortably readable at that
  size, keep a single column or widen the *shared species container*; do not let
  the figures silently break out of the alignment system. Prefer a container
  query based on available figure width if the project is comfortable adding
  one; otherwise set the media breakpoint from the measured minimum readable
  figure width.

## Implementation steps

1. Add `--species-content-width` and `--species-gutter` to `:root`.
2. Replace repeated `72rem` and `clamp(1.25rem, 3vw, 2rem)` values in the
   species header, metadata panel, and figures layout with those properties.
   Do not mechanically replace unrelated padding on the descriptions page.
3. Add `.figures-section` to the centred species-width container rule.
4. Make `.section-heading` and `.figures` use the shared horizontal gutter;
   remove `.section-heading`'s independent width and centring declarations.
5. Keep `.section-heading` as the alignment row for the link. Preserve its
   transparent, borderless, underlined treatment and the mobile
   `align-self: flex-start` rule.
6. Preserve `#plot-descriptions-link`, the JavaScript URL/language logic, and
   `h3#figures-heading`. The hidden heading must continue to label the figures
   section through `aria-labelledby`.
7. Test figure legibility at the resulting desktop track width. If the current
   two-column layout is too dense, apply the responsive decision above before
   considering the work complete.

## Acceptance criteria

- The species header, metadata panel, and figures section have the same outer
  maximum width and are centred on the same axis.
- The left and right edges of the metadata content, plot-description link row,
  and figure grid match to within 1 CSS pixel.
- On wider screens, the link's right edge aligns with the figure grid and
  metadata inner rail, not the metadata panel border.
- At 700 px and below, the link begins at the same left edge as the figures and
  metadata content.
- At 1500, 1280, 1100, 768, 700, 390, and 360 px viewport widths, there is no
  horizontal page scrolling, clipped focus outline, or unintended double
  gutter. Check both sides of the 700 px breakpoint.
- At the breakpoint where the figure grid changes columns, every plot label is
  readable at 100% browser zoom. At 200% zoom, content remains available
  without two-dimensional page scrolling.
- Spacing from metadata panel to link and from link to figures looks deliberate
  and consistent; loading and error messages do not disturb the rail.
- The link is visible, keyboard-focusable, and has a clear focus indicator.
- English shows `Plot descriptions`; French shows `Description des graphiques`,
  and both resolve to the selected species and language.
- `h3#figures-heading` remains in the accessibility tree and continues to label
  the figures section.

## Verification

1. Run `node --check report/web/app.js` and
   `node --check report/web/plot-descriptions.js`.
2. Run `git diff --check`.
3. Rebuild with `Rscript report/R/10-build-web.R`.
4. Preview `report/web/generated/` locally and inspect Pacific Cod plus
   Rougheye/Blackspotted Rockfish Complex. Also inspect a species that displays
   notes or conservation status so wrapped metadata is covered.
5. Capture or compare views at 1500, 1280, 1100, 768, 700, 390, and 360 px.
   Check exact rail edges, figure-label legibility, grid reflow, and horizontal
   overflow. Test 699 and 701 px when adjusting the mobile breakpoint.
6. Test keyboard focus, 200% zoom, English/French link text and URLs, species
   selection, back/forward navigation, image loading/error states, and the
   browser console.
7. Inspect the accessibility tree to confirm the figures section retains its
   accessible name.

## Scope

This is a layout and presentation change only. Do not change species data,
figure generation, figure assets, or language-routing behaviour. Adjusting the
figure-column breakpoint is in scope only when needed to preserve legibility
after applying the shared rail.
