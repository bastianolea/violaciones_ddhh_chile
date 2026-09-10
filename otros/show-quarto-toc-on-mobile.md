# Showing the Quarto TOC on mobile (and keeping it in place)

By default, Quarto hides the table of contents (TOC) on narrow screens. This
gist shows how to bring it back on mobile, drop it inline at a specific point
in the page, and stop it from following the scroll — while keeping the original
desktop TOC untouched.

> Written up from a real fix on a Quarto HTML site with a right-hand TOC
> (`toc-location: right`). Tested with a recent Quarto release.

## The problem

The site uses a right-hand TOC on desktop:

```yaml
format:
  html:
    toc: true
    toc-location: right
    theme: custom.scss
```

On desktop this renders a sticky TOC in the right margin. On mobile (viewports
under ~768px), the TOC disappears entirely. Quarto does this on purpose to save
horizontal space, but if the TOC is important for navigation you may want it
back — ideally positioned somewhere sensible in the flow of the page rather
than crammed into a margin.

## Root cause

Quarto ships a built-in media query that hides **both** the TOC nav and its
container below `767.98px`:

```css
@media (max-width: 767.98px) {
  nav[role="doc-toc"] { display: none; }
  /* #quarto-margin-sidebar is also collapsed */
}
```

So re-showing the TOC on mobile takes two things:

1. **CSS** to override the `display: none` and undo the margin/grid positioning
   the container inherits from its desktop styling.
2. **A little JavaScript** to move the TOC into a meaningful spot in the page on
   mobile, and restore it to the margin on desktop.

## Part 1 — CSS override

Add this to your theme SCSS (e.g. `custom.scss`). It targets the same
breakpoint Quarto uses so the two agree on what "mobile" means.

```scss
@media (max-width: 767.98px) {
  // Re-show the TOC nav that Quarto hides on mobile.
  #quarto-content nav[role="doc-toc"] {
    display: block;
    position: static;
    max-height: none;
  }

  // The TOC's container. On desktop it lives in a CSS grid margin column
  // (grid-column / grid-row) and is `position: sticky` so it follows the
  // scroll. On mobile we re-show it, let it flow in the normal body column,
  // and make it static so it does NOT stick to the top while scrolling.
  #quarto-margin-sidebar {
    display: block;
    grid-column: body-content-start / body-content-end;
    grid-row: auto;
    position: static;
    top: auto;
  }
}
```

Key points:

- `display: block` reverses Quarto's `display: none`.
- Resetting `grid-column` / `grid-row` frees the container from the right-hand
  margin column so it flows in the main body instead of being pinned to the
  edge or jumping to the top of the grid.
- `position: static; top: auto;` is what stops the TOC from sticking to the top
  of the viewport as you scroll on mobile. Without this, it inherits the
  desktop `position: sticky` and floats over the content.

## Part 2 — JavaScript to reposition the TOC

CSS alone shows the TOC, but it appears wherever the container sits in the DOM.
To drop it at a specific spot on mobile (here, just above the section with
`id="detenciones"`) and restore it on desktop, use a small script.

We move the **whole `#quarto-margin-sidebar` container**, not just the inner
`nav`, because the container carries the `.sidebar` class that all the TOC
styling depends on.

```html
<script>
document.addEventListener("DOMContentLoaded", function () {
  const sidebar = document.querySelector("#quarto-margin-sidebar");
  // The section you want the TOC to appear above on mobile.
  const target  = document.querySelector("section#detenciones");
  if (!sidebar || !target) return;

  // Remember the original position so we can restore it on desktop.
  const originalParent = sidebar.parentNode;
  const originalNext   = sidebar.nextSibling;
  const mq = window.matchMedia("(max-width: 767.98px)");

  function place(e) {
    if (e.matches) {
      // Mobile: insert the TOC right before the target section.
      target.parentNode.insertBefore(sidebar, target);
    } else {
      // Desktop: return the TOC to its original spot in the margin.
      originalParent.insertBefore(sidebar, originalNext);
    }
  }

  place(mq);                        // run once on load
  mq.addEventListener("change", place); // re-run on resize / rotation
});
</script>
```

Using `matchMedia` (rather than a one-time check) means resizing the window or
rotating the device moves the TOC to the right place without a reload.

To choose a different insertion point, change the `target` selector to any
element on your page — for example `document.querySelector("#my-section")` or
`document.querySelectorAll("h2")[2]`.

## Part 3 — Wiring it up

Reference the SCSS theme and inject the script after the body via the document
front matter (or `_quarto.yml`):

```yaml
format:
  html:
    toc: true
    toc-title: Índice
    toc-location: right
    theme: custom.scss
    include-after-body: toc-mobile.html   # the <script> from Part 2
```

Put the `<script>` block in a standalone file (e.g. `toc-mobile.html`) so
`include-after-body` can pick it up.

## Result

- **Desktop (≥768px):** unchanged — the TOC stays sticky in the right margin.
- **Mobile (<768px):** the TOC appears inline just above the chosen section,
  keeps its original styling (it still lives inside `.sidebar`), and scrolls
  away with the content instead of sticking to the top.

## Notes / caveats

- The `767.98px` breakpoint matches Quarto's internal media query. If you use a
  different value, the CSS and Quarto can disagree about what "mobile" is.
- The `#detenciones` id is specific to this site. Swap in whatever target makes
  sense for yours; if the element doesn't exist the script exits harmlessly.
- Selectors like `#quarto-margin-sidebar` are Quarto internals and could change
  in future releases — worth a quick check after upgrading.
