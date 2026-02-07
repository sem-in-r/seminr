# Plan: Fix Lambda/Greek Symbol Rendering in Plot Export (Issue #226)

> **Note:** This planning document should be manually removed when no longer needed. Please ask the maintainer to delete it at the appropriate time.

## Problem Summary

When using `save_plot()` to export SEMinR plots to PDF/PNG, Greek symbols (lambda, beta, gamma) and the superscript-2 symbol fail to render on many systems. The symbols display correctly in RStudio's viewer (which uses a browser-based HTML widget) but break in the rsvg-based export pipeline.

**Affected users:** Windows users, Linux users with `locale: C`, and some macOS users — essentially anyone whose system fonts lack the specific Unicode glyphs used.

## Root Cause Analysis

The code currently uses **Supplementary Multilingual Plane (SMP)** Unicode characters from the "Mathematical Alphanumeric Symbols" block:

| Symbol | Current Code | Unicode Point | Block |
|--------|-------------|---------------|-------|
| Lambda | `\U0001D706` | U+1D706 | Mathematical Alphanumeric Symbols (SMP) |
| Beta   | `\U0001D6FD` | U+1D6FD | Mathematical Alphanumeric Symbols (SMP) |
| Gamma  | `\U0001D6FE` / `\U0001D738` | U+1D6FE / U+1D738 | Mathematical Alphanumeric Symbols (SMP) |
| Squared | `\U00B2` | U+00B2 | Latin-1 Supplement (BMP) — this one is fine |

These SMP characters (code points above U+FFFF) are problematic because:

1. **Font coverage is sparse**: The default font `helvetica` (and most standard system fonts like Arial, Times New Roman) do **not** include glyphs for the Mathematical Alphanumeric Symbols block. Only specialized math fonts (STIX, Cambria Math, Latin Modern Math) include them.

2. **The rsvg rendering pipeline** (`DiagrammeRsvg::export_svg()` → `rsvg::rsvg_pdf()`) depends on the system's font resolution (via librsvg/Pango/fontconfig). When no installed font contains the requested glyph, the character renders as a blank, box, or question mark.

3. **Browser rendering works** because HTML widget viewers (RStudio viewer, web browsers) have much better Unicode fallback chains and can substitute fonts automatically. This is why `plot()` looks correct in RStudio but `save_plot()` does not.

4. **Writing raw SVG works** (as user `singledoggy` discovered) because SVG viewers also have better font fallback. The problem is specifically in rsvg's rasterization/PDF conversion step.

### Why the superscript-2 (`\U00B2`) works

U+00B2 is in the Basic Multilingual Plane (BMP) Latin-1 Supplement block — virtually every font includes this glyph. This confirms the issue is specifically about SMP character support in fonts.

## Proposed Fix

**Replace SMP Mathematical Alphanumeric characters with BMP Greek letters**, using HTML-like markup in DOT labels for italic styling where desired.

### Character Mapping

| Symbol | Current (SMP) | Proposed (BMP) | Unicode Point |
|--------|--------------|----------------|---------------|
| Lambda | `\U0001D706` (math italic λ) | `\u03BB` (Greek small letter lambda) | U+03BB |
| Beta   | `\U0001D6FD` (math italic β) | `\u03B2` (Greek small letter beta) | U+03B2 |
| Gamma (non-bold) | `\U0001D6FE` (math italic γ) | `\u03B3` (Greek small letter gamma) | U+03B3 |
| Gamma (bold) | `\U0001D738` (math bold italic γ) | `\u03B3` (Greek small letter gamma) | U+03B3 |

BMP Greek letters (U+0370–U+03FF) are supported by virtually all standard fonts including Helvetica, Arial, Times New Roman, and Liberation families across all major platforms.

### Why This Works

- DOT labels already use HTML-like syntax: `label = < {variable} = {value} >` (see `format_edge_boot_label()` at `plot_dot.R:735`)
- Edge templates like `edge_template_default()` already use HTML tags like `<BR />` and `<FONT POINT-SIZE='7'>`
- BMP Greek letters are universally supported in system fonts
- The visual difference between mathematical italic lambda (𝜆) and regular lambda (λ) is minimal and unlikely to matter for plot readability

## Files to Modify

### `R/plot_dot.R`

1. **Lines 1507-1512** — Lambda symbol for measurement model edges:
   ```r
   # Change from:
   lambda <- "\U0001D706"
   # Change to:
   lambda <- "\u03BB"
   ```

2. **Lines 1028-1035** — Beta/Gamma symbols for structural model edges:
   ```r
   # Change from:
   beta <- "\U0001D6FD"
   gamma <- "\U0001D6FE"
   gamma <- "\U0001D738"
   # Change to:
   beta <- "\u03B2"
   gamma <- "\u03B3"
   ```

3. **Lines 910-914** — Squared symbol: No change needed (already BMP U+00B2).

## Testing & Verification

### Automated Tests

All existing vdiffr visual regression tests in the project are **commented out** and dormant. The only active plot test (`test-plot-save-plot.R`) checks that `save_plot()` produces non-empty files — it does not verify visual content.

**No feasible automated test exists for this issue.** The bug is in the rsvg rasterization/PDF step (SVG → PDF/PNG font resolution), not in the SVG text content. A test would need to render to PDF/PNG then OCR or pixel-inspect the output, which is fragile and not practical for CRAN.

The existing test suite validates that the change doesn't break anything:
- `devtools::test(filter = "plot")` → **61 passed, 0 failed** (after fix applied)

### Manual Verification (Completed)

**Before fix** (`\U0001D706` SMP characters):
- DOT source hex: `f0 9d 9b bd` (4-byte UTF-8 for U+1D6FD math italic beta)
- Renders on macOS (good font fallback) but fails on Windows/Linux with limited fonts

**After fix** (`\u03BB` BMP characters):
- DOT source hex: `ce bb` (2-byte UTF-8 for U+03BB Greek small letter lambda)
- Uses BMP Greek block (U+0370–U+03FF) supported by virtually all system fonts
- Visual output is identical — both PNG renders show λ, β, r² symbols correctly

Before/after PNG and PDF comparison files are in the project root (`before.png`, `after.png`, `before.pdf`, `after.pdf`) for manual review. These should be deleted before committing.

## Risk Assessment

- **Low risk**: The change is minimal (3 lines of character literals) and the fallback mechanism (`plot.specialcharacters = FALSE`) remains unchanged
- **Visual difference**: Negligible — BMP Greek letters look nearly identical to their mathematical italic SMP counterparts in most fonts
- **Backward compatibility**: Users who previously set `plot.specialcharacters = FALSE` as a workaround will now be able to set it back to `TRUE` (the default)
- **No API changes**: No function signatures, theme parameters, or user-facing behavior changes
