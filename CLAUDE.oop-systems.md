# OOP Systems Analysis: R6 vs S3 vs Plain Functions for seminr

> **Related to:** `CLAUDE.ray-refactor-model-matrices.md` (encapsulation refactoring plan)
> **See also:** `CLAUDE.function-naming.md` (naming conventions)
> **Question:** Now that the naming convention uses object names (e.g., `construct`) in function names, would R6 classes be more natural?

## Observation

The proposed naming conventions in `CLAUDE.function-naming.md` produce function names like `construct_items()`, `construct_mode()`, `construct_antecedents()`, `construct_type()` — all prefixed with the object they operate on. This is manual namespace simulation: the function name carries the "receiver" because R's function dispatch doesn't provide one automatically.

In an OOP system, these become methods on an object, and the receiver disappears from the name:

| Plain function style | R6 method style | S3 generic style |
|---|---|---|
| `construct_items(mmMatrix, name)` | `mm$items(name)` | `items(mmMatrix, name)` |
| `construct_mode(mmMatrix, name)` | `mm$mode(name)` | `mode(mmMatrix, name)` |
| `construct_antecedents(smMatrix, name)` | `sm$antecedents(name)` | `antecedents(smMatrix, name)` |
| `all_endogenous(smMatrix)` | `sm$all_endogenous()` | `all_endogenous(smMatrix)` |
| `all_reflective(mmMatrix)` | `mm$all_reflective()` | `all_reflective(mmMatrix)` |

This raises the question: should we use a class system instead of prefixed plain functions?

## R's OOP Options

### S3 (what seminr already uses)

- Informal: class is just a string attribute on any object
- Single dispatch on the first argument's class
- No encapsulation — all fields are public list elements
- Convention-based: methods are `generic.class()` functions
- The R ecosystem default; what CRAN reviewers and R users expect

### S4 (formal classes via `methods` package)

- Formal class definitions with typed slots
- Multiple dispatch (on multiple argument types)
- Formal inheritance with `contains`
- Still value semantics (copy-on-modify)
- Common in Bioconductor, rare in CRAN tidyverse-adjacent packages
- Heavier API surface and learning curve

### R6 (reference classes via `R6` package)

- True OOP with encapsulation (public/private fields and methods)
- Reference semantics (mutable, no copy-on-modify)
- Methods live on the object: `obj$method()`
- Familiar to users of Python/Java/JavaScript classes
- External dependency (`R6` package)

### R5 / Reference Classes (built-in)

- Base R's reference class system (`setRefClass()`)
- Reference semantics like R6 but heavier and less popular
- Generally considered superseded by R6

## Evaluation for seminr

### Arguments for R6

**1. Natural method naming.** Methods like `mm$items("Reputation")` instead of `construct_items(mmMatrix, "Reputation")` eliminate the object-name prefix. The naming convention problem dissolves — you don't need naming rules for function prefixes when the object provides the namespace.

**2. Encapsulation with private fields.** R6 lets you hide the raw matrix behind `private$data` and expose only the accessor API. This is the strongest form of the "representation independence" goal — callers literally cannot access the internal matrix.

**3. Mutable state for model building.** During model specification and estimation, objects are built up incrementally (rows appended to mmMatrix, weights computed iteratively). R6's reference semantics make mutation natural rather than fighting R's copy-on-modify.

**4. Method discoverability.** `mm$` with tab-completion in RStudio shows all available operations on the object. With plain functions, you need to know the prefix convention and hope autocomplete surfaces `construct_items` when you type `construct_`.

**5. Grouping related state.** An R6 `MeasurementMatrix` could hold the matrix, precomputed lookups (the `indicators_of` map from the caching analysis), and derived state in one coherent unit.

### Arguments against R6

**1. R6 is a dependency.** seminr currently has no OOP package dependencies. Adding `R6` is lightweight (~50KB, no transitive deps), but it's a new dependency for what is currently an internal refactoring. For a CRAN package that values minimal dependencies, this matters.

**2. Reference semantics clash with existing code.** The codebase extensively uses value semantics:
```r
mmMatrix <- seminr_model$mmMatrix   # local copy
mmMatrix <- rbind(mmMatrix, new_rows)  # modify the copy
```
With R6, `mmMatrix <- seminr_model$mmMatrix` would create a *reference*, not a copy. Modifying it modifies the original. Every site that takes a "local copy" of a matrix would become a subtle aliasing bug. This is the single biggest risk — it would require auditing every assignment of mmMatrix/smMatrix to determine intent.

**3. Breaks R idioms.** R users expect `$` to access data slots, not call methods. `model$mmMatrix[i, j]` is idiomatic R; `model$mm$items("x")` is not. The DSL layer (`constructs()`, `relationships()`, `paths()`) is designed to read like a specification language, not an OOP method chain.

**4. S3 methods would need coexistence or migration.** seminr's S3 classes (`pls_model`, `boot_seminr_model`, etc.) with their `print()`, `summary()`, `plot()` methods would need to coexist with R6 internals. R6 and S3 can coexist, but it creates a dual-paradigm codebase — some objects are S3 lists with generic dispatch, others are R6 with method dispatch.

**5. Matrix operations don't fit OOP.** The hot path is vectorized matrix algebra — `normData %*% outer_weights`, `stats::cov(...)`, `solve(...)`. These operate on raw numeric matrices and vectors. Wrapping them in R6 method calls adds indirection with no benefit. The matrices being encapsulated (mmMatrix, smMatrix) are lookup tables for *metadata*, not the numeric computation itself.

**6. Testing and debugging are harder with reference semantics.** Value semantics mean you can inspect any intermediate state without worrying about mutation. Reference semantics require careful tracking of when and where objects are modified. `testthat` expectations on R6 objects need to account for shared state between test cases.

**7. The refactoring effort is substantial.** Every file that touches mmMatrix or smMatrix (~48 files, ~175+ access sites) would need to change from matrix subsetting to method calls. This is roughly the same mechanical effort as the plain-function approach, but with added risk from the semantic shift (value → reference).

## Evaluation for S3 (aggressive generics)

A middle ground: instead of R6, use S3 generics more aggressively. The naming convention already proposes S3 dispatch for `construct_items` and `construct_names`. This could be extended:

```r
# Generic
items <- function(x, ...) UseMethod("items")

# Methods
items.mmMatrix <- function(x, construct_name, ...) { ... }
items.construct <- function(x, ...) { ... }
items.pls_model <- function(x, construct_name, ...) { ... }
```

This gives clean names (`items(mm, "x")` instead of `construct_items(mm, "x")`) while staying in R's native paradigm.

**Advantages over R6:**
- No new dependency
- Value semantics preserved — no aliasing bugs
- Consistent with existing S3 classes in seminr
- CRAN-friendly and familiar to R users
- S3 dispatch is fast (~1µs overhead)

**Disadvantages:**
- No true encapsulation — callers can still access `mm[i,j]` directly
- Generic names like `items()` risk collision with other packages in the user's namespace
- Single dispatch only — can't dispatch on both container type and query type
- Still need the `object_qualifier` prefix for non-generic functions to avoid ambiguity

**Key risk — name collisions:** Generic names like `items()`, `mode()`, `type()` are dangerously broad. `mode()` already exists in base R. `type()` conflicts with common usage. Even `items()` could collide with other packages. The current convention of `construct_items()` avoids this entirely by being specific. For S3 generics that are *internal* (not exported), this is less of a concern — but if they're ever exported, collisions become real.

## Environments as Lightweight Namespaces

R environments offer a middle path that's easy to overlook: you can get the `obj$method()` syntax and shared private state *without* R6, using only base R. An environment is a bag of named bindings with a parent pointer — closures that share the same enclosing environment effectively share private state.

### The pattern

A constructor function creates an environment, binds the raw data into it, and attaches accessor closures:

```r
mm_accessor <- function(mmMatrix) {
  self <- new.env(parent = emptyenv())

  # Private — accessible to closures but not to callers (by convention)
  data <- mmMatrix

  # Public methods
  self$items <- function(construct_name) {
    data[data[, "construct"] == construct_name, "measurement"]
  }

  self$mode <- function(construct_name) {
    data[data[, "construct"] == construct_name, "type"][1]
  }

  self$all_constructs <- function() {
    unique(data[, "construct"])
  }

  self$all_reflective <- function() {
    unique(data[data[, "type"] == "C", "construct"])
  }

  self
}

# Usage:
mm <- mm_accessor(seminr_model$mmMatrix)
mm$items("Reputation")
mm$mode("Reputation")
mm$all_constructs()
```

This gives the `mm$items("x")` syntax, tab-completion in RStudio, and shared access to the underlying matrix — all with zero dependencies.

### How it compares

| Criterion | Plain functions | Environments | R6 |
|---|---|---|---|
| Syntax | `construct_items(mm, "x")` | `mm$items("x")` | `mm$items("x")` |
| Dependencies | None | None | `R6` package |
| Encapsulation | Convention-only | Closure-based (soft) | Private fields (enforced) |
| Semantics | Value | Reference (envs are ref objects) | Reference |
| Inheritance | N/A | Manual (prototype chains) | Formal (`inherit`) |
| `$`-completion | No | Yes | Yes |
| Active bindings | No | Yes (`makeActiveBinding`) | Yes (`active` field) |
| `clone()` | N/A | Manual | Built-in |
| Formal class? | No | No (unless you add one) | Yes |

### What environments get right for seminr

**1. Zero dependencies.** Pure base R — `new.env()` and closures. No package to add to DESCRIPTION.

**2. Same `$method()` syntax as R6.** Callers write `mm$items("x")`, get tab-completion, and don't need to know the object prefix convention. The naming problem dissolves the same way it does with R6.

**3. Soft encapsulation.** The raw matrix lives in the closure's enclosing environment, not as a named element on the returned object. Callers can't accidentally do `mm$data[i,j]` because `data` isn't bound in `self`. (They *can* reach it with `environment(mm$items)$data`, but that's deliberate introspection, not accidental access.)

**4. Reference semantics are opt-in.** You can choose whether the constructor returns the environment directly (reference semantics) or wraps it in a list (value semantics via copy-on-modify of the list shell). For seminr's use case — immutable lookup tables — you'd return the environment directly since nothing mutates anyway.

**5. Precomputed lookups fit naturally.** The constructor can precompute indices at creation time, just like the caching analysis suggested:

```r
mm_accessor <- function(mmMatrix) {
  self <- new.env(parent = emptyenv())
  data <- mmMatrix

  # Precompute at construction time
  constructs <- unique(data[, "construct"])
  items_cache <- lapply(
    setNames(constructs, constructs),
    function(c) data[data[, "construct"] == c, "measurement"]
  )

  self$items <- function(construct_name) items_cache[[construct_name]]
  # ...
  self
}
```

### What environments get wrong

**1. No formal class identity.** An environment-based object isn't an S3 or R6 class. You can't write `is(mm, "MeasurementMatrix")` unless you manually assign a class attribute. S3 generics like `print.MeasurementMatrix` require this extra wiring.

**2. No inheritance mechanism.** R6 gives you `inherit = ParentClass`. Environments require you to manually chain prototypes or copy methods — error-prone and non-standard.

**3. Reference semantics are still reference semantics.** The same aliasing concerns from R6 apply: `mm2 <- mm` creates a reference, not a copy. For immutable lookup tables this is fine (nothing mutates), but if you ever add mutation, the aliasing bugs return. Unlike R6, there's no built-in `$clone()` — you'd need to write your own.

**4. No private/public distinction.** The "privacy" relies on the closure pattern — data in the enclosing environment is hidden from `ls(mm)` but reachable via `environment()`. R6's `private` is a stronger convention (though still breakable via `$.__enclos_env__$private`). In practice, the difference is negligible for internal code — neither truly prevents determined access.

**5. Unfamiliar pattern for R contributors.** Most R developers haven't seen environment-as-namespace used this way. R6 is unusual in CRAN packages but well-documented; environment-based objects are unusual *and* ad-hoc. Contributors would need to understand closures and environment scoping to modify the accessors.

**6. No method documentation pattern.** roxygen2 documents functions and S3/S4/R6 methods. There's no established convention for documenting methods on an environment-based object. Since these are internal, this matters less — but it's a gap if the pattern ever extends to user-facing API.

### Environment-based objects in the wild

This isn't a novel pattern. Several well-known R packages use it:

- **`proto` package** — formalizes prototype-based OOP using environments
- **`ggplot2` internals** (early versions) — ggproto is essentially this pattern formalized
- **`R6` itself** — R6 objects *are* environments under the hood; R6 is a DSL for constructing this pattern with guardrails
- **`modules` package** — uses environments as module namespaces, very close to this use case

### Where this sits in the progressive path

Environments slot in between plain functions and R6:

1. **Now:** Plain accessor functions with `object_qualifier` names (current plan)
2. **Possible intermediate:** Wrap accessors in environment-based objects for `$method()` syntax
3. **If needed later:** Migrate to R6 for formal inheritance, clone, active bindings

Step 2 is lower-commitment than R6 — no dependency, easy to prototype, and the closures are literally the same accessor function bodies. If the `$method()` syntax proves valuable in practice, graduating to R6 is mechanical (replace `new.env()` with `R6Class$new()` and move methods into the class definition).

## Comparison Matrix

| Criterion | Plain functions | S3 generics | Environments | R6 classes |
|---|---|---|---|---|
| Naming clarity | Object prefix required | Clean short names | Clean method names | Clean method names |
| Encapsulation | Convention-only | Convention-only | Closure-based (soft) | Enforced (private fields) |
| Dependencies | None | None | None | `R6` package |
| Semantics | Value (safe) | Value (safe) | Reference (but immutable data is safe) | Reference (aliasing risk) |
| R ecosystem fit | Standard | Standard | Uncommon but pure base R | Unusual for CRAN packages |
| Migration risk | Low (mechanical) | Low (mechanical) | Low (same function bodies) | High (semantic shift) |
| Discoverability | Know the prefix | `methods()` | Tab-complete on `$` | Tab-complete on `$` |
| Coexistence with S3 | Natural | Natural | Needs manual class attr | Dual paradigm |
| Numeric matrix ops | Natural | Natural | Natural (closures over raw data) | Awkward wrapping |
| Name collision risk | None (specific names) | High (generic names) | None (methods on object) | None (methods on object) |
| Inheritance | N/A | Informal | Manual prototype chains | Formal (`inherit`) |
| Contributor familiarity | High | High | Low (ad-hoc pattern) | Medium (well-documented) |

## Recommendation

**Stick with plain functions for this refactoring phase.** The naming convention concern is real but cosmetic — it doesn't cause bugs, slow development, or create architectural problems. The `object_qualifier` prefix pattern (`construct_items`, `construct_mode`) is well-established in the R ecosystem (cf. `str_detect`, `str_replace` in `stringr`; `vec_cast`, `vec_ptype` in `vctrs`).

The current plan (plain accessor functions) achieves the primary goals — representation independence and bug prevention — with the lowest risk. The function names are verbose but unambiguous.

### When R6 *would* be justified

R6 becomes the right choice if any of these conditions emerge:

1. **Mutable state management becomes a real problem.** If model building or estimation starts needing transactional updates, rollback, or coordinated mutation across multiple fields, reference semantics earn their keep.

2. **The matrices evolve into richer objects.** If mmMatrix grows to hold not just the 3-column lookup table but also precomputed indices, cached derived state, validation rules, or lazy-computed properties, a class with methods becomes more natural than a growing bag of functions that all take the same first argument.

3. **Multiple interacting internal objects need coordination.** If mmMatrix and smMatrix need to stay in sync (e.g., renaming a construct updates both), an object that owns both matrices and enforces consistency is cleaner than functions that take two matrices and hope callers pass the right pair.

4. **The package grows a plugin or extension API.** If external packages need to extend seminr's internal model representation, R6's inheritance and method override mechanisms are more robust than S3 informal dispatch.

None of these conditions currently apply. The matrices are small immutable lookup tables used during estimation. They don't mutate, they don't need caching, they don't need validation, and no external packages extend their structure.

### Performance analysis confirms

Quantified analysis of the PLS hot path confirms R6 caching would not be worthwhile. Matrix metadata lookups (mmMatrix/smMatrix subsetting on 30-50 row tables) account for ~3% of PLS iteration time (~100-170us of lookup vs. ~3-5ms of actual computation per iteration). On matrices this small, caching overhead could approach the cost of the raw lookup itself. Simple precomputation — a few local variables before the PLS iteration loop — captures the same ~3% with zero architectural cost. See precomputation tasks in Phase 2 of `CLAUDE.ray-refactor-model-matrices.md`.

### Progressive path

The plain-function refactoring is *compatible* with future migration to either environments or R6:

1. **Now:** Plain accessor functions with `object_qualifier` names (current plan)
2. **Optional intermediate:** Wrap accessors in environment-based objects for `$method()` syntax — zero dependencies, same function bodies, easy to prototype
3. **Later (if justified):** Graduate to R6 for formal inheritance, clone, active bindings — the environment closures become R6 method bodies mechanically

Step 1 is prerequisite to both steps 2 and 3 — you can't wrap matrices in any object system while 175+ sites access them raw. The accessor functions become method bodies in either approach. No work is wasted.

Step 2 is a low-commitment way to test whether the `$method()` syntax is actually worth it in practice before committing to R6's dependency and formalism.

### S3 generics — use selectively

The `CLAUDE.function-naming.md` plan already proposes S3 generics for `construct_items` and `construct_names` where multiple types need the same operation. This is the right amount of S3 — use it where polymorphism provides real value (one name, multiple implementations), not as a wholesale replacement for the naming convention.

## Decision

**Plain functions (status quo plan), with S3 generics where polymorphism is needed.** Revisit R6 only if the matrices evolve beyond simple lookup tables.

---

Last updated: 2026-02-25
