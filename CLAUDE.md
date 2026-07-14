# Homecook — agent context

Homecook is a **fully offline, serverless chess-opening trainer**. A user enters
or imports an opening, optionally edits it, branches it into variants, and then
does spaced-repetition practice runs to memorize it. Everything runs in the
browser: the repertoire ("home cook") lives in **IndexedDB**, and the only
import/export is **downloading / uploading a home cook file**. There is no
backend and there must never be one.

## Non-negotiable product constraints

- **No server.** All state is client-side (IndexedDB). Never introduce a
  network dependency for core functionality.
- **Portable saves.** A "home cook" can be exported to a file and re-imported on
  another device. Keep the on-disk format stable and versioned.
- **Memorization that resists AFK.** Practice tracks *recall time* and *number
  of tries*, and schedules reviews with spaced repetition. Timings must be
  robust to the user walking away mid-card (cap/discard absurd durations, pause
  on tab blur) so an idle tab never poisons the schedule.
- **Variants are first-class.** A repertoire is a *tree* of moves, not a single
  line.

## Demoing features — ALWAYS record video for BOTH mobile and desktop

> When you want to show off / demo a feature, **send the user a screen-recording
> video, and always produce one for BOTH a mobile viewport and a desktop
> viewport.** This is a standing instruction, not a one-off.

The reusable recorder lives in `demo/`. It builds the site, serves it locally,
drives it with Playwright, and writes an `.mp4`/`.webm` per viewport.

```
# Build the site, then record every scenario in demo/scenarios/ for both
# mobile + desktop viewports into demo/out/.
node demo/record.mjs                 # all scenarios, both viewports
node demo/record.mjs practice        # one scenario, both viewports
```

Then attach the resulting files from `demo/out/` with SendUserFile (use
`status: "proactive"` when the user is away). Send the mobile and desktop clips
together so both form factors are always covered.

Chromium + ffmpeg are preinstalled at `/opt/pw-browsers`; do **not** run
`playwright install`. `PLAYWRIGHT_BROWSERS_PATH` is already set.

## Build & run

The app is **OCaml compiled to JS with `js_of_ocaml`**, using Jane Street's
**Bonsai** UI framework. Output is a single static bundle — perfect for the
offline requirement.

```
opam install . --deps-only -y      # first time (needs opam.ocaml.org access)
opam exec -- dune build            # produces _build/install/default/bin/
python3 -m http.server -d _build/install/default/bin 8000   # serve locally
```

CI (`.github/workflows/deploy.yml`) builds with `ocaml/setup-ocaml` and deploys
`_build/install/default/bin/*` to GitHub Pages on push to `main`.

### Environment note for agents

Some sandboxes block `opam.ocaml.org` at the egress proxy. If `opam install`
fails with a 403 CONNECT tunnel error, point opam at the GitHub mirror instead:

```
opam init --bare --disable-sandboxing -y https://github.com/ocaml/opam-repository.git
```

If the toolchain cannot be built at all in the sandbox, rely on CI to compile,
and keep OCaml changes small and idiomatic so they land cleanly.

## Architecture

```
src/            homecook_lib — pure logic, no browser deps (depends only on core)
  color / file / rank / square / piece / piece_kind   chess primitives (full sexp)
  chessboard.ml   board state, move legality, history, undo, Ruleset interface
  repertoire.ml   opening tree (variants) + per-move SRS state + card enumeration
  srs.ml          spaced-repetition scheduler (attempts-aware, AFK-capped)
  home_cook.ml    versioned, portable save file (a collection of repertoires)
bin/            Bonsai web frontend (js_of_ocaml)
  homecook.ml     app entry (Bonsai_web.Start.start App.component)
  app.ml          shell: Openings list, Board tab, practice session, editor
  practice.ml     tap-to-move board renderer + practice helpers (pure)
  chessboard.ml   free-play board rendering, drag-and-drop, move panel
  storage.ml      localStorage persistence + .homecook file download
resources/      piece SVGs (embedded/copied into the bundle)
```

Storage note: persistence currently uses **localStorage** (the saved home cook
is a small text sexp, so the synchronous API is a simpler, robust fit). The
`Storage` module hides this behind `load`/`save`, so it can move to IndexedDB
later without touching callers.

Not yet built (natural next steps): importing a `.homecook` file (upload; the
download half exists), SAN/PGN paste import, recall-time weighting +
pause-on-blur in practice, and flipping board orientation when training Black.

## CI note for agents

`.github/workflows/preview.yml` builds any `claude/**` branch, runs the library
tests, and publishes the compiled bundle to the `preview` branch. When the local
sandbox can't build OCaml, push and fetch `origin/preview` to get the compiled
`homecook.bc.js`, serve it locally, and record demos against it.

## Conventions

- Jane Street style: `open! Core`, `.mli` for every module, `[@@deriving …]`,
  `Or_error` for fallible pure code, sexp for serialization.
- Keep `src/` free of any `js_of_ocaml` / browser dependency — it must stay
  unit-testable with `dune runtest` (inline `ppx_expect` tests).
- Bonsai uses the `Cont` API (`Bonsai.graph`, `let%arr`, `Bonsai.state_machine`).
  Match the existing idioms in `bin/chessboard.ml`.
- Serialization for saved home cooks is versioned; never break old saves without
  a migration.
