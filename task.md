# vulpea-ui Requirements Document

## Overview

**vulpea-ui** is a sidebar infrastructure and widget framework for vulpea notes. It provides a customisable sidebar that displays contextual information about the currently focused vulpea note, along with a set of default widgets and an API for creating custom widgets.

**Dependencies:**
- `vulpea` (core note database and queries)
- `vui` (UI component library)

**No external utility libraries.** Use built-in `seq.el`, `map.el`, `cl-lib`, and threading macros (`thread-first`, `thread-last`) instead of dash.el. This reduces dependency footprint and improves native-compilation optimisation.

**Design Principles:**
- Performance is critical—sidebar rendering must not introduce perceptible lag
- Widgets are vui components, giving developers full power of the component model
- vulpea-ui provides infrastructure; it does not impose workflows
- Minimal dependencies

---

## 1. Sidebar System

### 1.1 Scope & Lifecycle

**Scope:** Per-frame. Each frame may have its own sidebar instance.

**Activation:**
- `vulpea-ui-sidebar-open` — interactive command to create/show sidebar
- `vulpea-ui-sidebar-close` — interactive command to hide/destroy sidebar
- `vulpea-ui-sidebar-toggle` — interactive command to toggle visibility
- Users add to hooks as needed (e.g., `(add-hook 'org-mode-hook #'vulpea-ui-sidebar-open)`)

**Sidebar Buffer:**
- Dedicated buffer per frame (e.g., `*vulpea-ui-sidebar*` or `*vulpea-ui-sidebar:<frame-id>*`)
- Uses `vulpea-ui-sidebar-mode` as its major mode
- Major mode provides keymap for sidebar-specific bindings

**Sidebar Creation Heuristics:**
- If the frame has no sidebar and user calls `vulpea-ui-sidebar-open` → create sidebar according to configured position
- If the frame already has a sidebar → do not modify window layout, only update content
- If the frame is already split in a complex way → do not restructure; attach sidebar to the frame's side (as a side window)

### 1.2 Window Management

**Position:** Configurable via `vulpea-ui-sidebar-position` — one of `left`, `right`, `top`, `bottom`. Default: `right`.

**Size:** Configurable via `vulpea-ui-sidebar-size` — float between 0.0 and 1.0 representing fraction of frame width (for left/right) or height (for top/bottom). Default: `0.33`.

**Implementation:** Use `display-buffer-in-side-window` with appropriate `side` and `slot` parameters. The sidebar window should have:
- `no-delete-other-windows: t` — protected from `delete-other-windows`
- `dedicated: t` — buffer is dedicated to this window
- Appropriate `window-parameters` to prevent accidental splitting/resizing

**Navigation from Sidebar:**
When user clicks a link or presses RET on a widget item, the target note must open in the "main" window (the most recently used non-sidebar window), preserving the current layout. Widgets should use a shared navigation function (`vulpea-ui-visit-note`) that handles this correctly.

### 1.3 Content Updates

**Trigger:** Sidebar content updates when the user switches to a different vulpea note in any non-sidebar window within the frame.

**Non-vulpea Buffers:** If user switches to a non-vulpea buffer, sidebar retains its current (stale) content. It only refreshes when a vulpea note gains focus.

**Tracking Mechanism:** Use `window-buffer-change-functions` or similar hook to detect buffer changes. Determine the "current note" by:
1. Finding the most recently selected window that is not the sidebar
2. Checking if its buffer is a vulpea note
3. If yes, update sidebar for that note; if no, do nothing

**Debouncing:** Not required for V1 if rendering is fast enough. Design should allow easy addition of debounce (e.g., `vulpea-ui-sidebar-update-delay` defaulting to `0`).

### 1.4 Keybindings

The sidebar buffer uses `vulpea-ui-sidebar-mode`, which provides `vulpea-ui-sidebar-mode-map`:

| Key | Command | Description |
|-----|---------|-------------|
| `q` | `vulpea-ui-sidebar-close` | Close sidebar |
| `g` | `vulpea-ui-sidebar-refresh` | Force refresh |
| `TAB` | `vulpea-ui-widget-toggle-at-point` | Toggle collapse/expand widget |
| `RET` | `vulpea-ui-follow-link-at-point` | Follow link under point |

**Extension by other packages:** Packages like vulpea-journal may define their own derived mode or use `define-key` to extend `vulpea-ui-sidebar-mode-map` with additional bindings (e.g., `t` for today, `<`/`>` for previous/next day). This is outside vulpea-ui's scope but the architecture should not prevent it.

### 1.5 Configuration Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `vulpea-ui-sidebar-position` | symbol | `'right` | Side of frame: `left`, `right`, `top`, `bottom` |
| `vulpea-ui-sidebar-size` | float | `0.33` | Fraction of frame width/height (0.0–1.0) |
| `vulpea-ui-sidebar-widgets` | list | (see §3) | Ordered list of widgets to display |
| `vulpea-ui-default-widget-collapsed` | boolean | `nil` | Default collapsed state for all widgets |

---

## 2. Widget API

### 2.1 Widget Definition

A widget is a **vui component** (a function returning vui elements). Widgets receive the current note context and return UI to render.

```elisp
;; Minimal widget signature
(defun my-widget ()
  "A custom widget."
  (let ((note (vulpea-ui-current-note)))  ; access via context
    (vui-box
      (vui-text (vulpea-note-title note)))))
```

### 2.2 Provided Contexts

vulpea-ui provides the following contexts available to all widgets:

| Context | Accessor | Value |
|---------|----------|-------|
| Current note | `vulpea-ui-current-note` | `vulpea-note` struct or `nil` |

Widgets may define their own internal state using vui's `use-state`, `use-memo`, etc.

### 2.3 Widget Registration

Widgets are referenced by symbol in `vulpea-ui-sidebar-widgets`:

```elisp
(setq vulpea-ui-sidebar-widgets
      '(vulpea-ui-widget-outline
        vulpea-ui-widget-backlinks
        vulpea-ui-widget-links
        vulpea-ui-widget-stats))
```

**Dynamic Widget Lists:** `vulpea-ui-sidebar-widgets` can be set buffer-locally or overridden by external packages. For example, vulpea-journal may do:

```elisp
(defun vulpea-journal--setup-sidebar ()
  (setq-local vulpea-ui-sidebar-widgets
              '(vulpea-journal-widget-calendar
                vulpea-journal-widget-this-day-across-years
                vulpea-journal-widget-created-today
                vulpea-ui-widget-backlinks)))
```

### 2.4 Standard Widget Wrapper

vulpea-ui provides `vulpea-ui-widget` — a wrapper component that handles:
- Collapsible header with title
- Respects `vulpea-ui-default-widget-collapsed`
- Consistent styling/spacing

```elisp
(defun vulpea-ui-widget-backlinks ()
  (vulpea-ui-widget
    :title "Backlinks"
    :count (length backlinks)  ; shown in header, e.g., "Backlinks (5)"
    :children (lambda () ...)))
```

Widgets may choose not to use this wrapper if they need full control.

### 2.5 Shared Components

**`vulpea-ui-note-preview`** — a component for rendering a preview of a note's content.

```elisp
(vulpea-ui-note-preview
  :note note
  :max-lines 10           ; optional, limit preview length
  :strip-drawers t        ; default t, remove :PROPERTIES: etc.
  :strip-metadata t)      ; default t, remove #+TITLE, #+FILETAGS, etc.
```

Handles:
- Drawer stripping (configurable)
- Metadata stripping (configurable)
- Proper indentation/offset for nested display
- Link rendering (display titles, not raw IDs)

**`vulpea-ui-note-link`** — a clickable link component.

```elisp
(vulpea-ui-note-link
  :note note
  :on-click #'vulpea-ui-visit-note)  ; default
```

### 2.6 Utility Functions

**`vulpea-ui-visit-note`** — navigate to a note, opening in the main window (not sidebar).

```elisp
(vulpea-ui-visit-note note)
```

**`vulpea-ui-current-note`** — retrieve current note from context (for use within widgets).

---

## 3. Default Widgets

### 3.1 Outline (`vulpea-ui-widget-outline`)

Displays the heading structure of the current note as a navigable tree.

**Behaviour:**
- Shows all headings (configurable depth via `vulpea-ui-outline-max-depth`, default unlimited)
- Clicking a heading jumps to that position in the note
- Respects folded/unfolded state of sections (or shows all?)

**Performance Notes:**
- Parse headings from buffer content (fast, no DB query needed)
- Can use `org-element` or simple regexp for speed

### 3.2 Backlinks (`vulpea-ui-widget-backlinks`)

Displays notes that link to the current note.

**Behaviour:**
- Shows list of note titles as clickable links
- Header shows count: "Backlinks (12)"
- Clicking opens the note in main window

**Performance Notes:**
- Use `vulpea-db-query-by-links-some` for efficient lookup
- Single query, no iteration

```elisp
(vulpea-db-query-by-links-some
  (list (cons "id" (vulpea-note-id note))))
```

### 3.3 Forward Links (`vulpea-ui-widget-links`)

Displays notes that the current note links to.

**Behaviour:**
- Shows list of note titles as clickable links
- Header shows count: "Links (8)"
- Optionally group by tags (V2, configurable)

**Performance Notes:**
- Links are stored in `vulpea-note-links` — no DB query needed, just resolve IDs
- Batch-fetch linked notes: `vulpea-db-query-by-ids`

### 3.4 Stats (`vulpea-ui-widget-stats`)

Displays statistics about the current note.

**Behaviour:**
- Character count
- Word count
- Link count (outgoing)

**Display:** Compact, single line or small grid:
```
1,234 chars · 256 words · 8 links
```

**Performance Notes:**
- Character/word count from buffer content (fast)
- Link count from `vulpea-note-links` (no DB query)

---

## 4. Performance Requirements

### 4.1 General Principles

1. **Batch reads over iterative reads** — reading 5,000 notes from DB in one query is faster than 100 individual reads
2. **Use specialised queries** — `vulpea-db-query-by-links-some`, `vulpea-db-query-by-tags-all`, `vulpea-db-query-by-ids` are optimised
3. **Leverage vui** — use `use-memo` for expensive computations, component-level caching
4. **No blocking operations** — if something is slow, it must be made async or deferred (component-level responsibility)

### 4.2 Acceptable Latency

- Sidebar full render: < 50ms target
- Individual widget render: < 10ms target
- Navigation (clicking link): immediate (< 16ms)

### 4.3 What to Avoid

- Running queries in loops
- Synchronous subprocess calls
- Parsing org content repeatedly (cache results)
- Timer-delayed rendering to "hide" slowness (this is not a fix)

---

## 5. API Summary

### Commands

| Command | Description |
|---------|-------------|
| `vulpea-ui-sidebar-open` | Open/show sidebar in current frame |
| `vulpea-ui-sidebar-close` | Close/hide sidebar in current frame |
| `vulpea-ui-sidebar-toggle` | Toggle sidebar visibility |
| `vulpea-ui-sidebar-refresh` | Force refresh sidebar content |

### Functions

| Function | Description |
|----------|-------------|
| `vulpea-ui-visit-note` | Navigate to note in main window |
| `vulpea-ui-current-note` | Get current note (context accessor) |
| `vulpea-ui-follow-link-at-point` | Follow link/item under point |
| `vulpea-ui-widget-toggle-at-point` | Toggle widget collapse at point |

### Components

| Component | Description |
|-----------|-------------|
| `vulpea-ui-widget` | Standard widget wrapper with collapsible header |
| `vulpea-ui-note-link` | Clickable note link |
| `vulpea-ui-note-preview` | Rendered note content preview |

### Configuration

| Variable | Description |
|----------|-------------|
| `vulpea-ui-sidebar-position` | Sidebar placement (`left`, `right`, `top`, `bottom`) |
| `vulpea-ui-sidebar-size` | Sidebar size as fraction (0.0–1.0) |
| `vulpea-ui-sidebar-widgets` | List of widget symbols to display |
| `vulpea-ui-default-widget-collapsed` | Default collapse state for widgets |
| `vulpea-ui-outline-max-depth` | Max heading depth for outline widget |

### Modes

| Mode | Description |
|------|-------------|
| `vulpea-ui-sidebar-mode` | Major mode for sidebar buffer; provides keymap |

---

## 6. Open Questions / Future Considerations

1. **Conditional widget visibility** — mechanism for "show only if tag X" or "hide if empty". Defer to V2 or let consuming packages handle via dynamic `vulpea-ui-sidebar-widgets`.

2. **State persistence** — remembering collapsed/expanded state across note switches and sessions. Defer to V2.

3. **Debouncing** — add if profiling shows rapid switching causes issues.

4. **Async loading pattern** — document recommended approach for widgets with slow data sources.

5. **Multiple sidebars** — e.g., one on each side. Not planned, but architecture shouldn't preclude it.

6. **Widget keybinding contributions** — V1 uses standard keybindings; extending packages can modify `vulpea-ui-sidebar-mode-map` directly.
