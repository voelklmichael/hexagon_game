# UI Plan

Layout decisions are deferred. Each panel is described by what it shows and what controls it exposes.

---

## Panel: Game Board

Renders the current game state visually.

- Display the board using the SVG output from `GameState::render_task(...).render(...)`
- Clicking a hexagon selects it (sets `selected_hexagon`), showing a tile placement preview
- Animation parameter `animation_t` (0.0–1.0) drives player movement animation

---

## Panel: Player Hand

Shows the active player and their available tiles.

- Color swatch + label for the current player (`current_player`)
- List of tiles in the current player's `hand`
  - Small visual preview of each tile
  - Rotate left / rotate right buttons per tile (`GameState::rotate_tile`)
  - Click to select a tile for placement
- Disabled when it is not the local player's turn or when the game is over

---

## Panel: Statistics

Shows per-player game statistics from `GameState::statistics`.

- Table with columns: Player | Path Segments | Total Weight | Max Velocity
- Sourced from `Statistics::{ total_path_segments, total_path_weight, max_velocity }`
- Win/draw/loss banner when `GameState::result` is `Some`

---

## Panel: Game State JSON

Shows the serialized game state for debugging.

- `serde_json::to_string_pretty(&game_state)` in a scrollable, read-only text area
- Copy button that puts the JSON into the clipboard

---

## Panel: Options

Lets the user configure a new game. Two sections, one per `GameOptions` variant.

### Standard (`GameOptionsStandard`)
- `board_radius` — integer slider
- `outer_connectors` — radio: `OnlyDeathEnds` / `ReducedDeathEnds`
- `random_seed` — integer input
- `player_count` — integer slider
- `collision_mode` — radio: `PassThrough` / `BothDie`
- `winning_condition` — radio: `LastManStanding` / `LongestWay` / `HighestVelocity`
- `hand_size` — integer slider

### Delivery (`GameOptionsDelivery`)
- `board_radius` — integer slider
- `outer_connectors` — radio: `OnlyDeathEnds` / `ReducedDeathEnds`
- `random_seed` — integer input
- `npc_count` — integer slider
- `player_has_target` — checkbox
- `hand_size` — integer slider

---

## Panel: Controls

Game flow controls.

- **Restart this game** — re-run the same `GameOptions` with the same seed
- **New game** — re-run the same `GameOptions` with a new random seed
- **Undo** — step back to the previous `GameState` snapshot
- **Redo** — step forward again after an undo

---

## Panel: Music

Audio controls (placeholder until a sound library is chosen).

- Volume slider (0–100)
- Mute toggle button

---

## Panel: Help / Tutorial

Static informational content.

- How to play
- Explanation of connector types (OnHex, HexToHex, Outside, DeadEnd)
- Winning conditions overview
- Controls reference

---

## Panel: Predefined Games

Quick-start with a preset configuration.

- List of hardcoded `GameOptions` presets, e.g.:
  - "2-player classic" (Standard, radius 2, HighestVelocity)
  - "Delivery solo" (Delivery, radius 2, 2 NPCs)
  - "4-player chaos" (Standard, radius 3, BothDie, LastManStanding)
- Each entry shows a name, short description, and a "Load" button that starts the game immediately

---

## App State

Fields needed to support all panels above.

```rust
struct HexApp {
    // Core
    game: Option<GameState>,
    undo_stack: Vec<GameState>,
    redo_stack: Vec<GameState>,

    // Board interaction
    player_colors: HashMap<PlayerId, Color>,
    selected_tile: Option<usize>,
    selected_hexagon: Option<HexagonPosition>,
    animation_t: f32,

    // Option editors
    options_standard: GameOptionsStandard,
    options_delivery: GameOptionsDelivery,

    // Music
    volume: f32,
    muted: bool,
}
```
