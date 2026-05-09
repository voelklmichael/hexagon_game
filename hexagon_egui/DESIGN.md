# Layout Design

## Overall structure

Three-column layout. The game board fills all available space; the side panels are capped at 20% of the total width each. The right panel is collapsible.

```
┌──────────────┬──────────────────────────────┬──────────────┐
│  Left panel  │                              │ Right panel  │
│  max 20 %    │        Game Board            │  max 20 %    │
│  always      │    (fills remaining space)   │ collapsible  │
│  visible     │                              │   ☰ / ✕     │
└──────────────┴──────────────────────────────┴──────────────┘
```

When the right panel is collapsed it disappears entirely and the board expands to fill that space.

---

## Left panel — always visible, max 20 % width

Burger menu (☰) to switch between tabs:

- **Options** — game setup, predefined games, Start New Game
- **Player Hand** — current player's tiles, rotate, play
- **Controls** — restart, new game, undo, redo
- **Music** — volume, mute
- **Rendering** — board and player colors

---

## Right panel — collapsible, max 20 % width

A toggle button (☰ to open, ✕ to close) shows or hides the panel.  
When open, a burger menu switches between tabs:

- **Help / Tutorial** — how to play, connector types, winning conditions
- **Statistics** — per-player path segments, weight, velocity
- **Game State JSON** — pretty-printed JSON, copy button

---

## Centre — game board

Takes all space not used by the side panels (minimum: 60 %, maximum: 80 % when right panel is collapsed). The board scales to fit the available rect.
