# Assembly Tetris Game

A classic Tetris implementation in x86 assembly language featuring real-time gameplay, score tracking, and sound effects.

## Features

### Gameplay Elements
- Four different Tetris shapes
- Real-time shape movement and collision detection
- Row clearing mechanism
- Score tracking system
- Game timer (5-minute gameplay sessions)
- Next shape preview
- Sound effects for collisions and row clearing

### Visual Interface
- Classic text-mode graphics
- Score display
- Timer display
- Next shape preview window
- Game over screen with final score

### Controls
- **←** : Move shape left
- **→** : Move shape right
- **↓** : Move shape down faster
- **Enter** : Start game / Continue after game over

## Technical Details

### Hardware Requirements
- x86 compatible system
- VGA compatible display
- PC Speaker for sound effects
- DOS environment or emulator (e.g., DOSBox)

### Implementation Features
- Interrupt-based keyboard handling
- Timer interrupt for game timing
- Direct VGA memory manipulation
- PC Speaker sound generation
- Collision detection system

### Game Mechanics
1. **Shape Generation**
   - Random shape selection
   - Preview of next shape
   - Initial positioning at top of play field

2. **Movement System**
   - Boundary checking
   - Collision detection with other shapes
   - Automatic downward movement

3. **Scoring System**
   - 10 points per cleared row
   - Score display updates in real-time

4. **Game Over Conditions**
   - Shapes reaching the top of the play field
   - 5-minute time limit reached

### Sound System
- Collision sound effects
- Row clearing sound effects
- Game over sound sequence

## How to Run

1. Assemble the game using an x86 assembler:
   ```
   nasm -f bin game.asm -o tetris.com
   ```

2. Run the game in DOS or DOSBox:
   ```
   tetris.com
   ```

## Game Structure

- Main game loop with interrupt handling
- Separate routines for:
  - Shape movement
  - Collision detection
  - Score management
  - Display updates
  - Sound generation

## Tips

- Watch the preview window to plan your next move
- Clear multiple rows at once for better scoring
- Use the down arrow to drop pieces faster
- Keep track of the 5-minute time limit

## Technical Notes

- Uses VGA text mode (80x25)
- Direct memory access at 0xB800
- PC Speaker port manipulation for sound
- Keyboard interrupt (INT 9h) handling
- Timer interrupt (INT 8h) for game timing