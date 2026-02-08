#!/usr/bin/env python3
"""
Terminal Pac-Man Game - Smooth Rendering Edition
No flicker, responsive controls, classic gameplay
"""
import sys
import termios
import tty
import time
import random
import threading
from collections import deque

class RawInput:
    def __init__(self):
        self.fd = sys.stdin.fileno()
        self.old_settings = termios.tcgetattr(self.fd)
        
    def __enter__(self):
        tty.setcbreak(self.fd)
        return self
        
    def __exit__(self, *args):
        termios.tcsetattr(self.fd, termios.TCSADRAIN, self.old_settings)

class Game:
    def __init__(self):
        # Maze layout
        self.maze = [
            "############################",
            "#............##............#",
            "#.####.#####.##.#####.####.#",
            "#O####.#####.##.#####.####O#",
            "#.####.#####.##.#####.####.#",
            "#..........................#",
            "#.####.##.########.##.####.#",
            "#.####.##.########.##.####.#",
            "#......##....##....##......#",
            "######.##### ## #####.######",
            "     #.##### ## #####.#     ",
            "     #.##          ##.#     ",
            "     #.## ###--### ##.#     ",
            "######.## #      # ##.######",
            "      .   #      #   .      ",
            "######.## #      # ##.######",
            "     #.## ######## ##.#     ",
            "     #.##          ##.#     ",
            "     #.## ######## ##.#     ",
            "######.## ######## ##.######",
            "#............##............#",
            "#.####.#####.##.#####.####.#",
            "#.####.#####.##.#####.####.#",
            "#O..##.......  .......##..O#",
            "###.##.##.########.##.##.###",
            "###.##.##.########.##.##.###",
            "#......##....##....##......#",
            "#.##########.##.##########.#",
            "#.##########.##.##########.#",
            "#..........................#",
            "############################"
        ]
        
        self.maze = [list(row) for row in self.maze]
        self.h = len(self.maze)
        self.w = len(self.maze[0])
        
        # Player
        self.px, self.py = 14, 23
        self.dx, self.dy = 1, 0
        self.next_dx, self.next_dy = 1, 0
        
        # Ghosts: [x, y, dx, dy, char, frightened_timer]
        self.ghosts = [
            [12, 11, 0, 1, 'R', 0],
            [14, 11, 0, 1, 'P', 0],
            [13, 13, 1, 0, 'C', 0],
            [15, 13, -1, 0, 'O', 0]
        ]
        
        # Game state
        self.score = 0
        self.lives = 3
        self.dots = sum(row.count('.') + row.count('O') for row in self.maze)
        self.running = True
        self.won = False
        self.game_over = False
        
        # Input handling
        self.key_buffer = deque(maxlen=1)
        self.input_thread = None
        
        # Display buffer for smooth rendering
        self.last_buffer = None
        
    def is_valid(self, x, y):
        """Check if position is valid (not a wall)"""
        if 0 <= y < self.h and 0 <= x < self.w:
            return self.maze[y][x] != '#'
        return False
    
    def hide_cursor(self):
        """Hide terminal cursor"""
        print("\033[?25l", end="", flush=True)
    
    def show_cursor(self):
        """Show terminal cursor"""
        print("\033[?25h", end="", flush=True)
    
    def move_cursor(self, row, col):
        """Move cursor to specific position"""
        print(f"\033[{row};{col}H", end="", flush=True)
    
    def clear_screen_once(self):
        """Clear screen only once at start"""
        print("\033[2J\033[H", end="", flush=True)
    
    def draw(self):
        """Render the game with minimal flicker"""
        # Create display buffer
        display = [row[:] for row in self.maze]
        
        # Color codes
        GHOST_COLOR = '\033[91m'      # Red
        PACMAN_COLOR = '\033[93m'     # Yellow
        POWER_COLOR = '\033[96m'      # Cyan
        DOT_COLOR = '\033[37m'        # White
        WALL_COLOR = '\033[94m'       # Blue
        FRIGHT_COLOR = '\033[95m'     # Magenta
        RESET = '\033[0m'
        
        # Draw ghosts
        for gx, gy, _, _, gchar, ftimer in self.ghosts:
            if ftimer > 0:
                display[gy][gx] = 'F'
            else:
                display[gy][gx] = gchar
        
        # Draw Pac-Man
        if self.dx == 1:
            pc = '>'
        elif self.dx == -1:
            pc = '<'
        elif self.dy == 1:
            pc = 'v'
        else:
            pc = '^'
        display[self.py][self.px] = pc
        
        # Build output with colors
        output_lines = []
        for y, row in enumerate(display):
            line = ""
            for x, char in enumerate(row):
                if char == '#':
                    line += WALL_COLOR + char + RESET
                elif char in ['R', 'P', 'C', 'O']:
                    line += GHOST_COLOR + char + RESET
                elif char == 'F':
                    line += FRIGHT_COLOR + char + RESET
                elif char in ['>', '<', 'v', '^']:
                    line += PACMAN_COLOR + char + RESET
                elif char == 'O':
                    line += POWER_COLOR + char + RESET
                elif char == '.':
                    line += DOT_COLOR + char + RESET
                else:
                    line += char
            output_lines.append(line)
        
        # Status line
        status = f"\n{PACMAN_COLOR}Score: {self.score}{RESET}  " \
                f"{GHOST_COLOR}Lives: {self.lives}{RESET}  " \
                f"{DOT_COLOR}Dots: {self.dots}{RESET}"
        output_lines.append(status)
        output_lines.append("WASD=Move  Q=Quit")
        
        if self.game_over:
            if self.won:
                output_lines.append(f"\n{PACMAN_COLOR}*** YOU WIN! ***{RESET}")
            else:
                output_lines.append(f"\n{GHOST_COLOR}*** GAME OVER ***{RESET}")
        
        # Convert to string
        current_buffer = '\n'.join(output_lines)
        
        # Only redraw if changed (reduces flicker further)
        if current_buffer != self.last_buffer:
            self.move_cursor(1, 1)
            print(current_buffer, end='', flush=True)
            self.last_buffer = current_buffer
    
    def input_worker(self):
        """Background thread for input"""
        with RawInput():
            while self.running:
                try:
                    ch = sys.stdin.read(1)
                    self.key_buffer.append(ch.lower())
                except:
                    break
    
    def handle_input(self):
        """Process input buffer"""
        if self.key_buffer:
            key = self.key_buffer.popleft()
            
            if key == 'w':
                self.next_dx, self.next_dy = 0, -1
            elif key == 's':
                self.next_dx, self.next_dy = 0, 1
            elif key == 'a':
                self.next_dx, self.next_dy = -1, 0
            elif key == 'd':
                self.next_dx, self.next_dy = 1, 0
            elif key == 'q':
                self.running = False
    
    def move_player(self):
        """Move Pac-Man"""
        # Try to turn
        nx = self.px + self.next_dx
        ny = self.py + self.next_dy
        if self.is_valid(nx, ny):
            self.dx = self.next_dx
            self.dy = self.next_dy
        
        # Move in current direction
        nx = self.px + self.dx
        ny = self.py + self.dy
        
        if self.is_valid(nx, ny):
            self.px, self.py = nx, ny
            
            # Collect items
            cell = self.maze[ny][nx]
            if cell == '.':
                self.maze[ny][nx] = ' '
                self.score += 10
                self.dots -= 1
            elif cell == 'O':
                self.maze[ny][nx] = ' '
                self.score += 50
                self.dots -= 1
                # Frighten ghosts
                for ghost in self.ghosts:
                    ghost[5] = 40
    
    def move_ghosts(self):
        """Move all ghosts"""
        for i, ghost in enumerate(self.ghosts):
            gx, gy, gdx, gdy, gchar, ftimer = ghost
            
            # Update frightened timer
            if ftimer > 0:
                ghost[5] = ftimer - 1
            
            # Choose direction
            if ftimer > 0:
                # Move randomly when frightened
                directions = [(0, -1), (0, 1), (-1, 0), (1, 0)]
                random.shuffle(directions)
                for dx, dy in directions:
                    if self.is_valid(gx + dx, gy + dy):
                        ghost[2], ghost[3] = dx, dy
                        break
            else:
                # Chase Pac-Man
                best_dist = 999999
                best_dir = (gdx, gdy)
                
                for dx, dy in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
                    nx, ny = gx + dx, gy + dy
                    if self.is_valid(nx, ny):
                        dist = abs(nx - self.px) + abs(ny - self.py)
                        if dist < best_dist:
                            best_dist = dist
                            best_dir = (dx, dy)
                
                ghost[2], ghost[3] = best_dir
            
            # Move ghost
            nx = gx + ghost[2]
            ny = gy + ghost[3]
            if self.is_valid(nx, ny):
                ghost[0], ghost[1] = nx, ny
    
    def check_collisions(self):
        """Check ghost collisions"""
        for ghost in self.ghosts:
            gx, gy = ghost[0], ghost[1]
            if gx == self.px and gy == self.py:
                if ghost[5] > 0:
                    # Eat ghost
                    self.score += 200
                    ghost[0], ghost[1] = 14, 13
                    ghost[5] = 0
                else:
                    # Die
                    self.lives -= 1
                    if self.lives <= 0:
                        self.game_over = True
                        self.running = False
                    else:
                        # Reset
                        self.px, self.py = 14, 23
                        self.dx, self.dy = 1, 0
                        self.next_dx, self.next_dy = 1, 0
                        self.ghosts[0][0:2] = [12, 11]
                        self.ghosts[1][0:2] = [14, 11]
                        self.ghosts[2][0:2] = [13, 13]
                        self.ghosts[3][0:2] = [15, 13]
                        for ghost in self.ghosts:
                            ghost[5] = 0
                        time.sleep(1)
                    return
    
    def check_win(self):
        """Check win condition"""
        if self.dots == 0:
            self.won = True
            self.game_over = True
            self.running = False
    
    def run(self):
        """Main game loop"""
        # Setup terminal
        self.hide_cursor()
        self.clear_screen_once()
        
        try:
            # Start input thread
            self.input_thread = threading.Thread(target=self.input_worker, daemon=True)
            self.input_thread.start()
            
            self.draw()
            
            frame = 0
            last_time = time.time()
            
            while self.running:
                current_time = time.time()
                dt = current_time - last_time
                
                # Target 10 FPS
                if dt < 0.1:
                    time.sleep(0.1 - dt)
                    continue
                
                last_time = current_time
                frame += 1
                
                # Handle input
                self.handle_input()
                
                # Move player
                self.move_player()
                
                # Move ghosts every 2 frames
                if frame % 2 == 0:
                    self.move_ghosts()
                
                # Check collisions and win
                self.check_collisions()
                self.check_win()
                
                # Draw
                self.draw()
            
        finally:
            # Restore terminal
            self.show_cursor()
            print("\n\nPress Enter to exit...", flush=True)
            with RawInput():
                sys.stdin.read(1)

def main():
    try:
        game = Game()
        game.run()
    except KeyboardInterrupt:
        print("\033[?25h\n\nThanks for playing!")
    except Exception as e:
        print("\033[?25h")
        print(f"\nError: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()
