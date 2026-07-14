// Baseline scenario: play a short opening on the current board to exercise
// drag-and-drop, legal-move highlighting, and the move-history panel.
import { waitForBoard, playMoves } from '../lib/board.mjs';

export default {
  name: 'board-tour',
  description: 'Play the Italian Game opening on the board.',
  async run(page) {
    await waitForBoard(page);
    await page.waitForTimeout(600);
    // Italian Game: 1.e4 e5 2.Nf3 Nc6 3.Bc4
    await playMoves(page, [
      ['E2', 'E4'],
      ['E7', 'E5'],
      ['G1', 'F3'],
      ['B8', 'C6'],
      ['F1', 'C4'],
    ]);
    await page.waitForTimeout(1200);
  },
};
