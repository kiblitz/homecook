// App-shell scenario: land on the Openings list, add sample repertoires, then
// switch to the free-play board and play a couple of moves.
import { waitForBoard, playMoves } from '../lib/board.mjs';

export default {
  name: 'openings',
  description: 'Add sample openings to the list, then switch to the board.',
  async run(page) {
    await page.waitForTimeout(700);
    const add = page.getByRole('button', { name: /Add sample/ });
    await add.click();
    await page.waitForTimeout(500);
    await add.click(); // a second entry so the list is non-trivial
    await page.waitForTimeout(900);
    // Switch to the board and play the start of the line.
    await page.getByRole('button', { name: 'Board' }).click();
    await waitForBoard(page);
    await page.waitForTimeout(400);
    await playMoves(page, [
      ['E2', 'E4'],
      ['E7', 'E5'],
      ['G1', 'F3'],
    ]);
    await page.waitForTimeout(700);
    // Back to the openings list.
    await page.getByRole('button', { name: 'Openings' }).click();
    await page.waitForTimeout(1100);
  },
};
