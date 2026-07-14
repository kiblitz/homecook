// Practice scenario: add a repertoire, start a practice run, and play the
// prepared moves by tapping source then target square. Includes one wrong tap
// to show the "try again" feedback, and the correct answer to advance.
import { squareSelector } from '../lib/board.mjs';

const tap = async (page, coord) => {
  await page.locator(squareSelector(coord)).click();
  await page.waitForTimeout(350);
};

// Play a move as two taps, pausing so it reads clearly on video.
const playMove = async (page, from, to) => {
  await tap(page, from);
  await tap(page, to);
  await page.waitForTimeout(500);
};

const next = async (page) => {
  await page.getByRole('button', { name: 'Next' }).click();
  await page.waitForTimeout(500);
};

export default {
  name: 'practice',
  description: 'Spaced-repetition practice: play prepared moves by tapping.',
  async run(page) {
    await page.waitForTimeout(600);
    await page.getByRole('button', { name: /Add sample/ }).click();
    await page.waitForTimeout(500);
    await page.getByRole('button', { name: 'Practice' }).click();
    await page.waitForTimeout(700);

    // Card 1 (1.e4): a wrong tap first to show feedback, then the right move.
    await tap(page, 'E2');
    await tap(page, 'E3'); // wrong target -> "try again"
    await page.waitForTimeout(700);
    await playMove(page, 'E2', 'E4');
    await next(page);

    // Card 2 (2.Nf3)
    await playMove(page, 'G1', 'F3');
    await next(page);

    // Card 3 (3.Bc4)
    await playMove(page, 'F1', 'C4');
    await next(page);

    // Card 4 (3.Bb5)
    await playMove(page, 'F1', 'B5');
    await page.waitForTimeout(400);
    await next(page);

    // Session complete screen.
    await page.waitForTimeout(1400);
  },
};
