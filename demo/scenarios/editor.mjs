// Editor scenario: create a new opening, name it, build a line by tapping
// moves on the board, then save it into the home cook.
export default {
  name: 'editor',
  description: 'Build your own opening by tapping moves, then save it.',
  async run(page) {
    await page.waitForTimeout(700);
    await page.getByRole('button', { name: /New opening/ }).click();
    await page.waitForTimeout(600);
    await page.getByPlaceholder('Opening name').fill('My Italian');
    await page.waitForTimeout(500);

    const tap = async (c) => {
      await page.locator(`[coord="${c}"]`).click();
      await page.waitForTimeout(320);
    };
    const move = async (a, b) => {
      await tap(a);
      await tap(b);
      await page.waitForTimeout(380);
    };

    // 1.e4 e5 2.Nf3 Nc6 3.Bc4
    await move('E2', 'E4');
    await move('E7', 'E5');
    await move('G1', 'F3');
    await move('B8', 'C6');
    await move('F1', 'C4');
    await page.waitForTimeout(500);

    await page.getByRole('button', { name: 'Save opening' }).click();
    await page.waitForTimeout(1200); // back on the openings list, now with "My Italian"
  },
};
