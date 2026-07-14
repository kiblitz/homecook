// Persistence scenario: add an opening, reload the page, and show it is still
// there — the home cook lives in the browser (localStorage), no server.
export default {
  name: 'persist',
  description: 'Add an opening, reload, and it persists (offline storage).',
  async run(page) {
    await page.waitForTimeout(700);
    await page.getByRole('button', { name: /Add sample/ }).click();
    await page.waitForTimeout(800);
    // Reload: with a server this would be gone; here it is restored from
    // browser storage.
    await page.reload({ waitUntil: 'networkidle' });
    await page.getByText('moves').first().waitFor({ timeout: 15000 });
    await page.waitForTimeout(1500);
  },
};
