// Helpers for driving the homecook chessboard in demos.
//
// The board renders each square as a <div coord="A1"> ... </div> with files
// A-H (left->right for white) and ranks 1-8 (bottom->top). Pieces are <img>
// elements that use native HTML5 drag-and-drop.
//
// The app's drag handlers key off dragstart / dragenter / dragend DOM events
// (not DataTransfer payloads), and the dragend handler reads the hovered
// square from its *render-time* Bonsai closure. That means the board must
// re-render between events: we dispatch dragstart, yield a frame so Bonsai
// commits the new hover state, dispatch dragenter over the target, yield
// again, then dragend. Synthetic events (rather than emulated pointer/touch)
// keep this identical on mobile and desktop.

export function squareSelector(coord) {
  return `[coord="${coord}"]`;
}

// Perform an opening as a list of [from, to] coordinate pairs, pausing between
// moves so the motion is legible in the recording.
export async function playMoves(page, moves, { pauseMs = 700 } = {}) {
  for (const [from, to] of moves) {
    await dragPiece(page, from, to);
    await page.waitForTimeout(pauseMs);
  }
}

async function fire(page, coord, type, childImg) {
  await page.evaluate(
    ([sel, type, childImg]) => {
      const el = document.querySelector(sel);
      if (!el) throw new Error('no square ' + sel);
      const target = childImg ? el.querySelector('img') ?? el : el;
      const dt = new DataTransfer();
      target.dispatchEvent(
        new DragEvent(type, { bubbles: true, cancelable: true, dataTransfer: dt }),
      );
    },
    [squareSelector(coord), type, childImg],
  );
}

// Let Bonsai flush its state machine and re-render between events.
async function yieldFrame(page) {
  await page.evaluate(() => new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))));
}

export async function dragPiece(page, from, to) {
  await page.locator(squareSelector(from)).scrollIntoViewIfNeeded();
  await fire(page, from, 'dragstart', true);
  await yieldFrame(page);
  await fire(page, to, 'dragenter', false);
  await fire(page, to, 'dragover', false);
  await yieldFrame(page);
  await fire(page, from, 'dragend', true);
  await yieldFrame(page);
}

export async function waitForBoard(page) {
  await page.locator('[coord="E2"]').waitFor({ state: 'visible', timeout: 30000 });
}
