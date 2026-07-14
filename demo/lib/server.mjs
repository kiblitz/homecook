// Tiny static file server used to serve the built homecook bundle on localhost
// so Playwright can drive it. No external deps.
import http from 'node:http';
import { readFile, stat } from 'node:fs/promises';
import { join, extname, normalize } from 'node:path';

const MIME = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.json': 'application/json',
  '.wasm': 'application/wasm',
  '.map': 'application/json',
};

export async function serveDir(root, { port = 0 } = {}) {
  const server = http.createServer(async (req, res) => {
    try {
      let path = decodeURIComponent(new URL(req.url, 'http://x').pathname);
      if (path.endsWith('/')) path += 'index.html';
      const file = join(root, normalize(path).replace(/^(\.\.[/\\])+/, ''));
      const info = await stat(file);
      if (info.isDirectory()) {
        res.writeHead(302, { Location: path + '/' });
        return res.end();
      }
      const body = await readFile(file);
      res.writeHead(200, {
        'Content-Type': MIME[extname(file)] ?? 'application/octet-stream',
        'Content-Length': body.length,
      });
      res.end(body);
    } catch {
      res.writeHead(404, { 'Content-Type': 'text/plain' });
      res.end('not found');
    }
  });
  await new Promise((resolve) => server.listen(port, '127.0.0.1', resolve));
  const { port: actualPort } = server.address();
  return {
    url: `http://127.0.0.1:${actualPort}`,
    close: () => new Promise((r) => server.close(r)),
  };
}
