import { readFile, readdir, stat } from "node:fs/promises";
import { basename, extname, resolve } from "node:path";
import type { Plugin } from "vite";

export const visualizationApiPath = "/api/visualizations";

type VisualizationSummary = {
  name: string;
  size: number;
  modifiedAt: string;
};

type VisualizationError = {
  name: string;
  error: string;
};

export type VisualizationCatalog = {
  directory: string;
  files: VisualizationSummary[];
  errors: VisualizationError[];
};

export async function readVisualizationCatalog(configuredDirectory: string): Promise<VisualizationCatalog> {
  const directory = resolve(configuredDirectory);
  const entries = await readdir(directory, { withFileTypes: true });
  const candidates = entries
    .filter((entry) => entry.isFile() && extname(entry.name).toLowerCase() === ".json")
    .sort((left, right) => left.name.localeCompare(right.name));
  const files: VisualizationSummary[] = [];
  const errors: VisualizationError[] = [];

  for (const entry of candidates) {
    try {
      await readVisualizationFile(directory, entry.name);
      const details = await stat(resolve(directory, entry.name));
      files.push({ name: entry.name, size: details.size, modifiedAt: details.mtime.toISOString() });
    } catch (error) {
      errors.push({ name: entry.name, error: messageOf(error) });
    }
  }
  return { directory, files, errors };
}

export async function readVisualizationFile(configuredDirectory: string, filename: string) {
  const directory = resolve(configuredDirectory);
  if (filename !== basename(filename) || extname(filename).toLowerCase() !== ".json") {
    throw new Error("invalid visualization filename");
  }
  const parsed = JSON.parse(await readFile(resolve(directory, filename), "utf8")) as unknown;
  if (!isVisualizationDataset(parsed)) throw new Error("not a DNS Regressor visualization export (version 2)");
  return parsed;
}

export function visualizationDirectory(options: { directory?: string }): Plugin {
  return {
    name: "dns-regressor-visualization-directory",
    apply: "serve",
    configureServer(server) {
      server.middlewares.use(visualizationApiPath, async (request, response) => {
        response.setHeader("Content-Type", "application/json; charset=utf-8");
        response.setHeader("Cache-Control", "no-store");
        if (request.method !== "GET") return send(response, 405, { error: "method not allowed" });
        if (!options.directory) {
          return send(response, 503, { error: "DNS_REGRESSOR_VISUALIZATION_DIR is not configured" });
        }
        try {
          const url = new URL(request.url ?? "/", "http://localhost");
          const filename = url.searchParams.get("file");
          const result = filename
            ? await readVisualizationFile(options.directory, filename)
            : await readVisualizationCatalog(options.directory);
          return send(response, 200, result);
        } catch (error) {
          return send(response, errorCode(error), { error: messageOf(error) });
        }
      });
    },
  };
}

function isVisualizationDataset(value: unknown): value is Record<string, unknown> {
  if (!value || typeof value !== "object") return false;
  const dataset = value as Record<string, unknown>;
  return dataset.version === 2
    && Array.isArray(dataset.rootServers)
    && Array.isArray(dataset.zoneFiles)
    && Array.isArray(dataset.sources)
    && Array.isArray(dataset.rules)
    && Array.isArray(dataset.matches);
}

function send(response: { statusCode: number; end(body: string): void }, status: number, value: unknown) {
  response.statusCode = status;
  response.end(JSON.stringify(value));
}

function errorCode(error: unknown) {
  return error && typeof error === "object" && "code" in error && error.code === "ENOENT" ? 404 : 400;
}

function messageOf(error: unknown) {
  return error instanceof Error ? error.message : String(error);
}
