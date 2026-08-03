import { readFile, readdir, stat } from "node:fs/promises";
import { basename, extname, resolve } from "node:path";

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
      errors.push({ name: entry.name, error: visualizationErrorMessage(error) });
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

export function visualizationErrorCode(error: unknown) {
  return error && typeof error === "object" && "code" in error && error.code === "ENOENT" ? 404 : 400;
}

export function visualizationErrorMessage(error: unknown) {
  return error instanceof Error ? error.message : String(error);
}
