import {
  readVisualizationCatalog,
  readVisualizationFile,
  visualizationErrorCode,
  visualizationErrorMessage,
} from "../../../build/visualization-directory";

export const dynamic = "force-dynamic";

export async function GET(request: Request) {
  const directory = process.env.DNS_REGRESSOR_VISUALIZATION_DIR;
  if (!directory) {
    return json({ error: "DNS_REGRESSOR_VISUALIZATION_DIR is not configured" }, 503);
  }

  try {
    const filename = new URL(request.url).searchParams.get("file");
    const result = filename
      ? await readVisualizationFile(directory, filename)
      : await readVisualizationCatalog(directory);
    return json(result, 200);
  } catch (error) {
    return json({ error: visualizationErrorMessage(error) }, visualizationErrorCode(error));
  }
}

function json(value: unknown, status: number) {
  return Response.json(value, {
    status,
    headers: { "Cache-Control": "no-store" },
  });
}
