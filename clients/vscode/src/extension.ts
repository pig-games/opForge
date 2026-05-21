import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

const OUTPUT_CHANNEL_NAME = "opforge-lsp-debug";

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel | undefined;

export function activate(context: vscode.ExtensionContext): void {
  outputChannel ??= vscode.window.createOutputChannel(OUTPUT_CHANNEL_NAME);
  context.subscriptions.push(outputChannel);
  logOutput("activate");
  void startClient(context);

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (!event.affectsConfiguration("opforgeLsp")) {
        return;
      }

      if (event.affectsConfiguration("opforgeLsp.serverPath")) {
        logOutput("configuration changed: opforgeLsp.serverPath");
        void restartClient(context);
        return;
      }

      logOutput("configuration changed: forwarding workspace/didChangeConfiguration");
      client
        ?.sendNotification("workspace/didChangeConfiguration", {
          settings: { opforgeLsp: buildInitializationOptions().opforgeLsp },
        })
        .then(
          () => undefined,
          () => undefined,
        );
    }),
  );

  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument((document) => {
      if (!isTrackedDocument(document)) {
        return;
      }
      logDocumentEvent("didOpen", document);
    }),
  );

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      const document = editor?.document;
      if (!document || !isTrackedDocument(document)) {
        return;
      }
      logDocumentEvent("activeEditor", document);
    }),
  );
}

export async function deactivate(): Promise<void> {
  await stopClient();
}

async function startClient(context: vscode.ExtensionContext): Promise<void> {
  if (client) {
    logOutput("startClient skipped: client already active");
    return;
  }

  const serverPath = resolveServerPath(context);
  const initializationOptions = buildInitializationOptions();
  logOutput(`startClient serverPath=${serverPath}`);
  logOutput(
    `startClient roots=${JSON.stringify(initializationOptions.opforgeLsp.roots ?? [])}`,
  );
  const serverOptions: ServerOptions = {
    run: {
      command: serverPath,
      transport: TransportKind.stdio,
    },
    debug: {
      command: serverPath,
      transport: TransportKind.stdio,
    },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "opforge" },
      { scheme: "file", language: "asm" },
      { scheme: "file", pattern: "**/*.asm" },
      { scheme: "file", pattern: "**/*.inc" },
      { scheme: "untitled", language: "opforge" },
      { scheme: "untitled", language: "asm" },
      { scheme: "untitled", pattern: "**/*.asm" },
      { scheme: "untitled", pattern: "**/*.inc" },
    ],
    initializationOptions,
    synchronize: {
      configurationSection: "opforgeLsp",
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{asm,inc}"),
    },
    outputChannel,
    traceOutputChannel: outputChannel,
    middleware: {
      provideHover: async (document, position, token, next) => {
        logOutput(
          `hover request uri=${document.uri.toString()} languageId=${document.languageId} line=${position.line} char=${position.character}`,
        );
        const result = await next(document, position, token);
        logOutput(
          `hover result uri=${document.uri.toString()} line=${position.line} char=${position.character} hasResult=${result ? "yes" : "no"}`,
        );
        return result;
      },
      provideDefinition: async (document, position, token, next) => {
        logOutput(
          `definition request uri=${document.uri.toString()} languageId=${document.languageId} line=${position.line} char=${position.character}`,
        );
        const result = await next(document, position, token);
        const hasResult = Array.isArray(result)
          ? result.length > 0
          : result !== null && result !== undefined;
        logOutput(
          `definition result uri=${document.uri.toString()} line=${position.line} char=${position.character} hasResult=${hasResult ? "yes" : "no"}`,
        );
        return result;
      },
    },
  };

  client = new LanguageClient(
    "opforgeLsp",
    "opforge-lsp",
    serverOptions,
    clientOptions,
  );
  context.subscriptions.push(client);
  await client.start();
  logOutput("client started");
}

async function stopClient(): Promise<void> {
  if (!client) {
    logOutput("stopClient skipped: no active client");
    return;
  }

  const activeClient = client;
  client = undefined;
  logOutput("stopping client");

  try {
    await activeClient.stop();
    logOutput("client stopped");
  } catch (error) {
    console.warn("opforge-lsp client stop failed during restart", error);
    logOutput(`client stop failed: ${String(error)}`);
    activeClient.dispose();
  }
}

async function restartClient(context: vscode.ExtensionContext): Promise<void> {
  logOutput("restartClient");
  await stopClient();
  await startClient(context);
}

function buildInitializationOptions(): { opforgeLsp: Record<string, unknown> } {
  const config = vscode.workspace.getConfiguration("opforgeLsp");
  const configuredRoots = config.get<string[]>("roots", []);
  const workspaceRoots = (vscode.workspace.workspaceFolders ?? []).map(
    (folder) => folder.uri.fsPath,
  );
  return {
    opforgeLsp: {
      roots: Array.from(new Set([...configuredRoots, ...workspaceRoots])),
      includePaths: config.get<string[]>("includePaths", []),
      modulePaths: config.get<string[]>("modulePaths", []),
      defines: config.get<string[]>("defines", []),
      defaultCpu: config.get<string | null>("defaultCpu", null),
      validation: {
        debounceMs: config.get<number>("validation.debounceMs", 500),
        onSave: config.get<boolean>("validation.onSave", true),
      },
    },
  };
}

function resolveServerPath(context: vscode.ExtensionContext): string {
  const configured = vscode.workspace
    .getConfiguration("opforgeLsp")
    .get<string | null>("serverPath", null);
  if (configured && configured.trim().length > 0) {
    return resolveConfiguredServerPath(configured.trim());
  }

  for (const folder of vscode.workspace.workspaceFolders ?? []) {
    for (const profile of ["debug", "release"]) {
      const candidate = path.join(folder.uri.fsPath, "target", profile, "opforge-lsp");
      if (fs.existsSync(candidate)) {
        return candidate;
      }
    }
  }

  for (const profile of ["debug", "release"]) {
    const devCandidate = context.asAbsolutePath(
      path.join("..", "..", "..", "target", profile, "opforge-lsp"),
    );
    if (fs.existsSync(devCandidate)) {
      return devCandidate;
    }
  }

  return "opforge-lsp";
}

function resolveConfiguredServerPath(configuredPath: string): string {
  const expandedHome = configuredPath.startsWith("~/")
    ? path.join(requireHomeDirectory(), configuredPath.slice(2))
    : configuredPath;
  const workspaceExpanded = expandWorkspaceFolderToken(expandedHome);

  if (path.isAbsolute(workspaceExpanded)) {
    return workspaceExpanded;
  }

  const firstFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  if (firstFolder) {
    return path.resolve(firstFolder, workspaceExpanded);
  }

  return path.resolve(workspaceExpanded);
}

function expandWorkspaceFolderToken(configuredPath: string): string {
  const firstFolder = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
  if (!firstFolder) {
    return configuredPath;
  }

  return configuredPath.split("${workspaceFolder}").join(firstFolder);
}

function requireHomeDirectory(): string {
  return process.env.HOME ?? process.env.USERPROFILE ?? "~";
}

function logOutput(message: string): void {
  outputChannel?.appendLine(`[${new Date().toISOString()}] ${message}`);
}

function logDocumentEvent(
  kind: "didOpen" | "activeEditor",
  document: vscode.TextDocument,
): void {
  logOutput(
    `${kind} uri=${document.uri.toString()} languageId=${document.languageId} scheme=${document.uri.scheme}`,
  );
}

function isTrackedDocument(document: vscode.TextDocument): boolean {
  const pathLower = document.uri.fsPath.toLowerCase();
  return pathLower.endsWith(".asm") || pathLower.endsWith(".inc");
}
