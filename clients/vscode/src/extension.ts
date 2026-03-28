import * as path from "path";
import * as fs from "fs";
import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  void startClient(context);

  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (!event.affectsConfiguration("opforgeLsp")) {
        return;
      }

      if (event.affectsConfiguration("opforgeLsp.serverPath")) {
        void restartClient(context);
        return;
      }

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
}

export async function deactivate(): Promise<void> {
  await stopClient();
}

async function startClient(context: vscode.ExtensionContext): Promise<void> {
  if (client) {
    return;
  }

  const serverPath = resolveServerPath(context);
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
    ],
    initializationOptions: buildInitializationOptions(),
    synchronize: {
      configurationSection: "opforgeLsp",
      fileEvents: vscode.workspace.createFileSystemWatcher("**/*.{asm,inc}"),
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
}

async function stopClient(): Promise<void> {
  if (!client) {
    return;
  }

  const activeClient = client;
  client = undefined;

  try {
    await activeClient.stop();
  } catch (error) {
    console.warn("opforge-lsp client stop failed during restart", error);
    activeClient.dispose();
  }
}

async function restartClient(context: vscode.ExtensionContext): Promise<void> {
  await stopClient();
  await startClient(context);
}

function buildInitializationOptions(): { opforgeLsp: Record<string, unknown> } {
  const config = vscode.workspace.getConfiguration("opforgeLsp");
  return {
    opforgeLsp: {
      roots: config.get<string[]>("roots", []),
      includePaths: config.get<string[]>("includePaths", []),
      modulePaths: config.get<string[]>("modulePaths", []),
      defines: config.get<string[]>("defines", []),
      defaultCpu: config.get<string | null>("defaultCpu", null),
      opforgePath: config.get<string | null>("opforgePath", null),
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
