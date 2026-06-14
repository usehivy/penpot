#!/usr/bin/env node

import crypto from "node:crypto";

const defaults = {
  baseUrl: "http://localhost:3450",
  publicUrl: "https://localhost:3449",
  orgId: "demo-org",
  userId: "demo-user",
  agentId: "demo-agent",
  fileId: null,
  pageId: null,
  key: process.env.PENPOT_HIVY_CONTROL_PLANE_KEY || "hivy-local-dev-control-plane-key",
};

function parseArgs(argv) {
  const args = { ...defaults };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const next = argv[i + 1];

    switch (arg) {
      case "--base-url":
        args.baseUrl = next;
        i += 1;
        break;
      case "--public-url":
        args.publicUrl = next;
        i += 1;
        break;
      case "--key":
        args.key = next;
        i += 1;
        break;
      case "--org-id":
        args.orgId = next;
        i += 1;
        break;
      case "--user-id":
        args.userId = next;
        i += 1;
        break;
      case "--agent-id":
        args.agentId = next;
        i += 1;
        break;
      case "--file-id":
        args.fileId = next;
        i += 1;
        break;
      case "--page-id":
        args.pageId = next;
        i += 1;
        break;
      case "-h":
      case "--help":
        console.log(`Usage: fake-hivy-control-plane.mjs [options]

Creates a local fake Hivy org, user account, and agent account through Penpot's
/api/hivy control-plane endpoints, then prints short-lived launch JWTs.

Options:
  --base-url URL    Penpot API origin. Default: ${defaults.baseUrl}
  --public-url URL  Browser-facing Penpot origin. Default: ${defaults.publicUrl}
  --key KEY         Hivy control-plane key/JWT secret. Default: env or local dev key
  --org-id ID       Fake Hivy org id. Default: ${defaults.orgId}
  --user-id ID      Fake Hivy user id. Default: ${defaults.userId}
  --agent-id ID     Fake Hivy agent id. Default: ${defaults.agentId}
  --file-id UUID    Optional file id to include in launch URLs.
  --page-id UUID    Optional page id to include in launch URLs.`);
        process.exit(0);
        break;
      default:
        throw new Error(`Unknown argument: ${arg}`);
    }
  }

  return args;
}

function base64url(input) {
  return Buffer.from(input)
    .toString("base64")
    .replaceAll("+", "-")
    .replaceAll("/", "_")
    .replaceAll("=", "");
}

function signJwt(payload, key) {
  const header = { alg: "HS256", typ: "JWT" };
  const body = {
    iss: "hivy",
    aud: "penpot-canvas",
    iat: Math.floor(Date.now() / 1000),
    exp: Math.floor(Date.now() / 1000) + 10 * 60,
    ...payload,
  };
  const signingInput = `${base64url(JSON.stringify(header))}.${base64url(JSON.stringify(body))}`;
  const signature = crypto.createHmac("sha256", key).update(signingInput).digest();
  return `${signingInput}.${base64url(signature)}`;
}

function uuidFromName(name) {
  const bytes = crypto.createHash("sha1").update(name).digest().subarray(0, 16);
  bytes[6] = (bytes[6] & 0x0f) | 0x50;
  bytes[8] = (bytes[8] & 0x3f) | 0x80;

  const hex = bytes.toString("hex");
  return [
    hex.slice(0, 8),
    hex.slice(8, 12),
    hex.slice(12, 16),
    hex.slice(16, 20),
    hex.slice(20),
  ].join("-");
}

async function hivyFetch({ baseUrl, key }, path, body) {
  const response = await fetch(new URL(path, baseUrl), {
    method: "POST",
    headers: {
      authorization: `Bearer ${key}`,
      "content-type": "application/json",
    },
    body: JSON.stringify(body),
  });

  let responseBody = null;
  const text = await response.text();
  if (text.length > 0) {
    try {
      responseBody = JSON.parse(text);
    } catch {
      responseBody = text;
    }
  }

  if (!response.ok) {
    throw new Error(`${path} failed: HTTP ${response.status} ${JSON.stringify(responseBody)}`);
  }

  return responseBody;
}

function cleanTransit(value) {
  if (Array.isArray(value)) {
    return value.map(cleanTransit);
  }

  if (value && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([key, entry]) => [
        key.startsWith("~:") ? key.slice(2) : key,
        cleanTransit(entry),
      ]),
    );
  }

  if (typeof value === "string" && value.startsWith("~u")) {
    return value.slice(2);
  }

  return value;
}

function makeLaunch({ publicUrl, key, fileId, pageId }, profileId, teamId) {
  const token = signJwt({
    profile_id: profileId,
    team_id: teamId,
    ...(fileId ? { file_id: fileId } : {}),
    ...(pageId ? { page_id: pageId } : {}),
  }, key);

  const url = new URL("/api/hivy/session", publicUrl);
  url.searchParams.set("token", token);

  return { token, url: url.toString() };
}

async function main() {
  const options = parseArgs(process.argv.slice(2));
  const teamId = uuidFromName(`hivy:team:${options.orgId}`);
  const userProfileId = uuidFromName(`hivy:profile:user:${options.userId}:${options.orgId}`);
  const agentProfileId = uuidFromName(`hivy:profile:agent:${options.agentId}:${options.orgId}`);

  const team = await hivyFetch(options, "/api/hivy/teams", {
    "team-id": teamId,
    "hivy-id": options.orgId,
    name: `Hivy ${options.orgId}`,
  });

  const user = await hivyFetch(options, "/api/hivy/profiles", {
    "profile-id": userProfileId,
    "team-id": teamId,
    "hivy-id": `${options.userId}-${options.orgId}`,
    email: `${options.userId}@example.usehivy.local`,
    fullname: "Fake Hivy User",
  });

  const agent = await hivyFetch(options, "/api/hivy/profiles", {
    "profile-id": agentProfileId,
    "team-id": teamId,
    "hivy-id": `${options.agentId}-${options.orgId}`,
    email: `${options.agentId}@agents.usehivy.local`,
    fullname: "Fake Hivy Agent",
  });

  const userLaunch = makeLaunch(options, userProfileId, teamId);
  const agentLaunch = makeLaunch(options, agentProfileId, teamId);

  console.log(JSON.stringify({
    baseUrl: options.baseUrl,
    publicUrl: options.publicUrl,
    team: cleanTransit(team),
    user: cleanTransit(user),
    agent: cleanTransit(agent),
    userLaunch,
    agentLaunch,
  }, null, 2));
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
