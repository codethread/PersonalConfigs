import { z } from "zod";
import path from "path";

const SlackChannels = {
  ME: "D04D9NKQM7U",
} as const;

const SlackAuth = z
  .object({
    slack: z.object({
      slackToken: z.string(),
      slackDCookie: z.string(),
      slackDSCookie: z.string().optional(),
    }),
  })
  .transform((auth) => ({
    token: auth.slack.slackToken,
    dCookie: auth.slack.slackDCookie,
    dSCookie: auth.slack.slackDSCookie,
  }));

type SlackAuth = z.output<typeof SlackAuth>;

const SlackChatResponse = z.discriminatedUnion("ok", [
  z.object({
    // more in here of course, just using what I need
    ok: z.literal(true),
    channel: z.string(),
    message: z.object({
      ts: z.string(),
    }),
  }),
  z.object({
    ok: z.literal(false),
    error: z.string(),
  }),
]);

type SlackChatResponse = z.output<typeof SlackChatResponse>;

export async function slackCmd() {
  try {
    const auth = await fetchAuth();
    const a = await slack({
      auth,
      body: { text: ":mr: <https://google.com|link!> hey bun" },
    });
    console.log(a);
    if (a.ok) {
      slackUpdate({
        auth,
        body: {
          text: ":mr: <https://google.com|link link!> update! hey bun",
          ts: a.message.ts,
        },
      });
    } else {
      console.error(`ah! ${a}`);
    }
  } catch (e) {
    console.error(e);
  }
}

async function fetchAuth() {
  const json: unknown = await Bun.file(
    path.join(
      process.env.HOME!,
      "/Library/Application Support/pomo/client.json",
    ),
  ).json();
  return SlackAuth.parse(json);
}

type SlackMessage = {
  text: string;
  channel?: string;
};

function slack({ auth, body }: { auth: SlackAuth; body: SlackMessage }) {
  return fetch(`https://perkbox.slack.com/api/chat.postMessage`, {
    method: "POST",
    body: JSON.stringify({
      ...body,
      channel: body.channel ?? SlackChannels.ME,
      type: "mrkdwn",
    }),
    headers: {
      "accept-language": "en-US,en;q=0.9",
      accept: "*/*",
      cookie: `d=${auth.dCookie}; d-s=${auth.dSCookie};`,
      "content-type": "application/json; charset=utf-8",
      // authorization: `Bearer ${auth.token}`,
      "user-agent":
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36",
    },
  }).then((r) => {
    console.log({ ok: r.ok, b: r.body });
    return r.json().then((r) => {
      console.log({ r });
      return SlackChatResponse.parse(r);
    });
  });
}
function slackUpdate({
  auth,
  body,
}: {
  auth: SlackAuth;
  body: {
    text: string;
    channel?: string;
    ts: string;
  };
}) {
  return fetch(`https://perkbox.slack.com/api/chat.postMessage`, {
    method: "POST",
    body: JSON.stringify({
      ...body,
      channel: body.channel ?? SlackChannels.ME,
      type: "mrkdwn",
    }),
    headers: {
      "accept-language": "en-US,en;q=0.9",
      accept: "*/*",
      cookie: `d=${auth.dCookie}; d-s=${auth.dSCookie};`,
      "content-type": "application/json; charset=utf-8",
      authorization: `Bearer ${auth.token}`,
      "user-agent":
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36",
    },
  }).then((r) => r.json().then((r) => SlackChatResponse.parse(r)));
}
