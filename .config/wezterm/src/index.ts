import * as wezterm from "wezterm";
import { Settings } from "./settings";

const act = wezterm.action;

const config = wezterm.config_builder();
Settings.applyToConfig(config);

// NOTE
// if session exists, switch to it
// if no session run the current
// if no session and not in appropriate term, start default (first in list)
const runWorkProject = wezterm.action_callback((w, p) => {
  const TARGET = "work-web";
  const sessions = wezterm.mux.get_workspace_names();

  const runningSession = sessions.find((s) => s.startsWith(TARGET));
  if (runningSession) {
    w.perform_action(
      act.SwitchToWorkspace({
        name: runningSession,
      }),
      p,
    );
    return;
  }

  const projects = ["deals-light-ui", "fe-review", "fe-native"] as const;
  const choices = projects.map((label) => ({ label }));

  w.perform_action(
    act.InputSelector({
      fuzzy: true,
      fuzzy_description: wezterm.format([
        { Attribute: { Intensity: "Bold" } },
        { Foreground: { AnsiColor: "Fuchsia" } },
        { Text: "foo bar" },
      ]),
      choices: choices,
      action: wezterm.action_callback((w, p, _, l) => {
        w.perform_action(
          act.SwitchToWorkspace({
            name: TARGET + "-" + l,
            spawn: {
              cwd: "/Users/adam.hall/" + "work/" + l,
              args: ["testy"],
            },
          }),
          p,
        );
      }),
    }),
    p,
  );
});

export { config, runWorkProject };
