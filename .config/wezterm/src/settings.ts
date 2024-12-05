import * as v from "@codethread/tstl-validate";
import * as wezterm from "wezterm";
import { ok, Ok } from "@codethread/tstl-result";
import { Fs } from "./Fs";

export const Settings = {
  getEnvs() {
    return Fs.readFile(wezterm.home_dir + "/.config/envy/envs.json")
      .map((f) => f.replace("%s+", ""))
      .map(wezterm.json_parse)
      .andThen((tbl) => v.safeParse(v.record(v.string(), v.string()), tbl));
  },

  applyToConfig(config: wezterm.Config) {
    config.set_environment_variables = this.getEnvs()
      .orElse((e) => {
        if (typeof e !== "string") {
          wezterm.log_warn(v.flatten(e));
        } else {
          wezterm.log_warn("missing: " + e);
        }
        return ok({});
      })
      ._unsafeUnwrap();
    config.default_prog = ["nu", "-l"];
    // not sure if this will be annoying
    config.exit_behavior = "CloseOnCleanExit";
    config.font_size = 14;
  },
};
