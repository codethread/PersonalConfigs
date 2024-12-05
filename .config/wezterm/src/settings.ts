import * as wezterm from "wezterm";

const isString2: (val: unknown) => asserts val is string = (val) => {
  if (typeof val !== "string") throw new Error("Nope");
};

export const Assert = {
  assert(condition: any, msg?: string): asserts condition {
    if (!condition) {
      throw new Error(msg);
    }
  },
  isString: isString2,
  // isString: (s: unknown, msg?: string): asserts s is string = (s) => {
  //   if (typeof s !== "string") {
  //     throw new Error(msg);
  //   }
  // },
  // isString(s: unknown): s is string {
  //     return typeof s === 'string'
  // }
};

const fileCache = new Map<string, string>();

export const Fs = {
  readFile(file: string, opts: { noCache?: boolean } = {}) {
    if (!opts.noCache && fileCache.has(file)) {
      return fileCache.get(file);
    }
    const [f] = io.open(file, "rb");
    if (!f) throw new Error("ah");
    const content = f.read("*all" as any);
    if (typeof content != "string") throw new Error("ah");
    f.close();
    fileCache.set(file, content);
    return content;
  },
};

export const Settings = {
  getEnvs() {
    const envs_raw = Fs.readFile(
      wezterm.home_dir + "/.config/envy/envs.json",
    )?.replace("%s+", "");
    // isString2(envs_raw)
    Assert.isString(envs_raw); // ??
    const envs = wezterm.json_parse(envs_raw);
    // return envs;
  },

  applyToConfig(config: any) {
    config.set_environment_variables = this.getEnvs();
    config.default_prog = ["nu", "-l"];
    // not sure if this will be annoying
    config.exit_behavior = "CloseOnCleanExit";
  },
};
