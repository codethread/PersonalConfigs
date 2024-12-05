/** @noSelf */
declare module "wezterm" {
  interface Config {
    font_size?: number;
  }

  interface Action {
    foo: string;
  }

  export function config_builder(): Config;
  export function json_parse(str: string): unknown;
  export const action: Action;
  export const home_dir: string;
}
