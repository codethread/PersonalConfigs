/** @noSelf */
declare module "wezterm" {
  interface Config {
    font_size?: number;
  }

  interface Action {
    foo: string;
  }

  export function config_builder(): Config;
  export const action: Action;
}
