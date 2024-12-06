/** @noSelf */
declare module "wezterm" {
  /** @noSelf */
  interface Config {
    font_size?: number;
    set_environment_variables: Record<string, string | string[]>;
    default_prog: string[];
    exit_behavior: string;
  }

  type Format = "_Format";
  type Formats =
    | {
        Attribute: { Intensity: string };
      }
    | {
        Foreground: { AnsiColor: string };
      }
    | {
        Text: string;
      };

  /** @noSelf */
  interface Spawn {
    cwd: string;
    args?: string[];
  }

  type ActionTypes = {
    selector: "selector";
    none: "none";
  };

  type WezAction<A extends string = ActionTypes["none"]> =
    `_wezterm.action_callback--${A}`;

  /** @noSelf */
  interface Action {
    SwitchToWorkspace(arg: { name: string; spawn?: Spawn }): WezAction;
    InputSelector(arg: {
      fuzzy?: true;
      fuzzy_description?: string | Format;
      choices: Array<{ label: string; id?: string }>;
      action: WezAction<ActionTypes["selector"]>;
    }): WezAction;
  }

  /** @noSelf */
  interface Mux {
    get_workspace_names(): string[];
    get_active_workspace(): string;
  }

  interface Window {
    perform_action(action: WezAction, pane: Pane): void;
  }

  interface Pane {
    perform_action(action: WezAction, pane: Pane): void;
  }

  export const action: Action;
  export const home_dir: string;
  export const mux: Mux;

  export function action_callback<
    A extends
      | ActionTypes["none"]
      | ActionTypes["selector"] = ActionTypes["none"],
  >(
    cb: A extends ActionTypes["selector"]
      ? (window: Window, pane: Pane, id: string | undefined, label: string) => void
      : (window: Window, pane: Pane) => void,
  ): WezAction<A>;

  export function config_builder(): Config;

  export function json_parse(str: string): { [k: string]: unknown };
  export function json_encode(tbl: any): string;

  export function log_warn(...args: any[]): void;

  export function format(formats: Formats[]): Format;
}
