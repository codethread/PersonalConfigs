import * as wezterm from "wezterm";
import { Settings } from "./settings";
const act = wezterm.action;

const config = wezterm.config_builder();
Settings.applyToConfig(config);

config.font_size = 14;

export { config };
