// export class Settings {
//   static getEnvs = () => {};
//
//   static applyToConfig = (config: any) => {
//     config.set_environment_variables = Settings.getEnvs();
//     config.default_prog = ["nu", "-l"];
//     // not sure if this will be annoying
//     config.exit_behavior = "CloseOnCleanExit";
//   };
// }
export const Settings = {
  getEnvs: () => {},

  applyToConfig(config: any) {
    config.set_environment_variables = this.getEnvs();
    config.default_prog = ["nu", "-l"];
    // not sure if this will be annoying
    config.exit_behavior = "CloseOnCleanExit";
  },
};
