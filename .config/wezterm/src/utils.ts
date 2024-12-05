export const Utils = {
  tap:
    (msg?: string) =>
    <A>(s: A) => {
      print(`tap${msg ? " " + msg : ""}:`, s);
      return s;
    },
};
