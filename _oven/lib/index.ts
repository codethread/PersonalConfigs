import "@total-typescript/ts-reset";

export function foo() {
  return 1 + 1;
}
export function bar() {
  let s: Array<number | null> = [];
  const y = s.filter(Boolean);
  return Bun.version;
}
