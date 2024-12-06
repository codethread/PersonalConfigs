import { Result, ok, err } from "@codethread/tstl-result";

const fileCache = new LuaMap<string, string>();

export const Fs = {
  readFile(
    file: string,
    opts: { noCache?: boolean } = {},
  ): Result<string, string> {
    const inCache = fileCache.get(file);
    if (!opts.noCache && inCache) {
      return ok(inCache);
    }
    const [f] = io.open(file, "rb");
    if (!f) return err(file);
    const content = f.read("*all" as any);
    if (typeof content != "string") throw new Error("ah");
    f.close();
    fileCache.set(file, content);
    return ok(content);
  },
};
