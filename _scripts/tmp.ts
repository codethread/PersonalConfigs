import { parseArgs } from "jsr:@std/cli/parse-args";
import { Chalk } from "npm:chalk";
import { EOL } from "node:os";
import { z, ZodFirstPartyTypeKind as Z } from "npm:zod@^3";

const chalk = new Chalk({ level: 3 });

export const SlackSchema = z
  .object({
    help: z.boolean().optional(),
    email: z.string().email(),
    password: z
      .string()
      .describe(
        "You should pass this via some external command, e.g 1password. Can also be passed with ENV: SLACK_PASS",
      )
      .optional(),
    domain: z.string().url().default("https://slack.com"),
    browserPath: z
      .string()
      .default("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"),
  })
  .describe("cli stuff")
  .strict();

type Primitive = z.ZodNumber | z.ZodString | z.ZodBoolean;

type ValidSchema = {
  [key: string]: Primitive | z.ZodOptional<Primitive> | z.ZodDefault<Primitive>;
};

type Checks = z.ZodNumberCheck | z.ZodStringCheck;
const interestingChecks: Checks["kind"][] = [
  "email",
  "url",
  "ip",
  "uuid",
  "date",
  "time",
  "datetime",
  "duration",
];

function generateHelpDoc(
  schema: ValidSchema,
  opts: {
    name?: string;
    description?: string;
    examples?: string[];
  } = {},
) {
  const entries = Object.entries(schema);
  const widestName = entries.reduce(
    (longest, [name]) => (name.length > longest ? name.length : longest),
    0,
  );

  const stringOpts = entries.map(([name, optionalSchema]) => {
    const typeName = optionalSchema._def.typeName;
    const required = !optionalSchema.isOptional();

    const defaultValue =
      typeName === Z.ZodDefault ? optionalSchema._def.defaultValue() : null;
    const primitive =
      typeName === Z.ZodOptional || typeName === Z.ZodDefault
        ? optionalSchema._def.innerType
        : (optionalSchema as Primitive);

    // deno-lint-ignore no-explicit-any
    const checks: string[] = (primitive._def as any).checks
      ?.reduce(
        (acc: string[], curr: Checks) =>
          interestingChecks.includes(curr.kind) ? acc.concat(curr.kind) : acc,
        [],
      )
      .join(",");

    const padding = " ".repeat(widestName - name.length) + " ";
    const pad = "     " + " ".repeat(widestName);

    return [
      " ",
      chalk.cyan(`--${name}`),
      `${required ? chalk.red("*") : chalk.blue("?")}`,
      padding,
      primitive.description && primitive.description + EOL + pad,
      chalk.green(
        `[${primitive._def.typeName.toLowerCase().replace("zod", "")}]`,
      ),
      checks && chalk.dim.white(` {${checks}}`),
      defaultValue && chalk.dim.italic(` ${defaultValue}`),
      EOL,
    ]
      .filter(Boolean)
      .join("");
  });

  const options =
    stringOpts.length &&
    [" ", chalk.yellow("Options:"), " ", stringOpts].flat();

  const examples =
    opts.examples &&
    [
      " ",
      chalk.yellow("Example Usage"),
      " ",
      opts.examples.map((e) => [chalk.magentaBright(`$ ${e}`), " "]),
    ].flat(2);

  return [
    opts.name && chalk.magenta.bold(opts.name),
    " ",
    opts.description,
    options,
    examples,
  ]
    .filter(Boolean)
    .flat()
    .join(EOL);
}

const args = parseArgs(Deno.args, { "--": false });

console.log(args);
const parsed = SlackSchema.safeParse(args);
if (parsed.success) {
  console.log("yay", parsed.data);
} else {
  console.error(parsed.error);
  console.log(
    generateHelpDoc(SlackSchema.shape, {
      name: "My Cli",
      description: SlackSchema.description,
      examples: ["run me!", "or me..."],
    }),
  );
}
