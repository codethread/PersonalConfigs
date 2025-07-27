import { z } from "zod/v4";

// Zod schemas for kitty API responses
export const KittyTabSchema = z.object({
  id: z.number(),
  title: z.string(),
  is_active: z.boolean().optional(),
  is_focused: z.boolean().optional(),
});

export const KittyOSWindowSchema = z.object({
  tabs: z.array(KittyTabSchema),
});

export const KittyListResponseSchema = z.array(KittyOSWindowSchema);

export type KittyTab = z.infer<typeof KittyTabSchema>;
