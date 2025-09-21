#!/usr/bin/env bun

import { ElevenLabsClient } from "elevenlabs";
import { promises as fs } from "fs";
import { resolve } from "path";
import { parseArgs } from "util";
import { exec } from "child_process";
import { promisify } from "util";

const execAsync = promisify(exec);

// Parse command line arguments
const { values, positionals } = parseArgs({
  args: Bun.argv.slice(2),
  options: {
    voice: { type: "string", default: "Jessica" },
    "no-play": { type: "boolean", default: false },
    stdin: { type: "boolean", default: false },
    help: { type: "boolean", short: "h", default: false },
  },
  strict: false,
  allowPositionals: true,
});

function showHelp() {
  console.log(`
ElevenLabs Text-to-Speech CLI tool for reading text files.

Usage:
    speak-ts <file_path> [options]
    speak-ts --stdin [options]
    cat file.txt | speak-ts --stdin

Options:
    --voice VOICE     Voice name (default: Jessica)
    --no-play         Don't play audio after generation
    --stdin           Read from standard input instead of file
    -h, --help        Show this help

Examples:
    speak-ts README.md
    speak-ts specs/document.md --voice Brian
    echo "Hello world" | speak-ts --stdin
    cat article.txt | speak-ts --stdin --voice Liam

Available voices:
    Jessica, Rachel, Brian, Liam, Sarah, Charlie, Alice, Matilda, Will, Eric, Chris, Daniel, Lily, Bill
`);
}

function cleanMarkdown(text: string): string {
  // Remove code blocks but keep their content description
  text = text.replace(/```[\w]*\n(.*?)```/gs, "Code block: $1");

  // Convert headers to spoken format
  text = text.replace(/^#{1,6}\s+(.+)$/gm, "$1.");

  // Remove markdown formatting
  text = text.replace(/\*\*(.+?)\*\*/g, "$1"); // Bold
  text = text.replace(/\*(.+?)\*/g, "$1"); // Italic
  text = text.replace(/`(.+?)`/g, "$1"); // Inline code

  // Convert lists to spoken format
  text = text.replace(/^[-*+]\s+/gm, "• ");
  text = text.replace(/^\d+\.\s+/gm, "");

  // Remove URLs from link text
  text = text.replace(/\[([^\]]+)\]\([^\)]+\)/g, "$1");

  // Remove image references
  text = text.replace(/!\[([^\]]*)\]\([^\)]+\)/g, "Image: $1");

  // Clean up tables
  text = text.replace(/\|/g, " ");
  text = text.replace(/^[-\s]+$/gm, "");

  // Remove multiple blank lines
  text = text.replace(/\n{3,}/g, "\n\n");

  return text.trim();
}

function chunkText(text: string, maxChars = 2500): string[] {
  const paragraphs = text.split("\n\n");
  const chunks: string[] = [];
  let currentChunk = "";

  for (const para of paragraphs) {
    if (currentChunk.length + para.length + 2 <= maxChars) {
      currentChunk += para + "\n\n";
    } else {
      if (currentChunk) {
        chunks.push(currentChunk.trim());
      }
      currentChunk = para + "\n\n";
    }
  }

  if (currentChunk) {
    chunks.push(currentChunk.trim());
  }

  return chunks;
}

async function getVoiceId(client: ElevenLabsClient, voiceName: string): Promise<string> {
  // Check if it's already a voice ID (typically has specific format)
  if (voiceName.length > 20 && !voiceName.includes(" ")) {
    return voiceName;
  }

  // Otherwise, look up the voice by name
  try {
    const response = await client.voices.getAll();
    const voicesArray = Array.isArray(response) ? response : response.voices || [];
    const voice = voicesArray.find((v: any) => v.name.toLowerCase() === voiceName.toLowerCase());

    if (voice) {
      return voice.voice_id;
    } else {
      console.error(`Voice "${voiceName}" not found.`);
      console.error("Available: Jessica, Rachel, Brian, Liam, Sarah, Charlie, Alice, Matilda, Will, Eric, Chris, Daniel, Lily, Bill");
      process.exit(1);
    }
  } catch (error) {
    console.error(`Error looking up voice: ${error}`);
    process.exit(1);
  }
}

async function main() {
  if (values.help) {
    showHelp();
    process.exit(0);
  }

  // Get API key from environment
  const apiKey = process.env.ELEVEN_API_KEY;
  if (!apiKey) {
    console.error("Error: ElevenLabs API key required");
    console.error("Set ELEVEN_API_KEY environment variable");
    process.exit(1);
  }

  // Initialize client
  const client = new ElevenLabsClient({ apiKey });

  let text: string;
  let inputName: string;

  if (values.stdin) {
    // Read from stdin
    console.log("Reading from stdin...");
    const decoder = new TextDecoder();
    const chunks: Uint8Array[] = [];

    for await (const chunk of Bun.stdin.stream()) {
      chunks.push(chunk);
    }

    text = decoder.decode(Buffer.concat(chunks));
    inputName = "stdin";

    // Check if input looks like markdown (simple heuristic)
    if (text.includes("#") || text.includes("```") || text.includes("**")) {
      console.log("Detected markdown formatting, cleaning...");
      text = cleanMarkdown(text);
    }
  } else {
    // Check if file provided
    const filePath = positionals[0];
    if (!filePath) {
      showHelp();
      process.exit(1);
    }

    // Read file
    const resolvedPath = resolve(filePath);
    try {
      text = await fs.readFile(resolvedPath, "utf-8");
    } catch (error) {
      console.error(`Error reading file: ${error}`);
      process.exit(1);
    }

    inputName = filePath.split("/").pop()?.replace(/\.[^.]+$/, "") || "output";

    // Clean markdown if it's a .md file
    if (resolvedPath.endsWith(".md")) {
      console.log("Cleaning markdown formatting...");
      text = cleanMarkdown(text);
    }
  }

  // Chunk text if needed (2500 char limit for free tier)
  const chunks =
    text.length > 2500
      ? (console.log(`Text is ${text.length} characters, splitting into chunks...`), chunkText(text))
      : [text];

  if (chunks.length > 1) {
    console.log(`Processing ${chunks.length} chunks...`);
  }

  try {
    const audioBuffers: Buffer[] = [];

    for (let i = 0; i < chunks.length; i++) {
      if (chunks.length > 1) {
        console.log(`Generating audio for chunk ${i + 1}/${chunks.length}...`);
      } else {
        console.log("Generating audio...");
      }

      const voiceId = await getVoiceId(client, values.voice as string);
      const audioStream = await client.textToSpeech.convert(voiceId, {
        text: chunks[i],
        model_id: "eleven_monolingual_v1",
      });

      // Collect the stream data
      const audioChunks: Uint8Array[] = [];
      for await (const chunk of audioStream) {
        audioChunks.push(chunk);
      }

      audioBuffers.push(Buffer.concat(audioChunks));
    }

    // Combine all audio buffers
    const combinedAudio = Buffer.concat(audioBuffers);

    // Save audio to .audio directory with filename based on input
    const outputPath = resolve(`.audio/${inputName}.mp3`);
    // Ensure directory exists
    await fs.mkdir(".audio", { recursive: true });
    console.log(`Saving to ${outputPath}...`);
    await fs.writeFile(outputPath, combinedAudio);
    console.log(`Audio saved to: ${outputPath}`);

    // Play by default, unless --no-play is set
    if (!values["no-play"]) {
      console.log("Playing audio...");
      await execAsync(`afplay "${outputPath}"`);
    }
  } catch (error: any) {
    console.error(`Error generating audio: ${error.message || error}`);
    if (error.stack) {
      console.error("\nStack trace:", error.stack);
    }
    if (error.response) {
      console.error("\nAPI Response:", error.response);
    }
    console.error("\nPossible issues:");
    console.error("  - Invalid API key");
    console.error("  - Voice name not found");
    console.error("  - API rate limit exceeded");
    console.error("  - Network connection issues");
    console.error("  - Text too long (max 2500 chars for free tier)");
    process.exit(1);
  }
}

main().catch(console.error);