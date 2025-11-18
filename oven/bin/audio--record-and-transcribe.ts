// :module: Audio recording and transcription utility

import {type Subprocess, spawn, spawnSync} from "bun";
import {existsSync} from "fs";
import {unlink, writeFile} from "fs/promises";
import {homedir} from "os";
import {join} from "path";
import readline from "readline";

// Type for readline keypress event
interface Key {
	name: string;
	ctrl: boolean;
	meta: boolean;
	shift: boolean;
}

// Configuration
const WHISPER_MODEL = join(homedir(), "dev/models/ggml-medium.bin");
const TEMP_DIR = "/tmp";
const PID_FILE = join(TEMP_DIR, "audio-recording.pid");

// ANSI color codes
const colors = {
	RED: "\x1b[31m",
	GREEN: "\x1b[32m",
	YELLOW: "\x1b[33m",
	RESET: "\x1b[0m",
} as const;

// Track processes for cleanup
let soxProcess: Subprocess | null = null;
let whisperProcess: Subprocess | null = null;
let rl: readline.Interface | null = null;
let tempAudioFile: string | null = null;

// Cleanup function
async function cleanup(_cancelled = false): Promise<void> {
	// Kill sox process if running
	if (soxProcess) {
		try {
			soxProcess.kill();
			await soxProcess.exited;
		} catch {
			// Force kill if normal kill fails
			try {
				soxProcess.kill(9);
			} catch {}
		}
		soxProcess = null;
	}

	// Kill whisper process if running
	if (whisperProcess) {
		try {
			whisperProcess.kill();
			await whisperProcess.exited;
		} catch {
			// Force kill if normal kill fails
			try {
				whisperProcess.kill(9);
			} catch {}
		}
		whisperProcess = null;
	}

	// Restore terminal and close readline
	if (rl) {
		rl.close();
		rl = null;
	}

	// Clean up temp file
	if (tempAudioFile && existsSync(tempAudioFile)) {
		try {
			await unlink(tempAudioFile);
		} catch {}
		tempAudioFile = null;
	}

	// Clean up PID file
	if (existsSync(PID_FILE)) {
		try {
			await unlink(PID_FILE);
		} catch {}
	}

	// Restore terminal settings
	if (process.stdin.isTTY) {
		process.stdin.setRawMode(false);
	}
}

// Check dependencies
function checkDependencies(): void {
	const missing: string[] = [];

	// Check for sox
	const soxCheck = spawnSync(["sh", "-c", "which sox"], {
		stdout: "pipe",
		stderr: "pipe",
	});
	if (soxCheck.exitCode !== 0) {
		missing.push("sox");
	}

	// Check for whisper-cli
	const whisperCheck = spawnSync(["sh", "-c", "which whisper-cli"], {
		stdout: "pipe",
		stderr: "pipe",
	});
	if (whisperCheck.exitCode !== 0) {
		missing.push("whisper-cli");
	}

	// Check for whisper model
	if (!existsSync(WHISPER_MODEL)) {
		missing.push(`whisper model at ${WHISPER_MODEL}`);
	}

	if (missing.length > 0) {
		console.error(`${colors.RED}Error: Missing dependencies:${colors.RESET}`);
		for (const dep of missing) {
			console.error(`  - ${dep}`);
		}
		console.error(`${colors.YELLOW}Install sox with: brew install sox${colors.RESET}`);
		console.error(`${colors.YELLOW}Install whisper-cli with: brew install whisper-cpp${colors.RESET}`);
		console.error(
			`${colors.YELLOW}Download model: Run your nushell boot script or manually download to ~/dev/models/${colors.RESET}`,
		);
		process.exit(1);
	}
}

// Record audio
async function recordAudio(): Promise<string | null> {
	const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
	tempAudioFile = join(TEMP_DIR, `recording_${timestamp}.wav`);

	// Start sox recording in background
	soxProcess = spawn(["sox", "-d", "-r", "16000", "-c", "1", "-b", "16", tempAudioFile], {
		stdout: "pipe",
		stderr: "pipe",
	});

	// Set up readline for keyboard input
	rl = readline.createInterface({
		input: process.stdin,
		output: process.stdout,
	});

	// Enable raw mode for single keypress detection
	if (process.stdin.isTTY) {
		process.stdin.setRawMode(true);
	}
	readline.emitKeypressEvents(process.stdin);

	console.error(
		`${colors.GREEN}🎤 Recording... Press Space to stop and transcribe, Escape or Ctrl+C to cancel${colors.RESET}`,
	);

	return new Promise((resolve) => {
		const keypressHandler = (_: string, key: Key) => {
			if (key) {
				// Space bar - stop and transcribe
				if (key.name === "space") {
					process.stdin.off("keypress", keypressHandler);
					console.error(`\n${colors.GREEN}✓ Recording stopped${colors.RESET}`);
					resolve(tempAudioFile);
				}
				// Escape key - cancel
				else if (key.name === "escape") {
					process.stdin.off("keypress", keypressHandler);
					console.error(`\n${colors.YELLOW}✗ Recording cancelled${colors.RESET}`);
					resolve(null);
				}
				// Ctrl+C - cancel
				else if (key.ctrl && key.name === "c") {
					process.stdin.off("keypress", keypressHandler);
					console.error(`\n${colors.YELLOW}✗ Recording cancelled${colors.RESET}`);
					resolve(null);
				}
			}
		};

		process.stdin.on("keypress", keypressHandler);
	});
}

// Record audio in immediate mode (waits for SIGUSR1 signal to stop)
async function recordAudioImmediate(): Promise<string | null> {
	const timestamp = new Date().toISOString().replace(/[:.]/g, "-");
	tempAudioFile = join(TEMP_DIR, `recording_${timestamp}.wav`);

	// Write PID file for external control
	await writeFile(PID_FILE, process.pid.toString());

	// Start sox recording in background
	soxProcess = spawn(["sox", "-d", "-r", "16000", "-c", "1", "-b", "16", tempAudioFile], {
		stdout: "pipe",
		stderr: "pipe",
	});

	console.error(
		`${colors.GREEN}🎤 Recording started (PID: ${process.pid})... Send SIGUSR1 to stop${colors.RESET}`,
	);

	return new Promise((resolve) => {
		const signalHandler = () => {
			console.error(`\n${colors.GREEN}✓ Received stop signal${colors.RESET}`);
			resolve(tempAudioFile);
		};

		process.on("SIGUSR1", signalHandler);
	});
}

// Transcribe audio
async function transcribeAudio(audioFile: string): Promise<void> {
	// Stop sox recording first
	if (soxProcess) {
		try {
			soxProcess.kill();
			await soxProcess.exited;
		} catch {}
		soxProcess = null;
	}

	// Check file size
	const stats = Bun.file(audioFile);
	const fileSize = stats.size;

	if (fileSize < 1000) {
		console.error(`${colors.YELLOW}Recording too short or empty${colors.RESET}`);
		return;
	}

	console.error(`${colors.YELLOW}📝 Transcribing...${colors.RESET}`);

	// Run whisper with minimal output, send transcription to stdout
	whisperProcess = spawn(["whisper-cli", "-m", WHISPER_MODEL, "-nt", "-np", audioFile], {
		stdout: "pipe",
		stderr: "pipe",
	});

	// Stream stdout directly
	if (whisperProcess.stdout && typeof whisperProcess.stdout !== "number") {
		for await (const chunk of whisperProcess.stdout) {
			process.stdout.write(chunk);
		}
	}

	await whisperProcess.exited;
	whisperProcess = null;
}

// Main function
async function main(): Promise<void> {
	// Check for help flag
	const args = process.argv.slice(2);
	if (args.includes("-h") || args.includes("--help")) {
		console.log("record-and-transcribe - Record audio and transcribe using Whisper");
		console.log("");
		console.log("Usage: record-and-transcribe [--immediate]");
		console.log("");
		console.log("Options:");
		console.log("  -h, --help      Show this help message");
		console.log("  --immediate     Start recording and wait for SIGUSR1 signal to stop");
		console.log("");
		console.log("Description:");
		console.log("  Records audio from the default microphone and transcribes it using");
		console.log("  OpenAI's Whisper model.");
		console.log("");
		console.log("  Interactive mode (default): Press Space to stop and transcribe,");
		console.log("  or Escape/Ctrl+C to cancel.");
		console.log("");
		console.log("  Immediate mode (--immediate): Starts recording immediately and waits");
		console.log("  for SIGUSR1 signal. Send 'kill -SIGUSR1 <pid>' to stop and transcribe.");
		console.log("  PID is written to /tmp/audio-recording.pid");
		console.log("");
		console.log("Requirements:");
		console.log("  - sox (brew install sox)");
		console.log("  - whisper-cli (brew install whisper-cpp)");
		console.log("  - Whisper model at ~/dev/models/ggml-medium.bin");
		console.log("");
		console.log("Examples:");
		console.log("  record-and-transcribe              # Interactive mode");
		console.log("  record-and-transcribe --immediate  # Signal-based mode");
		process.exit(0);
	}

	const immediateMode = args.includes("--immediate");

	// Set up signal handlers for cleanup
	const signalHandler = async (_signal: string) => {
		await cleanup(true);
		process.exit(130); // Standard exit code for Ctrl+C
	};

	process.on("SIGINT", signalHandler);
	process.on("SIGTERM", signalHandler);

	try {
		checkDependencies();

		const audioFile = immediateMode ? await recordAudioImmediate() : await recordAudio();

		if (audioFile) {
			await transcribeAudio(audioFile);
			await cleanup(false);
			process.exit(0);
		} else {
			await cleanup(true);
			process.exit(130);
		}
	} catch (error) {
		console.error(`${colors.RED}Error:`, error, colors.RESET);
		await cleanup(true);
		process.exit(2);
	}
}

// Run the main function
if (import.meta.main) {
	main().catch(async (error) => {
		console.error(`${colors.RED}Fatal error:`, error, colors.RESET);
		await cleanup(true);
		process.exit(2);
	});
}
