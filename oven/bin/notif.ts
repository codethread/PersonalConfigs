// :module: System notification command-line utility

import {execSync} from "child_process";
import {existsSync, readFileSync, unlinkSync, writeFileSync} from "fs";
import {parseArgs} from "util";

// Show macOS native notification
// Usage: notif [options] [title] [message...]
// Options:
//   --aerospace, -a         Use 'aerospace' as title
//   --id <id>              Delayed notification with ID
//   --clear <id>           Clear delayed notification by ID
//   --delay <ms>           Delay in milliseconds (default: 5000)
//   --help, -h             Show help

const logMessage = (message: string) => {
	const timestamp = new Date().toString();
	const logEntry = `${timestamp}: ${message}\n`;
	try {
		execSync(`echo "${logEntry}" >> /tmp/notif.log`, {stdio: "ignore"});
	} catch {
		// Ignore logging errors
	}
};

const showHelp = () => {
	console.log(`
notif - Show macOS native notifications

Usage: notif [options] [title] [message...]

Options:
  --aerospace, -a         Use 'aerospace' as title
  --id <id>              Delayed notification with ID
  --clear <id>           Clear delayed notification by ID
  --delay <ms>           Delay in milliseconds (default: 5000)
  --help, -h             Show help

Examples:
  notif "Hello" "World"
  notif --aerospace "Workspace changed"
  notif --id=test --delay=3000 "Delayed" "This appears in 3 seconds"
  notif --clear=test

Use <> for line breaks in messages: notif "Title" "line1<>line2"
`);
};

interface NotifOptions {
	aerospace: boolean;
	id?: string;
	clear?: string;
	delay: number;
	help: boolean;
}

const parseCliArgs = (): {options: NotifOptions; title?: string; message?: string} => {
	const {values, positionals} = parseArgs({
		args: Bun.argv.slice(2),
		options: {
			aerospace: {type: "boolean", short: "a"},
			id: {type: "string"},
			clear: {type: "string"},
			delay: {type: "string", default: "5000"},
			help: {type: "boolean", short: "h"},
		},
		allowPositionals: true,
	});

	const options: NotifOptions = {
		aerospace: values.aerospace ?? false,
		id: values.id,
		clear: values.clear,
		delay: parseInt(values.delay as string, 10),
		help: values.help ?? false,
	};

	logMessage(`notif called with options: ${JSON.stringify(options)} positionals: ${positionals.join(" ")}`);

	let title: string | undefined;
	let message: string | undefined;

	if (options.aerospace) {
		title = "aerospace";
		message = positionals.length > 0 ? positionals.join(" ") : "empty";
	} else if (positionals.length === 0) {
		title = "Notification";
		message = "empty";
	} else if (positionals.length === 1) {
		title = positionals[0];
		message = "empty";
	} else {
		title = positionals[0];
		message = positionals.slice(1).join(" ");
	}

	return {options, title, message};
};

const clearNotification = (clearId: string): void => {
	const tmpFile = `/tmp/notif_${clearId}.tmp`;
	if (existsSync(tmpFile)) {
		logMessage(`clearing notification ID: ${clearId}`);
		unlinkSync(tmpFile);
		logMessage(`removed tmp file: ${tmpFile}`);
	} else {
		logMessage(`no tmp file found for ID: ${clearId}`);
	}
	process.exit(0);
};

const processMultilineMessage = (message: string): string => {
	if (!message.includes("<>")) {
		return message;
	}

	logMessage(`original message: '${message}'`);

	const formattedMessage = message.replace(/<>/g, "\n");
	logMessage(`after replacement: '${formattedMessage}'`);

	const lines = formattedMessage.split("\n");
	let maxLength = 0;

	logMessage(`split into ${lines.length} lines`);
	for (const line of lines) {
		logMessage(`line: '${line}' (length: ${line.length})`);
		if (line.length > maxLength) {
			maxLength = line.length;
		}
	}

	const paddedLines = lines.map((line) => {
		const paddingNeeded = maxLength - line.length;
		return line + " ".repeat(paddingNeeded);
	});

	logMessage(`processed multiline message, max_length=${maxLength}`);
	return paddedLines.join("\n");
};

const showNotification = (title: string, message: string): void => {
	try {
		execSync(`osascript -e "display notification \\"${message}\\" with title \\"${title}\\""`, {
			stdio: "pipe",
		});
	} catch (error) {
		logMessage(`Error showing notification: ${error}`);
	}
};

const createDelayedNotification = (options: {
	title: string;
	message: string;
	notificationId: string;
	delay: number;
}): void => {
	const {title, message, notificationId, delay} = options;
	const tmpFile = `/tmp/notif_${notificationId}.tmp`;

	const data = JSON.stringify({
		title,
		message,
		timestamp: new Date().toString(),
	});

	writeFileSync(tmpFile, data);
	logMessage(`created delayed notification with ID: ${notificationId}, delay: ${delay}ms`);

	setTimeout(() => {
		if (existsSync(tmpFile)) {
			logMessage(`executing delayed notification ID: ${notificationId}`);

			try {
				const storedData = JSON.parse(readFileSync(tmpFile, "utf8"));
				unlinkSync(tmpFile);
				showNotification(storedData.title, storedData.message);
				logMessage(`delayed notification completed for ID: ${notificationId}`);
			} catch (error) {
				logMessage(`Error executing delayed notification: ${error}`);
			}
		} else {
			logMessage(`delayed notification was cleared for ID: ${notificationId}`);
		}
	}, delay);

	logMessage("background process started for delayed notification");
};

const main = () => {
	const {options, title, message} = parseCliArgs();

	if (options.help) {
		showHelp();
		process.exit(0);
	}

	if (options.clear) {
		clearNotification(options.clear);
		return;
	}

	if (!title || !message) {
		console.error("Error: title and message are required");
		showHelp();
		process.exit(1);
	}

	logMessage(`title='${title}', message='${message}'`);

	const processedMessage = processMultilineMessage(message);

	if (options.id) {
		createDelayedNotification({
			title,
			message: processedMessage,
			notificationId: options.id,
			delay: options.delay,
		});
	} else {
		showNotification(title, processedMessage);
	}
};

if (import.meta.main) {
	main();
}
