// :module: Shared reporting utilities for CLI tools

export interface ReportOptions {
	quiet?: boolean;
	format?: "text" | "json";
}

/**
 * Standard report function for CLI output
 */
export function report(result: unknown, options: ReportOptions = {}) {
	if (options.quiet) return;

	if (options.format === "json") {
		console.log(JSON.stringify(result, null, 2));
	} else {
		// Handle different types of results
		if (typeof result === "string") {
			console.log(result);
		} else if (result && typeof result === "object" && "message" in result) {
			console.log((result as {message: string}).message);
		} else {
			console.log(result);
		}
	}
}

/**
 * Standard error reporting function
 */
export function reportError(error: unknown): void {
	if (error instanceof Error) {
		console.error(`Error: ${error.message}`);
		if (process.env.DEBUG) {
			console.error(error.stack);
		}
	} else if (typeof error === "string") {
		console.error(`Error: ${error}`);
	} else {
		console.error(`Error: ${JSON.stringify(error, null, 2)}`);
	}
}
