import {$} from "bun";
import {readdir} from "fs/promises";
import {join} from "path";

export async function cleanBuilds() {
	console.log("Cleaning build artifacts");
	const ovenDir = join(import.meta.dir, "..");
	const ovenFiles = await readdir(ovenDir);
	const buildArtifacts = ovenFiles.filter((f) => f.endsWith(".bun-build"));

	for (const artifact of buildArtifacts) {
		const artifactPath = join(ovenDir, artifact);
		await $`rm ${artifactPath}`.quiet();
		console.log(`  Removed: ${artifact}`);
	}

	if (buildArtifacts.length === 0) {
		console.log("  No build artifacts to clean");
	}
}
