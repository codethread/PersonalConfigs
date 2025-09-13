#!/usr/bin/env bun

import { readFile, access, mkdtemp, writeFile, rm } from "fs/promises";
import { join } from "path";
import { existsSync } from "fs";
import { tmpdir } from "os";

interface LogEvent {
    timestamp: string;
    unix_timestamp: number;
    event: string;
    session_id: string;
    cwd: string;
    transcript_path: string;
    tool_name?: string;
    tool_input?: {
        subagent_type: string;
        description: string;
    };
    raw_data?: {
        transcript_path?: string;
        tool_response?: {
            totalDurationMs: number;
            totalTokens: number;
            totalToolUseCount: number;
        };
    };
}

interface TranscriptEvent {
    isSidechain?: boolean;
    type?: string;
    message?: {
        model?: string;
        content?: Array<{
            type: string;
            name?: string;
        }>;
    };
    toolUseResult?: {
        totalDurationMs: number;
        totalTokens: number;
        totalToolUseCount: number;
    };
}

interface PairedCall {
    call_id: string;
    agent_type: string;
    description: string;
    start_time: string;
    end_time?: string;
    duration_ms?: number;
    transcript_path?: string;
    tool_response?: {
        totalDurationMs: number;
        totalTokens: number;
        totalToolUseCount: number;
    };
    status?: string;
}

class SubagentAnalyzer {
    private logFile: string;
    private tempDir: string = "";

    constructor(logFile: string) {
        this.logFile = logFile;
    }

    async analyze(): Promise<void> {
        await this.checkFile();
        await this.setupTempDir();

        try {
            console.log("=== Claude Code Subagent Analysis ===");
            console.log(`Log file: ${this.logFile}`);
            console.log("");

            const { preEvents, postEvents } = await this.extractEvents();
            const agentCalls = preEvents.length;
            
            console.log(`Total subagent calls: ${agentCalls}`);
            console.log("");

            if (agentCalls === 0) {
                console.log("No subagents were used in this session.");
                return;
            }

            const pairedCalls = await this.pairEvents(preEvents, postEvents);
            
            this.showTimeline(pairedCalls);
            await this.showDetailedAnalysis(pairedCalls);
            this.showSummaryStats(pairedCalls, agentCalls);

        } finally {
            await this.cleanup();
        }
    }

    private async checkFile(): Promise<void> {
        try {
            await access(this.logFile);
        } catch {
            console.error(`Error: Log file not found: ${this.logFile}`);
            process.exit(1);
        }
    }

    private async setupTempDir(): Promise<void> {
        this.tempDir = await mkdtemp(join(tmpdir(), 'analyze-subagents-'));
    }

    private async cleanup(): Promise<void> {
        if (this.tempDir) {
            await rm(this.tempDir, { recursive: true, force: true });
        }
    }

    private async extractEvents(): Promise<{ preEvents: LogEvent[], postEvents: LogEvent[] }> {
        const content = await readFile(this.logFile, 'utf-8');
        const lines = content.trim().split('\n').filter(line => line.trim());
        
        const preEvents: LogEvent[] = [];
        const postEvents: LogEvent[] = [];

        for (const line of lines) {
            try {
                const event = JSON.parse(line) as LogEvent;
                if (event.tool_name === "Task") {
                    if (event.event === "PreToolUse") {
                        preEvents.push(event);
                    } else if (event.event === "PostToolUse") {
                        postEvents.push(event);
                    }
                }
            } catch (error) {
                // Skip invalid JSON lines
                continue;
            }
        }

        return { preEvents, postEvents };
    }

    private async pairEvents(preEvents: LogEvent[], postEvents: LogEvent[]): Promise<PairedCall[]> {
        const pairedCalls: PairedCall[] = [];

        for (let i = 0; i < preEvents.length; i++) {
            const pre = preEvents[i];
            const callId = (i + 1).toString();

            // Find matching PostToolUse event
            const matchingPost = postEvents.find(post => 
                post.unix_timestamp > pre.unix_timestamp &&
                post.tool_input?.subagent_type === pre.tool_input?.subagent_type
            );

            if (matchingPost) {
                const durationMs = matchingPost.unix_timestamp - pre.unix_timestamp;
                
                pairedCalls.push({
                    call_id: callId,
                    agent_type: pre.tool_input!.subagent_type,
                    description: pre.tool_input!.description,
                    start_time: pre.timestamp,
                    end_time: matchingPost.timestamp,
                    duration_ms: durationMs,
                    transcript_path: matchingPost.raw_data?.transcript_path,
                    tool_response: matchingPost.raw_data?.tool_response
                });
            } else {
                // Handle unpaired PreToolUse
                pairedCalls.push({
                    call_id: callId,
                    agent_type: pre.tool_input!.subagent_type,
                    description: pre.tool_input!.description,
                    start_time: pre.timestamp,
                    status: "running_or_failed"
                });
            }
        }

        return pairedCalls;
    }

    private showTimeline(pairedCalls: PairedCall[]): void {
        console.log("=== Agent Execution Timeline ===");
        console.log("");

        pairedCalls.forEach(call => {
            const formattedTime = call.start_time.replace('T', ' ').replace(/\.[0-9]*Z$/, '');
            
            if (call.status === "running_or_failed") {
                console.log(`${formattedTime} | Call ${call.call_id} | ${call.agent_type} | ${call.description} | [INCOMPLETE/FAILED]`);
            } else {
                const durationSec = Math.floor((call.duration_ms || 0) / 1000);
                console.log(`${formattedTime} | Call ${call.call_id} | ${call.agent_type} | ${call.description} | Duration: ${durationSec}s`);
            }
        });

        console.log("");
    }

    private async showDetailedAnalysis(pairedCalls: PairedCall[]): Promise<void> {
        console.log("=== Detailed Agent Analysis ===");
        console.log("");

        for (const call of pairedCalls) {
            console.log(`═══ Agent Call #${call.call_id}: ${call.agent_type} ═══`);
            console.log(`Description: ${call.description}`);
            console.log(`Start Time: ${call.start_time.replace('T', ' ').replace(/\.[0-9]*Z$/, '')}`);

            if (call.status === "running_or_failed") {
                console.log("Status: INCOMPLETE/FAILED");
                console.log("");
                continue;
            }

            const endTime = call.end_time!.replace('T', ' ').replace(/\.[0-9]*Z$/, '');
            const durationSec = Math.floor((call.duration_ms || 0) / 1000);
            
            console.log(`End Time: ${endTime}`);
            console.log(`Duration: ${call.duration_ms}ms (${durationSec}s)`);

            if (call.tool_response) {
                const agentDurationSec = Math.floor(call.tool_response.totalDurationMs / 1000);
                console.log(`Agent Execution Time: ${call.tool_response.totalDurationMs}ms (${agentDurationSec}s)`);
                console.log(`Tokens Used: ${call.tool_response.totalTokens}`);
                console.log(`Tools Called: ${call.tool_response.totalToolUseCount}`);
            }

            // Get detailed info from transcript if available
            if (call.transcript_path && existsSync(call.transcript_path)) {
                await this.analyzeTranscript(call.transcript_path);
            }

            console.log("");
        }
    }

    private async analyzeTranscript(transcriptPath: string): Promise<void> {
        try {
            const content = await readFile(transcriptPath, 'utf-8');
            const lines = content.trim().split('\n').filter(line => line.trim());
            
            let model = "";
            const toolCounts: Record<string, number> = {};

            for (const line of lines) {
                try {
                    const event = JSON.parse(line) as TranscriptEvent;
                    
                    if (event.isSidechain && event.type === "assistant" && event.message?.model && !model) {
                        model = event.message.model;
                    }

                    if (event.isSidechain && event.message?.content) {
                        for (const content of event.message.content) {
                            if (content.type === "tool_use" && content.name) {
                                toolCounts[content.name] = (toolCounts[content.name] || 0) + 1;
                            }
                        }
                    }
                } catch {
                    continue;
                }
            }

            if (model) {
                console.log(`Model Used: ${model}`);
            }

            if (Object.keys(toolCounts).length > 0) {
                console.log("Tool Usage Breakdown:");
                Object.entries(toolCounts)
                    .sort(([, a], [, b]) => b - a)
                    .forEach(([tool, count]) => {
                        console.log(`  ${count.toString().padStart(3)} x ${tool}`);
                    });
            }
        } catch (error) {
            // Skip transcript analysis if there's an error
        }
    }

    private showSummaryStats(pairedCalls: PairedCall[], totalCalls: number): void {
        console.log("=== Summary Statistics ===");
        console.log("");

        // Summary by agent type
        console.log("Agents Used:");
        const agentCounts: Record<string, number> = {};
        pairedCalls.forEach(call => {
            agentCounts[call.agent_type] = (agentCounts[call.agent_type] || 0) + 1;
        });

        Object.entries(agentCounts)
            .sort(([, a], [, b]) => b - a)
            .forEach(([type, count]) => {
                console.log(`  ${count.toString().padStart(2)} x ${type}`);
            });

        console.log("");

        // Overall statistics
        const completedCalls = pairedCalls.filter(call => call.status !== "running_or_failed");
        const totalDuration = completedCalls.reduce((sum, call) => sum + (call.tool_response?.totalDurationMs || 0), 0);
        const totalTokens = completedCalls.reduce((sum, call) => sum + (call.tool_response?.totalTokens || 0), 0);
        const totalTools = completedCalls.reduce((sum, call) => sum + (call.tool_response?.totalToolUseCount || 0), 0);

        console.log(`Completed Calls: ${completedCalls.length}/${totalCalls}`);
        console.log(`Total Agent Time: ${totalDuration}ms (${Math.floor(totalDuration / 1000)}s)`);
        console.log(`Total Tokens: ${totalTokens}`);
        console.log(`Total Tools Called: ${totalTools}`);
        
        if (completedCalls.length > 0) {
            const avgDuration = Math.floor(totalDuration / completedCalls.length);
            const avgTokens = Math.floor(totalTokens / completedCalls.length);
            console.log(`Average per Call: ${avgDuration}ms, ${avgTokens} tokens`);
        } else {
            console.log("Average per Call: N/A");
        }
    }
}

async function main() {
    const args = process.argv.slice(2);
    const logFile = args[0] || ".logs/claude-session-current.jsonl";
    
    const analyzer = new SubagentAnalyzer(logFile);
    await analyzer.analyze();
}

// Run if called directly
if (import.meta.main) {
    main().catch(console.error);
}