import type { Event, AgentNode } from "../types";
import { MarkdownContent } from "./MarkdownContent";

interface EventDetailsPanelProps {
  event: Event | null;
  agents: AgentNode[];
  onClose: () => void;
}

export function EventDetailsPanel({ event, agents, onClose }: EventDetailsPanelProps) {
  if (!event) return null;

  const agent = agents.find((a) => a.id === event.agentId);
  const agentName = agent?.name || event.agentId || "Main Agent";
  const laneNumber = agents.findIndex((a) => a.id === event.agentId);

  return (
    <div className="fixed inset-y-0 right-0 w-[600px] bg-gray-900 border-l border-gray-800 shadow-2xl flex flex-col z-50">
      {/* Header */}
      <div className="flex-shrink-0 border-b border-gray-800 p-4">
        <div className="flex items-center justify-between mb-3">
          <h2 className="text-lg font-semibold text-gray-100">Event Details</h2>
          <button
            type="button"
            onClick={onClose}
            className="text-gray-400 hover:text-gray-100 transition-colors p-1 hover:bg-gray-800 rounded"
            title="Close panel (Esc)"
          >
            <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </div>

        {/* Agent Info Section */}
        <div className="mb-4 p-3 bg-blue-950 border border-blue-800 rounded">
          <div className="space-y-1.5">
            <div className="flex items-center gap-2 text-sm">
              <span className="text-blue-300 font-semibold">Agent:</span>
              <span className="text-blue-100">{agentName}</span>
              <span className="text-blue-400 text-xs">(Lane #{laneNumber})</span>
            </div>
            {agent?.subagentType && (
              <div className="flex items-center gap-2 text-sm">
                <span className="text-blue-300 font-semibold">Type:</span>
                <span className="text-blue-200 font-mono">{agent.subagentType}</span>
                {agent.model && (
                  <>
                    <span className="text-blue-400">-</span>
                    <span className="text-blue-300 font-semibold">Model:</span>
                    <span className="text-blue-200 font-mono">{agent.model}</span>
                  </>
                )}
              </div>
            )}
          </div>
        </div>

        {/* Event metadata */}
        <div className="space-y-2">
          <div className="flex items-center gap-2 text-sm">
            <span className="text-gray-500">Time:</span>
            <span className="font-mono text-gray-300">{formatTime(event.timestamp)}</span>
          </div>
          <div className="flex items-center gap-2 text-sm">
            <span className="text-gray-500">Type:</span>
            <div className={`text-xs px-2 py-0.5 rounded ${getEventTypeBadgeClass(event.type)}`}>
              {event.type}
            </div>
          </div>
          <div className="flex items-center gap-2 text-sm">
            <span className="text-gray-500">Event ID:</span>
            <span className="font-mono text-xs text-gray-400">{event.id}</span>
          </div>
        </div>
      </div>

      {/* Content */}
      <div className="flex-1 overflow-y-auto p-4">
        <EventContent event={event} />
      </div>
    </div>
  );
}

function EventContent({ event }: { event: Event }) {
  const { data } = event;

  switch (data.type) {
    case "user-message":
      return (
        <div className="space-y-3">
          <SectionHeader>User Message</SectionHeader>
          <div className="bg-gray-800 p-4 rounded border border-gray-700">
            <MarkdownContent>{data.text}</MarkdownContent>
          </div>
        </div>
      );

    case "assistant-message":
      return (
        <div className="space-y-3">
          <SectionHeader>Assistant Message</SectionHeader>
          <div className="bg-gray-800 p-4 rounded border border-gray-700">
            <MarkdownContent>{data.text}</MarkdownContent>
          </div>
        </div>
      );

    case "thinking":
      return (
        <div className="space-y-3">
          <SectionHeader emoji="💭">Thinking</SectionHeader>
          <div className="bg-gray-800 p-4 rounded border border-gray-700">
            <MarkdownContent className="text-gray-400">{data.content}</MarkdownContent>
          </div>
        </div>
      );

    case "tool-use":
      const isTaskTool = data.toolName === "Task";
      const subagentType = isTaskTool ? (data.input.subagent_type as string | undefined) : undefined;
      const model = isTaskTool ? (data.input.model as string | undefined) : undefined;

      return (
        <div className="space-y-4">
          <SectionHeader emoji="🔧">Tool Use</SectionHeader>

          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-500">Tool:</span>
              <span className="text-sm text-blue-400 font-semibold">{data.toolName}</span>
            </div>
            {data.description && (
              <div className="flex items-start gap-2">
                <span className="text-sm text-gray-500">Description:</span>
                <span className="text-sm text-gray-300">{data.description}</span>
              </div>
            )}
            {subagentType && (
              <div className="flex items-center gap-2">
                <span className="text-sm text-gray-500">Type:</span>
                <span className="text-sm text-purple-400 font-mono">{subagentType}</span>
              </div>
            )}
            {model && (
              <div className="flex items-center gap-2">
                <span className="text-sm text-gray-500">Model:</span>
                <span className="text-sm text-green-400 font-mono">{model}</span>
              </div>
            )}
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-500">Tool ID:</span>
              <span className="text-xs text-gray-400 font-mono">{data.toolId}</span>
            </div>
          </div>

          <div>
            <div className="text-sm text-gray-400 mb-2">Input Parameters:</div>
            <pre className="text-xs text-gray-300 whitespace-pre-wrap overflow-x-auto bg-gray-800 p-4 rounded border border-gray-700">
              {JSON.stringify(data.input, null, 2)}
            </pre>
          </div>
        </div>
      );

    case "tool-result":
      return (
        <div className="space-y-4">
          <SectionHeader emoji={data.success ? "✅" : "❌"}>Tool Result</SectionHeader>

          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-500">Status:</span>
              <span className={`text-sm font-semibold ${data.success ? "text-green-400" : "text-red-400"}`}>
                {data.success ? "Success" : "Error"}
              </span>
            </div>
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-500">Tool Use ID:</span>
              <span className="text-xs text-gray-400 font-mono">{data.toolUseId}</span>
            </div>
            {data.agentId && (
              <div className="flex items-center gap-2">
                <span className="text-sm text-gray-500">From Agent:</span>
                <span className="text-sm text-blue-400">{data.agentId}</span>
              </div>
            )}
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-500">Output Size:</span>
              <span className="text-sm text-gray-300">{data.output.length} characters</span>
            </div>
          </div>

          <div>
            <div className="text-sm text-gray-400 mb-2">Output:</div>
            <pre className="text-xs text-gray-300 whitespace-pre-wrap overflow-x-auto bg-gray-800 p-4 rounded border border-gray-700 max-h-96 overflow-y-auto">
              {data.output}
            </pre>
          </div>
        </div>
      );

    case "agent-spawn":
      return (
        <div className="space-y-4">
          <SectionHeader emoji="🚀">Agent Spawn</SectionHeader>

          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-500">Agent ID:</span>
              <span className="text-sm text-blue-400 font-mono">{data.agentId}</span>
            </div>
            {data.model && (
              <div className="flex items-center gap-2">
                <span className="text-sm text-gray-500">Model:</span>
                <span className="text-sm text-gray-300">{data.model}</span>
              </div>
            )}
            {data.description && (
              <div className="flex items-start gap-2">
                <span className="text-sm text-gray-500">Description:</span>
                <span className="text-sm text-gray-300">{data.description}</span>
              </div>
            )}
          </div>

          <div>
            <div className="text-sm text-gray-400 mb-2">Prompt:</div>
            <div className="bg-gray-800 p-4 rounded border border-gray-700">
              <MarkdownContent>{data.prompt}</MarkdownContent>
            </div>
          </div>
        </div>
      );

    case "summary":
      return (
        <div className="space-y-3">
          <SectionHeader emoji="📝">Summary</SectionHeader>
          <div className="text-sm text-gray-300 bg-gray-800 p-4 rounded border border-gray-700">
            {data.summary}
          </div>
        </div>
      );

    default:
      return (
        <div className="space-y-3">
          <SectionHeader>Unknown Event Type</SectionHeader>
          <pre className="text-xs text-gray-500 whitespace-pre-wrap bg-gray-800 p-4 rounded border border-gray-700">
            {JSON.stringify(data, null, 2)}
          </pre>
        </div>
      );
  }
}

function SectionHeader({ emoji, children }: { emoji?: string; children: React.ReactNode }) {
  return (
    <h3 className="text-sm font-semibold text-gray-200 flex items-center gap-2">
      {emoji && <span>{emoji}</span>}
      <span>{children}</span>
    </h3>
  );
}

function getEventTypeBadgeClass(type: string): string {
  const classes: Record<string, string> = {
    "user-message": "bg-cyan-900 text-cyan-200",
    "assistant-message": "bg-purple-900 text-purple-200",
    thinking: "bg-purple-800 text-purple-200",
    "tool-use": "bg-blue-900 text-blue-200",
    "tool-result": "bg-green-900 text-green-200",
    "agent-spawn": "bg-yellow-900 text-yellow-200",
    summary: "bg-gray-700 text-gray-200",
  };
  return classes[type] || "bg-gray-700 text-gray-200";
}

function formatTime(date: Date | string | null | undefined): string {
  if (!date) return "00:00:00.000";
  const d = typeof date === "string" ? new Date(date) : date;
  return d.toLocaleTimeString("en-US", {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    fractionalSecondDigits: 3,
  });
}
