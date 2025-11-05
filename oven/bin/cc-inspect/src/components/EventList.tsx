import type { Event, AgentNode } from "../types";
import { useState } from "react";

interface EventListProps {
  events: Event[];
  agents: AgentNode[];
}

export function EventList({ events, agents }: EventListProps) {
  return (
    <div className="space-y-2 font-mono text-sm">
      {events.map((event) => (
        <EventItem key={event.id} event={event} agents={agents} />
      ))}
    </div>
  );
}

interface EventItemProps {
  event: Event;
  agents: AgentNode[];
}

function EventItem({ event, agents }: EventItemProps) {
  const [expanded, setExpanded] = useState(false);

  const agent = agents.find((a) => a.id === event.agentId);
  const agentName = agent?.name || event.agentId || "main";
  const isMainAgent = event.agentId === agents[0]?.id || !event.agentId;

  return (
    <div className="border border-gray-700 rounded bg-gray-800 overflow-hidden">
      {/* Event header */}
      <button
        type="button"
        onClick={() => setExpanded(!expanded)}
        className="w-full px-4 py-2 flex items-center gap-3 hover:bg-gray-750 transition-colors text-left"
      >
        {/* Timeline marker */}
        <div className="flex items-center gap-2">
          <div className={`w-2 h-2 rounded-full ${isMainAgent ? "bg-green-500" : "bg-blue-500"}`} />
          <div className="text-xs text-gray-500">{formatTime(event.timestamp)}</div>
        </div>

        {/* Agent name */}
        <div className="text-xs text-gray-400 min-w-[120px] truncate">{agentName}</div>

        {/* Event type badge */}
        <div className={`text-xs px-2 py-0.5 rounded ${getEventTypeBadgeClass(event.type)}`}>
          {event.type}
        </div>

        {/* Event summary */}
        <div className="flex-1 text-gray-300 truncate">{getEventSummary(event)}</div>

        {/* Expand icon */}
        <div className="text-gray-500">
          {expanded ? "▼" : "▶"}
        </div>
      </button>

      {/* Expanded details */}
      {expanded && (
        <div className="px-4 py-3 bg-gray-900 border-t border-gray-700">
          <EventDetails event={event} />
        </div>
      )}
    </div>
  );
}

function EventDetails({ event }: { event: Event }) {
  const { data } = event;

  switch (data.type) {
    case "user-message":
      return <pre className="text-xs text-gray-300 whitespace-pre-wrap">{data.text}</pre>;

    case "assistant-message":
      return <pre className="text-xs text-gray-300 whitespace-pre-wrap">{data.text}</pre>;

    case "thinking":
      return (
        <div>
          <div className="text-xs text-purple-400 mb-2">💭 Thinking</div>
          <pre className="text-xs text-gray-400 whitespace-pre-wrap">{data.content}</pre>
        </div>
      );

    case "tool-use":
      return (
        <div className="space-y-2">
          <div className="flex items-center gap-2">
            <div className="text-xs text-blue-400">🔧 Tool: {data.toolName}</div>
            {data.description && (
              <div className="text-xs text-gray-500">• {data.description}</div>
            )}
          </div>
          <div className="text-xs text-gray-500">ID: {data.toolId}</div>
          <details className="text-xs">
            <summary className="text-gray-400 cursor-pointer hover:text-gray-300">
              Input parameters
            </summary>
            <pre className="mt-2 text-gray-500 whitespace-pre-wrap overflow-x-auto">
              {JSON.stringify(data.input, null, 2)}
            </pre>
          </details>
        </div>
      );

    case "tool-result":
      return (
        <div className="space-y-2">
          <div className="flex items-center gap-2">
            <div className={`text-xs ${data.success ? "text-green-400" : "text-red-400"}`}>
              {data.success ? "✅" : "❌"} Result
            </div>
            <div className="text-xs text-gray-500">Tool use: {data.toolUseId}</div>
            {data.agentId && (
              <div className="text-xs text-blue-400">Agent: {data.agentId}</div>
            )}
          </div>
          <details open={data.output.length < 500} className="text-xs">
            <summary className="text-gray-400 cursor-pointer hover:text-gray-300">
              Output ({data.output.length} chars)
            </summary>
            <pre className="mt-2 text-gray-300 whitespace-pre-wrap overflow-x-auto max-h-96 overflow-y-auto">
              {data.output}
            </pre>
          </details>
        </div>
      );

    case "agent-spawn":
      return (
        <div className="space-y-2">
          <div className="text-xs text-blue-400">🚀 Spawned Agent: {data.agentId}</div>
          {data.model && <div className="text-xs text-gray-500">Model: {data.model}</div>}
          {data.description && (
            <div className="text-xs text-gray-400">{data.description}</div>
          )}
          <details className="text-xs">
            <summary className="text-gray-400 cursor-pointer hover:text-gray-300">
              Prompt
            </summary>
            <pre className="mt-2 text-gray-300 whitespace-pre-wrap">{data.prompt}</pre>
          </details>
        </div>
      );

    case "summary":
      return (
        <div className="text-xs text-gray-300">
          📝 {data.summary}
        </div>
      );

    default:
      return <pre className="text-xs text-gray-500">{JSON.stringify(data, null, 2)}</pre>;
  }
}

function getEventTypeBadgeClass(type: string): string {
  const classes: Record<string, string> = {
    "user-message": "bg-cyan-900 text-cyan-200",
    "assistant-message": "bg-purple-900 text-purple-200",
    "thinking": "bg-purple-800 text-purple-200",
    "tool-use": "bg-blue-900 text-blue-200",
    "tool-result": "bg-green-900 text-green-200",
    "agent-spawn": "bg-yellow-900 text-yellow-200",
    "summary": "bg-gray-700 text-gray-200",
  };
  return classes[type] || "bg-gray-700 text-gray-200";
}

function getEventSummary(event: Event): string {
  const { data } = event;

  switch (data.type) {
    case "user-message":
      return data.text.substring(0, 80) + (data.text.length > 80 ? "..." : "");

    case "assistant-message":
      return data.text.substring(0, 80) + (data.text.length > 80 ? "..." : "");

    case "thinking":
      return "Thinking: " + data.content.substring(0, 60) + "...";

    case "tool-use":
      return `${data.toolName}${data.description ? `: ${data.description}` : ""}`;

    case "tool-result":
      return `Result: ${data.success ? "success" : "error"} (${data.output.length} chars)`;

    case "agent-spawn":
      return `Spawned: ${data.agentId}`;

    case "summary":
      return data.summary;

    default:
      return "Unknown event";
  }
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
