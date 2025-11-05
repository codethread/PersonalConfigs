import type { SessionData, Event, AgentNode } from "../types";
import { useMemo } from "react";

interface GraphTimelineProps {
  sessionData: SessionData;
  onSelectEvent: (event: Event | null) => void;
  selectedEvent: Event | null;
}

interface LaneAssignment {
  agentId: string;
  lane: number;
  color: string;
}

interface EventWithLane extends Event {
  lane: number;
  color: string;
}

interface Connection {
  fromIndex: number;
  toIndex: number;
  fromLane: number;
  toLane: number;
  type: "spawn" | "merge" | "resume";
  color: string;
}

const LANE_COLORS: readonly string[] = [
  "rgb(34, 197, 94)", // green-500 for main
  "rgb(59, 130, 246)", // blue-500
  "rgb(168, 85, 247)", // purple-500
  "rgb(236, 72, 153)", // pink-500
  "rgb(251, 146, 60)", // orange-500
  "rgb(14, 165, 233)", // sky-500
  "rgb(245, 158, 11)", // amber-500
  "rgb(139, 92, 246)", // violet-500
] as const;

export function GraphTimeline({ sessionData, onSelectEvent, selectedEvent }: GraphTimelineProps) {

  // Calculate lane assignments for all agents
  const laneAssignments = useMemo(() => {
    const assignments: LaneAssignment[] = [];
    const allAgents = [sessionData.mainAgent, ...sessionData.mainAgent.children];

    allAgents.forEach((agent, index) => {
      assignments.push({
        agentId: agent.id,
        lane: index,
        color: LANE_COLORS[index % LANE_COLORS.length] as string,
      });
    });

    return assignments;
  }, [sessionData]);

  // Enhance events with lane information
  const eventsWithLanes = useMemo(() => {
    return sessionData.allEvents.map((event): EventWithLane => {
      const assignment = laneAssignments.find((a) => a.agentId === event.agentId);
      return {
        ...event,
        lane: assignment?.lane ?? 0,
        color: (assignment?.color ?? LANE_COLORS[0]) as string,
      };
    });
  }, [sessionData.allEvents, laneAssignments]);

  // Calculate connections between events (spawn/merge/resume lines)
  const connections = useMemo(() => {
    const conns: Connection[] = [];

    // Find Task tool uses and their results
    for (let index = 0; index < eventsWithLanes.length; index++) {
      const event = eventsWithLanes[index];
      if (!event || event.data.type !== "tool-use" || event.data.toolName !== "Task") continue;

      const toolData = event.data;
      const isResume = toolData.isResume;
      const resumesAgentId = toolData.resumesAgentId;

      // Find the corresponding tool-result
      const toolUseId = event.data.toolId;
      const resultIndex = eventsWithLanes.findIndex(
        (e, i) => i > index && e.data.type === "tool-result" && e.data.toolUseId === toolUseId
      );

      if (resultIndex !== -1) {
        const resultEvent = eventsWithLanes[resultIndex];
        if (!resultEvent) continue;
        const spawnedAgentId = resultEvent.data.type === "tool-result" ? resultEvent.data.agentId : undefined;

        if (spawnedAgentId) {
          const spawnedLane = laneAssignments.find((a) => a.agentId === spawnedAgentId);

          if (spawnedLane) {
            // Find first event from spawned agent (after tool-use)
            const firstAgentEventIndex = eventsWithLanes.findIndex(
              (e, i) => i > index && e.agentId === spawnedAgentId
            );

            if (firstAgentEventIndex !== -1) {
              // If this is a resume, draw a dotted line from the last event of the resumed agent
              if (isResume && resumesAgentId === spawnedAgentId) {
                // Find the last event from this agent before the resume
                let lastEventBeforeResume = -1;
                for (let i = index - 1; i >= 0; i--) {
                  const evt = eventsWithLanes[i];
                  if (evt && evt.agentId === spawnedAgentId) {
                    lastEventBeforeResume = i;
                    break;
                  }
                }

                if (lastEventBeforeResume !== -1) {
                  // Resume line: from last event before resume to resume tool-use
                  conns.push({
                    fromIndex: lastEventBeforeResume,
                    toIndex: index,
                    fromLane: spawnedLane.lane,
                    toLane: event.lane,
                    type: "resume",
                    color: spawnedLane.color,
                  });
                }
              } else {
                // Spawn line: from tool-use to first agent event
                conns.push({
                  fromIndex: index,
                  toIndex: firstAgentEventIndex,
                  fromLane: event.lane,
                  toLane: spawnedLane.lane,
                  type: "spawn",
                  color: spawnedLane.color,
                });
              }

              // Merge line: from last agent event before result to result
              // Find all agent events between first and result
              let lastAgentEventIndex = firstAgentEventIndex;
              for (let i = firstAgentEventIndex + 1; i < resultIndex; i++) {
                const evt = eventsWithLanes[i];
                if (evt && evt.agentId === spawnedAgentId) {
                  lastAgentEventIndex = i;
                }
              }

              conns.push({
                fromIndex: lastAgentEventIndex,
                toIndex: resultIndex,
                fromLane: spawnedLane.lane,
                toLane: event.lane,
                type: "merge",
                color: spawnedLane.color,
              });
            }
          }
        }
      }
    }

    return conns;
  }, [eventsWithLanes, laneAssignments]);

  const laneWidth = 24; // Reduced from 40 to handle many agents
  const totalLaneWidth = laneAssignments.length * laneWidth;
  const maxVisibleLanes = Math.min(laneAssignments.length, 20); // Limit visible lanes
  const displayWidth = maxVisibleLanes * laneWidth + 40;

  return (
    <div className="bg-gray-900 border border-gray-800 rounded-lg">
      {/* Lane header - sticky, sticks to top when scrolling */}
      <div className="sticky top-0 z-50 bg-gray-900 border-b border-gray-800 p-4 shadow-lg">
        <div className="flex overflow-x-auto">
          <div className="flex" style={{ width: totalLaneWidth }}>
            {laneAssignments.map((assignment) => {
              const agent =
                assignment.agentId === sessionData.mainAgent.id
                  ? sessionData.mainAgent
                  : sessionData.mainAgent.children.find((a) => a.id === assignment.agentId);

              return (
                <div
                  key={assignment.agentId}
                  className="text-xs text-gray-400 flex-shrink-0"
                  style={{ width: laneWidth }}
                  title={agent?.name || assignment.agentId}
                >
                  <div
                    className="w-2 h-2 rounded-full mx-auto mb-1"
                    style={{ backgroundColor: assignment.color }}
                  />
                  <div className="truncate text-center text-[10px]">{assignment.lane}</div>
                </div>
              );
            })}
          </div>
        </div>
      </div>

      {/* Timeline events with details */}
      <div className="relative">
        {/* Vertical lane lines */}
        <svg
          className="absolute inset-0 pointer-events-none"
          style={{ width: totalLaneWidth, height: eventsWithLanes.length * 60 }}
        >
          {laneAssignments.map((assignment) => (
            <line
              key={assignment.agentId}
              x1={assignment.lane * laneWidth + laneWidth / 2}
              y1={0}
              x2={assignment.lane * laneWidth + laneWidth / 2}
              y2={eventsWithLanes.length * 60}
              stroke="rgb(55, 65, 81)"
              strokeWidth="1"
              opacity="0.3"
            />
          ))}
        </svg>

        {eventsWithLanes.map((event, index) => (
          <EventRow
            key={event.id}
            event={event}
            index={index}
            laneWidth={laneWidth}
            totalLanes={laneAssignments.length}
            connections={connections.filter((c) => c.fromIndex === index || c.toIndex === index)}
            allEvents={eventsWithLanes}
            isSelected={selectedEvent?.id === event.id}
            onSelect={() => onSelectEvent(event)}
            agents={[sessionData.mainAgent, ...sessionData.mainAgent.children]}
          />
        ))}
      </div>
    </div>
  );
}

interface EventRowProps {
  event: EventWithLane;
  index: number;
  laneWidth: number;
  totalLanes: number;
  connections: Connection[];
  allEvents: EventWithLane[];
  isSelected: boolean;
  onSelect: () => void;
  agents: AgentNode[];
}

function EventRow({
  event,
  index,
  laneWidth,
  totalLanes,
  connections,
  allEvents,
  isSelected,
  onSelect,
  agents,
}: EventRowProps) {
  const rowHeight = 60;
  const agent = agents.find((a) => a.id === event.agentId);
  const agentName = agent?.name || event.agentId || "main";

  return (
    <button
      type="button"
      onClick={onSelect}
      className={`relative border-b border-gray-800 transition-colors w-full text-left ${
        isSelected ? "bg-gray-800" : "hover:bg-gray-850"
      }`}
      style={{ minHeight: rowHeight }}
      title={getEventSummary(event)}
    >
      <div className="flex items-start">
        {/* Left: Graph visualization */}
        <div className="relative flex-shrink-0" style={{ width: totalLanes * laneWidth, height: rowHeight }}>
          {/* SVG for connections */}
          <svg className="absolute inset-0 pointer-events-none" style={{ width: totalLanes * laneWidth }}>
            {connections.map((conn, i) => {
              const isFrom = conn.fromIndex === index;

              if (isFrom) {
                const rowsToNext = conn.toIndex - index;
                const y1 = rowHeight / 2;
                const y2 = rowsToNext * rowHeight + rowHeight / 2;
                const x1 = conn.fromLane * laneWidth + laneWidth / 2;
                const x2 = conn.toLane * laneWidth + laneWidth / 2;

                const dashArray = conn.type === "merge" ? "4 4" : conn.type === "resume" ? "2 2" : undefined;

                return (
                  <path
                    key={`${conn.fromIndex}-${conn.toIndex}-${i}`}
                    d={`M ${x1} ${y1} C ${x1} ${y1 + 20}, ${x2} ${y2 - 20}, ${x2} ${y2}`}
                    stroke={conn.color}
                    strokeWidth="2"
                    fill="none"
                    opacity="0.6"
                    strokeDasharray={dashArray}
                  />
                );
              }

              return null;
            })}
          </svg>

          {/* Event dot */}
          <div
            className={`absolute top-1/2 -translate-y-1/2 w-3 h-3 rounded-full border-2 transition-all ${
              isSelected ? "ring-2 ring-white ring-offset-2 ring-offset-gray-900" : ""
            }`}
            style={{
              left: event.lane * laneWidth + laneWidth / 2 - 6,
              backgroundColor: event.color,
              borderColor: "rgb(17, 24, 39)", // gray-900
            }}
          />
        </div>

        {/* Right: Event details */}
        <div className="flex-1 p-4 min-w-0">
          {/* Event header */}
          <div className="flex items-center gap-3 mb-2">
            <div className="text-xs text-gray-500 font-mono">{formatTime(event.timestamp)}</div>
            <div className="text-xs text-gray-400 min-w-[120px] truncate">{agentName}</div>
            <div className={`text-xs px-2 py-0.5 rounded ${getEventTypeBadgeClass(event.type)}`}>{event.type}</div>
          </div>

          {/* Event content */}
          <div className="text-sm text-gray-300">{renderEventContent(event)}</div>
        </div>
      </div>
    </button>
  );
}

function renderEventContent(event: Event) {
  const { data } = event;

  switch (data.type) {
    case "user-message":
      return <div className="font-mono text-xs whitespace-pre-wrap">{data.text}</div>;

    case "assistant-message":
      return <div className="whitespace-pre-wrap line-clamp-3">{data.text}</div>;

    case "thinking":
      return (
        <div className="text-gray-400 italic line-clamp-2">
          <span className="text-purple-400">Thinking:</span> {data.content}
        </div>
      );

    case "tool-use":
      return (
        <div>
          <span className="text-blue-400 font-semibold">{data.toolName}</span>
          {data.description && <span className="text-gray-500 ml-2">• {data.description}</span>}
        </div>
      );

    case "tool-result":
      return (
        <div>
          <span className={data.success ? "text-green-400" : "text-red-400"}>
            {data.success ? "✓" : "✗"} Result
          </span>
          <span className="text-gray-500 ml-2">({data.output.length} chars)</span>
          {data.agentId && <span className="text-blue-400 ml-2">from agent</span>}
        </div>
      );

    case "summary":
      return <div className="text-gray-400">{data.summary}</div>;

    default:
      return <div className="text-gray-500">Unknown event type</div>;
  }
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
