import type { AgentNode } from "../types";

interface AgentTreeProps {
  agent: AgentNode;
  onSelectAgent: (agentId: string | null) => void;
  selectedAgent: string | null;
  level?: number;
}

export function AgentTree({ agent, onSelectAgent, selectedAgent, level = 0 }: AgentTreeProps) {
  const isSelected = selectedAgent === agent.id;
  const isMain = level === 0;

  return (
    <div className={level > 0 ? "ml-4 mt-2" : ""}>
      <button
        type="button"
        onClick={() => onSelectAgent(isSelected ? null : agent.id)}
        className={`
          w-full text-left px-3 py-2 rounded text-sm transition-colors
          ${isSelected ? "bg-blue-900 text-blue-100" : "hover:bg-gray-800 text-gray-300"}
          ${isMain ? "font-semibold" : ""}
        `}
      >
        <div className="flex items-center gap-2">
          <div className={`w-2 h-2 rounded-full ${isMain ? "bg-green-500" : "bg-blue-500"}`} />
          <div className="flex-1 min-w-0">
            <div className="truncate">{agent.name || agent.id}</div>
            {agent.model && <div className="text-xs text-gray-500">{agent.model}</div>}
          </div>
          <div className="text-xs text-gray-500">{agent.events.length}</div>
        </div>
        {agent.description && (
          <div className="text-xs text-gray-500 mt-1 truncate" title={agent.description}>
            {agent.description}
          </div>
        )}
      </button>

      {agent.children.length > 0 && (
        <div className="border-l border-gray-700 ml-2">
          {agent.children.map((child) => (
            <AgentTree
              key={child.id}
              agent={child}
              onSelectAgent={onSelectAgent}
              selectedAgent={selectedAgent}
              level={level + 1}
            />
          ))}
        </div>
      )}
    </div>
  );
}
