import "./index.css";
import { useState, useEffect } from "react";
import type { SessionData, Event } from "./types";
import { GraphTimeline } from "./components/GraphTimeline";
import { EventDetailsPanel } from "./components/EventDetailsPanel";

export function App() {
  const [sessionData, setSessionData] = useState<SessionData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedEvent, setSelectedEvent] = useState<Event | null>(null);

  useEffect(() => {
    fetch("/api/session")
      .then((res) => res.json())
      .then((data) => {
        setSessionData(data);
        setLoading(false);
      })
      .catch((err) => {
        setError(err.message);
        setLoading(false);
      });
  }, []);

  // Handle keyboard shortcuts
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape" && selectedEvent) {
        setSelectedEvent(null);
      }
    };

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [selectedEvent]);

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-950 flex items-center justify-center">
        <div className="text-gray-400">Loading session data...</div>
      </div>
    );
  }

  if (error || !sessionData) {
    return (
      <div className="min-h-screen bg-gray-950 flex items-center justify-center">
        <div className="text-red-400">Error: {error || "Failed to load session data"}</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-950 text-gray-100">
      {/* Header */}
      <header className="border-b border-gray-800 bg-gray-900">
        <div className="max-w-[1800px] mx-auto px-6 py-4">
          <h1 className="text-2xl font-bold">Claude Code Session Inspector</h1>
          <p className="text-sm text-gray-400 mt-1">
            Session: {sessionData.sessionId} • {sessionData.allEvents.length} events •{" "}
            {sessionData.mainAgent.children.length + 1} agents
          </p>
        </div>
      </header>

      {/* Main content - Timeline */}
      <div className="max-w-[1800px] mx-auto px-6 py-6">
        <GraphTimeline sessionData={sessionData} onSelectEvent={setSelectedEvent} selectedEvent={selectedEvent} />
      </div>

      {/* Side panel for event details */}
      {selectedEvent && (
        <EventDetailsPanel
          event={selectedEvent}
          agents={[sessionData.mainAgent, ...sessionData.mainAgent.children]}
          onClose={() => setSelectedEvent(null)}
        />
      )}
    </div>
  );
}

export default App;
