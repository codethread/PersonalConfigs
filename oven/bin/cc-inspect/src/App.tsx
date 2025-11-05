import "./index.css";
import { useState, useEffect } from "react";
import type { SessionData, Event, Session, DirectoriesResponse, SessionsResponse, SessionDataResponse } from "./types";
import { GraphTimeline } from "./components/GraphTimeline";
import { EventDetailsPanel } from "./components/EventDetailsPanel";

export function App() {
  const [sessionData, setSessionData] = useState<SessionData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedEvent, setSelectedEvent] = useState<Event | null>(null);

  // Selection state
  const [directories, setDirectories] = useState<string[]>([]);
  const [selectedDirectory, setSelectedDirectory] = useState<string>("");
  const [sessions, setSessions] = useState<Session[]>([]);
  const [selectedSession, setSelectedSession] = useState<string>("");
  const [loadingDirectories, setLoadingDirectories] = useState(false);
  const [loadingSessions, setLoadingSessions] = useState(false);

  // Try to load pre-loaded session on mount
  useEffect(() => {
    const loadInitialData = async () => {
      try {
        const res = await fetch("/api/session");
        const data: SessionDataResponse = await res.json();
        if (data.status === "success") {
          setSessionData(data.data);
          setLoading(false);
        } else {
          // No pre-loaded session, load directories
          setLoadingDirectories(true);
          setLoading(false);
          try {
            const dirRes = await fetch("/api/directories");
            const dirData: DirectoriesResponse = await dirRes.json();
            if (dirData.status === "success") {
              setDirectories(dirData.directories);
            } else {
              setError(dirData.error);
            }
          } catch (err) {
            const message = err instanceof Error ? err.message : String(err);
            setError(`Failed to load directories: ${message}`);
          } finally {
            setLoadingDirectories(false);
          }
        }
      } catch (err) {
        // No pre-loaded session, load directories
        setLoadingDirectories(true);
        setLoading(false);
        try {
          const dirRes = await fetch("/api/directories");
          const dirData: DirectoriesResponse = await dirRes.json();
          if (dirData.status === "success") {
            setDirectories(dirData.directories);
          } else {
            setError(dirData.error);
          }
        } catch (dirErr) {
          const message = dirErr instanceof Error ? dirErr.message : String(dirErr);
          setError(`Failed to load directories: ${message}`);
        } finally {
          setLoadingDirectories(false);
        }
      }
    };

    loadInitialData();
  }, []);

  // Load sessions when directory is selected
  useEffect(() => {
    if (!selectedDirectory) {
      setSessions([]);
      setSelectedSession("");
      return;
    }

    const loadSessions = async () => {
      setLoadingSessions(true);
      setSessions([]);
      setSelectedSession("");
      try {
        const res = await fetch(`/api/sessions?directory=${encodeURIComponent(selectedDirectory)}`);
        const data: SessionsResponse = await res.json();
        if (data.status === "success") {
          setSessions(data.sessions);
        } else {
          setError(data.error);
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        setError(`Failed to load sessions: ${message}`);
      } finally {
        setLoadingSessions(false);
      }
    };

    loadSessions();
  }, [selectedDirectory]);

  // Load session data when session is selected
  useEffect(() => {
    if (!selectedSession) {
      return;
    }

    const loadSessionData = async () => {
      setLoading(true);
      setError(null);
      try {
        const res = await fetch(`/api/session?path=${encodeURIComponent(selectedSession)}`);
        const data: SessionDataResponse = await res.json();
        if (data.status === "success") {
          setSessionData(data.data);
        } else {
          setError(data.error);
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        setError(`Failed to load session: ${message}`);
      } finally {
        setLoading(false);
      }
    };

    loadSessionData();
  }, [selectedSession]);

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

  // Show selection UI if no session loaded
  if (!sessionData) {
    return (
      <div className="min-h-screen bg-gray-950 text-gray-100">
        <header className="border-b border-gray-800 bg-gray-900">
          <div className="max-w-[1200px] mx-auto px-6 py-4">
            <h1 className="text-2xl font-bold">Claude Code Session Inspector</h1>
            <p className="text-sm text-gray-400 mt-1">Select a session to visualize</p>
          </div>
        </header>

        <div className="max-w-[1200px] mx-auto px-6 py-8">
          {error && (
            <div className="mb-6 p-4 bg-red-900/20 border border-red-900 rounded-lg text-red-400">
              {error}
            </div>
          )}

          {/* Directory selection */}
          <div className="mb-6">
            <label htmlFor="directory-select" className="block text-sm font-medium text-gray-300 mb-2">
              Select Project Directory
            </label>
            <select
              id="directory-select"
              value={selectedDirectory}
              onChange={(e) => setSelectedDirectory(e.target.value)}
              disabled={loadingDirectories}
              className="w-full px-4 py-2 bg-gray-800 border border-gray-700 rounded-lg text-gray-100 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
            >
              <option value="">
                {loadingDirectories ? "Loading directories..." : "-- Select a directory --"}
              </option>
              {directories.map((dir) => (
                <option key={dir} value={dir}>
                  {dir}
                </option>
              ))}
            </select>
          </div>

          {/* Session selection */}
          {selectedDirectory && (
            <div className="mb-6">
              <label htmlFor="session-select" className="block text-sm font-medium text-gray-300 mb-2">
                Select Session
              </label>
              <select
                id="session-select"
                value={selectedSession}
                onChange={(e) => setSelectedSession(e.target.value)}
                disabled={loadingSessions || sessions.length === 0}
                className="w-full px-4 py-2 bg-gray-800 border border-gray-700 rounded-lg text-gray-100 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
              >
                <option value="">
                  {loadingSessions ? "Loading sessions..." : sessions.length === 0 ? "No sessions found" : "-- Select a session --"}
                </option>
                {sessions.map((session) => (
                  <option key={session.sessionId} value={session.path}>
                    {session.sessionId} (modified: {new Date(session.modifiedAt).toLocaleString()})
                  </option>
                ))}
              </select>
            </div>
          )}

          {loading && selectedSession && (
            <div className="text-center py-8">
              <div className="text-gray-400">Loading session data...</div>
            </div>
          )}
        </div>
      </div>
    );
  }

  // Show session data
  return (
    <div className="min-h-screen bg-gray-950 text-gray-100">
      {/* Header */}
      <header className="border-b border-gray-800 bg-gray-900">
        <div className="max-w-[1800px] mx-auto px-6 py-4">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-2xl font-bold">Claude Code Session Inspector</h1>
              <p className="text-sm text-gray-400 mt-1">
                Session: {sessionData.sessionId} • {sessionData.allEvents.length} events •{" "}
                {sessionData.mainAgent.children.length + 1} agents
              </p>
            </div>
            <button
              type="button"
              onClick={() => {
                setSessionData(null);
                setSelectedDirectory("");
                setSelectedSession("");
                setError(null);
              }}
              className="px-4 py-2 bg-gray-800 hover:bg-gray-700 border border-gray-700 rounded-lg text-sm text-gray-300 transition-colors"
            >
              Change Session
            </button>
          </div>
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
