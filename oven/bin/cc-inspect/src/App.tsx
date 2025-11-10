import "./index.css";
import { useState, useEffect } from "react";
import type { SessionData, Event, Session, DirectoriesResponse, SessionsResponse, SessionDataResponse } from "./types";
import { GraphTimeline } from "./components/GraphTimeline";
import { EventDetailsPanel } from "./components/EventDetailsPanel";

// URL parameter management
function getUrlParams() {
  const params = new URLSearchParams(window.location.search);
  return {
    directory: params.get("directory") || "",
    sessionPath: params.get("session") || "",
  };
}

function updateUrlParams(directory: string, sessionPath: string) {
  const params = new URLSearchParams();
  if (directory) params.set("directory", directory);
  if (sessionPath) params.set("session", sessionPath);

  const newUrl = `${window.location.pathname}${params.toString() ? `?${params.toString()}` : ""}`;
  window.history.replaceState({}, "", newUrl);
}

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

  // Initialize from URL parameters and load initial data
  useEffect(() => {
    const loadInitialData = async () => {
      const urlParams = getUrlParams();

      // Load directories first
      setLoadingDirectories(true);
      try {
        const dirRes = await fetch("/api/directories");
        const dirData: DirectoriesResponse = await dirRes.json();
        if (dirData.status === "success") {
          setDirectories(dirData.directories);

          // If we have URL params, validate and restore them
          if (urlParams.directory && dirData.directories.includes(urlParams.directory)) {
            setSelectedDirectory(urlParams.directory);
          }
        } else {
          setError(dirData.error);
        }
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        setError(`Failed to load directories: ${message}`);
      } finally {
        setLoadingDirectories(false);
      }

      // Try to load CLI-provided session or URL session
      try {
        const res = await fetch("/api/session");
        const data: SessionDataResponse = await res.json();
        if (data.status === "success") {
          setSessionData(data.data);
          setLoading(false);
          return;
        }
      } catch {
        // No CLI session, continue with URL params
      }

      setLoading(false);

      // If we have session path from URL, try to load sessions for that directory
      if (urlParams.sessionPath && urlParams.directory) {
        setSelectedSession(urlParams.sessionPath);
      }
    };

    loadInitialData();
  }, []);

  // Update URL when directory or session changes
  useEffect(() => {
    updateUrlParams(selectedDirectory, selectedSession);
  }, [selectedDirectory, selectedSession]);

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
      try {
        const res = await fetch(`/api/sessions?directory=${encodeURIComponent(selectedDirectory)}`);
        const data: SessionsResponse = await res.json();
        if (data.status === "success") {
          setSessions(data.sessions);

          // Check if URL session param is valid for this directory
          const urlParams = getUrlParams();
          const validSession = data.sessions.find(s => s.path === urlParams.sessionPath);
          if (validSession && !selectedSession) {
            setSelectedSession(validSession.path);
          } else if (!validSession && selectedSession) {
            // URL session not valid for this directory, clear it
            setSelectedSession("");
          }
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

  return (
    <div className="min-h-screen bg-gray-950 text-gray-100">
      {/* Header with persistent selectors */}
      <header className="border-b border-gray-800 bg-gray-900">
        <div className="max-w-[1800px] mx-auto px-6 py-4">
          <div className="flex items-start justify-between gap-6">
            <div className="flex-shrink-0">
              <h1 className="text-2xl font-bold">Claude Code Session Inspector</h1>
              {sessionData && (
                <p className="text-sm text-gray-400 mt-1">
                  {sessionData.allEvents.length} events • {sessionData.mainAgent.children.length + 1} agents
                </p>
              )}
            </div>

            {/* Selector controls */}
            <div className="flex-1 flex gap-4 items-start max-w-3xl">
              {/* Directory selection */}
              <div className="flex-1">
                <label htmlFor="directory-select" className="block text-xs font-medium text-gray-400 mb-1">
                  Project Directory
                </label>
                <select
                  id="directory-select"
                  value={selectedDirectory}
                  onChange={(e) => {
                    setSelectedDirectory(e.target.value);
                    setSessionData(null);
                  }}
                  disabled={loadingDirectories}
                  className="w-full px-3 py-1.5 text-sm bg-gray-800 border border-gray-700 rounded text-gray-100 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
                >
                  <option value="">
                    {loadingDirectories ? "Loading..." : "-- Select directory --"}
                  </option>
                  {directories.map((dir) => (
                    <option key={dir} value={dir}>
                      {dir}
                    </option>
                  ))}
                </select>
              </div>

              {/* Session selection */}
              <div className="flex-1">
                <label htmlFor="session-select" className="block text-xs font-medium text-gray-400 mb-1">
                  Session
                </label>
                <select
                  id="session-select"
                  value={selectedSession}
                  onChange={(e) => {
                    setSelectedSession(e.target.value);
                    setSessionData(null);
                  }}
                  disabled={!selectedDirectory || loadingSessions || sessions.length === 0}
                  className="w-full px-3 py-1.5 text-sm bg-gray-800 border border-gray-700 rounded text-gray-100 focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:opacity-50"
                >
                  <option value="">
                    {loadingSessions
                      ? "Loading..."
                      : sessions.length === 0
                        ? "No sessions"
                        : "-- Select session --"}
                  </option>
                  {sessions.map((session) => (
                    <option key={session.sessionId} value={session.path}>
                      {session.sessionId} ({new Date(session.modifiedAt).toLocaleString()})
                    </option>
                  ))}
                </select>
              </div>
            </div>
          </div>
        </div>
      </header>

      {/* Main content */}
      <div className="max-w-[1800px] mx-auto px-6 py-6">
        {error && (
          <div className="mb-6 p-4 bg-red-900/20 border border-red-900 rounded-lg text-red-400">
            {error}
          </div>
        )}

        {loading && selectedSession && !sessionData && (
          <div className="text-center py-12">
            <div className="text-gray-400">Loading session data...</div>
          </div>
        )}

        {!sessionData && !loading && (
          <div className="text-center py-12">
            <div className="text-gray-500">Select a project directory and session to view timeline</div>
          </div>
        )}

        {sessionData && (
          <GraphTimeline sessionData={sessionData} onSelectEvent={setSelectedEvent} selectedEvent={selectedEvent} />
        )}
      </div>

      {/* Side panel for event details */}
      {selectedEvent && sessionData && (
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
