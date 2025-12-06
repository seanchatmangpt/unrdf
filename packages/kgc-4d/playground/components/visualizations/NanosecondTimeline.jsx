'use client';

/**
 * NanosecondTimeline - High-precision BigInt timeline with zoom from decades to nanoseconds
 *
 * Uses d3-scale for domain mapping (BigInt → screen pixels) and canvas rendering
 * for high-density event visualization (1000s of events).
 *
 * Features:
 * - Zoom from decades → years → days → seconds → nanoseconds
 * - Canvas rendering for performance
 * - Click to select event
 * - Highlight active time range
 */

import { useRef, useEffect, useState, useCallback } from 'react';
import { scaleLinear } from 'd3-scale';
import { ZoomIn, ZoomOut, RefreshCw } from 'lucide-react';

export function NanosecondTimeline({ events = [], selectedTime, onSelect, highlightRange }) {
  const canvasRef = useRef(null);
  const containerRef = useRef(null);
  const [dimensions, setDimensions] = useState({ width: 800, height: 80 });
  const [zoomLevel, setZoomLevel] = useState(1); // 1 = full range, 10 = 10x zoom

  // Update dimensions on resize
  useEffect(() => {
    if (!containerRef.current) return;

    const updateDimensions = () => {
      const rect = containerRef.current.getBoundingClientRect();
      setDimensions({ width: rect.width, height: 80 });
    };

    updateDimensions();
    window.addEventListener('resize', updateDimensions);
    return () => window.removeEventListener('resize', updateDimensions);
  }, []);

  // Render timeline on canvas
  useEffect(() => {
    if (!canvasRef.current || events.length === 0) return;

    const canvas = canvasRef.current;
    const ctx = canvas.getContext('2d');
    const { width, height } = dimensions;

    // Set canvas resolution
    canvas.width = width * 2; // Retina
    canvas.height = height * 2;
    canvas.style.width = `${width}px`;
    canvas.style.height = `${height}px`;
    ctx.scale(2, 2);

    // Clear canvas
    ctx.clearRect(0, 0, width, height);

    // Find time range
    const times = events.map((e) => BigInt(e.t_ns));
    const minTime = times.reduce((a, b) => (a < b ? a : b));
    const maxTime = times.reduce((a, b) => (a > b ? a : b));

    // Apply zoom (center on selected time or middle)
    const range = maxTime - minTime;
    const zoomedRange = range / BigInt(Math.floor(zoomLevel));
    const center = selectedTime ? BigInt(selectedTime) : minTime + range / 2n;
    const zoomedMin = center - zoomedRange / 2n;
    const zoomedMax = center + zoomedRange / 2n;

    // Create scale: BigInt domain → screen pixels
    const scale = scaleLinear()
      .domain([Number(zoomedMin), Number(zoomedMax)])
      .range([20, width - 20]);

    // Draw axis
    ctx.strokeStyle = '#475569'; // slate-600
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(20, height / 2);
    ctx.lineTo(width - 20, height / 2);
    ctx.stroke();

    // Draw highlight range (if provided)
    if (highlightRange && highlightRange.start && highlightRange.end) {
      const startX = scale(Number(BigInt(highlightRange.start)));
      const endX = scale(Number(BigInt(highlightRange.end)));

      ctx.fillStyle = 'rgba(99, 102, 241, 0.2)'; // indigo-500 with alpha
      ctx.fillRect(startX, 10, endX - startX, height - 20);
    }

    // Draw events
    events.forEach((event) => {
      const time = BigInt(event.t_ns);
      if (time < zoomedMin || time > zoomedMax) return; // Outside visible range

      const x = scale(Number(time));
      const y = height / 2;

      // Color by event type
      let color = '#94a3b8'; // slate-400 (default)
      if (event.type === 'SNAPSHOT') color = '#22c55e'; // green-500
      else if (event.type === 'CREATE') color = '#3b82f6'; // blue-500
      else if (event.type === 'UPDATE') color = '#f59e0b'; // amber-500
      else if (event.type === 'DELETE') color = '#ef4444'; // red-500

      // Highlight selected event
      if (selectedTime && BigInt(selectedTime) === time) {
        ctx.fillStyle = '#818cf8'; // indigo-400
        ctx.fillRect(x - 3, 5, 6, height - 10);
      }

      // Draw event marker
      ctx.fillStyle = color;
      ctx.beginPath();
      ctx.arc(x, y, 3, 0, 2 * Math.PI);
      ctx.fill();
    });

    // Draw time labels
    ctx.fillStyle = '#cbd5e1'; // slate-300
    ctx.font = '11px monospace';
    ctx.textAlign = 'left';
    ctx.fillText(formatTime(zoomedMin), 20, height - 10);
    ctx.textAlign = 'right';
    ctx.fillText(formatTime(zoomedMax), width - 20, height - 10);
    ctx.textAlign = 'center';
    ctx.fillText(formatTime(center), width / 2, height - 10);
  }, [events, dimensions, selectedTime, highlightRange, zoomLevel]);

  // Handle canvas click
  const handleClick = useCallback(
    (e) => {
      if (!canvasRef.current || events.length === 0) return;

      const rect = canvasRef.current.getBoundingClientRect();
      const x = e.clientX - rect.left;

      // Find time range
      const times = events.map((e) => BigInt(e.t_ns));
      const minTime = times.reduce((a, b) => (a < b ? a : b));
      const maxTime = times.reduce((a, b) => (a > b ? a : b));
      const range = maxTime - minTime;
      const zoomedRange = range / BigInt(Math.floor(zoomLevel));
      const center = selectedTime ? BigInt(selectedTime) : minTime + range / 2n;
      const zoomedMin = center - zoomedRange / 2n;
      const zoomedMax = center + zoomedRange / 2n;

      const scale = scaleLinear()
        .domain([Number(zoomedMin), Number(zoomedMax)])
        .range([20, dimensions.width - 20]);

      const clickedTime = scale.invert(x);

      // Find nearest event
      let nearest = null;
      let minDist = Infinity;
      events.forEach((event) => {
        const time = Number(BigInt(event.t_ns));
        const dist = Math.abs(time - clickedTime);
        if (dist < minDist) {
          minDist = dist;
          nearest = event;
        }
      });

      if (nearest && onSelect) {
        onSelect(nearest.t_ns, nearest);
      }
    },
    [events, dimensions, selectedTime, zoomLevel, onSelect]
  );

  return (
    <div className="space-y-2">
      {/* Controls */}
      <div className="flex items-center justify-between">
        <div className="text-sm font-medium text-slate-300">
          Nanosecond Timeline ({events.length} events)
        </div>
        <div className="flex items-center gap-2">
          <button
            onClick={() => setZoomLevel(Math.min(zoomLevel * 2, 100))}
            className="p-1.5 rounded bg-slate-800 hover:bg-slate-700 text-slate-400"
            title="Zoom In"
          >
            <ZoomIn className="w-4 h-4" />
          </button>
          <button
            onClick={() => setZoomLevel(Math.max(zoomLevel / 2, 1))}
            className="p-1.5 rounded bg-slate-800 hover:bg-slate-700 text-slate-400"
            title="Zoom Out"
          >
            <ZoomOut className="w-4 h-4" />
          </button>
          <button
            onClick={() => setZoomLevel(1)}
            className="p-1.5 rounded bg-slate-800 hover:bg-slate-700 text-slate-400"
            title="Reset Zoom"
          >
            <RefreshCw className="w-4 h-4" />
          </button>
          <div className="text-xs text-slate-500 font-mono">{zoomLevel.toFixed(1)}x</div>
        </div>
      </div>

      {/* Canvas Timeline */}
      <div ref={containerRef} className="bg-slate-900 border border-slate-700 rounded-lg overflow-hidden">
        <canvas
          ref={canvasRef}
          onClick={handleClick}
          className="cursor-pointer"
          style={{ display: 'block' }}
        />
      </div>

      {/* Legend */}
      <div className="flex flex-wrap gap-3 text-xs text-slate-400">
        <div className="flex items-center gap-1">
          <div className="w-2 h-2 rounded-full bg-green-500" />
          <span>Snapshot</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-2 h-2 rounded-full bg-blue-500" />
          <span>Create</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-2 h-2 rounded-full bg-amber-500" />
          <span>Update</span>
        </div>
        <div className="flex items-center gap-1">
          <div className="w-2 h-2 rounded-full bg-red-500" />
          <span>Delete</span>
        </div>
      </div>
    </div>
  );
}

/**
 * Format BigInt timestamp for display
 */
function formatTime(time) {
  if (!time) return '—';
  const t = typeof time === 'bigint' ? time : BigInt(time);
  const ms = Number(t / 1000000n);
  const date = new Date(ms);
  return date.toISOString().slice(0, 23); // Include milliseconds
}
