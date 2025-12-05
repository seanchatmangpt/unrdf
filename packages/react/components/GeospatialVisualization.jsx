'use client';

import { useMemo } from 'react';
import { DeckGL } from '@deck.gl/react';
import { ScatterplotLayer, ArcLayer } from '@deck.gl/layers';

export default function GeospatialVisualization({ data }) {
  const { jtbd = [] } = data || {};

  // Generate synthetic geographic coordinates for JTBD scenarios
  const operatorLocations = useMemo(() => {
    const baseLatitude = 40.7128; // NYC
    const baseLongitude = -74.006;

    return [
      {
        id: 'μ₁',
        name: 'Validation',
        lat: baseLatitude - 0.02,
        lon: baseLongitude,
        entropy: 50,
        color: [255, 165, 0],
      },
      {
        id: 'μ₂',
        name: 'Temporal',
        lat: baseLatitude - 0.015,
        lon: baseLongitude + 0.02,
        entropy: 45.8,
        color: [255, 140, 0],
      },
      {
        id: 'μ₃',
        name: 'Geographic',
        lat: baseLatitude - 0.01,
        lon: baseLongitude + 0.04,
        entropy: 40.0,
        color: [255, 100, 0],
      },
      {
        id: 'μ₄',
        name: 'Batch',
        lat: baseLatitude - 0.005,
        lon: baseLongitude + 0.06,
        entropy: 32.9,
        color: [255, 69, 0],
      },
      {
        id: 'μ₅',
        name: 'Threshold',
        lat: baseLatitude,
        lon: baseLongitude + 0.08,
        entropy: 26.6,
        color: [220, 20, 60],
      },
      {
        id: 'μ₆',
        name: 'Cascading',
        lat: baseLatitude + 0.005,
        lon: baseLongitude + 0.1,
        entropy: 20.4,
        color: [178, 34, 52],
      },
      {
        id: 'μ₇',
        name: 'Complex',
        lat: baseLatitude + 0.01,
        lon: baseLongitude + 0.12,
        entropy: 14.2,
        color: [139, 0, 0],
      },
      {
        id: 'μ₈',
        name: 'Deterministic',
        lat: baseLatitude + 0.015,
        lon: baseLongitude + 0.14,
        entropy: 8.0,
        color: [75, 0, 130],
      },
    ];
  }, []);

  // Flow between operators showing information cascade
  const operatorFlows = useMemo(() => {
    const flows = [];
    for (let i = 0; i < operatorLocations.length - 1; i++) {
      const from = operatorLocations[i];
      const to = operatorLocations[i + 1];
      flows.push({
        sourcePosition: [from.lon, from.lat],
        targetPosition: [to.lon, to.lat],
        entropy: from.entropy - to.entropy,
      });
    }
    return flows;
  }, [operatorLocations]);

  // JTBD scenario locations (synthetic)
  const jtbdLocations = useMemo(() => {
    const baseLat = 40.7128;
    const baseLon = -74.006;
    return jtbd.map((job, idx) => ({
      position: [baseLon - 0.25 + idx * 0.08, baseLat + 0.02 + Math.random() * 0.04],
      latency: job.latency,
      reduction: job.reduction,
      name: job.name,
      color: [100 + idx * 20, 150, 200],
      size: Math.min(job.reduction * 100, 50000),
    }));
  }, [jtbd]);

  const layers = [
    // Arc layer for operator information flow
    new ArcLayer({
      id: 'operator-flow',
      data: operatorFlows,
      getSourcePosition: (d) => d.sourcePosition,
      getTargetPosition: (d) => d.targetPosition,
      getSourceColor: (d) => [100, 150, 255, 200],
      getTargetColor: (d) => [100, 100, 255, 100],
      getWidth: (d) => d.entropy * 2,
      widthScale: 20,
      widthMinPixels: 2,
      interactive: true,
      pickable: true,
    }),

    // Scatterplot for operators
    new ScatterplotLayer({
      id: 'operators',
      data: operatorLocations,
      getPosition: (d) => [d.lon, d.lat],
      getRadius: (d) => Math.sqrt(d.entropy) * 1000,
      getColor: (d) => d.color,
      radiusScale: 6,
      radiusMinPixels: 15,
      radiusMaxPixels: 40,
      interactive: true,
      pickable: true,
      autoHighlight: true,
      onHover: (info) => {
        if (info.object) {
          document.body.style.cursor = 'pointer';
        } else {
          document.body.style.cursor = 'default';
        }
      },
    }),

    // Scatterplot for JTBD scenarios
    new ScatterplotLayer({
      id: 'jtbd-scenarios',
      data: jtbdLocations,
      getPosition: (d) => d.position,
      getRadius: (d) => d.size,
      getColor: (d) => d.color,
      radiusScale: 10,
      radiusMinPixels: 8,
      radiusMaxPixels: 30,
      interactive: true,
      pickable: true,
      autoHighlight: true,
    }),
  ];

  const initialViewState = {
    longitude: -74.006,
    latitude: 40.7128,
    zoom: 10,
    pitch: 45,
    bearing: 0,
  };

  const handleHover = (info) => {
    const element = info.element;
    if (element) {
      element.style.cursor = 'pointer';
    }
  };

  return (
    <div className="card space-y-4 h-96">
      <div className="space-y-2">
        <h3 className="text-lg font-semibold text-cyan-400">Geographic Operator Distribution</h3>
        <p className="text-sm text-slate-400">
          Information cascade across 8 operators with JTBD scenario density. Arc width represents
          entropy reduction per operator.
        </p>
      </div>

      <div className="relative h-96 rounded border border-slate-700 overflow-hidden bg-slate-950">
        <DeckGL
          layers={layers}
          initialViewState={initialViewState}
          controller={true}
          onViewStateChange={({ viewState }) => {}}
        />

        {/* Tooltip overlay */}
        <div className="absolute bottom-4 left-4 bg-slate-900/80 border border-slate-700 rounded p-3 text-xs text-slate-300 backdrop-blur">
          <div className="font-semibold text-cyan-400 mb-2">Map Controls</div>
          <div>🔄 Drag to rotate | 🔍 Scroll to zoom | Right-click drag to tilt</div>
          <div className="mt-2 space-y-1">
            <div>🟠 Orange: Operators μ₁-μ₄ (high entropy)</div>
            <div>🔴 Red: Operators μ₅-μ₇ (medium entropy)</div>
            <div>🟣 Purple: μ₈ (deterministic output)</div>
            <div>🔵 Blue: JTBD scenarios (density heatmap)</div>
          </div>
        </div>
      </div>

      {/* Legend */}
      <div className="grid grid-cols-2 gap-4 text-xs">
        <div className="space-y-2">
          <div className="font-semibold text-cyan-400">Operator Cascade</div>
          {operatorLocations.slice(0, 4).map((op) => (
            <div key={op.id} className="flex items-center gap-2">
              <div
                className="w-3 h-3 rounded-full"
                style={{ backgroundColor: `rgb(${op.color.join(',')})` }}
              ></div>
              <span className="text-slate-300">
                {op.id}: {op.name}
              </span>
            </div>
          ))}
        </div>
        <div className="space-y-2">
          <div className="font-semibold text-cyan-400">Entropy Levels</div>
          {operatorLocations.slice(4).map((op) => (
            <div key={op.id} className="flex items-center gap-2">
              <div
                className="w-3 h-3 rounded-full"
                style={{ backgroundColor: `rgb(${op.color.join(',')})` }}
              ></div>
              <span className="text-slate-300">
                {op.id}: {op.entropy.toFixed(1)} nats
              </span>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
