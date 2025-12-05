'use client';

import { useEffect, useRef, useState } from 'react';
import { Canvas, useFrame } from '@react-three/fiber';
import { OrbitControls, Text, Box, Sphere, Line } from '@react-three/drei';
import * as THREE from 'three';

function OperatorNode({ position, operator, stage, isActive }) {
  const meshRef = useRef(null);
  const [hovered, setHovered] = useState(false);

  useFrame(() => {
    if (meshRef.current) {
      meshRef.current.rotation.x += 0.003;
      meshRef.current.rotation.y += 0.005;
      const scale = isActive ? 1.2 : hovered ? 1.1 : 1;
      meshRef.current.scale.lerp(new THREE.Vector3(scale, scale, scale), 0.1);
    }
  });

  return (
    <group position={position}>
      <Sphere
        ref={meshRef}
        args={[0.4, 32, 32]}
        onPointerEnter={() => setHovered(true)}
        onPointerLeave={() => setHovered(false)}
      >
        <meshStandardMaterial
          color={isActive ? '#06b6d4' : hovered ? '#0ea5e9' : '#1e293b'}
          emissive={isActive ? '#06b6d4' : '#334155'}
          emissiveIntensity={isActive ? 0.5 : 0.2}
          wireframe={hovered}
        />
      </Sphere>

      <Text
        position={[0, 0, 0.5]}
        fontSize={0.3}
        color={isActive ? '#06b6d4' : '#cbd5e1'}
        anchorX="center"
        anchorY="middle"
      >
        {operator}
      </Text>

      {hovered && (
        <Text
          position={[0, -0.8, 0]}
          fontSize={0.15}
          color="#94a3b8"
          anchorX="center"
          anchorY="middle"
          maxWidth={2}
        >
          {`Δ ${stage.reduction.toFixed(1)} nats`}
        </Text>
      )}
    </group>
  );
}

function EntropyFlow({ fromPos, toPos, progress }) {
  const lineRef = useRef(null);

  useFrame(() => {
    if (lineRef.current) {
      const points = [
        new THREE.Vector3(...fromPos),
        new THREE.Vector3(
          fromPos[0] + (toPos[0] - fromPos[0]) * progress,
          fromPos[1] + (toPos[1] - fromPos[1]) * progress,
          fromPos[2] + (toPos[2] - fromPos[2]) * progress
        ),
      ];
      lineRef.current.geometry.setPositions(points.flatMap((p) => [p.x, p.y, p.z]));
    }
  });

  return (
    <Line ref={lineRef} points={[fromPos, toPos]} color="#a855f7" lineWidth={2} dashed={false} />
  );
}

function EntropyStage({ index, stage, totalStages, isActive }) {
  const x = (index - totalStages / 2) * 2.5;
  const entropyLevel = 50 - stage.reduction * (index + 1);
  const y = Math.max(0, entropyLevel / 10);
  const position = [x, y, 0];

  return (
    <group key={stage.operator}>
      <OperatorNode
        position={position}
        operator={stage.operator}
        stage={stage}
        isActive={isActive}
      />

      {index < totalStages - 1 && (
        <EntropyFlow
          fromPos={position}
          toPos={[
            (index + 1 - totalStages / 2) * 2.5,
            Math.max(0, (50 - stage.reduction * (index + 2)) / 10),
            0,
          ]}
          progress={isActive ? 1 : 0.5}
        />
      )}
    </group>
  );
}

function EntropyVisualizationScene({ data, animationProgress }) {
  const stages = data.entropyReduction.stages;

  return (
    <>
      <ambientLight intensity={0.6} />
      <pointLight position={[10, 10, 10]} intensity={0.8} />
      <pointLight position={[-10, -10, -10]} intensity={0.4} />

      <group position={[0, -2, 0]}>
        {/* Operator nodes with connections */}
        {stages.map((stage, index) => (
          <EntropyStage
            key={stage.operator}
            index={index}
            stage={stage}
            totalStages={stages.length}
            isActive={index < Math.ceil(animationProgress * stages.length)}
          />
        ))}

        {/* Initial entropy sphere */}
        <group position={[-(stages.length * 2.5) / 2 - 2, 5, 0]}>
          <Sphere args={[0.6, 32, 32]}>
            <meshStandardMaterial color="#f97316" emissive="#ea580c" emissiveIntensity={0.8} />
          </Sphere>
          <Text
            position={[0, 0, 0.7]}
            fontSize={0.3}
            color="#ffffff"
            anchorX="center"
            anchorY="middle"
          >
            50 nats
          </Text>
        </group>

        {/* Final entropy sphere */}
        <group position={[(stages.length * 2.5) / 2 + 2, 0.1, 0]}>
          <Sphere args={[0.3, 32, 32]}>
            <meshStandardMaterial color="#22c55e" emissive="#16a34a" emissiveIntensity={0.8} />
          </Sphere>
          <Text
            position={[0, 0, 0.4]}
            fontSize={0.2}
            color="#ffffff"
            anchorX="center"
            anchorY="middle"
          >
            1 nat
          </Text>
        </group>
      </group>

      <OrbitControls autoRotate autoRotateSpeed={2} enableZoom enablePan enableRotate />
    </>
  );
}

export default function EntropyVisualization3D({ data }) {
  const [animationProgress, setAnimationProgress] = useState(0);

  useEffect(() => {
    const interval = setInterval(() => {
      setAnimationProgress((prev) => (prev < 1 ? prev + 0.02 : 0));
    }, 50);
    return () => clearInterval(interval);
  }, []);

  return (
    <div className="space-y-6">
      <div className="section-header">3D Entropy Cascade Visualization</div>

      <p className="text-slate-400 text-sm max-w-2xl">
        Interactive 3D visualization of entropy reduction through the μ(O) operator chain. Watch as
        user intent (50 nats) transforms through 8 deterministic operators to produce a single
        observable outcome (1 nat). Use mouse to rotate, scroll to zoom.
      </p>

      {/* 3D Canvas */}
      <div className="card h-96 relative overflow-hidden">
        <Canvas camera={{ position: [0, 0, 15], fov: 50 }}>
          <EntropyVisualizationScene data={data} animationProgress={animationProgress} />
        </Canvas>

        {/* Controls hint overlay */}
        <div className="absolute top-4 right-4 text-xs text-slate-400 bg-slate-900/80 px-3 py-2 rounded pointer-events-none">
          <div>🖱️ Drag to rotate</div>
          <div>🔍 Scroll to zoom</div>
          <div>Auto-rotate enabled</div>
        </div>
      </div>

      {/* Transformation Pipeline */}
      <div className="card space-y-4">
        <h3 className="card-header">Operator Chain Transformation Pipeline</h3>

        <div className="space-y-3">
          <div className="flex items-center gap-4">
            <div className="flex-shrink-0 w-16 h-16 bg-gradient-to-br from-orange-500 to-red-500 rounded-lg flex items-center justify-center">
              <span className="text-white font-bold text-lg">50</span>
            </div>
            <div className="flex-1">
              <div className="font-semibold text-slate-200">User Intent (λ)</div>
              <div className="text-sm text-slate-400">
                Shannon entropy of all possible user actions
              </div>
            </div>
          </div>

          {/* Operator chain */}
          <div className="grid grid-cols-4 md:grid-cols-8 gap-2 my-6">
            {data.entropyReduction.stages.map((stage, index) => (
              <div key={stage.operator} className="space-y-1">
                <div className="aspect-square bg-slate-800 rounded-lg border border-slate-700 flex items-center justify-center">
                  <span className="font-mono font-bold text-cyan-400 text-sm">
                    {stage.operator}
                  </span>
                </div>
                <div className="text-xs text-slate-400 text-center">
                  -{stage.reduction.toFixed(1)}
                </div>
              </div>
            ))}
          </div>

          <div className="flex items-center gap-4">
            <div className="flex-shrink-0 w-16 h-16 bg-gradient-to-br from-green-500 to-cyan-500 rounded-lg flex items-center justify-center">
              <span className="text-white font-bold text-lg">1</span>
            </div>
            <div className="flex-1">
              <div className="font-semibold text-slate-200">Observable Outcome (A)</div>
              <div className="text-sm text-slate-400">Deterministic result visible to user</div>
            </div>
          </div>
        </div>

        {/* Information loss visualization */}
        <div className="bg-slate-800/50 rounded-lg p-4 space-y-3 mt-6">
          <div className="grid grid-cols-3 gap-4">
            <div>
              <div className="text-xs text-slate-400 uppercase">Information Loss</div>
              <div className="text-3xl font-bold text-orange-400 mt-1">98%</div>
              <div className="text-xs text-slate-500">49 of 50 nats</div>
            </div>
            <div>
              <div className="text-xs text-slate-400 uppercase">Avg per Operator</div>
              <div className="text-3xl font-bold text-cyan-400 mt-1">6.1</div>
              <div className="text-xs text-slate-500">nats reduced</div>
            </div>
            <div>
              <div className="text-xs text-slate-400 uppercase">Determinism</div>
              <div className="text-3xl font-bold text-green-400 mt-1">100%</div>
              <div className="text-xs text-slate-500">Reproducible outcomes</div>
            </div>
          </div>
        </div>
      </div>

      {/* 3D Visualization Insights */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="card space-y-3">
          <h3 className="card-header">Spatial Interpretation</h3>
          <ul className="space-y-2 text-sm text-slate-400">
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1.5 flex-shrink-0"></span>
              <span>Node size represents operator impact on information flow</span>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-purple-400 mt-1.5 flex-shrink-0"></span>
              <span>Vertical position indicates cumulative entropy reduction</span>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1.5 flex-shrink-0"></span>
              <span>Connections show information flow between operators</span>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-orange-400 mt-1.5 flex-shrink-0"></span>
              <span>Animation progress cycles through operator activation</span>
            </li>
          </ul>
        </div>

        <div className="card space-y-3">
          <h3 className="card-header">Interactive Features</h3>
          <ul className="space-y-2 text-sm text-slate-400">
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1.5 flex-shrink-0"></span>
              <span>Hover over nodes to see entropy reduction values</span>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-purple-400 mt-1.5 flex-shrink-0"></span>
              <span>Drag to rotate the 3D visualization from any angle</span>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1.5 flex-shrink-0"></span>
              <span>Scroll to zoom in/out on the operator chain</span>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-orange-400 mt-1.5 flex-shrink-0"></span>
              <span>Auto-rotation provides continuous 360° view</span>
            </li>
          </ul>
        </div>
      </div>
    </div>
  );
}
