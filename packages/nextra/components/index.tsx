import React from 'react';

export interface TypeReferenceProps {
  schema: string;
  package: string;
}

export function TypeReference({ schema, package: pkg }: TypeReferenceProps) {
  return (
    <div className="type-reference">
      <strong>{schema}</strong> from <code>{pkg}</code>
    </div>
  );
}

export interface FunctionAPIProps {
  name: string;
  package: string;
}

export function FunctionAPI({ name, package: pkg }: FunctionAPIProps) {
  return (
    <div className="function-api">
      <strong>{name}()</strong> from <code>{pkg}</code>
    </div>
  );
}
