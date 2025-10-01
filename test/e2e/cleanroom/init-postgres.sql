-- PostgreSQL Initialization for Cleanroom Testing
-- Purpose: Create RDF triple store schema
-- Database: unrdf_cleanroom

-- Create extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm"; -- For text search

-- Create schema for RDF storage
CREATE SCHEMA IF NOT EXISTS rdf;

-- RDF Quads table (subject, predicate, object, graph)
CREATE TABLE IF NOT EXISTS rdf.quads (
    id BIGSERIAL PRIMARY KEY,
    subject TEXT NOT NULL,
    predicate TEXT NOT NULL,
    object TEXT NOT NULL,
    object_type VARCHAR(50) NOT NULL DEFAULT 'uri', -- 'uri', 'literal', 'bnode'
    object_datatype TEXT, -- For typed literals
    object_language VARCHAR(10), -- For language-tagged literals
    graph TEXT NOT NULL DEFAULT 'default',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    UNIQUE (subject, predicate, object, graph)
);

-- Indexes for efficient querying
CREATE INDEX idx_quads_subject ON rdf.quads(subject);
CREATE INDEX idx_quads_predicate ON rdf.quads(predicate);
CREATE INDEX idx_quads_object ON rdf.quads(object);
CREATE INDEX idx_quads_graph ON rdf.quads(graph);
CREATE INDEX idx_quads_spo ON rdf.quads(subject, predicate, object);
CREATE INDEX idx_quads_sp ON rdf.quads(subject, predicate);
CREATE INDEX idx_quads_po ON rdf.quads(predicate, object);

-- Full-text search index for object values
CREATE INDEX idx_quads_object_trgm ON rdf.quads USING gin (object gin_trgm_ops);

-- Namespace prefixes table
CREATE TABLE IF NOT EXISTS rdf.namespaces (
    id SERIAL PRIMARY KEY,
    prefix VARCHAR(50) UNIQUE NOT NULL,
    uri TEXT UNIQUE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Insert common RDF namespaces
INSERT INTO rdf.namespaces (prefix, uri) VALUES
    ('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'),
    ('rdfs', 'http://www.w3.org/2000/01/rdf-schema#'),
    ('xsd', 'http://www.w3.org/2001/XMLSchema#'),
    ('owl', 'http://www.w3.org/2002/07/owl#'),
    ('schema', 'https://schema.org/'),
    ('foaf', 'http://xmlns.com/foaf/0.1/'),
    ('dc', 'http://purl.org/dc/elements/1.1/'),
    ('dcterms', 'http://purl.org/dc/terms/'),
    ('skos', 'http://www.w3.org/2004/02/skos/core#')
ON CONFLICT (prefix) DO NOTHING;

-- SHACL shapes table for validation
CREATE TABLE IF NOT EXISTS rdf.shapes (
    id SERIAL PRIMARY KEY,
    shape_uri TEXT UNIQUE NOT NULL,
    shape_data JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Hook executions log
CREATE TABLE IF NOT EXISTS rdf.hook_executions (
    id BIGSERIAL PRIMARY KEY,
    hook_id TEXT NOT NULL,
    execution_time TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    duration_ms INTEGER,
    success BOOLEAN NOT NULL,
    error_message TEXT,
    trace_id TEXT, -- OpenTelemetry trace ID
    span_id TEXT,  -- OpenTelemetry span ID
    metadata JSONB
);

-- Index for trace correlation
CREATE INDEX idx_hook_executions_trace ON rdf.hook_executions(trace_id);
CREATE INDEX idx_hook_executions_hook_id ON rdf.hook_executions(hook_id);
CREATE INDEX idx_hook_executions_time ON rdf.hook_executions(execution_time DESC);

-- Helper function: Insert or update quad
CREATE OR REPLACE FUNCTION rdf.upsert_quad(
    p_subject TEXT,
    p_predicate TEXT,
    p_object TEXT,
    p_object_type VARCHAR(50) DEFAULT 'uri',
    p_object_datatype TEXT DEFAULT NULL,
    p_object_language VARCHAR(10) DEFAULT NULL,
    p_graph TEXT DEFAULT 'default'
) RETURNS BIGINT AS $$
DECLARE
    v_id BIGINT;
BEGIN
    INSERT INTO rdf.quads (subject, predicate, object, object_type, object_datatype, object_language, graph)
    VALUES (p_subject, p_predicate, p_object, p_object_type, p_object_datatype, p_object_language, p_graph)
    ON CONFLICT (subject, predicate, object, graph) DO UPDATE
    SET object_type = EXCLUDED.object_type,
        object_datatype = EXCLUDED.object_datatype,
        object_language = EXCLUDED.object_language,
        created_at = NOW()
    RETURNING id INTO v_id;

    RETURN v_id;
END;
$$ LANGUAGE plpgsql;

-- Helper function: Delete quads by subject
CREATE OR REPLACE FUNCTION rdf.delete_by_subject(
    p_subject TEXT,
    p_graph TEXT DEFAULT 'default'
) RETURNS INTEGER AS $$
DECLARE
    v_count INTEGER;
BEGIN
    DELETE FROM rdf.quads
    WHERE subject = p_subject AND graph = p_graph;

    GET DIAGNOSTICS v_count = ROW_COUNT;
    RETURN v_count;
END;
$$ LANGUAGE plpgsql;

-- Helper function: SPARQL-like query (simplified)
CREATE OR REPLACE FUNCTION rdf.query_triples(
    p_subject TEXT DEFAULT NULL,
    p_predicate TEXT DEFAULT NULL,
    p_object TEXT DEFAULT NULL,
    p_graph TEXT DEFAULT 'default',
    p_limit INTEGER DEFAULT 100
) RETURNS TABLE (
    subject TEXT,
    predicate TEXT,
    object TEXT,
    object_type VARCHAR(50),
    object_datatype TEXT,
    object_language VARCHAR(10)
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        q.subject,
        q.predicate,
        q.object,
        q.object_type,
        q.object_datatype,
        q.object_language
    FROM rdf.quads q
    WHERE (p_subject IS NULL OR q.subject = p_subject)
      AND (p_predicate IS NULL OR q.predicate = p_predicate)
      AND (p_object IS NULL OR q.object = p_object)
      AND q.graph = p_graph
    LIMIT p_limit;
END;
$$ LANGUAGE plpgsql;

-- Grant permissions
GRANT ALL PRIVILEGES ON SCHEMA rdf TO cleanroom_user;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA rdf TO cleanroom_user;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA rdf TO cleanroom_user;

-- Insert test data
INSERT INTO rdf.quads (subject, predicate, object, object_type, graph) VALUES
    ('https://example.org/alice', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'https://schema.org/Person', 'uri', 'test'),
    ('https://example.org/alice', 'https://schema.org/name', 'Alice Doe', 'literal', 'test'),
    ('https://example.org/alice', 'https://schema.org/email', 'alice@example.org', 'literal', 'test'),
    ('https://example.org/bob', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'https://schema.org/Person', 'uri', 'test'),
    ('https://example.org/bob', 'https://schema.org/name', 'Bob Smith', 'literal', 'test'),
    ('https://example.org/alice', 'https://schema.org/knows', 'https://example.org/bob', 'uri', 'test')
ON CONFLICT DO NOTHING;

-- Analyze tables for query optimization
ANALYZE rdf.quads;
ANALYZE rdf.namespaces;
ANALYZE rdf.shapes;
ANALYZE rdf.hook_executions;
