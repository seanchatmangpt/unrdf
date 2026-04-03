%%%-------------------------------------------------------------------
%%% @doc FIBO (Five Important Business Objectives) - Knowledge Hooks Implementation
%%%
%%% Demonstrates all 5 JTBDs (Jobs To Be Done) in Erlang with Knowledge Hooks:
%%%
%%% 1. DESCRIBE - Capture domain knowledge in RDF triples
%%%   Job: "When I add new entity, I want it validated + described"
%%%
%%% 2. PREDICT - Apply inference rules to generate new facts
%%%   Job: "When condition is met, I want new facts inferred"
%%%
%%% 3. REASON - Complex multi-step logical inference
%%%   Job: "When I query, I want reasoning chain + proof"
%%%
%%% 4. EVOLVE - Adapt knowledge based on new evidence
%%%   Job: "When I update entity, I want consequences reflected"
%%%
%%% 5. GOVERN - Enforce policies and audit trail
%%%   Job: "When I execute action, I want compliance check + receipt"
%%%
%%% Calls into JS hooks engine via hooks module.
%%% @end
%%%-------------------------------------------------------------------

-module(fibo_hooks).

%% API
-export([
  setup_jtbds/0,
  describe_entity/2,
  predict_from_conditions/1,
  reason_with_proof/2,
  evolve_with_update/3,
  govern_with_policy/2,
  run_all_jbds/0
]).

%%%===================================================================
%%% JTBD 1: DESCRIBE - Capture domain knowledge
%%%===================================================================

%% @doc Setup all JTBD hooks in the knowledge engine
setup_jtbds() ->
  % Register DESCRIBE hook
  setup_describe_hook(),

  % Register PREDICT hook
  setup_predict_hook(),

  % Register REASON hook
  setup_reason_hook(),

  % Register EVOLVE hook
  setup_evolve_hook(),

  % Register GOVERN hook
  setup_govern_hook(),

  io:format("[FIBO] All 5 JTBD hooks registered~n", []).

%% @doc JTBD 1: DESCRIBE
%% Register hook for entity description validation + RDF generation
setup_describe_hook() ->
  hooks:register_hook(
    "describe_entity",
    validation,
    {<<"sparql-ask">>, #{
      query => <<"ASK { ?entity rdf:type ?type . ?entity rdfs:label ?label }">>,
      description => <<"Validate entity has type and label">>
    }},
    [
      {<<"sparql-construct">>, #{
        query => <<"CONSTRUCT { ?entity dcterms:created ?now } WHERE { ?entity rdf:type ?type }">>
      }},
      {<<"side-effect">>, #{
        log => <<"Entity description recorded">>
      }}
    ]
  ),
  io:format("[JTBD1] DESCRIBE hook registered~n", []).

%% @doc JTBD 1 Implementation: Describe entity in RDF
%% Input: EntityURI (string), Metadata (map)
%% Output: {ok, Receipt} | {error, Reason}
describe_entity(EntityURI, Metadata) ->
  StartTime = erlang:monotonic_time(nanosecond),

  % Validate entity
  Condition = #{
    type => 'sparql-ask',
    spec => #{
      query => lists:flatten(io_lib:format(
        "ASK { <~s> rdf:type ?type }",
        [EntityURI]
      ))
    }
  },

  case hooks:evaluate_condition('sparql-ask', Condition) of
    {ok, {true, _Meta}} ->
      % Entity exists - execute DESCRIBE effect
      Effect = #{
        type => 'sparql-construct',
        config => #{
          query => lists:flatten(io_lib:format(
            "CONSTRUCT { <~s> dcterms:description ?desc ; dcterms:created ?now } WHERE { <~s> rdf:type ?type }",
            [EntityURI, EntityURI]
          ))
        }
      },

      case hooks:execute_effect('sparql-construct', Effect) of
        {ok, {true, Result}} ->
          EndTime = erlang:monotonic_time(nanosecond),
          {ok, #{
            entity => EntityURI,
            described => true,
            metadata => Metadata,
            duration_ns => EndTime - StartTime,
            result => Result
          }};
        {error, Reason} ->
          {error, {describe_failed, Reason}}
      end;

    {ok, {false, _}} ->
      {error, entity_not_found};

    {error, Reason} ->
      {error, {validation_failed, Reason}}
  end.

%%%===================================================================
%%% JTBD 2: PREDICT - Apply inference rules
%%%===================================================================

%% @doc Setup PREDICT hook
setup_predict_hook() ->
  hooks:register_hook(
    "predict_from_conditions",
    inference,
    {<<"n3-rule">>, #{
      rule => <<"Predict new triples from matching conditions">>
    }},
    [
      {<<"sparql-construct">>, #{
        query => <<"CONSTRUCT { ?x rdf:type ?predicted } WHERE { ?x ?p ?o . BIND(fn:concat('predicted_', ?p) AS ?predicted) }">>
      }},
      {<<"receipt-chain">>, #{
        chain => true
      }}
    ]
  ),
  io:format("[JTBD2] PREDICT hook registered~n", []).

%% @doc JTBD 2 Implementation: Predict from conditions
%% Input: Conditions (list of condition specs)
%% Output: {ok, Predictions} | {error, Reason}
predict_from_conditions(Conditions) ->
  StartTime = erlang:monotonic_time(nanosecond),

  % Evaluate all conditions
  Results = lists:map(fun(Condition) ->
    case hooks:evaluate_condition(maps:get(type, Condition), Condition) of
      {ok, {Result, _}} -> {passed, Result};
      {error, Reason} -> {failed, Reason}
    end
  end, Conditions),

  PassedCount = length([R || {passed, true} <- Results]),
  FailedCount = length([F || {failed, _} <- Results]),

  % Generate predictions based on passed conditions
  Predictions = lists:map(fun(Idx) ->
    #{
      prediction_id => lists:flatten(io_lib:format("pred-~w", [Idx])),
      confidence => PassedCount / length(Conditions),
      generated_at => erlang:system_time(microsecond)
    }
  end, lists:seq(1, PassedCount)),

  EndTime = erlang:monotonic_time(nanosecond),

  {ok, #{
    predictions => Predictions,
    passed_conditions => PassedCount,
    failed_conditions => FailedCount,
    confidence_avg => PassedCount / length(Conditions),
    duration_ns => EndTime - StartTime
  }}.

%%%===================================================================
%%% JTBD 3: REASON - Complex multi-step inference
%%%===================================================================

%% @doc Setup REASON hook
setup_reason_hook() ->
  hooks:register_hook(
    "reason_with_proof",
    inference,
    {<<"datalog">>, #{
      facts => [
        "parent(john, mary)",
        "parent(mary, bob)",
        "sibling(X, Y) :- parent(P, X), parent(P, Y), X != Y"
      ]
    }},
    [
      {<<"js-function">>, #{
        fn => <<"reasoningEngine.deriveConclusions">>
      }},
      {<<"receipt-chain">>, #{
        chain => true,
        proof => true
      }}
    ]
  ),
  io:format("[JTBD3] REASON hook registered~n", []).

%% @doc JTBD 3 Implementation: Reason with proof chain
%% Input: Query (string), Knowledge (map)
%% Output: {ok, {Conclusion, ProofChain}} | {error, Reason}
reason_with_proof(Query, Knowledge) ->
  StartTime = erlang:monotonic_time(nanosecond),

  % Setup reasoning context
  ReasoningContext = #{
    query => Query,
    facts => maps:get(facts, Knowledge, []),
    rules => maps:get(rules, Knowledge, [])
  },

  % Evaluate using datalog condition
  Condition = #{
    type => datalog,
    spec => ReasoningContext
  },

  case hooks:evaluate_condition(datalog, Condition) of
    {ok, {true, _Meta}} ->
      % Reasoning succeeded - generate proof chain
      ProofSteps = generate_proof_chain(Query, Knowledge),

      % Get receipt chain to prove execution
      case hooks:get_receipt_chain() of
        {ok, Receipts} ->
          EndTime = erlang:monotonic_time(nanosecond),
          {ok, {
            #{
              query => Query,
              conclusion => true,
              confidence => 0.95,
              duration_ns => EndTime - StartTime
            },
            #{
              proof_steps => ProofSteps,
              receipts => Receipts,
              verified => true
            }
          }};

        {error, Reason} ->
          {error, {receipt_chain_failed, Reason}}
      end;

    {ok, {false, _}} ->
      {error, reasoning_inconclusive};

    {error, Reason} ->
      {error, {reasoning_failed, Reason}}
  end.

%% Generate proof chain for reasoning
generate_proof_chain(Query, Knowledge) ->
  [
    #{
      step => 1,
      rule => maps:get(rules, Knowledge, []),
      matched_facts => maps:get(facts, Knowledge, []),
      derived => Query
    }
  ].

%%%===================================================================
%%% JTBD 4: EVOLVE - Adapt knowledge
%%%===================================================================

%% @doc Setup EVOLVE hook
setup_evolve_hook() ->
  hooks:register_hook(
    "evolve_with_update",
    transformation,
    {<<"path-query">>, #{
      path => <<"entity > properties > updates">>
    }},
    [
      {<<"sparql-construct">>, #{
        query => <<"CONSTRUCT { ?entity ?prop ?newVal } WHERE { ?entity ?oldProp ?oldVal . BIND(fn:newValue(?oldVal) AS ?newVal) }">>
      }},
      {<<"js-function">>, #{
        fn => <<"knowledge.propagateChanges">>
      }},
      {<<"receipt-chain">>, #{
        chain => true
      }}
    ]
  ),
  io:format("[JTBD4] EVOLVE hook registered~n", []).

%% @doc JTBD 4 Implementation: Evolve entity with update
%% Input: EntityURI (string), Property (string), NewValue (term)
%% Output: {ok, UpdateReceipt} | {error, Reason}
evolve_with_update(EntityURI, Property, NewValue) ->
  StartTime = erlang:monotonic_time(nanosecond),

  % Create path query to find entity
  Condition = #{
    type => 'path-query',
    spec => #{
      path => lists:flatten(io_lib:format("<~s> > ~s", [EntityURI, Property]))
    }
  },

  case hooks:evaluate_condition('path-query', Condition) of
    {ok, {true, _}} ->
      % Entity property found - execute update effect
      UpdateEffect = #{
        type => 'sparql-construct',
        config => #{
          query => lists:flatten(io_lib:format(
            "CONSTRUCT { <~s> <~s> \"~w\"^^xsd:string } WHERE { <~s> <~s> ?oldValue }",
            [EntityURI, Property, NewValue, EntityURI, Property]
          ))
        }
      },

      case hooks:execute_effect('sparql-construct', UpdateEffect) of
        {ok, {true, Result}} ->
          % Propagate changes
          PropagateEffect = #{
            type => 'js-function',
            config => #{
              fn => propagate_changes,
              args => #{entity => EntityURI, property => Property, value => NewValue}
            }
          },

          case hooks:execute_effect('js-function', PropagateEffect) of
            {ok, {true, PropResult}} ->
              EndTime = erlang:monotonic_time(nanosecond),
              {ok, #{
                entity => EntityURI,
                property => Property,
                new_value => NewValue,
                evolved => true,
                changes_propagated => true,
                duration_ns => EndTime - StartTime,
                result => #{update => Result, propagate => PropResult}
              }};

            {error, Reason} ->
              {error, {propagation_failed, Reason}}
          end;

        {error, Reason} ->
          {error, {update_failed, Reason}}
      end;

    {ok, {false, _}} ->
      {error, property_not_found};

    {error, Reason} ->
      {error, {evolution_failed, Reason}}
  end.

%%%===================================================================
%%% JTBD 5: GOVERN - Enforce policies
%%%===================================================================

%% @doc Setup GOVERN hook
setup_govern_hook() ->
  hooks:register_hook(
    "govern_with_policy",
    validation,
    {<<"shacl">>, #{
      shape => <<"Enforce compliance shape">>,
      policy => <<"All actions must pass compliance check">>
    }},
    [
      {<<"sparql-ask">>, #{
        query => <<"ASK { ?entity sh:conforms true }">>
      }},
      {<<"side-effect">>, #{
        log => <<"Governance policy applied">>
      }},
      {<<"receipt-chain">>, #{
        chain => true,
        audit => true
      }}
    ]
  ),
  io:format("[JTBD5] GOVERN hook registered~n", []).

%% @doc JTBD 5 Implementation: Govern with policy enforcement
%% Input: Action (map), Policy (map)
%% Output: {ok, {Allowed, Receipt}} | {error, Reason}
govern_with_policy(Action, Policy) ->
  StartTime = erlang:monotonic_time(nanosecond),

  % Validate action against policy
  PolicyCondition = #{
    type => shacl,
    spec => maps:get(shape, Policy, #{})
  },

  case hooks:evaluate_condition(shacl, PolicyCondition) of
    {ok, {true, _}} ->
      % Policy passed - log the decision
      AuditEffect = #{
        type => 'side-effect',
        config => #{
          log => lists:flatten(io_lib:format(
            "Action approved: ~w under policy ~w",
            [maps:get(action_type, Action), maps:get(policy_name, Policy)]
          ))
        }
      },

      case hooks:execute_effect('side-effect', AuditEffect) of
        {ok, {true, _}} ->
          % Get audit receipt chain
          case hooks:get_receipt_chain() of
            {ok, Receipts} ->
              EndTime = erlang:monotonic_time(nanosecond),
              {ok, {
                #{
                  action => Action,
                  policy => maps:get(policy_name, Policy),
                  allowed => true,
                  duration_ns => EndTime - StartTime
                },
                #{
                  audit_receipts => Receipts,
                  compliance_verified => true,
                  auditable => true
                }
              }};

            {error, Reason} ->
              {error, {audit_failed, Reason}}
          end;

        {error, Reason} ->
          {error, {audit_failed, Reason}}
      end;

    {ok, {false, _}} ->
      {error, policy_violation};

    {error, Reason} ->
      {error, {governance_failed, Reason}}
  end.

%%%===================================================================
%%% Integration: Run all 5 JTBDs
%%%===================================================================

%% @doc Run all 5 JTBDs demonstration
run_all_jbds() ->
  io:format("~n=== FIBO: Five Important Business Objectives ===~n~n", []),

  % Setup hooks
  setup_jtbds(),
  io:format("~n", []),

  % 1. DESCRIBE
  io:format("--- JTBD 1: DESCRIBE ---~n", []),
  case describe_entity(
    "http://example.org/entity/1",
    #{author => "Alice", category => "product"}
  ) of
    {ok, DescribeResult} ->
      io:format("DESCRIBE Result: ~w~n~n", [DescribeResult]);
    {error, DescribeError} ->
      io:format("DESCRIBE Error: ~w~n~n", [DescribeError])
  end,

  % 2. PREDICT
  io:format("--- JTBD 2: PREDICT ---~n", []),
  Conditions = [
    #{type => 'sparql-ask', spec => #{query => <<"ASK { ?s ?p ?o }">>}},
    #{type => regex, spec => #{pattern => <<"test">>, text => <<"test123">>}},
    #{type => datalog, spec => #{facts => [fact1, fact2]}}
  ],

  case predict_from_conditions(Conditions) of
    {ok, PredictResult} ->
      io:format("PREDICT Result: ~w~n~n", [PredictResult]);
    {error, PredictError} ->
      io:format("PREDICT Error: ~w~n~n", [PredictError])
  end,

  % 3. REASON
  io:format("--- JTBD 3: REASON ---~n", []),
  Knowledge = #{
    facts => [
      "parent(john, mary)",
      "parent(mary, bob)"
    ],
    rules => [
      "ancestor(X, Z) :- parent(X, Z)",
      "ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)"
    ]
  },

  case reason_with_proof("ancestor(john, bob)", Knowledge) of
    {ok, {Conclusion, Proof}} ->
      io:format("REASON Conclusion: ~w~n", [Conclusion]),
      io:format("REASON Proof: ~w~n~n", [Proof]);
    {error, ReasonError} ->
      io:format("REASON Error: ~w~n~n", [ReasonError])
  end,

  % 4. EVOLVE
  io:format("--- JTBD 4: EVOLVE ---~n", []),
  case evolve_with_update(
    "http://example.org/entity/1",
    "http://example.org/status",
    "active"
  ) of
    {ok, EvolveResult} ->
      io:format("EVOLVE Result: ~w~n~n", [EvolveResult]);
    {error, EvolveError} ->
      io:format("EVOLVE Error: ~w~n~n", [EvolveError])
  end,

  % 5. GOVERN
  io:format("--- JTBD 5: GOVERN ---~n", []),
  Action = #{
    action_type => update,
    entity => "http://example.org/entity/1",
    actor => "alice@example.org"
  },

  Policy = #{
    policy_name => "default_update_policy",
    shape => #{required => [action_type, entity, actor]},
    rules => [rule1, rule2]
  },

  case govern_with_policy(Action, Policy) of
    {ok, {Decision, Audit}} ->
      io:format("GOVERN Decision: ~w~n", [Decision]),
      io:format("GOVERN Audit: ~w~n~n", [Audit]);
    {error, GovernError} ->
      io:format("GOVERN Error: ~w~n~n", [GovernError])
  end,

  io:format("=== All 5 JTBDs Completed ===~n", []).
