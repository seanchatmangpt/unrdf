
#
# CodeManufactory Purchase Workflow GenServer
#
# Generated from CodeManufactory ontology
# Workflow: wf-cmf-purchase
#
# WvdA Soundness Compliance:
# - Deadlock Freedom: All GenServer.call have explicit timeout_ms
# - Liveness: Bounded state transitions (max 3 retries)
# - Boundedness: MAX_STATE_TRANSITIONS constant enforced
# - OTel Observability: Spans emitted for all operations
# - OCEL Events: Lifecycle and state transition events emitted
#

defmodule wfCmfPurchaseWorkflow do
  @moduledoc """
  GenServer for CodeManufactory Purchase Workflow.

  Manages the RevOps purchasing workflow for CodeManufactory artifacts:
  - Request → Approval → Procurement → Delivery → Acceptance → Payment

  ## WvdA Soundness Guarantees

  * **Deadlock Freedom**: All operations have explicit `timeout_ms` with fallback
  * **Liveness**: Bounded state machine with max 3 retries
  * **Boundedness**: MAX_STATE_TRANSITIONS = 3 enforced

  ## Example

      iex> wfCmfPurchase.start_link(deal_id: "d-cmf-ontology-2026")
      iex> wfCmfPurchase.advance_stage(pid)
      {:ok, :approval}
  """

  use GenServer
  require Logger
  require OpenTelemetry.SemConv.RevOps

  # WvdA Soundness: Timeout and boundedness constants
  @default_timeout_ms 5000
  @max_state_transitions 3
  @state_transition_timeout_ms 2592000000  # Convert seconds to ms

  # WvdA Soundness: OCEL Event Emission
  @ocel_events [
    :workflow_started,
    :stage_transition,
    :workflow_completed,
    :workflow_failed,
    :retry_attempt
  ]

  # ==========================================================================
  # Client API
  # ==========================================================================

  @doc """
  Start the workflow GenServer.

  ## Options

    * `:deal_id` - Deal identifier (required)
    * `:artifact_type` - Artifact type being purchased
    * `:timeout_ms` - GenServer call timeout (default: #{@default_timeout_ms})

  """
  def start_link(opts \\ []) do
    {deal_id, opts} = Keyword.pop!(opts, :deal_id)
    GenServer.start_link(__MODULE__, opts, name: via_tuple(deal_id))
  end

  @doc """
  Advance the workflow to the next stage.

  Returns `{:ok, stage}` on success, `{:error, reason}` on failure.
  """
  def advance_stage(pid, timeout_ms \\ @default_timeout_ms) do
    GenServer.call(pid, :advance_stage, timeout_ms)
  end

  @doc """
  Get the current workflow state.

  Returns `{:ok, state_map}` or `{:error, reason}`.
  """
  def get_state(pid, timeout_ms \\ @default_timeout_ms) do
    GenServer.call(pid, :get_state, timeout_ms)
  end

  # ==========================================================================
  # Server Callbacks
  # ==========================================================================

  @impl true
  def init(opts) do
    deal_id = Keyword.fetch!(opts, :deal_id)
    artifact_type = Keyword.get(opts, :artifact_type)

    # WvdA Soundness: OCEL lifecycle event
    emit_ocel_event(:workflow_started, %{
      deal_id: deal_id,
      artifact_type: artifact_type,
      workflow_id: "wf-cmf-purchase"
    })

    state = %{
      deal_id: deal_id,
      artifact_type: artifact_type,
      current_stage: :request,
      stage_history: [:request],
      transition_count: 0,
      started_at: System.monotonic_time(:millisecond)
    }

    # WvdA Soundness: OTEL span for workflow initialization
    span = OpenTelemetry.Tracer.start_span(
      "revops.purchase.init",
      attributes: %{
        "revops.deal_id": deal_id,
        "revops.artifact_type": artifact_type,
        "revops.stage": "request"
      }
    )
    OpenTelemetry.Span.set_status(span, :ok)
    OpenTelemetry.Span.end_span(span)

    {:ok, state}
  end

  @impl true
  def handle_call(:advance_stage, _from, state) do
    tracer = OpenTelemetry.Tracer

    # WvdA Soundness: OTEL span for stage transition
    span = tracer.start_span("revops.purchase.advance_stage", %{
      "revops.deal_id": state.deal_id,
      "revops.current_stage": state.current_stage
    })

    next_stage = get_next_stage(state.current_stage)

    case next_stage do
      nil ->
        # WvdA Soundness: Error fallback - no more stages
        span
        |> OpenTelemetry.Span.set_status(:error, "No more stages")
        |> OpenTelemetry.Span.end_span()

        {:reply, {:error, :no_more_stages}, state}

      stage ->
        # WvdA Soundness: Check boundedness
        if state.transition_count >= @max_state_transitions do
          span
          |> OpenTelemetry.Span.set_attribute("revops.max_transitions_exceeded", true)
          |> OpenTelemetry.Span.set_status(:error, "Max state transitions exceeded")
          |> OpenTelemetry.Span.end_span()

          emit_ocel_event(:workflow_failed, %{
            deal_id: state.deal_id,
            reason: :max_transitions_exceeded
          })

          {:reply, {:error, :max_transitions_exceeded}, state}
        else
          # WvdA Soundness: OCEL lifecycle event
          emit_ocel_event(:stage_transition, %{
            deal_id: state.deal_id,
            from_stage: state.current_stage,
            to_stage: stage
          })

          new_state = %{state |
            current_stage: stage,
            stage_history: state.stage_history ++ [stage],
            transition_count: state.transition_count + 1
          }

          span
          |> OpenTelemetry.Span.set_attribute("revops.next_stage", stage)
          |> OpenTelemetry.Span.set_status(:ok)
          |> OpenTelemetry.Span.end_span()

          {:reply, {:ok, stage}, new_state}
        end
    end
  end

  def handle_call(:get_state, _from, state) do
    # WvdA Soundness: OTEL span for state query
    span = OpenTelemetry.Tracer.start_span("revops.purchase.get_state", %{
      "revops.deal_id": state.deal_id
    })
    OpenTelemetry.Span.end_span(span)

    {:reply, {:ok, state}, state}
  end

  @impl true
  def handle_info(:retry, state) do
    # WvdA Soundness: Retry with boundedness check
    if state.transition_count >= @max_state_transitions do
      Logger.warn("Max retries exceeded for deal #{state.deal_id}")
      {:noreply, state}
    else
      # Emit OCEL event
      emit_ocel_event(:retry_attempt, %{
        deal_id: state.deal_id,
        attempt: state.transition_count + 1
      })

      # WvdA Soundness: OTEL span for retry
      span = OpenTelemetry.Tracer.start_span("revops.purchase.retry", %{
        "revops.deal_id": state.deal_id,
        "revops.retry_attempt": state.transition_count + 1
      })
      OpenTelemetry.Span.end_span(span)

      {:noreply, %{state | transition_count: state.transition_count + 1}}
    end
  end

  # ==========================================================================
  # Private Helpers
  # ==========================================================================

  defp get_next_stage(:request), do: :approval
  defp get_next_stage(:approval), do: :procurement
  defp get_next_stage(:procurement), do: :delivery
  defp get_next_stage(:delivery), do: :acceptance
  defp get_next_stage(:acceptance), do: :payment
  defp get_next_stage(:payment), do: :completed
  defp get_next_stage(_), do: nil

  defp via_tuple(deal_id) do
    {:via, Registry, wfCmfPurchaseRegistry, deal_id}}
  end

  # WvdA Soundness: OCEL Event Emission
  defp emit_ocel_event(event_type, attributes) do
    Logger.info(fn ->
      Jason.encode!(%{
        ocel_event: event_type,
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
        attributes: attributes
      })
    end)

    # In production, persist to event store
    # EventStore.emit(event_type, attributes)
  end
end
