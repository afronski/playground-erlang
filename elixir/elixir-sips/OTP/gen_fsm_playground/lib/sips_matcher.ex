defmodule SipsMatcher do
  ### Public API

  def start_link do
    {:ok, pid} = :gen_fsm.start_link __MODULE__, [], []
    pid
  end

  def consume_s(fsm) do
    :gen_fsm.sync_send_event(fsm, :s)
  end

  def consume_i(fsm) do
    :gen_fsm.sync_send_event(fsm, :i)
  end

  def consume_p(fsm) do
    :gen_fsm.sync_send_event(fsm, :p)
  end

  def consume_not_s(fsm) do
    :gen_fsm.sync_send_event(fsm, :not_s)
  end

  ### :gen_fsm API

  def init(_) do
    {:ok, :starting, []}
  end

  def starting(:s, _from, state_data) do
    {:reply, :got_s, :got_s, state_data}
  end

  def starting(:not_s, _from, state_data) do
    {:reply, :starting, :starting, state_data}
  end

  def got_s(:i, _from, state_data) do
    {:reply, :got_si, :got_si, state_data}
  end

  def got_si(:p, _from, state_data) do
    {:reply, :got_sip, :got_sip, state_data}
  end

  def got_sip(:s, _from, state_data) do
    {:reply, :got_sips, :got_sips, state_data}
  end

  def got_sip(:not_s, _from, state_data) do
    {:reply, :starting, :starting, state_data}
  end

  def got_sips(_message, _from, state_data) do
    {:reply, :got_sips, :got_sips, state_data}
  end
end
