defmodule App do
  use GenServerP

  # client APIs
  @spec start_link :: pid
  def start_link() do
    {:ok, pid} = GenServerP.start_link(__MODULE__, :init)
    pid
  end

  @spec push(pid, String.t) :: :ok
  def push(pid, element) do
    GenServerP.cast(pid, {:push, element})
  end

  def pop(pid) do
    GenServerP.call(pid, :pop)
  end

  def print(pid) do
    GenServerP.call(pid, :print)
  end

  # server implementation
  def init(:init) do
    {:ok, []}
  end

  # @spec handle_cast({:push, integer}, [integer]) :: {:noreply, [integer]}
  # def handle_cast({:push, element}, state) do
  #   {:noreply, [element | state]}
  # end

  def handle_call(:pop, _from, [h|t]) do
    {:reply, h, t}
  end

  def handle_call(:print, _from, state) do
    {:reply, state, state}
  end

  # @spec safe_div(integer, integer) :: {:ok, integer} | {:error, String.t()}
  # def safe_div(_, 0) do
  #   {:error, 0}
  # end

  # def safe_div(x, y) do
  #   {:ok, div(x, y)}
  # end
end
