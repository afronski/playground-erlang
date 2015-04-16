defmodule SineWave do
  defstruct amplitude: 1, frequency: 440

  def value_at(%__MODULE__{amplitude: a, frequency: f}, time) do
    angular_frequency = 2 * f * :math.pi
    a * :math.sin(angular_frequency * time)
  end
end
