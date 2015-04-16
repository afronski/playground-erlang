defmodule PcmSampler do
  @sample_rate 16_000
  @channels 1
  @max_amplitude 32_767

  def sample(oscillator = %SineWave{}, duration) do
    num_samples = trunc(@sample_rate * duration)
    pre_data = for sample_number <- 1 .. num_samples do
      value = @max_amplitude * SineWave.value_at(oscillator, sample_number / @sample_rate)
              |> trunc
      << value :: signed-big-integer-size(16) >>
    end

    IO.iodata_to_binary(pre_data)
  end
end
