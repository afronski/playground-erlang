defmodule PerlinNoise do
  use Bitwise

  def generate(x, y, persistence, number_of_octaves) do
    _generate(x, y, persistence, number_of_octaves, 0)
  end

  defp _generate(_, _, _, 0, result), do: result

  defp _generate(x, y, p, i, result) do
    frequency = :math.pow(2, i)
    amplitude = :math.pow(p, i)

    _generate(x, y, p, i - 1, result + interpolated_noise(x * frequency, y * frequency) * amplitude)
  end

  defp interpolated_noise(x, y) do
      v1 = smooth_noise(x    , y    )
      v2 = smooth_noise(x + 1, y    )
      v3 = smooth_noise(x    , y + 1)
      v4 = smooth_noise(x + 1, y + 1)

      i1 = lerp(v1, v2, 0.5)
      i2 = lerp(v3, v4, 0.5)

      lerp(i1, i2, 0.5)
  end

  defp smooth_noise(x, y) do
    corners = (noise(x - 1, y - 1) + noise(x + 1, y - 1) + noise(x - 1, y + 1) + noise(x + 1, y + 1)) / 16
    sides   = (noise(x - 1, y    ) + noise(x + 1, y    ) + noise(x    , y - 1) + noise(x    , y + 1)) / 8

    center = noise(x, y) / 4

    corners + sides + center
  end

  defp noise(x, y) do
    n = trunc(x + y * 57)
    factor = (n <<< 13) ^^^ n

    1.0 - (trunc(factor * (factor * factor * 15731 + 789221) + 1376312589) &&& 0x7fffffff) / 1073741824.0
  end

  defp lerp(a, b, w) do
    (1.0 - w) * a + w * b
  end
end

defmodule Heightmap do
  @max_octaves 64
  def create(n) do
    octaves = :random.uniform(@max_octaves) - 1
    persistence = :random.uniform

    1 .. n
      |> Stream.map(&(create_line(n, &1, persistence, octaves)))
      |> Enum.take(n)
  end

  defp create_line(n, line, persistence, octaves) do
    1 .. n
      |> Stream.map(&(map_to_grayscale(PerlinNoise.generate(&1, line, persistence, octaves))))
      |> Enum.take(n)
  end

  defp map_to_grayscale(value) do
    pixel = abs(trunc(value * 255))

    { pixel, pixel, pixel }
  end
end

defmodule Drawing do
  def convert_line_to_ASCII(line) do
    line
      |> Stream.map(&to_character/1)
      |> Enum.join
  end

  defp to_character({ ch, ch, ch }) when ch >= 0 and ch < 25, do: '.'
  defp to_character({ ch, ch, ch }) when ch >= 25 and ch < 50, do: ','
  defp to_character({ ch, ch, ch }) when ch >= 50 and ch < 75, do: 'x'
  defp to_character({ ch, ch, ch }) when ch >= 75 and ch < 150, do: '*'
  defp to_character({ ch, ch, ch }) when ch >= 150 and ch < 200, do: 'X'
  defp to_character({ ch, ch, ch }) when ch >= 200 and ch <= 255, do: '#'
end

for line <- Heightmap.create(80), do:
  IO.puts Drawing.convert_line_to_ASCII(line)