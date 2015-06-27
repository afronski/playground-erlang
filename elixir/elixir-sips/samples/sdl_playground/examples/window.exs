defmodule SdlWindow do
  def start do
    :ok = :sdl.start([:video])
    :ok = :sdl.stop_on_exit

    {:ok, window} = :sdl_window.create('Hello SDL', 10, 10, 500, 500, [])
    {:ok, renderer} = :sdl_renderer.create(window, -1, [:accelereated, :present_vsync])

    loop(renderer)
  end

  defp loop(renderer) do
    :ok = :sdl_renderer.set_draw_color(renderer, 255, 255, 255, 255)
    :ok = :sdl_renderer.clear(renderer)

    :ok = :sdl_renderer.set_draw_color(renderer, 255, 0, 0, 255)
    :ok = :sdl_renderer.fill_rect(renderer, %{x: 100, y: 100, w: 50, h: 50})

    :ok = :sdl_renderer.present(renderer)

    case :sdl_events.poll do
      %{type: :quit} -> :ok
      _              -> loop(renderer)
    end
  end
end

SdlWindow.start
