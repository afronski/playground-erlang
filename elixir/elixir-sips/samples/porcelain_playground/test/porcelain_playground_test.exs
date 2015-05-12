defmodule PorcelainPlaygroundTest do
  use ExUnit.Case

  @test_files_path "test/files/elixir_port"

  setup do
    File.mkdir_p(@test_files_path)

    on_exit fn ->
      File.rm_rf(@test_files_path)
      :ok
    end

    :ok
  end

  test "evaluating and giving a response" do
    File.mkdir_p(@test_files_path <> "/1")
    File.touch(@test_files_path <> "/1/first")
    File.touch(@test_files_path <> "/1/second")

    expected_output = "first\nsecond\n"

    assert expected_output == Porcelain.shell("ls #{@test_files_path}/1").out
    assert expected_output == Porcelain.exec("ls", [ "#{@test_files_path}/1" ]).out
  end

  test "managing pipelines" do
    opus = """
    poppycock
    garnish
    rutabaga
    pipsqueak
    """

    sorted = """
    garnish
    pipsqueak
    poppycock
    rutabaga
    """

    File.mkdir_p(@test_files_path <> "/2")
    text_file = @test_files_path <> "/2/text_file"
    output_file = @test_files_path <> "/2/output_file"

    File.write!(text_file, opus)
    Porcelain.exec("sort", [], in: {:path, text_file}, out: {:append, output_file})

    assert sorted == File.read!(output_file)

    numstream = Stream.cycle([ 1, 2, 3 ]) |> Stream.take(10)
    Porcelain.exec("sort", [], in: numstream, out: {:path, output_file})

    assert <<1,2,3,1,2,3,1,2,3,1>> <> "\n" == File.read!(output_file)
  end

  test "interacting with a bash shell" do
    alias Porcelain.Process, as: Proc

    proc = %Proc{pid: pid} = Porcelain.spawn("bash", ["--noediting", "-i"],
                                             in: :receive, out: {:send, self()})

    Proc.send_input(proc, "echo foo > #{@test_files_path}/foo\n")
    Proc.send_input(proc, "cat #{@test_files_path}/foo\n")

    assert_receive {^pid, :data, :out, "foo\n"}, 1000
  end
end
