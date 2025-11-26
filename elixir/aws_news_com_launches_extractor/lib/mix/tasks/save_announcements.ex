defmodule Mix.Tasks.SaveAnnouncements do
  use Mix.Task

  def run(args) do
    Mix.Task.run("app.start")

    {:ok, cutoff_date} = Timex.parse(Enum.at(args, 0), "%Y-%m-%d", :strftime)
    cutoff_date = Timex.to_date(cutoff_date)

    AwsNewsComLaunchesExtractor.get_pre_reinvent_launches(cutoff_date)
    |> AwsNewsComLaunchesExtractor.save_to_csv()

    Mix.shell().info("Done!")
  end
end
