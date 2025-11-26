defmodule Mix.Tasks.ShowAnnouncements do
  use Mix.Task

  def run(args) do
    Mix.Task.run("app.start")

    {:ok, cutoff_date} = Timex.parse(Enum.at(args, 0), "%Y-%m-%d", :strftime)
    cutoff_date = Timex.to_date(cutoff_date)

    list = AwsNewsComLaunchesExtractor.get_pre_reinvent_launches(cutoff_date)

    Mix.shell().info("Announcements count: #{Jason.encode!(list, pretty: true)}")
  end
end
