defmodule MailmanPlayground.Mailer do
  def deliver(email) do
    Mailman.deliver(email, config)
  end

  def config do
    %Mailman.Context{
      config: %Mailman.TestConfig{},
      composer: %Mailman.EexComposeConfig{}
    }
  end
end
