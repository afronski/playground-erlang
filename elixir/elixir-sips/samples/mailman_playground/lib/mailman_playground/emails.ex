defmodule MailmanPlayground.Emails do
  def testing_email do
    %Mailman.Email{
      subject: "Hey!",
      from: "test@mailman.ex",
      to: [ "afronski@gmail.com" ],
      cc: [ "afronski@op.pl" ],
      bcc: [],
      data: [
        name: "TestUser"
      ],
      text: "Hello <%= name %>!",
      html: """
<html>
  <body>
    <b>Hello <%= name %>!</b>
  </body>
</html>
      """
    }
  end
end
