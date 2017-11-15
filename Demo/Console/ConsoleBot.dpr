program ConsoleBot;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegAPI.Bot,
  TelegAPI.Bot.Recesiver.Console,
  System.SysUtils,
  TelegAPI.Types;

procedure Main;
var
  LBot: ITelegramBot;
  LRecesiver: TTgBotRecesiverConsole;
  LStop: string;
  I: Integer;
begin
  LBot := CreateTelegramBot('283107814:AAF9VZC6TRv6qKmOMCsLFoI8SBlV_xFMI80');
  LRecesiver := TTgBotRecesiverConsole.Create(nil);
  LRecesiver.Bot := LBot;
  try
    LRecesiver.OnMessage :=
      procedure(AMessage: ITgMessage)
      begin
        Writeln(AMessage.From.Username, ': ', AMessage.Text);
        LBot.SendMessage(AMessage.Chat.ID, '345t6yu');
      end;
    with LBot.GetMe do
    begin
      Writeln('Bot nick: ', Username);
    end;
    LRecesiver.IsReceiving := True;
    while LStop.ToLower.Trim <> 'exit' do
      Readln(LStop);
  finally
    // LRecesiver.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.message);
  end;

end.

