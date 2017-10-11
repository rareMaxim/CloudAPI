program ConsoleBot;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  TelegAPI.Bot,
  TelegAPI.Bot.Recesiver.Console,
  TelegAPI.Types,
  System.SysUtils;

procedure Main;
var
  LBot: TTelegramBot;
  LRecesiver: TTgBotRecesiverConsole;
  LStop: string;
begin
  LBot := TTelegramBot.Create(nil);
  LBot.Token := {$I ..\token.inc};
  LRecesiver := TTgBotRecesiverConsole.Create(nil);
  LRecesiver.Bot := LBot;
  try
    LRecesiver.OnMessage :=
      procedure(AMessage: TTgMessage)
      begin
        Writeln(AMessage.From.Username, ': ', AMessage.Text);
      end;
    with LBot.GetMe do
    begin
      Writeln('Bot nick: ', Username);
      Free;
    end;
    LRecesiver.IsReceiving := True;
    while LStop.ToLower.Trim <> 'exit' do
      Readln(LStop);
  finally
    LRecesiver.Free;
    LBot.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

