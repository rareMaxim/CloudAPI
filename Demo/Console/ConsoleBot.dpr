program ConsoleBot;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  TelegAPI.Bot.Console,
  TelegAPI.Types,
  System.SysUtils;

procedure Main;
var
  LBot: TTelegramBotConsole;
  LStop: string;
begin
  LBot := TTelegramBotConsole.Create({$I ..\token.inc});
  try
    LBot.OnMessage :=
      procedure(AMessage: TtgMessage)
      begin
        Writeln(AMessage.From.Username, ': ', AMessage.Text);
      end;
    with LBot.GetMe do
    begin
      Writeln('Bot nick: ', Username);
      Free;
    end;
    LBot.IsReceiving := True;
    while LStop.ToLower.Trim <> 'exit' do
      Readln(LStop);
  finally
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

