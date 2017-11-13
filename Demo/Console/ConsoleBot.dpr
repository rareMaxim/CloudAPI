program ConsoleBot;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegAPI.Bot,
  TelegAPI.Bot.Intf,
  TelegAPI.Bot.Recesiver.Console,
  TelegAPI.Types,
  System.SysUtils, TelegAPI.Types.Intf;

procedure Main;
var
  LBot: ITelegramBot;
  // LRecesiver: TTgBotRecesiverConsole;
  LUpd: TArray<ItgUpdate>;
  LStop: string;
  I: integer;
begin
  LBot := TTelegramBot.Create(nil);
  LBot.Token := !!!;
  // LRecesiver := TTgBotRecesiverConsole.Create(nil);
  // LRecesiver.Bot := LBot;
  try
    LUpd := LBot.GetUpdates(0, 100, 0);
    for I := Low(LUpd) to High(LUpd) do
      Writeln(LUpd[I].message.Text);
    // LRecesiver.OnMessage :=
    // procedure(AMessage: ITgMessage)
    // begin
    // Writeln(AMessage.From.Username, ': ', AMessage.Text);
    // end;
    // with LBot.GetMe do
    // begin
    // Writeln('Bot nick: ', Username);
    // end;
    // LRecesiver.IsReceiving := True;
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
