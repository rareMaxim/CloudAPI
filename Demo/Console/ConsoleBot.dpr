program ConsoleBot;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  TelegAPI.Bot,
  Rest.Json,
  TelegAPI.Receiver.Console,
  System.SysUtils,
  TelegAPI.Types,
  TelegAPI.Bot.Impl,
  TelegaPi.Exceptions;

procedure Main;
var
  LBot: ITelegramBot;
  LRecesiver: TtgReceiverConsole;
  LExcp: TtgExceptionManagerConsole;
  LStop: string;
begin
  LBot := TTelegramBot.Create('YOUR_TOKEN');
  LRecesiver := TtgReceiverConsole.Create(LBot);
  try
    LExcp := LBot.ExceptionManager as TtgExceptionManagerConsole;
    LExcp.OnApiException :=
      procedure(AMethod: string; AExp: EApiRequestException)
      begin
        Writeln(AExp.ToString);
      end;
    LExcp.OnGlobalException :=
      procedure(AMethod: string; AExp: Exception)
      begin
        Writeln(AExp.ToString);
      end;
    LRecesiver.OnStart :=
      procedure
      begin
        Writeln('started');
      end;
    LRecesiver.OnStop :=
      procedure
      begin
        Writeln('stoped');
      end;

    LRecesiver.OnMessage :=
      procedure(AMessage: ITgMessage)
      begin
        Writeln(AMessage.From.ID, ': ', AMessage.Text);
        LBot.SendMessage(AMessage.From.ID, AMessage.Text);
      end;
    Writeln('Bot nick: ', LBot.GetMe.Username);
    LRecesiver.IsActive := True;
    while LStop.ToLower.Trim <> 'exit' do
    begin
      Readln(LStop);
      if LStop.ToLower.Trim = 'stop' then
        LRecesiver.IsActive := False
      else if LStop.ToLower.Trim = 'start' then
        LRecesiver.IsActive := True;
    end;
  finally
    LRecesiver.Free;
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

