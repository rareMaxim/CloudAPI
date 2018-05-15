program ConsoleBot;

{$APPTYPE CONSOLE}
{$R *.res}

{/$DEFINE  USE_INDY_CORE}
uses
{$IFDEF  USE_INDY_CORE}
//Indy Http Core
  CrossUrl.Indy.HttpClient,
{$ELSE}
// System.Net HTTP Core
  CrossUrl.SystemNet.HttpClient,
{$ENDIF}

  Rest.Json,
  System.SysUtils,
  TelegAPI.Receiver.Console,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Bot.Impl,
  TelegAPI.Logger,
  TelegAPI.Logger.Old;

procedure SMG(ABot: ITelegramBot; AMessage: ITgMessage);
var
  Test: TtgInputMediaPhoto;
begin
  Test := TtgInputMediaPhoto.Create(TtgFileToSend.FromFile('D:\Repositories\Мои проекты\ms301-TelegAPI\Install\pJNqeRflXYU.png'),
    'Test');
  ABot.sendMediaGroup(AMessage.Chat.ID, [Test, Test])
end;

procedure Main;
var
  LBot: ITelegramBot;
  LReceiver: TtgReceiverConsole;
  LExcp: TtgExceptionManagerConsole;
  LStop: string;
begin
  LBot := TTelegramBot.Create('YOUR_TOKEN',
                              {$IFDEF  USE_INDY_CORE}
                                TcuHttpClientIndy.Create(nil)
                              {$ELSE}
                                TcuHttpClientSysNet.Create(nil)
                              {$ENDIF});
  LReceiver := TtgReceiverConsole.Create(LBot);
  LBot.Logger := TtgExceptionManagerConsole.Create;
  try
    LExcp := LBot.Logger as TtgExceptionManagerConsole;
    LExcp.OnLog :=
      procedure(level: TLogLevel; msg: string; e: Exception)
      begin
        if level >= TLogLevel.Error then
          Writeln('[' + e.ToString + '] ' + msg);
      end;
    LReceiver.OnStart :=
      procedure
      begin
        Writeln('started');
      end;
    LReceiver.OnStop :=
      procedure
      begin
        Writeln('stoped');
      end;
    LReceiver.OnMessage :=
      procedure(AMessage: ITgMessage)
      begin
        Writeln(AMessage.From.ID, ': ', AMessage.Text);
       // LBot.SendMessage(AMessage.From.ID, AMessage.Text);
        SMG(LBot, AMessage);
      end;
    Writeln('Bot nick: ', LBot.GetMe.Username);
    LReceiver.IsActive := True;
    while LStop.ToLower.Trim <> 'exit' do
    begin
      Readln(LStop);
      if LStop.ToLower.Trim = 'stop' then
        LReceiver.IsActive := False
      else if LStop.ToLower.Trim = 'start' then
        LReceiver.IsActive := True;
    end;
  finally
    LReceiver.Free;
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

