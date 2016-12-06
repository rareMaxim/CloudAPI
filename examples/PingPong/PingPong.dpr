program PingPong;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  WinAPI.Windows,
  // Telegram API BOT Library
  TelegaPI.Bot,
  TelegaPI.Classes,
  TelegaPI.Utils,
  // Delphi Library
  System.SysUtils,
  System.Threading;

Type
  TTelegaSamplePingPongBot = Class
  private
    FBot: TTelegramBot;
    Procedure OnError(Const Sender: TObject; Const Code: Integer;
      Const Message: String);
    Procedure OnUpdates(Sender: TObject; Const Updates: TArray<TtgUpdate>);
    Procedure UpdateManager(Const Update: TtgUpdate);
  public
    procedure Go;
    constructor Create;
    destructor Destroy; override;
  End;

  { TTelegaSamplePingPongBot }

constructor TTelegaSamplePingPongBot.Create;
begin
  FBot := TTelegramBot.Create(nil);
  FBot.OnError := OnError;
  FBot.OnUpdates := OnUpdates;
  FBot.IsReceiving := True;
  FBot.Token := {$I ..\telegaToken.inc};
end;

destructor TTelegaSamplePingPongBot.Destroy;
begin
  FBot.Free;
  inherited;
end;

procedure TTelegaSamplePingPongBot.Go;
begin
  Writeln('Starting ... ');
  while FBot.IsReceiving do
    Sleep(100);
end;

procedure TTelegaSamplePingPongBot.OnError(const Sender: TObject;
  const Code: Integer; const Message: String);
begin
  Writeln(string.Join(' ', ['Error', 'in', Sender.ClassName, 'Code =', Code,
    'Message =', Message]));
end;

procedure TTelegaSamplePingPongBot.OnUpdates(Sender: TObject;
  const Updates: TArray<TtgUpdate>);
begin
  TParallel.&For(low(Updates), High(Updates),
    Procedure(I: Integer)
    Begin
      UpdateManager(Updates[I]);
    End);
end;

procedure TTelegaSamplePingPongBot.UpdateManager(const Update: TtgUpdate);
var
  Cmd: TCommandHelper;
  LMessage: TtgMessage;
begin
  if NOT Assigned(Update.Message) then
    Exit;
  Writeln(Update.Message.From.Username + ': ' + Update.Message.Text);
  Cmd := TCommandHelper.Create(Update.Message.Text);
  try
    if Cmd.Command = '/ping' then
      LMessage := FBot.sendTextMessage(Update.Message.Chat.ID, '👑🙈😇');
  finally
    Cmd.Free;
    LMessage.Free;
  end;
end;

// ============
var
  Bot: TTelegaSamplePingPongBot;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    ReportMemoryLeaksOnShutdown := True;
    Bot := TTelegaSamplePingPongBot.Create;
    Bot.Go;
    Bot.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
