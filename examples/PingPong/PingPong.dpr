program PingPong;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  XSuperObject,
  TelegaPI.Bot,
  TelegaPI.Classes,
  TelegaPI.Utils,
  System.SysUtils,
  System.Threading,
  SM.Console in '..\..\SM.Console.pas';

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
  while True do
  Begin
    Sleep(500);
    FBot.getMe;
  End;
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
begin
  if NOT Assigned(Update.Message) then
    Exit;
  Writeln(Update.Message.From.Username + ': ' + Update.Message.Text);
  Cmd := TCommandHelper.Create(Update.Message.Text);
  try
    if Cmd.Command = '/ping' then
      FBot.sendTextMessage(Update.Message.Chat.ID, 'Pong');
  finally
    Cmd.Free;
  end;

end;

// ============
var
  Bot: TTelegaSamplePingPongBot;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Bot := TTelegaSamplePingPongBot.Create;
    Bot.Go;
    Bot.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
