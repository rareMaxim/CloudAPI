unit Telegram.Plugin.Welcome;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Utils,
  TelegAPI.Module,
  System.Classes;

Type
  TTgWelcomeBot = Class(TTgModule)
  private

  protected
    procedure OnUpdate(Const Sender: TObject; Const Update: TtgUpdate);
      override;
  End;

implementation

{ TTgWelcomeBot }

procedure TTgWelcomeBot.OnUpdate(const Sender: TObject;
  const Update: TtgUpdate);
Const
  WELCOME_TEXT = 'Добро пожаловать в наш чат!';
var
  Cmd: TCommandHelper;
begin
  Cmd := TCommandHelper.Create(Update.Message.Text);
  try
    if Cmd.Command = '/start' then
      (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
        WELCOME_TEXT);
  finally
    Cmd.Free;
  end;

end;

end.
