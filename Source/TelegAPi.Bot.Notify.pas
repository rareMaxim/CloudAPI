unit TelegAPi.Bot.Notify;

interface

uses
  TelegAPi.Bot,
  TelegAPi.Types,
  System.Classes;

type
  TTelegaUpdateNotify = class(TComponent)
  private
    FBot: TTelegramBot;
    Procedure OnBotHaveUpdate(Const Sender: TObject; Const Update: TTelegaUpdate);
    procedure SetBot(const Value: TTelegramBot);
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { published declarations }
    property Bot: TTelegramBot read FBot write SetBot;
  end;

implementation

uses
  System.SysUtils;
{ TTelegaUpdateNotify }

constructor TTelegaUpdateNotify.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TTelegaUpdateNotify.OnBotHaveUpdate(const Sender: TObject; const Update: TTelegaUpdate);

begin
  if Update.Message.Text.ToLower.Contains('напомни') then
  begin

  end;

end;

procedure TTelegaUpdateNotify.SetBot(const Value: TTelegramBot);
begin

  FBot := Value;
  if NOT Assigned(FBot) then
    Exit;
  FBot.UpdatePool.Add(OnBotHaveUpdate);
end;

end.
