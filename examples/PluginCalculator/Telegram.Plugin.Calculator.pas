unit Telegram.Plugin.Calculator;

interface

uses
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Utils,
  TelegAPI.Module;

Type
  TTgCalculatorBot = Class(TTgModule)
  protected
    procedure OnUpdate(Sender: TObject; Const Update: TtgUpdate); override;
  End;

implementation

uses
  BI.Expression;

{ TTgWelcomeBot }

procedure TTgCalculatorBot.OnUpdate(Sender: TObject; const Update: TtgUpdate);
var
  Cmd: TCommandHelper;
  Expr: TExpression;
begin
  Cmd := TCommandHelper.Create(Update.Message.Text);
  try
    if not(Cmd.Command = '/calc') then
      Exit;
        if Cmd.ParamCount>0 then

    (Sender as TTelegramBot).sendTextMessage(Update.Message.Chat.ID,
      Expr.Evaluate(Cmd.ParamsToString));
  finally
    Cmd.Free;
  end;

end;

end.
