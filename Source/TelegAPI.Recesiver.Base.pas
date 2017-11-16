unit TelegAPI.Recesiver.Base;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  TelegAPI.Base,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Types.Enums;

type
  ITgBotRecesiverBase = interface
    ['{98A444BC-D8E3-4542-B75F-3A7AACFCAE74}']
  end;

  TTgBotRecesiverBase = class(TtgAbstractComponent, ITgBotRecesiverBase)
  private
    FBot: ITelegramBot;
    FAllowedUpdates: TAllowedUpdates;
    FMessageOffset: Int64;
    FPollingInterval: Integer;
    FTask: ITask;
    FIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  protected
    function ReadUpdates: TArray<ItgUpdate>;
    procedure Go;
    procedure DoOnStart; virtual; abstract;
    procedure DoOnStop; virtual; abstract;
    procedure DoOnUpdates(AUpdates: TArray<ItgUpdate>); virtual; abstract;
    procedure DoOnUpdate(AUpdate: ItgUpdate); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(ABot: ITelegramBot); overload;
  published
    property Bot: ITelegramBot read FBot write FBot;
    [Default(0)]
    property MessageOffset: Int64 read FMessageOffset write FMessageOffset;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    [Default(1000)]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
    [Default(False)]
    property IsActive: Boolean read FIsActive write SetIsActive;
  end;

implementation

{ TTgBotRecesiverBase }

constructor TTgBotRecesiverBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MessageOffset := 0;
  AllowedUpdates := UPDATES_ALLOWED_ALL;
  PollingInterval := 1000;

end;

constructor TTgBotRecesiverBase.Create(ABot: ITelegramBot);
begin
  Create(TComponent(nil));
  Bot := ABot;
end;

procedure TTgBotRecesiverBase.Go;
var
  LUpdates: TArray<ItgUpdate>;
  LUpdate: ItgUpdate;
begin
  DoOnStart;
  while FIsActive do
  begin
    LUpdates := ReadUpdates;
    DoOnUpdates(LUpdates);
    for LUpdate in LUpdates do
    begin
      DoOnUpdate(LUpdate);
    end;
    MessageOffset := LUpdate.ID + 1;
    Sleep(FPollingInterval);
  end;
  DoOnStop;
end;

function TTgBotRecesiverBase.ReadUpdates: TArray<ItgUpdate>;
begin
  try
    Result := FBot.GetUpdates(MessageOffset, 100, 0, AllowedUpdates);
  except
    on E: Exception do
      if Bot.ExceptionManager <> nil then
        Bot.ExceptionManager.HaveGlobalExeption('TTgBotRecesiverBase.ReadUpdates', E)
      else
        raise E;
  end;
end;

procedure TTgBotRecesiverBase.SetIsActive(const Value: Boolean);
begin
  if FIsActive = Value then
    Exit;
  FIsActive := Value;
  if FIsActive then
  begin
    FTask := TTask.Run(Go);
  end
end;

end.

