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
  protected
    procedure SetIsRecesiving(AValue: Boolean); virtual;
    function ReadUpdates: TArray<ItgUpdate>;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Bot: ITelegramBot read FBot write FBot;
    [Default(0)]
    property MessageOffset: Int64 read FMessageOffset write FMessageOffset;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    [Default(1000)]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
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

function TTgBotRecesiverBase.ReadUpdates: TArray<ItgUpdate>;
begin
  try
    Result := FBot.GetUpdates(FMessageOffset, 100, 0, AllowedUpdates);
  except
    on E: Exception do
      if Bot.ExceptionManager <> nil then
        Bot.ExceptionManager.HaveGlobalExeption('TTgBotRecesiverBase.ReadUpdates', E)
      else
        raise E;
  end;
end;

procedure TTgBotRecesiverBase.SetIsRecesiving(AValue: Boolean);
begin
  FTask := TTask.Create(
    procedure
    begin

    end);
end;

end.

