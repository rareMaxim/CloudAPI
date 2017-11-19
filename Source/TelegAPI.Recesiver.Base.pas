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
    procedure Start;
    procedure Stop;
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
    function ReadUpdates: TArray<ItgUpdate>; virtual;
    procedure Go; virtual;
    procedure EventParser(AUpdates: TArray<ItgUpdate>); virtual;
    procedure TypeUpdate(AUpdate: ItgUpdate); virtual;
    //События
    procedure DoOnStart; virtual; abstract;
    procedure DoOnStop; virtual; abstract;
    procedure DoOnUpdates(AUpdates: TArray<ItgUpdate>); virtual; abstract;
    procedure DoOnUpdate(AUpdate: ItgUpdate); virtual; abstract;
    procedure DoOnMessage(AMessage: ITgMessage); virtual; abstract;
    procedure DoOnInlineQuery(AInlineQuery: ItgInlineQuery); virtual; abstract;
    procedure DoOnChosenInlineResult(AChosenInlineResult: ItgChosenInlineResult); virtual; abstract;
    procedure DoOnCallbackQuery(ACallbackQuery: ItgCallbackQuery); virtual; abstract;
    procedure DoOnEditedMessage(AEditedMessage: ITgMessage); virtual; abstract;
    procedure DoOnChannelPost(AChannelPost: ITgMessage); virtual; abstract;
    procedure DoOnEditedChannelPost(AEditedChannelPost: ITgMessage); virtual; abstract;
    procedure DoOnShippingQuery(AShippingQuery: ItgShippingQuery); virtual; abstract;
    procedure DoOnPreCheckoutQuery(APreCheckoutQuery: ItgPreCheckoutQuery); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(ABot: ITelegramBot); overload;
    procedure Start;
    procedure Stop;
  published
    property Bot: ITelegramBot read FBot write FBot;
    [Default(0)]
    property MessageOffset: Int64 read FMessageOffset write FMessageOffset;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    [Default(1000)]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
  public
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

procedure TTgBotRecesiverBase.EventParser(AUpdates: TArray<ItgUpdate>);
var
  LUpdate: ItgUpdate;
begin
  DoOnUpdates(AUpdates);
  for LUpdate in AUpdates do
  begin
    DoOnUpdate(LUpdate);
    TypeUpdate(LUpdate);
  end;
end;

procedure TTgBotRecesiverBase.Go;
var
  LUpdates: TArray<ItgUpdate>;
begin
  DoOnStart;
  while FIsActive do
  begin
    LUpdates := ReadUpdates;
    EventParser(LUpdates);
    if LUpdates <> nil then
      MessageOffset := LUpdates[High(LUpdates)].ID + 1;
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
    FTask := TTask.Run(Go)
  else
    FIsActive := False;
end;

procedure TTgBotRecesiverBase.Start;
begin
  IsActive := True;
end;

procedure TTgBotRecesiverBase.Stop;
begin
  IsActive := False;
end;

procedure TTgBotRecesiverBase.TypeUpdate(AUpdate: ItgUpdate);
begin
  case AUpdate.&Type of
    TtgUpdateType.MessageUpdate:
      DoOnMessage(AUpdate.Message);

    TtgUpdateType.InlineQueryUpdate:
      DoOnInlineQuery(AUpdate.InlineQuery);

    TtgUpdateType.ChosenInlineResultUpdate:
      DoOnChosenInlineResult(AUpdate.ChosenInlineResult);

    TtgUpdateType.CallbackQueryUpdate:
      DoOnCallbackQuery(AUpdate.CallbackQuery);

    TtgUpdateType.EditedMessage:
      DoOnEditedMessage(AUpdate.EditedMessage);

    TtgUpdateType.ChannelPost:
      DoOnChannelPost(AUpdate.ChannelPost);

    TtgUpdateType.EditedChannelPost:
      DoOnEditedChannelPost(AUpdate.EditedChannelPost);

    TtgUpdateType.ShippingQueryUpdate:
      DoOnShippingQuery(AUpdate.ShippingQuery);

    TtgUpdateType.PreCheckoutQueryUpdate:
      DoOnPreCheckoutQuery(AUpdate.PreCheckoutQuery);
  end;
end;

end.

