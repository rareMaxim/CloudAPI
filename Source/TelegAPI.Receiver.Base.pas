unit TelegAPI.Receiver.Base;

interface

uses
  System.Classes,
  System.SysUtils,
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

  TTgBotReceiverBase = class(TtgAbstractComponent, ITgBotRecesiverBase)
  private
    FBotDonor: ITelegramBot;
    FAllowedUpdates: TAllowedUpdates;
    FMessageOffset: Int64;
    FPollingInterval: Integer;
    FThread: TThread;
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
    function GetBot: ITelegramBot;
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    [Default(False)]
    property IsActive: Boolean read FIsActive write SetIsActive;
  published
    property Bot: ITelegramBot read FBotDonor write FBotDonor;
    [Default(0)]
    property MessageOffset: Int64 read FMessageOffset write FMessageOffset;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    [Default(1000)]
    property PollingInterval: Integer read FPollingInterval write FPollingInterval;
  end;

implementation

uses
  TelegAPI.Factory;
{ TTgBotReceiverBase }

constructor TTgBotReceiverBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MessageOffset := 0;
  AllowedUpdates := UPDATES_ALLOWED_ALL;
  PollingInterval := 1000;
end;

destructor TTgBotReceiverBase.Destroy;
begin
  inherited;
end;

procedure TTgBotReceiverBase.EventParser(AUpdates: TArray<ItgUpdate>);
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

function TTgBotReceiverBase.GetBot: ITelegramBot;
begin
  Result := TtgFactory.CreateTelegram(FBotDonor.Token);
  Result.ExceptionManager := FBotDonor.ExceptionManager;
end;

procedure TTgBotReceiverBase.Go;
var
  LUpdates: TArray<ItgUpdate>;
begin
  DoOnStart;
  while FIsActive do
  begin
    LUpdates := ReadUpdates;
    if Length(LUpdates) = 0 then
    begin
      Sleep(FPollingInterval);
      Continue;
    end;
    MessageOffset := LUpdates[High(LUpdates)].ID + 1;
    EventParser(LUpdates);
    Sleep(FPollingInterval);
  end;
  DoOnStop;
end;

function TTgBotReceiverBase.ReadUpdates: TArray<ItgUpdate>;
begin
  try
    Result := GetBot.GetUpdates(MessageOffset, 100, 0, AllowedUpdates);
  except
    on E: Exception do
      Bot.ExceptionManager.HaveGlobalExeption('TTgBotReceiverBase.ReadUpdates', E)
  end;
end;

procedure TTgBotReceiverBase.SetIsActive(const Value: Boolean);
begin
  if FIsActive = Value then
    Exit;
  FIsActive := Value;
  if FIsActive then
  begin
    FThread := TThread.CreateAnonymousThread(Go);
    FThread.FreeOnTerminate := True;
    FThread.Start;
  end
  else
  begin
    FThread.Terminate;
    FIsActive := False;
    FThread.WaitFor;
  end;
end;

procedure TTgBotReceiverBase.Start;
begin
  IsActive := True;
end;

procedure TTgBotReceiverBase.Stop;
begin
  IsActive := False;
end;

procedure TTgBotReceiverBase.TypeUpdate(AUpdate: ItgUpdate);
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

