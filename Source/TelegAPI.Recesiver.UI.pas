unit TelegAPI.Recesiver.UI;

interface

uses
  TelegAPI.Recesiver.Base,
  System.Classes,
  TelegAPI.Types;

type
  TtgOnUpdate = procedure(ASender: TObject; AUpdate: ItgUpdate) of object;

  TtgOnUpdates = procedure(ASender: TObject; AUpdates: TArray<ItgUpdate>) of object;

  TtgOnMessage = procedure(ASender: TObject; AMessage: ITgMessage) of object;

  TtgOnInlineQuery = procedure(ASender: TObject; AInlineQuery: ItgInlineQuery) of object;

  TtgOnInlineResultChosen = procedure(ASender: TObject; AChosenInlineResult: ItgChosenInlineResult) of object;

  TtgOnCallbackQuery = procedure(ASender: TObject; ACallbackQuery: ItgCallbackQuery) of object;

  TtgOnChannelPost = procedure(ASender: TObject; AChanelPost: ITgMessage) of object;

  TtgOnShippingQuery = procedure(ASender: TObject; AShippingQuery: ItgShippingQuery) of object;

  TtgOnPreCheckoutQuery = procedure(ASender: TObject; APreCheckoutQuery: ItgPreCheckoutQuery) of object;

  TtgRecesiverUI = class(TTgBotRecesiverBase)
  private
    FOnUpdate: TtgOnUpdate;
    FOnMessage: TtgOnMessage;
    FOnUpdates: TtgOnUpdates;
    FOnStop: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FOnEditedMessage: TtgOnMessage;
    FOnChannelPost: TtgOnMessage;
    FOnPreCheckoutQuery: TtgOnPreCheckoutQuery;
    FOnInlineQuery: TtgOnInlineQuery;
    FOnShippingQuery: TtgOnShippingQuery;
    FOnChosenInlineResult: TtgOnInlineResultChosen;
    FOnEditedChannelPost: TtgOnMessage;
  protected
    procedure DoOnStart; override;
    procedure DoOnStop; override;
    procedure DoOnUpdates(AUpdates: TArray<ItgUpdate>); override;
    procedure DoOnUpdate(AUpdate: ItgUpdate); override;
    procedure DoOnMessage(AMessage: ITgMessage); override;
    procedure DoOnInlineQuery(AInlineQuery: ItgInlineQuery); override;
    procedure DoOnChosenInlineResult(AChosenInlineResult: ItgChosenInlineResult); override;
    procedure DoOnEditedMessage(AEditedMessage: ITgMessage); override;
    procedure DoOnChannelPost(AChannelPost: ITgMessage); override;
    procedure DoOnEditedChannelPost(AEditedChannelPost: ITgMessage); override;
    procedure DoOnShippingQuery(AShippingQuery: ItgShippingQuery); override;
    procedure DoOnPreCheckoutQuery(APreCheckoutQuery: ItgPreCheckoutQuery); override;
  published
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnUpdates: TtgOnUpdates read FOnUpdates write FOnUpdates;
    property OnUpdate: TtgOnUpdate read FOnUpdate write FOnUpdate;
    property OnMessage: TtgOnMessage read FOnMessage write FOnMessage;
    property OnInlineQuery: TtgOnInlineQuery read FOnInlineQuery write FOnInlineQuery;
    property OnChosenInlineResult: TtgOnInlineResultChosen read FOnChosenInlineResult write FOnChosenInlineResult;
    property OnEditedMessage: TtgOnMessage read FOnEditedMessage write FOnEditedMessage;
    property OnChannelPost: TtgOnMessage read FOnChannelPost write FOnChannelPost;
    property OnEditedChannelPost: TtgOnMessage read FOnEditedChannelPost write FOnEditedChannelPost;
    property OnShippingQuery: TtgOnShippingQuery read FOnShippingQuery write FOnShippingQuery;
    property OnPreCheckoutQuery: TtgOnPreCheckoutQuery read FOnPreCheckoutQuery write FOnPreCheckoutQuery;
  end;

implementation

{ TtgRecesiverConsole }

procedure TtgRecesiverUI.DoOnChannelPost(AChannelPost: ITgMessage);
begin
  inherited;
  if Assigned(OnChannelPost) then
    OnChannelPost(Self, AChannelPost);
end;

procedure TtgRecesiverUI.DoOnChosenInlineResult(AChosenInlineResult: ItgChosenInlineResult);
begin
  inherited;
  if Assigned(OnChosenInlineResult) then
    OnChosenInlineResult(Self, AChosenInlineResult);
end;

procedure TtgRecesiverUI.DoOnEditedChannelPost(AEditedChannelPost: ITgMessage);
begin
  inherited;
  if Assigned(OnEditedChannelPost) then
    OnEditedChannelPost(Self, AEditedChannelPost);
end;

procedure TtgRecesiverUI.DoOnEditedMessage(AEditedMessage: ITgMessage);
begin
  inherited;
  if Assigned(OnEditedMessage) then
    OnEditedMessage(Self, AEditedMessage);
end;

procedure TtgRecesiverUI.DoOnInlineQuery(AInlineQuery: ItgInlineQuery);
begin
  inherited;
  if Assigned(OnInlineQuery) then
    OnInlineQuery(Self, AInlineQuery);
end;

procedure TtgRecesiverUI.DoOnMessage(AMessage: ITgMessage);
begin
  inherited;
  if Assigned(OnMessage) then
    OnMessage(Self, AMessage);
end;

procedure TtgRecesiverUI.DoOnPreCheckoutQuery(APreCheckoutQuery: ItgPreCheckoutQuery);
begin
  inherited;
  if Assigned(OnPreCheckoutQuery) then
    OnPreCheckoutQuery(Self, APreCheckoutQuery);
end;

procedure TtgRecesiverUI.DoOnShippingQuery(AShippingQuery: ItgShippingQuery);
begin
  inherited;
  if Assigned(OnShippingQuery) then
    OnShippingQuery(Self, AShippingQuery);
end;

procedure TtgRecesiverUI.DoOnStart;
begin
  inherited;
  if Assigned(OnStart) then
    OnStart(Self);
end;

procedure TtgRecesiverUI.DoOnStop;
begin
  inherited;
  if Assigned(OnStop) then
    OnStop(Self);
end;

procedure TtgRecesiverUI.DoOnUpdate(AUpdate: ItgUpdate);
begin
  inherited;
  if Assigned(OnUpdate) then
    OnUpdate(Self, AUpdate);
end;

procedure TtgRecesiverUI.DoOnUpdates(AUpdates: TArray<ItgUpdate>);
begin
  inherited;
  if Assigned(OnUpdates) then
    OnUpdates(Self, AUpdates);
end;

end.

