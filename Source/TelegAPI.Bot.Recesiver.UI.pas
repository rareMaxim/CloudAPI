unit TelegAPI.Bot.Recesiver.UI;

interface

uses
  System.Classes,
  TelegAPI.Base,
  TelegAPI.Bot,
  TelegAPI.Types;

type
  TtgOnUpdate = procedure(ASender: TObject; AUpdate: TtgUpdate) of object;

  TtgOnUpdates = procedure(ASender: TObject; AUpdates: TArray<TtgUpdate>) of object;

  TtgOnMessage = procedure(ASender: TObject; AMessage: TTgMessage) of object;

  TtgOnInlineQuery = procedure(ASender: TObject; AInlineQuery: TtgInlineQuery) of object;

  TtgOnInlineResultChosen = procedure(ASender: TObject; AChosenInlineResult: TtgChosenInlineResult) of object;

  TtgOnCallbackQuery = procedure(ASender: TObject; ACallbackQuery: TtgCallbackQuery) of object;

  TtgOnChannelPost = procedure(ASender: TObject; AChanelPost: TTgMessage) of object;

  TTgBotRecesiverUICore = class;

  TTgBotRecesiverUI = class(TtgAbstractComponent)
  private
    FBot: TTelegramBot;
    FIsReceiving: Boolean;
    FMessageOffset: Integer;
    FOnCallbackQuery: TtgOnCallbackQuery;
    FOnChannelPost: TtgOnChannelPost;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnInlineQuery: TtgOnInlineQuery;
    FOnInlineResultChosen: TtgOnInlineResultChosen;
    FOnMessage: TtgOnMessage;
    FOnMessageEdited: TtgOnMessage;
    FOnUpdate: TtgOnUpdate;
    FOnUpdates: TtgOnUpdates;
    FPollingInterval: Integer;
    FRecesiver: TTgBotRecesiverUICore;
    FUseSynchronize: Boolean;
  protected
    procedure SetIsReceiving(const Value: Boolean);
    procedure SetUseSynchronize(const Value: Boolean);
    procedure DoDisconnect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
{$REGION 'Property|Свойства'}
    /// <summary>
    /// <para>
    /// Indicates if receiving updates
    /// </para>
    /// <para>
    /// Асинхронный прием обновлений от сервера
    /// </para>
    /// </summary>
    property IsReceiving: Boolean read FIsReceiving write SetIsReceiving default False;
    /// <summary>
    /// Асинхронные вызовы событий (небезопасно, если при этом обновляется
    /// UI)
    /// </summary>
    property UseSynchronize: Boolean read FUseSynchronize write SetUseSynchronize default True;
    {$REGION 'Property|Свойства'}
    property Bot: TTelegramBot read FBot write FBot;
    /// <summary>
    ///   The current message offset
    /// </summary>
    property MessageOffset: Integer read FMessageOffset write FMessageOffset default 0;
    /// <summary>
    ///   Задержка между опросами
    /// </summary>
    property PollingInterval: Integer read FPollingInterval write FPollingInterval default 1000;
    {$ENDREGION}
{$ENDREGION}
{$REGION 'Events|События'}
    /// <summary>
    /// <para>
    /// Событие возникает когда получено <see cref="TelegAPi.Types|TtgUpdate" />
    /// </para>
    /// <para>
    /// Occurs when an <see cref="TelegAPi.Types|TtgUpdate" /> is
    /// received.
    /// </para>
    /// </summary>
    property OnUpdate: TtgOnUpdate read FOnUpdate write FOnUpdate;
    property OnUpdates: TtgOnUpdates read FOnUpdates write FOnUpdates;
    /// <summary>
    /// <para>
    /// Событие возникает когда получено <see cref="TelegAPi.Types|TtgMessage" />
    /// </para>
    /// <para>
    /// Occurs when a <see cref="TelegAPi.Types|TtgMessage" /> is
    /// recieved.
    /// </para>
    /// </summary>
    property OnMessage: TtgOnMessage read FOnMessage write FOnMessage;
    /// <summary>
    /// <para>
    /// Возникает когда <see cref="TelegAPi.Types|TtgMessage" /> было
    /// изменено.
    /// </para>
    /// <para>
    /// Occurs when <see cref="TelegAPi.Types|TtgMessage" /> was edited.
    /// </para>
    /// </summary>
    property OnMessageEdited: TtgOnMessage read FOnMessageEdited write FOnMessageEdited;
    property OnChannelPost: TtgOnChannelPost read FOnChannelPost write FOnChannelPost;
    /// <summary>
    /// <para>
    /// Возникает, когда получен <see cref="TelegAPi.Types|TtgInlineQuery" />
    /// </para>
    /// <para>
    /// Occurs when an <see cref="TelegAPi.Types|TtgInlineQuery" /> is
    /// received.
    /// </para>
    /// </summary>
    property OnInlineQuery: TtgOnInlineQuery read FOnInlineQuery write FOnInlineQuery;
    /// <summary>
    /// <para>
    /// Возникает когда получен <see cref="TelegAPi.Types|TtgChosenInlineResult" />
    /// </para>
    /// <para>
    /// Occurs when a <see cref="TelegAPi.Types|TtgChosenInlineResult" />
    /// is received.
    /// </para>
    /// </summary>
    property OnInlineResultChosen: TtgOnInlineResultChosen read FOnInlineResultChosen write FOnInlineResultChosen;
    /// <summary>
    /// <para>
    /// Возникает когда получен <see cref="TelegAPi.Types|TtgCallbackQuery" />
    /// </para>
    /// <para>
    /// Occurs when an <see cref="TelegAPi.Types|TtgCallbackQuery" /> is
    /// received
    /// </para>
    /// </summary>
    property OnCallbackQuery: TtgOnCallbackQuery read FOnCallbackQuery write FOnCallbackQuery;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
{$ENDREGION}
  end;

  TTgBotRecesiverUICore = class(TThread)
  private
    FUseSynchronize: Boolean;
    FParent: TTgBotRecesiverUI;
  protected
    procedure DoOnUpdates(AUpdates: TArray<TtgUpdate>);
    procedure DoOnUpdate(AUpdate: TtgUpdate); virtual;
    procedure DoUpdateWorker(AUpdates: TArray<TtgUpdate>);
    function DoGetUpdates: TArray<TtgUpdate>;
    /// <summary>
    /// Raises the <see cref="TelegAPI.Bot|TtgOnUpdate" />, <see cref="TelegAPI.Bot|TtgOnMessage" />
    /// , <see cref="TelegAPI.Bot|TtgOnInlineQuery" /> , <see cref="TelegAPI.Bot|TtgOnInlineResultChosen" />
    /// and <see cref="TelegAPI.Bot|TtgOnCallbackQuery" /> events.
    /// </summary>
    /// <param name="AValue">
    /// The <see cref="TelegAPi.Types|TtgUpdate">Update</see> instance
    /// containing the event data. <br />
    /// </param>
    /// <exception cref="TelegaPi.Exceptions|ETelegramException">
    /// Возникает если получено неизвестное обновление
    /// </exception>
    procedure DoUpdateTypeParser(AValue: TtgUpdate);
    procedure Execute; override;
  public
    property Parent: TTgBotRecesiverUI read FParent write FParent;
    property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
  end;

implementation

uses
  System.SysUtils,
  TelegAPI.Exceptions,
  TelegAPI.Types.Enums;
{ TTgRecesiver.TtgAsync }

function TTgBotRecesiverUICore.DoGetUpdates: TArray<TtgUpdate>;
begin
  try
    Result := Parent.Bot.GetUpdates(Parent.MessageOffset, 100, 0, Parent.Bot.AllowedUpdates);
  except
    on E: Exception do
      Parent.Bot.ErrorHandlerGeneral(E);
  end;
end;

procedure TTgBotRecesiverUICore.DoOnUpdate(AUpdate: TtgUpdate);
begin
  if not Assigned(Parent.OnUpdate) then
    Exit;
  if UseSynchronize then
    TThread.Synchronize(nil,
      procedure
      begin
        Parent.OnUpdate(Self, AUpdate);
      end)
  else
    Parent.OnUpdate(Self, AUpdate);
end;

procedure TTgBotRecesiverUICore.DoOnUpdates(AUpdates: TArray<TtgUpdate>);
begin
  if not Assigned(Parent.OnUpdates) then
    Exit;
  if FUseSynchronize then
    TThread.Synchronize(nil,
      procedure
      begin
        Parent.OnUpdates(Parent, AUpdates);
      end)
  else
  begin
    Parent.OnUpdates(Parent, AUpdates);
  end;
end;

procedure TTgBotRecesiverUICore.DoUpdateWorker(AUpdates: TArray<TtgUpdate>);
var
  I: Integer;
begin
  DoOnUpdates(AUpdates); // OnUpdates Fire
  for I := Low(AUpdates) to High(AUpdates) do
  begin
    DoOnUpdate(AUpdates[I]); // OnUpdate Fire
    DoUpdateTypeParser(AUpdates[I]);
    FreeAndNil(AUpdates[I]);
  end;
end;

procedure TTgBotRecesiverUICore.Execute;
var
  LUpdates: TArray<TtgUpdate>;
begin
  if Assigned(Parent.OnConnect) then
    Parent.OnConnect(Self);
  try
    repeat
      LUpdates := DoGetUpdates;
      if (Length(LUpdates) > 0) and (not Terminated) then
      begin
        Parent.MessageOffset := LUpdates[High(LUpdates)].ID + 1;
        Self.DoUpdateWorker(LUpdates); // free update items
      end;
      Sleep(Parent.PollingInterval);
    until (Terminated) or (not Parent.IsReceiving);
  finally
    LUpdates := nil;
  end;
end;

procedure TTgBotRecesiverUICore.DoUpdateTypeParser(AValue: TtgUpdate);
begin
  case AValue.&Type of
    TtgUpdateType.MessageUpdate:
      if Assigned(Parent.OnMessage) then
        Parent.OnMessage(Self, AValue.Message);
    TtgUpdateType.InlineQueryUpdate:
      if Assigned(Parent.OnInlineQuery) then
        Parent.OnInlineQuery(Self, AValue.InlineQuery);
    TtgUpdateType.ChosenInlineResultUpdate:
      if Assigned(Parent.OnInlineResultChosen) then
        Parent.OnInlineResultChosen(Self, AValue.ChosenInlineResult);
    TtgUpdateType.CallbackQueryUpdate:
      if Assigned(Parent.OnCallbackQuery) then
        Parent.OnCallbackQuery(Self, AValue.CallbackQuery);
    TtgUpdateType.EditedMessage:
      if Assigned(Parent.OnMessageEdited) then
        Parent.OnMessageEdited(Self, AValue.EditedMessage);
    TtgUpdateType.ChannelPost:
      if Assigned(Parent.OnChannelPost) then
        Parent.OnChannelPost(Self, AValue.ChannelPost);
  else
    raise ETelegramException.Create('Unknown update type');
  end
end;

{ TTgBotAsync }

constructor TTgBotRecesiverUI.Create(AOwner: TComponent);
begin
  inherited;
  FIsReceiving := False;
  FUseSynchronize := True;
  FPollingInterval := 1000;
  FMessageOffset := 0;
end;

destructor TTgBotRecesiverUI.Destroy;
begin
  { TODO -oM.E.Sysoev -cGeneral : Проверить, возможно стоит удалить? }
  if IsReceiving then
    IsReceiving := False;
  inherited;
end;

procedure TTgBotRecesiverUI.DoDisconnect(Sender: TObject);
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(Sender);
end;

procedure TTgBotRecesiverUI.SetIsReceiving(const Value: Boolean);
begin
  // duplicate FReceiver creation and freeing protection
  if (FIsReceiving = Value) then
    Exit;
  // if not Assigned(Bot) then
  // raise ETelegramException.Create('Property "Bot" must be assigned');
  FIsReceiving := Value;
  // if (csDesigning in ComponentState) then
  // Exit;
  if Value then
  begin
    FRecesiver := TTgBotRecesiverUICore.Create(True);
    FRecesiver.FreeOnTerminate := False;
    FRecesiver.FParent := Self;
    FRecesiver.OnTerminate := DoDisconnect;
    FRecesiver.Start;
  end
  else
  begin
    FreeAndNil(FRecesiver);
  end;
end;

procedure TTgBotRecesiverUI.SetUseSynchronize(const Value: Boolean);
begin
  if Assigned(FRecesiver) and (Value = FRecesiver.UseSynchronize) then
    Exit;
  if IsReceiving then
    IsReceiving := False;
  FRecesiver.UseSynchronize := Value;
  FUseSynchronize := Value;
end;

end.

