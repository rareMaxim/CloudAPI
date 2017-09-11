unit TelegAPI.Bot.Recesiver;

interface

uses
  System.Classes,
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

  TTgBotAsync = class(TComponent)
  private
    type
      TtgRecesiver = class(TThread)
      private
        FUseSynchronize: Boolean;
        FParent: TTgBotAsync;
      protected
        procedure DoOnUpdates(AUpdates: TArray<TtgUpdate>);
        procedure DoOnUpdate(AUpdates: TArray<TtgUpdate>); virtual;
        function GetUpdates: TArray<TtgUpdate>;
      /// <summary>
      ///   Raises the <see cref="TelegAPI.Bot|TtgOnUpdate" />, <see cref="TelegAPI.Bot|TtgOnMessage" />
      ///    , <see cref="TelegAPI.Bot|TtgOnInlineQuery" /> , <see cref="TelegAPI.Bot|TtgOnInlineResultChosen" />
      ///    and <see cref="TelegAPI.Bot|TtgOnCallbackQuery" /> events.
      /// </summary>
      /// <param name="AValue">
      ///   The <see cref="TelegAPi.Types|TtgUpdate">Update</see> instance
      ///   containing the event data. <br />
      /// </param>
      /// <exception cref="TelegaPi.Exceptions|ETelegramException">
      ///   Возникает если получено неизвестное обновление
      /// </exception>
        procedure OnUpdateReceived(AValue: TtgUpdate);
        procedure Execute; override;
      public
        property Parent: TTgBotAsync read FParent write FParent;
        property UseSynchronize: Boolean read FUseSynchronize write FUseSynchronize;
      end;
  private
    FBot: TTelegramBot;
    FRecesiver: TtgRecesiver;
    FIsReceiving: Boolean;
    FPollingInterval: Integer;
    FOnUpdate: TtgOnUpdate;
    FOnMessageEdited: TtgOnMessage;
    FOnChannelPost: TtgOnChannelPost;
    FOnCallbackQuery: TtgOnCallbackQuery;
    FOnMessage: TtgOnMessage;
    FOnInlineQuery: TtgOnInlineQuery;
    FOnInlineResultChosen: TtgOnInlineResultChosen;
    FOnUpdates: TtgOnUpdates;
    FUseSynchronize: Boolean;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FMessageOffset: Integer;
    procedure SetUseSynchronize(const Value: Boolean);
  protected
    procedure SetIsReceiving(const Value: Boolean);
    procedure DoDisconnect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
{$REGION 'Property|Свойства'}
    property Bot: TTelegramBot read FBot write FBot;
    /// <summary>
    ///   <para>
    ///     Indicates if receiving updates
    ///   </para>
    ///   <para>
    ///     Асинхронный прием обновлений от сервера
    ///   </para>
    /// </summary>
    property IsReceiving: Boolean read FIsReceiving write SetIsReceiving default False;
    /// <summary>
    ///   The current message offset
    /// </summary>
    property MessageOffset: Integer read FMessageOffset write FMessageOffset default 0;
    /// <summary>
    ///   Задержка между опросами
    /// </summary>
    property PollingInterval: Integer read FPollingInterval write FPollingInterval default 1000;
    /// <summary>
    ///   Асинхронные вызовы событий (небезопасно, если при этом обновляется
    ///   UI)
    /// </summary>
    property UseSynchronize: Boolean read FUseSynchronize write SetUseSynchronize default True;
{$ENDREGION}
{$REGION 'Events|События'}
/// <summary>
///   <para>
///     Событие возникает когда получено <see cref="TelegAPi.Types|TtgUpdate" />
///   </para>
///   <para>
///     Occurs when an <see cref="TelegAPi.Types|TtgUpdate" /> is
///     received.
///   </para>
/// </summary>
    property OnUpdate: TtgOnUpdate read FOnUpdate write FOnUpdate;
    property OnUpdates: TtgOnUpdates read FOnUpdates write FOnUpdates;
/// <summary>
///   <para>
///     Событие возникает когда получено <see cref="TelegAPi.Types|TtgMessage" />
///   </para>
///   <para>
///     Occurs when a <see cref="TelegAPi.Types|TtgMessage" /> is
///     recieved.
///   </para>
/// </summary>
    property OnMessage: TtgOnMessage read FOnMessage write FOnMessage;
/// <summary>
///   <para>
///     Возникает когда <see cref="TelegAPi.Types|TtgMessage" /> было
///     изменено.
///   </para>
///   <para>
///     Occurs when <see cref="TelegAPi.Types|TtgMessage" /> was edited.
///   </para>
/// </summary>
    property OnMessageEdited: TtgOnMessage read FOnMessageEdited write FOnMessageEdited;
    property OnChannelPost: TtgOnChannelPost read FOnChannelPost write FOnChannelPost;
/// <summary>
///   <para>
///     Возникает, когда получен <see cref="TelegAPi.Types|TtgInlineQuery" />
///   </para>
///   <para>
///     Occurs when an <see cref="TelegAPi.Types|TtgInlineQuery" /> is
///     received.
///   </para>
/// </summary>
    property OnInlineQuery: TtgOnInlineQuery read FOnInlineQuery write FOnInlineQuery;
/// <summary>
///   <para>
///     Возникает когда получен <see cref="TelegAPi.Types|TtgChosenInlineResult" />
///   </para>
///   <para>
///     Occurs when a <see cref="TelegAPi.Types|TtgChosenInlineResult" />
///     is received.
///   </para>
/// </summary>
    property OnInlineResultChosen: TtgOnInlineResultChosen read FOnInlineResultChosen write FOnInlineResultChosen;
/// <summary>
///   <para>
///     Возникает когда получен <see cref="TelegAPi.Types|TtgCallbackQuery" />
///   </para>
///   <para>
///     Occurs when an <see cref="TelegAPi.Types|TtgCallbackQuery" /> is
///     received
///   </para>
/// </summary>
    property OnCallbackQuery: TtgOnCallbackQuery read FOnCallbackQuery write FOnCallbackQuery;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
{$ENDREGION}
  end;

implementation

uses
  System.SysUtils,
  TelegAPI.Exceptions,
  TelegAPI.Types.Enums;
{ TTgRecesiver.TtgAsync }

procedure TTgBotAsync.TtgRecesiver.DoOnUpdate(AUpdates: TArray<TtgUpdate>);
begin
  if not Assigned(Parent.OnUpdates) then
    Exit;
  if UseSynchronize then
    TThread.Synchronize(nil,
      procedure
      begin
        Parent.OnUpdates(Self, AUpdates);
      end)
  else
    Parent.OnUpdates(Self, AUpdates);
end;

procedure TTgBotAsync.TtgRecesiver.DoOnUpdates(AUpdates: TArray<TtgUpdate>);
var
  I: Integer;
begin
  for I := Low(AUpdates) to High(AUpdates) do
  begin
    if FUseSynchronize then
      TThread.Synchronize(nil,
        procedure
        begin
          Self.OnUpdateReceived(AUpdates[I]);
          FreeAndNil(AUpdates[I]);
        end)
    else
    begin
      Self.OnUpdateReceived(AUpdates[I]);
      FreeAndNil(AUpdates[I]);
    end;
  end;
end;

procedure TTgBotAsync.TtgRecesiver.Execute;
var
  LUpdates: TArray<TtgUpdate>;
begin
  if Assigned(Parent.OnConnect) then
    Parent.OnConnect(Self);
  repeat
    LUpdates := Parent.Bot.GetUpdates;
    if (Assigned(LUpdates)) and (Length(LUpdates) > 0) and (not Terminated) then
    begin
      Parent.MessageOffset := LUpdates[High(LUpdates)].ID + 1;
      Self.DoOnUpdates(LUpdates);
      Self.DoOnUpdate(LUpdates);
    end;
    Sleep(Parent.PollingInterval);
  until (Terminated) or (not Parent.IsReceiving);
end;

function TTgBotAsync.TtgRecesiver.GetUpdates: TArray<TtgUpdate>;
begin
  try
    Result := Parent.Bot.GetUpdates(Parent.MessageOffset, 100, 0, Parent.Bot.AllowedUpdates);
  except
    on E: Exception do
      Parent.Bot.ErrorHandlerGeneral(E);
  end;
end;

procedure TTgBotAsync.TtgRecesiver.OnUpdateReceived(AValue: TtgUpdate);
begin
  if Assigned(Parent.OnUpdate) then
    Parent.OnUpdate(Self, AValue);
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

constructor TTgBotAsync.Create(AOwner: TComponent);
begin
  inherited;
  FIsReceiving := False;
  FPollingInterval := 1000;
  FMessageOffset := 0;
  FUseSynchronize := True;
end;

destructor TTgBotAsync.Destroy;
begin
  {TODO -oM.E.Sysoev -cGeneral : Проверить, возможно стоит удалить?}
  FPollingInterval := 0;
  if IsReceiving then
    IsReceiving := False;
  inherited;
end;

procedure TTgBotAsync.DoDisconnect(Sender: TObject);
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(Sender);
end;

procedure TTgBotAsync.SetIsReceiving(const Value: Boolean);
begin
  // duplicate FReceiver creation and freeing protection
  if (FIsReceiving = Value) then
    Exit;
 // if not Assigned(Bot) then
 //   raise ETelegramException.Create('Property "Bot" must be assigned');
  FIsReceiving := Value;
  //  if (csDesigning in ComponentState) then
  //    Exit;
  if Value then
  begin
    FRecesiver := TtgRecesiver.Create(True);
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

procedure TTgBotAsync.SetUseSynchronize(const Value: Boolean);
begin
  if Value = FRecesiver.UseSynchronize then
    Exit;
  if IsReceiving then
    IsReceiving := False;
  FRecesiver.UseSynchronize := Value;
  FUseSynchronize := Value;
end;

end.

