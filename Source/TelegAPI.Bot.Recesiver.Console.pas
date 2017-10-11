unit TelegAPI.Bot.Recesiver.Console;

interface

uses
  System.Classes,
  TelegAPI.Bot,
  TelegAPI.Types,
  System.SysUtils,
  TelegAPI.Bot.Recesiver.abstract;

type
  TtgRecesiverConsoleCore = class;

  TTgBotRecesiverConsole = class(TtgAbstractRecesiver)
  private
    FRecesiver: TtgRecesiverConsoleCore;
    FIsReceiving: Boolean;
    FOnUpdate: TProc<TtgUpdate>;
    FOnMessageEdited: TProc<TTgMessage>;
    FOnChannelPost: TProc<TTgMessage>;
    FOnCallbackQuery: TProc<TtgCallbackQuery>;
    FOnMessage: TProc<TTgMessage>;
    FOnInlineQuery: TProc<TtgInlineQuery>;
    FOnInlineResultChosen: TProc<TtgChosenInlineResult>;
    FOnUpdates: TProc<TArray<TtgUpdate>>;
    FOnConnect: TProc;
    FOnDisconnect: TProc;
  protected
    procedure SetIsReceiving(const Value: Boolean);
    procedure DoOnTerminate(Sender: TObject);
    procedure DoDisconnect;
    procedure DoConnect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  /// <summary>
  ///   <para>
  ///     Indicates if receiving updates
  ///   </para>
  ///   <para>
  ///     Асинхронный прием обновлений от сервера
  ///   </para>
  /// </summary>
    property IsReceiving: Boolean read FIsReceiving write SetIsReceiving default False;
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
    property OnUpdate: TProc<TtgUpdate> read FOnUpdate write FOnUpdate;
    property OnUpdates: TProc<TArray<TtgUpdate>> read FOnUpdates write FOnUpdates;
    /// <summary>
    ///   <para>
    ///     Событие возникает когда получено <see cref="TelegAPi.Types|TtgMessage" />
    ///   </para>
    ///   <para>
    ///     Occurs when a <see cref="TelegAPi.Types|TtgMessage" /> is
    ///     recieved.
    ///   </para>
    /// </summary>
    property OnMessage: TProc<TTgMessage> read FOnMessage write FOnMessage;
    /// <summary>
    ///   <para>
    ///     Возникает когда <see cref="TelegAPi.Types|TtgMessage" /> было
    ///     изменено.
    ///   </para>
    ///   <para>
    ///     Occurs when <see cref="TelegAPi.Types|TtgMessage" /> was edited.
    ///   </para>
    /// </summary>
    property OnMessageEdited: TProc<TTgMessage> read FOnMessageEdited write FOnMessageEdited;
    property OnChannelPost: TProc<TTgMessage> read FOnChannelPost write FOnChannelPost;
    /// <summary>
    ///   <para>
    ///     Возникает, когда получен <see cref="TelegAPi.Types|TtgInlineQuery" />
    ///   </para>
    ///   <para>
    ///     Occurs when an <see cref="TelegAPi.Types|TtgInlineQuery" /> is
    ///     received.
    ///   </para>
    /// </summary>
    property OnInlineQuery: TProc<TtgInlineQuery> read FOnInlineQuery write FOnInlineQuery;
    /// <summary>
    ///   <para>
    ///     Возникает когда получен <see cref="TelegAPi.Types|TtgChosenInlineResult" />
    ///   </para>
    ///   <para>
    ///     Occurs when a <see cref="TelegAPi.Types|TtgChosenInlineResult" />
    ///     is received.
    ///   </para>
    /// </summary>
    property OnInlineResultChosen: TProc<TtgChosenInlineResult> read FOnInlineResultChosen write FOnInlineResultChosen;
    /// <summary>
    ///   <para>
    ///     Возникает когда получен <see cref="TelegAPi.Types|TtgCallbackQuery" />
    ///   </para>
    ///   <para>
    ///     Occurs when an <see cref="TelegAPi.Types|TtgCallbackQuery" /> is
    ///     received
    ///   </para>
    /// </summary>
    property OnCallbackQuery: TProc<TtgCallbackQuery> read FOnCallbackQuery write FOnCallbackQuery;
    property OnConnect: TProc read FOnConnect write FOnConnect;
    property OnDisconnect: TProc read FOnDisconnect write FOnDisconnect;
{$ENDREGION}
  end;

  TtgRecesiverConsoleCore = class(TtgRecesiverAbstractCore)
  private
    FParent: TTgBotRecesiverConsole;
  protected
    procedure DoOnUpdates(AUpdates: TArray<TtgUpdate>);
    procedure DoOnUpdate(AUpdate: TtgUpdate); virtual;
    procedure DoUpdateWorker(AUpdates: TArray<TtgUpdate>);
    function DoGetUpdates: TArray<TtgUpdate>;
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
    procedure DoUpdateTypeParser(AValue: TtgUpdate);
    procedure Execute; override;
  public
    property Parent: TTgBotRecesiverConsole read FParent write FParent;
  end;

implementation

uses
  TelegAPI.Exceptions,
  TelegAPI.Types.Enums;
{ TTgRecesiver.TtgAsync }

function TtgRecesiverConsoleCore.DoGetUpdates: TArray<TtgUpdate>;
begin
  try
    Result := Parent.Bot.GetUpdates(Parent.MessageOffset, 100, 0, Parent.Bot.AllowedUpdates);
  except
    on E: Exception do
      Parent.Bot.ErrorHandlerGeneral(E);
  end;
end;

procedure TtgRecesiverConsoleCore.DoOnUpdate(AUpdate: TtgUpdate);
begin
  if not Assigned(Parent.OnUpdate) then
    Exit;
  Parent.OnUpdate(AUpdate);
end;

procedure TtgRecesiverConsoleCore.DoOnUpdates(AUpdates: TArray<TtgUpdate>);
begin
  if not Assigned(Parent.OnUpdates) then
    Exit;
  Parent.OnUpdates(AUpdates);
end;

procedure TtgRecesiverConsoleCore.DoUpdateWorker(AUpdates: TArray<TtgUpdate>);
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

procedure TtgRecesiverConsoleCore.Execute;
var
  LUpdates: TArray<TtgUpdate>;
begin
  Parent.DoConnect;
  try
    repeat
      LUpdates := DoGetUpdates;
      if (Length(LUpdates) > 0) and (not Terminated) then
      begin
        Parent.MessageOffset := LUpdates[High(LUpdates)].ID + 1;
        Self.DoUpdateWorker(LUpdates);  // free update items
      end;
      Sleep(Parent.PollingInterval);
    until (Terminated) or (not Parent.IsReceiving);
  finally
    LUpdates := nil;
  end;
end;

procedure TtgRecesiverConsoleCore.DoUpdateTypeParser(AValue: TtgUpdate);
begin
  case AValue.&Type of
    TtgUpdateType.MessageUpdate:
      if Assigned(Parent.OnMessage) then
        Parent.OnMessage(AValue.Message);
    TtgUpdateType.InlineQueryUpdate:
      if Assigned(Parent.OnInlineQuery) then
        Parent.OnInlineQuery(AValue.InlineQuery);
    TtgUpdateType.ChosenInlineResultUpdate:
      if Assigned(Parent.OnInlineResultChosen) then
        Parent.OnInlineResultChosen(AValue.ChosenInlineResult);
    TtgUpdateType.CallbackQueryUpdate:
      if Assigned(Parent.OnCallbackQuery) then
        Parent.OnCallbackQuery(AValue.CallbackQuery);
    TtgUpdateType.EditedMessage:
      if Assigned(Parent.OnMessageEdited) then
        Parent.OnMessageEdited(AValue.EditedMessage);
    TtgUpdateType.ChannelPost:
      if Assigned(Parent.OnChannelPost) then
        Parent.OnChannelPost(AValue.ChannelPost);
  else
    raise ETelegramException.Create('Unknown update type');
  end
end;

{ TTgBotAsync }

constructor TTgBotRecesiverConsole.Create(AOwner: TComponent);
begin
  inherited;
  FIsReceiving := False;
end;

destructor TTgBotRecesiverConsole.Destroy;
begin
  { TODO -oM.E.Sysoev -cGeneral : Проверить, возможно стоит удалить? }
  if IsReceiving then
    IsReceiving := False;
  inherited;
end;

procedure TTgBotRecesiverConsole.DoConnect;
begin
  if Assigned(OnConnect) then
    OnConnect;
end;

procedure TTgBotRecesiverConsole.DoDisconnect;
begin
  if Assigned(OnDisconnect) then
    OnDisconnect;
end;

procedure TTgBotRecesiverConsole.DoOnTerminate(Sender: TObject);
begin
  DoDisconnect;
end;

procedure TTgBotRecesiverConsole.SetIsReceiving(const Value: Boolean);
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
    FRecesiver := TtgRecesiverConsoleCore.Create(True);
    FRecesiver.FreeOnTerminate := False;
    FRecesiver.FParent := Self;
    FRecesiver.OnTerminate := DoOnTerminate;
    FRecesiver.Start;
  end
  else
  begin
    FreeAndNil(FRecesiver);
  end;
end;

end.

