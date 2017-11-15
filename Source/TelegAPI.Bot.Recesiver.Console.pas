unit TelegAPI.Bot.Recesiver.Console;

interface

uses
  System.Classes,
  System.SysUtils,
  TelegAPI.Base,
  TelegAPI.Bot.Intf,
  TelegAPI.Types,
  TelegAPI.Types.Intf,
  TelegAPI.Types.Enums;

type

  TTgBotRecesiverConsole = class(TtgAbstractComponent)
  private
    FBot: ITelegramBot;
    FIsReceiving: Boolean;
    FMessageOffset: Integer;
    FOnCallbackQuery: TProc<ItgCallbackQuery>;
    FOnChannelPost: TProc<ITgMessage>;
    FOnConnect: TProc;
    FOnDisconnect: TProc;
    FOnInlineQuery: TProc<ItgInlineQuery>;
    FOnInlineResultChosen: TProc<ItgChosenInlineResult>;
    FOnMessage: TProc<ITgMessage>;
    FOnMessageEdited: TProc<ITgMessage>;
    FOnUpdate: TProc<ItgUpdate>;
    FOnUpdates: TProc<TArray<ItgUpdate>>;
    FPollingInterval: Integer;
    FAllowedUpdates: TAllowedUpdates;
  protected
    procedure DoConnect;
    procedure DoDisconnect;
    procedure DoOnTerminate(Sender: TObject);
    procedure SetIsReceiving(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    // constructor Create(ABot:ITelegramBot) ;
    destructor Destroy; override;
  published
    /// <summary>
    /// <para>
    /// Indicates if receiving updates
    /// </para>
    /// <para>
    /// Асинхронный прием обновлений от сервера
    /// </para>
    /// </summary>
    property IsReceiving: Boolean read FIsReceiving write SetIsReceiving
      default False;
{$REGION 'Property|Свойства'}
    property Bot: ITelegramBot read FBot write FBot;
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates
      write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    /// <summary>
    /// The current message offset
    /// </summary>
    property MessageOffset: Integer read FMessageOffset write FMessageOffset
      default 0;
    /// <summary>
    /// Задержка между опросами
    /// </summary>
    property PollingInterval: Integer read FPollingInterval
      write FPollingInterval default 1000;
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
    property OnUpdate: TProc<ItgUpdate> read FOnUpdate write FOnUpdate;
    property OnUpdates: TProc < TArray < ItgUpdate >> read FOnUpdates
      write FOnUpdates;
    /// <summary>
    /// <para>
    /// Событие возникает когда получено <see cref="TelegAPi.Types|TtgMessage" />
    /// </para>
    /// <para>
    /// Occurs when a <see cref="TelegAPi.Types|TtgMessage" /> is
    /// recieved.
    /// </para>
    /// </summary>
    property OnMessage: TProc<ITgMessage> read FOnMessage write FOnMessage;
    /// <summary>
    /// <para>
    /// Возникает когда <see cref="TelegAPi.Types|TtgMessage" /> было
    /// изменено.
    /// </para>
    /// <para>
    /// Occurs when <see cref="TelegAPi.Types|TtgMessage" /> was edited.
    /// </para>
    /// </summary>
    property OnMessageEdited: TProc<ITgMessage> read FOnMessageEdited
      write FOnMessageEdited;
    property OnChannelPost: TProc<ITgMessage> read FOnChannelPost
      write FOnChannelPost;
    /// <summary>
    /// <para>
    /// Возникает, когда получен <see cref="TelegAPi.Types|TtgInlineQuery" />
    /// </para>
    /// <para>
    /// Occurs when an <see cref="TelegAPi.Types|TtgInlineQuery" /> is
    /// received.
    /// </para>
    /// </summary>
    property OnInlineQuery: TProc<ItgInlineQuery> read FOnInlineQuery
      write FOnInlineQuery;
    /// <summary>
    /// <para>
    /// Возникает когда получен <see cref="TelegAPi.Types|TtgChosenInlineResult" />
    /// </para>
    /// <para>
    /// Occurs when a <see cref="TelegAPi.Types|TtgChosenInlineResult" />
    /// is received.
    /// </para>
    /// </summary>
    property OnInlineResultChosen: TProc<ItgChosenInlineResult>
      read FOnInlineResultChosen write FOnInlineResultChosen;
    /// <summary>
    /// <para>
    /// Возникает когда получен <see cref="TelegAPi.Types|TtgCallbackQuery" />
    /// </para>
    /// <para>
    /// Occurs when an <see cref="TelegAPi.Types|TtgCallbackQuery" /> is
    /// received
    /// </para>
    /// </summary>
    property OnCallbackQuery: TProc<ItgCallbackQuery> read FOnCallbackQuery
      write FOnCallbackQuery;
    property OnConnect: TProc read FOnConnect write FOnConnect;
    property OnDisconnect: TProc read FOnDisconnect write FOnDisconnect;
{$ENDREGION}
  end;

implementation


//
// procedure TtgRecesiverConsoleCore.DoUpdateTypeParser(AValue: ItgUpdate);
// begin
// case AValue.&Type of
// TtgUpdateType.MessageUpdate:
// if Assigned(Parent.OnMessage) then
// Parent.OnMessage(AValue.Message);
// TtgUpdateType.InlineQueryUpdate:
// if Assigned(Parent.OnInlineQuery) then
// Parent.OnInlineQuery(AValue.InlineQuery);
// TtgUpdateType.ChosenInlineResultUpdate:
// if Assigned(Parent.OnInlineResultChosen) then
// Parent.OnInlineResultChosen(AValue.ChosenInlineResult);
// TtgUpdateType.CallbackQueryUpdate:
// if Assigned(Parent.OnCallbackQuery) then
// Parent.OnCallbackQuery(AValue.CallbackQuery);
// TtgUpdateType.EditedMessage:
// if Assigned(Parent.OnMessageEdited) then
// Parent.OnMessageEdited(AValue.EditedMessage);
// TtgUpdateType.ChannelPost:
// if Assigned(Parent.OnChannelPost) then
// Parent.OnChannelPost(AValue.ChannelPost);
// else
// raise ETelegramException.Create('Unknown update type');
// end
// end;

{ TTgBotRecesiverConsole }

constructor TTgBotRecesiverConsole.Create(AOwner: TComponent);
begin
  inherited;
  FIsReceiving := False;
  FPollingInterval := 1000;
  FMessageOffset := 0;
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

end;

end.
