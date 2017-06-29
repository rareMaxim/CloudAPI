unit TelegAPI.Bot;

interface

uses
  System.Rtti,
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.Net.Mime,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Generics.Collections,
  TelegAPI.Types,
  TelegAPI.Types.Enums,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Exceptions,
  XSuperObject;

type
  TtgOnUpdate = procedure(ASender: TObject; AUpdate: TtgUpdate) of object;

  TtgOnMessage = procedure(ASender: TObject; AMessage: TtgMessage) of object;

  TtgOnInlineQuery = procedure(ASender: TObject; AInlineQuery: TtgInlineQuery) of object;

  TtgOnInlineResultChosen = procedure(ASender: TObject; AChosenInlineResult: TtgChosenInlineResult) of object;

  TtgOnCallbackQuery = procedure(ASender: TObject; ACallbackQuery: TtgCallbackQuery) of object;

  TtgOnReceiveError = procedure(ASender: TObject; AApiRequestException: EApiRequestException) of object;

  TtgOnReceiveGeneralError = procedure(ASender: TObject; AException: Exception) of object;

  TtgOnReceiveRawData = procedure(ASender: TObject; const AData: string) of object;

  TTelegramBot = class;

  TtgRecesiver = class(TThread)
  private
    FBot: TTelegramBot;
  protected
    procedure Execute; override;
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
  public
    property Bot: TTelegramBot read FBot write FBot;
  end;

  TTelegramBotCore = class(TComponent)
  private
    FToken: string;
    FPollingTimeout: Integer;
    FMessageOffset: Integer;
    FRecesiver: TtgRecesiver;
    FIsReceiving: Boolean;
    FAllowedUpdates: TAllowedUpdates;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FProxySettings: TProxySettings;
    FOnUpdate: TtgOnUpdate;
    FOnMessage: TtgOnMessage;
    FOnMessageEdited: TtgOnMessage;
    FOnInlineQuery: TtgOnInlineQuery;
    FOnInlineResultChosen: TtgOnInlineResultChosen;
    FOnCallbackQuery: TtgOnCallbackQuery;
    FOnReceiveError: TtgOnReceiveError;
    FOnReceiveGeneralError: TtgOnReceiveGeneralError;
    FOnRawData: TtgOnReceiveRawData;
    procedure SetIsReceiving(const Value: Boolean);
    function GetVersionAPI: string;
    /// <summary>
    ///   Мастер-функция для запросов на сервак
    /// </summary>
    function API<T>(const Method: string; Parameters: TDictionary<string, TValue>): T;
    function ParamsToFormData(Parameters: TDictionary<string, TValue>): TMultipartFormData;
    function ArrayToString<T: class, constructor>(LArray: TArray<T>): string;
    procedure DoDisconnect(ASender: TObject);
  protected
    procedure ErrorHandler(AException: Exception);
  public
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    /// <summary>
    ///   <para>
    ///     Indicates if receiving updates
    ///   </para>
    ///   <para>
    ///     Асинхронный прием обновлений от сервера
    ///   </para>
    /// </summary>
    property IsReceiving: Boolean read FIsReceiving write SetIsReceiving default False;
  published
  {$REGION 'Property|Свойства'}
    /// <summary>
    ///   Proxy Settings to be used by the client.
    /// </summary>
    property ProxySettings: TProxySettings read FProxySettings write FProxySettings;
    /// <summary>
    ///   Задержка между опросами
    /// </summary>
    property PollingTimeout: Integer read FPollingTimeout write FPollingTimeout default 1000;
    /// <summary>
    ///   The current message offset
    /// </summary>
    property MessageOffset: Integer read FMessageOffset write FMessageOffset default 0;
    /// <summary>
    ///   <para>
    ///     List the types of updates you want your bot to receive.
    ///   </para>
    ///   <para>
    ///     Типы принимаемых сообщений
    ///   </para>
    /// </summary>
    property AllowedUpdates: TAllowedUpdates read FAllowedUpdates write FAllowedUpdates default UPDATES_ALLOWED_ALL;
    /// <summary>
    ///   Токен вашего бота.
    /// </summary>
    /// <remarks>
    ///   Создать бота и получить токен можно у @BotFather
    /// </remarks>
    /// <example>
    ///   283107813:AAG4hEElAvIogTSHNHXI6rZtE46A7XQvIH
    /// </example>
    property Token: string read FToken write FToken;
    /// <summary>
    ///   Поддерживаемая версия платформы BotAPI
    /// </summary>
    property VersionAPI: string read GetVersionAPI;
{$ENDREGION}
{$REGION 'События|Events'}
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
    /// <summary>
    ///   <para>
    ///     Возникает при возникновении ошибки во время запроса фоновых
    ///     обновлений.
    ///   </para>
    ///   <para>
    ///     Occurs when an error occures during the background update
    ///     pooling.
    ///   </para>
    /// </summary>
    property OnReceiveError: TtgOnReceiveError read FOnReceiveError write FOnReceiveError;
    /// <summary>
    ///   <para>
    ///     Возникает при возникновении ошибки во время запроса фоновых
    ///     обновлений.
    ///   </para>
    ///   <para>
    ///     Occurs when an error occures during the background update
    ///     pooling.
    ///   </para>
    /// </summary>
    property OnReceiveGeneralError: TtgOnReceiveGeneralError read FOnReceiveGeneralError write FOnReceiveGeneralError;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnReceiveRawData: TtgOnReceiveRawData read FOnRawData write FOnRawData;
{$ENDREGION}
  end;

  /// <summary>
  ///   <para>
  ///     Клиент для работы с Telegram Bot API
  ///   </para>
  ///   <para>
  ///     A client to use the Telegram Bot API
  ///   </para>
  /// </summary>
  TTelegramBot = class(TTelegramBotCore)
  public
{$REGION 'Getting updates'}
    /// <summary>
    ///   <para>
    ///     Используйте этот метод для получения обновлений используя long
    ///     polling.
    ///   </para>
    ///   <para>
    ///     Use this method to receive incoming updates using long polling.
    ///   </para>
    /// </summary>
    /// <param name="Offset">
    ///   Identifier of the first update to be returned. Must be greater by one
    ///   than the highest among the identifiers of previously received
    ///   updates. By default, updates starting with the earliest unconfirmed
    ///   update are returned. An update is considered confirmed as soon as <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
    ///   getUpdates</see> is called with an offset higher than its update_id.
    ///   The negative offset can be specified to retrieve updates starting
    ///   from -offset update from the end of the updates queue. All previous
    ///   updates will forgotten.
    /// </param>
    /// <param name="Limit">
    ///   Количество обновлений которые могут прийти в одном запросе.
    ///   Допустимое значение от 1 до 100. По умолчанию - 100.Limits the number
    ///   of updates to be retrieved. Values between 1—100 are accepted.
    ///   Defaults to 100.
    /// </param>
    /// <param name="Timeout">
    ///   Timeout in seconds for long polling. Defaults to 0, i.e. usual short
    ///   polling
    /// </param>
    /// <param name="AllowedUpdates">
    ///   List the types of updates you want your bot to receive. For example,
    ///   specify [“message”, “edited_channel_post”, “callback_query”] to only
    ///   receive updates of these types. See Update for a complete list of
    ///   available update types. Specify an empty list to receive all updates
    ///   regardless of type (default). If not specified, the previous setting
    ///   will be used. <br /><br />Please note that this parameter doesn't
    ///   affect updates created before the call to the getUpdates, so unwanted
    ///   updates may be received for a short period of time.
    /// </param>
    /// <returns>
    ///   An Array of Update objects is returned.
    /// </returns>
    /// <remarks>
    ///   1. This method will not work if an outgoing webhook is set up. 2. In
    ///   order to avoid getting duplicate updates, recalculate offset after
    ///   each server response.
    /// </remarks>
    function GetUpdates(const Offset: Integer = 0; const Limit: Integer = 100; const Timeout: Integer = 0; AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL): TArray<TtgUpdate>;
    /// <summary>
    ///   Use this method to specify a url and receive incoming updates via an
    ///   outgoing webhook. Whenever there is an update for the bot, we will
    ///   send an HTTPS POST request to the specified url, containing a
    ///   JSON-serialized Update. In case of an unsuccessful request, we will
    ///   give up after a reasonable amount of attempts.
    /// </summary>
    /// <param name="Url">
    ///   HTTPS url to send updates to. Use an empty string to remove webhook
    ///   integration
    /// </param>
    /// <param name="Certificate">
    ///   Upload your public key certificate so that the root certificate in
    ///   use can be checked. See our self-signed guide for details.
    /// </param>
    /// <param name="MaxConnections">
    ///   Maximum allowed number of simultaneous HTTPS connections to the
    ///   webhook for update delivery, 1-100. Defaults to 40. Use lower values
    ///   to limit the load on your bot‘s server, and higher values to increase
    ///   your bot’s throughput.
    /// </param>
    /// <param name="AllowedUpdates">
    ///   List the types of updates you want your bot to receive. For example,
    ///   specify [“message”, “edited_channel_post”, “callback_query”] to only
    ///   receive updates of these types. See Update for a complete list of
    ///   available update types. Specify an empty list to receive all updates
    ///   regardless of type (default). If not specified, the previous setting
    ///   will be used. <br /><br />Please note that this parameter doesn't
    ///   affect updates created before the call to the setWebhook, so unwanted
    ///   updates may be received for a short period of time.
    /// </param>
    /// <remarks>
    ///   <para>
    ///     Notes
    ///   </para>
    ///   <para>
    ///     1. You will not be able to receive updates using <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
    ///     getUpdates</see> for as long as an outgoing webhook is set up.
    ///   </para>
    ///   <para>
    ///     2. To use a self-signed certificate, you need to upload your <see href="https://core.telegram.org/bots/self-signed">
    ///     public key certificate</see> using <c>certificate</c> parameter.
    ///     Please upload as InputFile, sending a String will not work.
    ///   </para>
    ///   <para>
    ///     3. Ports currently supported for Webhooks: <b>443, 80, 88, 8443</b>
    ///      .
    ///   </para>
    ///   <para>
    ///     <b>NEW!</b> If you're having any trouble setting up webhooks,
    ///     please check out this <see href="https://core.telegram.org/bots/webhooks">
    ///     amazing guide to Webhooks</see>.
    ///   </para>
    /// </remarks>
    procedure SetWebhook(const Url: string; Certificate: TtgFileToSend = nil; MaxConnections: Integer = 40; AllowedUpdates: TAllowedUpdates = UPDATES_ALLOWED_ALL);
    /// <summary>
    ///   Use this method to remove webhook integration if you decide to switch
    ///   back to <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
    ///   getUpdates</see>.
    /// </summary>
    /// <returns>
    ///   Returns <c>True</c> on success.
    /// </returns>
    function DeleteWebhook: Boolean;

    /// <summary>
    ///   Use this method to get current webhook status.
    /// </summary>
    /// <returns>
    ///   On success, returns a <see cref="TelegAPi.Types|TtgWebhookInfo">
    ///   WebhookInfo</see> object
    /// </returns>
    /// <remarks>
    ///   If the bot is using <see cref="TelegAPI.Bot|TTelegramBot.GetUpdates(Integer,Integer,Integer,TAllowedUpdates)">
    ///   getUpdates</see>, will return an object with the url field empty
    /// </remarks>
    function GetWebhookInfo: TtgWebhookInfo;
{$ENDREGION}
{$REGION 'Basic methods'}
    /// <summary>
    ///   <para>
    ///     Простой метод для проверки токена вашего бота
    ///   </para>
    ///   <para>
    ///     A simple method for testing your bot's auth token.
    ///   </para>
    /// </summary>
    /// <returns>
    ///   <para>
    ///     Возвращает основную информацию о боте
    ///   </para>
    ///   <para>
    ///     Returns basic information about the bot in form of a <see cref="TelegAPi.Types|TtgUser">
    ///     User</see> object.
    ///   </para>
    /// </returns>
    function GetMe: TtgUser;
    /// <summary>
    ///   Use this method to send text messages.
    /// </summary>
    /// <param name="ChatId">
    ///   Integer or String. Unique identifier for the target chat or username
    ///   of the target channel (in the format <c>@channelusername</c> ).
    /// </param>
    /// <param name="Text">
    ///   Text of the message to be sent
    /// </param>
    /// <param name="ParseMode">
    ///   Send Markdown or HTML, if you want Telegram apps to show bold,
    ///   italic, fixed-width text or inline URLs in your bot's message.
    /// </param>
    /// <param name="DisableWebPagePreview">
    ///   Disables link previews for links in this message
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound.
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message
    /// </param>
    /// <param name="ReplyMarkup">
    ///   InlineKeyboardMarkup or ReplyKeyboardMarkup or ReplyKeyboardHide or
    ///   ForceReply. Additional interface options. A JSON-serialized object
    ///   for an inline keyboard, custom reply keyboard, instructions to hide
    ///   reply keyboard or to force a reply from the user.
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    function SendMessage(const ChatId: TValue; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: IReplyMarkup = nil): TtgMessage;

    /// <summary>
    ///   Use this method to forward messages of any kind.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="FromChatId">
    ///   Unique identifier for the chat where the original message was sent
    ///   (or channel username in the format @channelusername) <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="MessageId">
    ///   Unique message identifier <br />
    /// </param>
    /// <returns>
    ///   On success, the sent Message is returned.
    /// </returns>
    function ForwardMessage(ChatId: TValue; FromChatId: TValue; DisableNotification: Boolean = False; MessageId: Integer = 0): TtgMessage;
    /// <summary>
    ///   Use this method to send photos.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Photo">
    ///   Photo to send. You can either pass a file_id as String to resend a
    ///   photo that is already on the Telegram servers, or upload a new photo
    ///   using multipart/form-data. <br />
    /// </param>
    /// <param name="Caption">
    ///   Photo caption (may also be used when resending photos by file_id),
    ///   0-200 characters <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to remove reply
    ///   keyboard or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    /// <example>
    ///   <code lang="Delphi">var
    /// LMessage: TtgMessage;
    /// Begin
    /// //Если не известен ИД файла
    /// LMessage := sendPhoto(chatId, TtgFileToSend.Create('Путь к файлу'), nil);
    /// //Если известен ИД файла
    /// LMessage := sendPhoto(chatId, 'ИД Файла');
    /// ...
    /// LMessage.Free;
    /// End; </code>
    /// </example>
    function SendPhoto(ChatId: TValue; Photo: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to send audio files, if you want Telegram clients to
    ///   display them in the music player. Your audio must be in the .mp3
    ///   format.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Audio">
    ///   Audio file to send. You can either pass a file_id as String to resend
    ///   an audio that is already on the Telegram servers, or upload a new
    ///   audio file using multipart/form-data. <br />
    /// </param>
    /// <param name="Duration">
    ///   Duration of the audio in seconds <br />
    /// </param>
    /// <param name="Performer">
    ///   Performer <br />
    /// </param>
    /// <param name="Title">
    ///   Track name <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    /// <remarks>
    ///   Bots can currently send audio files of up to 50 MB in size, this
    ///   limit may be changed in the future. For sending voice messages, use
    ///   the <see cref="TelegAPI.Bot|TTelegramBot.SendVoice(TValue,TValue,Integer,Boolean,Integer,TtgReplyKeyboardMarkup)">
    ///   sendVoice</see> method instead.
    /// </remarks>
    function SendAudio(ChatId: TValue; Audio: TValue; Duration: Integer = 0; const Performer: string = ''; const Title: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to send general files.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Document">
    ///   File to send. You can either pass a file_id as String to resend a
    ///   file that is already on the Telegram servers, or upload a new file
    ///   using multipart/form-data. <br />
    /// </param>
    /// <param name="Caption">
    ///   Document caption (may also be used when resending documents by
    ///   file_id), 0-200 characters <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    /// <remarks>
    ///   Bots can currently send files of any type of up to 50 MB in size,
    ///   this limit may be changed in the future.
    /// </remarks>
    function SendDocument(ChatId: TValue; Document: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to send .webp stickers.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Sticker">
    ///   Sticker to send. You can either pass a file_id as String to resend a
    ///   sticker that is already on the Telegram servers, or upload a new
    ///   sticker using multipart/form-data. <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent Message is returned.
    /// </returns>
    function SendSticker(ChatId: TValue; Sticker: TValue; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to send video files, Telegram clients support mp4
    ///   videos (other formats may be sent as Document).
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Video">
    ///   Video to send. You can either pass a file_id as String to resend a
    ///   video that is already on the Telegram servers, or upload a new video
    ///   file using multipart/form-data. <br />
    /// </param>
    /// <param name="Duration">
    ///   Duration of sent video in seconds <br />
    /// </param>
    /// <param name="Width">
    ///   Video width <br />
    /// </param>
    /// <param name="Height">
    ///   Video height <br />
    /// </param>
    /// <param name="Caption">
    ///   Video caption (may also be used when resending videos by file_id),
    ///   0-200 characters <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent Message is returned.
    /// </returns>
    /// <remarks>
    ///   Bots can currently send video files of up to 50 MB in size, this
    ///   limit may be changed in the future.
    /// </remarks>
    function SendVideo(ChatId: TValue; Video: TValue; Duration: Integer = 0; Width: Integer = 0; Height: Integer = 0; const Caption: string = ''; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;

    /// <summary>
    ///   Use this method to send audio files, if you want Telegram clients to
    ///   display the file as a playable voice message. For this to work, your
    ///   audio must be in an .ogg file encoded with OPUS (other formats may be
    ///   sent as Audio or Document).
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Voice">
    ///   Audio file to send. You can either pass a file_id as String to resend
    ///   an audio that is already on the Telegram servers, or upload a new
    ///   audio file using multipart/form-data. <br />
    /// </param>
    /// <param name="Duration">
    ///   Duration of sent audio in seconds <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned
    /// </returns>
    /// <remarks>
    ///   Bots can currently send voice messages of up to 50 MB in size, this
    ///   limit may be changed in the future.
    /// </remarks>
    function SendVoice(ChatId: TValue; Voice: TValue; Duration: Integer = 0; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   As of <see href="https://telegram.org/blog/video-messages-and-telescope">
    ///   v.4.0</see>, Telegram clients support rounded square mp4 videos of up
    ///   to 1 minute long.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="VideoNote">
    ///   Video note to send. Pass a file_id as String to send a video note
    ///   that exists on the Telegram servers (recommended) or upload a new
    ///   video using multipart/form-data. More info on Sending Files ».
    ///   Sending video notes by a URL is currently unsupported <br />
    /// </param>
    /// <param name="Duration">
    ///   Duration of sent video in seconds <br />
    /// </param>
    /// <param name="Length">
    ///   Video width and height <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to remove reply
    ///   keyboard or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    /// <remarks>
    ///   Use this method to send video messages.
    /// </remarks>
    function SendVideoNote(ChatId: TValue; //
      VideoNote: TValue; //
      Duration: Integer = 0; //
      Length: Integer = 0; //
      DisableNotification: Boolean = False; //
      ReplyToMessageId: Integer = 0; //
      ReplyMarkup: TtgReplyKeyboardMarkup = nil //
    ): TtgMessage;

    /// <summary>
    ///   Use this method to send point on the map.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Location">
    ///   Latitude and Longitude of location
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message <see href="https://telegram.org/blog/channels-2-0#silent-messages">
    ///   silently</see>. iOS users will not receive a notification, Android
    ///   users will receive a notification with no sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    function SendLocation(ChatId: TValue; Location: TtgLocation; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to send information about a venue.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Venue">
    ///   Latitude and Longitude of the venue <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br /><br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide reply keyboard
    ///   or to force a reply from the user. <br />
    /// </param>
    /// <param name="title">
    ///   Name of the venue
    /// </param>
    /// <param name="address">
    ///   Address of the venue
    /// </param>
    /// <param name="foursquare_id">
    ///   Foursquare identifier of the venue
    /// </param>
    /// <returns>
    ///   On success, the sent Message is returned.
    /// </returns>
    function SendVenue(ChatId: TValue; Venue: TtgVenue; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to send phone contacts.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Contact">
    ///   Contact's phone number, first name, last name
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound.
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for an inline
    ///   keyboard, custom reply keyboard, instructions to hide keyboard or to
    ///   force a reply from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage">Message</see>
    ///    is returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#sendcontact" />
    function SendContact(ChatId: TValue; Contact: TtgContact; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method when you need to tell the user that something is
    ///   happening on the bot's side. The status is set for 5 seconds or less
    ///   (when a message arrives from your bot, Telegram clients clear its
    ///   typing status).
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="Action">
    ///   Type of action to broadcast. Choose one, depending on what the user
    ///   is about to receive: typing for text messages, upload_photo for
    ///   photos, record_video or upload_video for videos, record_audio or
    ///   upload_audio for audio files, upload_document for general files,
    ///   find_location for location data <br />
    /// </param>
    /// <remarks>
    ///   We only recommend using this method when a response from the bot will
    ///   take a noticeable amount of time to arrive.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#sendchataction" />
    procedure SendChatAction(ChatId: TValue; const Action: TtgSendChatAction);
    /// <summary>
    ///   Use this method to get a list of profile pictures for a user.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier of the target user <br />
    /// </param>
    /// <param name="Offset">
    ///   Sequential number of the first photo to be returned. By default, all
    ///   photos are returned. <br />
    /// </param>
    /// <param name="Limit">
    ///   Limits the number of photos to be retrieved. Values between 1—100 are
    ///   accepted. Defaults to 100. <br />
    /// </param>
    /// <returns>
    ///   Returns a <see cref="TelegAPi.Types|TtgUserProfilePhotos">
    ///   UserProfilePhotos</see> object.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#getuserprofilephotos" />
    function GetUserProfilePhotos(ChatId: TValue; Offset: Integer; Limit: Integer = 100): TtgUserProfilePhotos;
    /// <summary>
    ///   Use this method to get basic info about a file and prepare it for
    ///   downloading. For the moment, bots can download files of up to 20MB in
    ///   size.
    /// </summary>
    /// <param name="FileId">
    ///   File identifier to get info about <br />
    /// </param>
    /// <returns>
    ///   On success, a <see cref="TelegAPi.Types|TtgFile">File</see> object is
    ///   returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#getfile" />
    function GetFile(const FileId: string): TtgFile;
    /// <summary>
    ///   Use this method to kick a user from a group or a supergroup. In the
    ///   case of supergroups, the user will not be able to return to the group
    ///   on their own using invite links, etc., unless unbanned first. The bot
    ///   must be an administrator in the group for this to work.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target group or username of the target
    ///   supergroup (in the format @supergroupusername) <br />
    /// </param>
    /// <param name="UserId">
    ///   Unique identifier of the target user <br />
    /// </param>
    /// <returns>
    ///   Returns True on success.
    /// </returns>
    /// <remarks>
    ///   Note: This will method only work if the ‘All Members Are Admins’
    ///   setting is off in the target group. Otherwise members may only be
    ///   removed by the group's creator or by the member that added them.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#kickchatmember" />
    function KickChatMember(ChatId: TValue; UserId: Integer): Boolean;
    /// <summary>
    ///   Use this method to unban a previously kicked user in a supergroup.
    ///   The user will not return to the group automatically, but will be able
    ///   to join via link, etc.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target group or username of the target
    ///   supergroup (in the format @supergroupusername) <br />
    /// </param>
    /// <param name="UserId">
    ///   Unique identifier of the target user <br />
    /// </param>
    /// <returns>
    ///   Returns True on success.
    /// </returns>
    /// <remarks>
    ///   The bot must be an administrator in the group for this to work.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#unbanchatmember" />
    function UnbanChatMember(ChatId: TValue; UserId: Integer): Boolean;
    /// <summary>
    ///   Use this method for your bot to leave a group, supergroup or channel.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target group or username of the target
    ///   supergroup (in the format @supergroupusername) <br />
    /// </param>
    /// <returns>
    ///   Returns True on success.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#leavechat" />
    function LeaveChat(ChatId: TValue): Boolean;
    /// <summary>
    ///   Use this method to get up to date information about the chat (current
    ///   name of the user for one-on-one conversations, current username of a
    ///   user, group or channel, etc.)
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   supergroup or channel (in the format @channelusername) <br />
    /// </param>
    /// <returns>
    ///   Returns a <see cref="TelegAPi.Types|TtgChat">Chat</see> object on
    ///   success.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#getchat" />
    function GetChat(const ChatId: TValue): TtgChat;
    /// <summary>
    ///   Use this method to get a list of administrators in a chat
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   supergroup or channel (in the format @channelusername) <br />
    /// </param>
    /// <returns>
    ///   On success, returns an Array of <see cref="TelegAPi.Types|TtgChatMember">
    ///   ChatMember</see> objects that contains information about all chat
    ///   administrators except other bots. If the chat is a group or a
    ///   supergroup and no administrators were appointed, only the creator
    ///   will be returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#getchatadministrators" />
    function GetChatAdministrators(const ChatId: TValue): TArray<TtgChatMember>;
    /// <summary>
    ///   Use this method to get the number of members in a chat.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   supergroup or channel (in the format @channelusername) <br />
    /// </param>
    /// <returns>
    ///   Returns Integer on success.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#getchatmemberscount" />
    function GetChatMembersCount(const ChatId: TValue): Integer;
    /// <summary>
    ///   Use this method to get information about a member of a chat.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target group or username of the target
    ///   supergroup (in the format @supergroupusername) <br />
    /// </param>
    /// <param name="UserId">
    ///   Unique identifier of the target user <br />
    /// </param>
    /// <returns>
    ///   Returns a <see cref="TelegAPi.Types|TtgChatMember">ChatMember</see>
    ///   object on success.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#getchatmember" />
    function GetChatMember(ChatId: TValue; UserId: Integer): TtgChatMember;
    /// <summary>
    ///   Use this method to send answers to callback queries sent from inline
    ///   keyboards. The answer will be displayed to the user as a notification
    ///   at the top of the chat screen or as an alert.
    /// </summary>
    /// <param name="CallbackQueryId">
    ///   Unique identifier for the query to be answered <br />
    /// </param>
    /// <param name="Text">
    ///   Text of the notification. If not specified, nothing will be shown to
    ///   the user <br />
    /// </param>
    /// <param name="ShowAlert">
    ///   If true, an alert will be shown by the client instead of a
    ///   notification at the top of the chat screen. Defaults to false. <br />
    /// </param>
    /// <returns>
    ///   On success, True is returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#answercallbackquery" />
    function AnswerCallbackQuery(const CallbackQueryId: string; const Text: string = ''; ShowAlert: Boolean = False; Url: string = ''; CacheTime: Integer = 0): Boolean;
{$ENDREGION}
{$REGION 'Updating messages'}
    /// <summary>
    ///   Use this method to edit text messages sent by the bot or via the bot
    ///   (for inline bots).
    /// </summary>
    /// <param name="ChatId">
    ///   Required if inline_message_id is not specified. Unique identifier for
    ///   the target chat or username of the target channel (in the format
    ///   @channelusername) <br />
    /// </param>
    /// <param name="MessageId">
    ///   Required if inline_message_id is not specified. Unique identifier of
    ///   the sent message <br />
    /// </param>
    /// <param name="InlineMessageId">
    ///   Required if chat_id and message_id are not specified. Identifier of
    ///   the inline message <br />
    /// </param>
    /// <param name="Text">
    ///   New text of the message <br />
    /// </param>
    /// <param name="ParseMode">
    ///   Send Markdown or HTML, if you want Telegram apps to show bold,
    ///   italic, fixed-width text or inline URLs in your bot's message. <br />
    /// </param>
    /// <param name="DisableWebPagePreview">
    ///   Disables link previews for links in this message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   A JSON-serialized object for an inline keyboard. <br />
    /// </param>
    /// <returns>
    ///   On success, if edited message is sent by the bot, the edited Message
    ///   is returned, otherwise True is returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#editmessagetext" />
    function EditMessageText(ChatId: TValue; MessageId: Integer; const InlineMessageId: string; const Text: string; ParseMode: TtgParseMode = TtgParseMode.Default; DisableWebPagePreview: Boolean = False; ReplyMarkup: TtgReplyKeyboardMarkup = nil): Boolean;
    /// <summary>
    ///   Use this method to edit captions of messages sent by the bot or via
    ///   the bot (for inline bots).
    /// </summary>
    /// <param name="ChatId">
    ///   Required if InlineMessageId is not specified. Unique identifier for
    ///   the target chat or username of the target channel (in the format
    ///   @channelusername) <br />
    /// </param>
    /// <param name="MessageId">
    ///   Required if InlineMessageId is not specified. Unique identifier of <br />
    ///    the sent message <br />
    /// </param>
    /// <param name="InlineMessageId">
    ///   Required if ChatId and MessageId are not specified. Identifier of the
    ///   inline message <br />
    /// </param>
    /// <param name="Caption">
    ///   New caption of the message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   A JSON-serialized object for an inline keyboard. <br />
    /// </param>
    /// <returns>
    ///   On success, if edited message is sent by the bot, the edited Message
    ///   is returned, otherwise True is returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#editmessagereplymarkup" />
    function EditMessageCaption(ChatId: TValue; MessageId: Integer; const InlineMessageId: string; const Caption: string; ReplyMarkup: TtgReplyKeyboardMarkup = nil): Boolean;
    /// <summary>
    ///   Use this method to edit only the reply markup of messages sent by the
    ///   bot or via the bot (for inline bots).
    /// </summary>
    /// <param name="ChatId">
    ///   Required if InlineMessageId is not specified. Unique identifier for <br />
    ///    the target chat or username of the target channel (in the format <br />
    ///    @channelusername) <br />
    /// </param>
    /// <param name="MessageId">
    ///   Required if InlineMessageId is not specified. Unique identifier of <br />
    ///    the sent message <br />
    /// </param>
    /// <param name="InlineMessageId">
    ///   Required if ChatId and MessageId are not specified. Identifier of <br />
    ///    the inline message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   A JSON-serialized object for an inline keyboard. <br />
    /// </param>
    /// <returns>
    ///   On success, if edited message is sent by the bot, the edited Message
    ///   is returned, otherwise True is returned.
    /// </returns>
    function EditMessageReplyMarkup(ChatId: TValue; MessageId: Integer; const InlineMessageId: string; ReplyMarkup: TtgReplyKeyboardMarkup = nil): Boolean;
    /// <summary>
    ///   Use this method to delete a message.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat or username of the target
    ///   channel (in the format @channelusername) <br />
    /// </param>
    /// <param name="MessageId">
    ///   Identifier of the message to delete <br />
    /// </param>
    /// <returns>
    ///   Returns True on success.
    /// </returns>
    /// <remarks>
    ///   A message can only be deleted if it was sent less than 48 hours ago.
    ///   Any such recently sent outgoing message may be deleted. Additionally,
    ///   if the bot is an administrator in a group chat, it can delete any
    ///   message. If the bot is an administrator in a supergroup, it can
    ///   delete messages from any other user and service messages about people
    ///   joining or leaving the group (other types of service messages may
    ///   only be removed by the group creator). In channels, bots can only
    ///   remove their own messages.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#deletemessage" />
    function DeleteMessage(ChatId: TValue; MessageId: Integer): Boolean;
{$ENDREGION}
{$REGION 'Inline mode'}
    /// <summary>
    ///   Use this method to send answers to an inline query.
    /// </summary>
    /// <param name="InlineQueryId">
    ///   Unique identifier for the answered query <br />
    /// </param>
    /// <param name="Results">
    ///   A JSON-serialized array of results for the inline query <br />
    /// </param>
    /// <param name="CacheTime">
    ///   The maximum amount of time in seconds that the result of the inline
    ///   query may be cached on the server. Defaults to 300. <br />
    /// </param>
    /// <param name="IsPersonal">
    ///   Pass True, if results may be cached on the server side only for the
    ///   user that sent the query. By default, results may be returned to any
    ///   user who sends the same query <br />
    /// </param>
    /// <param name="NextOffset">
    ///   Pass the offset that a client should send in the next query with the
    ///   same text to receive more results. Pass an empty string if there are
    ///   no more results or if you don‘t support pagination. Offset length
    ///   can’t exceed 64 bytes. <br />
    /// </param>
    /// <param name="SwitchPmText">
    ///   If passed, clients will display a button with specified text that
    ///   switches the user to a private chat with the bot and sends the bot a
    ///   start message with the parameter switch_pm_parameter <br />
    /// </param>
    /// <param name="SwitchPmParameter">
    ///   Parameter for the start message sent to the bot when user presses the
    ///   switch button <br />
    /// </param>
    /// <returns>
    ///   On success, True is returned.
    /// </returns>
    /// <remarks>
    ///   No more than 50 results per query are allowed.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#answerinlinequery" />
    function AnswerInlineQuery(const InlineQueryId: string; Results: TArray<TtgInlineQueryResult>; CacheTime: Integer = 300; IsPersonal: Boolean = False; const NextOffset: string = ''; const SwitchPmText: string = ''; const SwitchPmParameter: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Payments'}
    /// <summary>
    ///   Use this method to send invoices.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target private chat <br />
    /// </param>
    /// <param name="Title">
    ///   Product name
    /// </param>
    /// <param name="Description">
    ///   Product description
    /// </param>
    /// <param name="Payload">
    ///   Bot-defined invoice payload, 1-128 bytes. This will not be displayed
    ///   to the user, use for your internal processes.
    /// </param>
    /// <param name="ProviderToken">
    ///   Payments provider token, obtained via Botfather
    /// </param>
    /// <param name="StartParameter">
    ///   Unique deep-linking parameter that can be used to generate this
    ///   invoice when used as a start parameter
    /// </param>
    /// <param name="Currency">
    ///   Three-letter ISO 4217 currency code, see more on currencies
    /// </param>
    /// <param name="Prices">
    ///   Price breakdown, a list of components (e.g. product price, tax,
    ///   discount, delivery cost, delivery tax, bonus, etc.)
    /// </param>
    /// <param name="PhotoUrl">
    ///   URL of the product photo for the invoice. Can be a photo of the goods
    ///   or a marketing image for a service.
    /// </param>
    /// <param name="PhotoSize">
    ///   Photo size
    /// </param>
    /// <param name="PhotoWidth">
    ///   Photo width
    /// </param>
    /// <param name="PhotoHeight">
    ///   Photo height
    /// </param>
    /// <param name="NeedName">
    ///   Pass True, if you require the user's full name to complete the order
    /// </param>
    /// <param name="NeedPhoneNumber">
    ///   Pass True, if you require the user's phone number to complete the
    ///   order
    /// </param>
    /// <param name="NeedEmail">
    ///   Pass True, if you require the user's email to complete the order
    /// </param>
    /// <param name="NeedShippingAddress">
    ///   Pass True, if you require the user's shipping address to complete the
    ///   order
    /// </param>
    /// <param name="IsFlexible">
    ///   Pass True, if the final price depends on the shipping method
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound.
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   Additional interface options. A JSON-serialized object for a custom
    ///   reply keyboard, instructions to hide keyboard or to force a reply
    ///   from the user. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent <see cref="TelegAPi.Types|TtgMessage" /> is
    ///   returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#sendinvoice" />
    function SendInvoice(ChatId: Integer; const Title: string; const Description: string; const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string; Prices: TArray<TtgLabeledPrice>; const PhotoUrl: string = ''; PhotoSize: Integer = 0; PhotoWidth: Integer = 0; PhotoHeight: Integer = 0; NeedName: Boolean = False; NeedPhoneNumber: Boolean = False; NeedEmail: Boolean = False; NeedShippingAddress: Boolean = False; IsFlexible: Boolean = False; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   If you sent an invoice requesting a shipping address and the
    ///   parameter is_flexible was specified, the Bot API will send an Update
    ///   with a shipping_query field to the bot. Use this method to reply to
    ///   shipping queries. On success, True is returned.
    /// </summary>
    /// <param name="ShippingQueryId">
    ///   Unique identifier for the query to be answered
    /// </param>
    /// <param name="Ok">
    ///   Specify True if delivery to the specified address is possible and
    ///   False if there are any problems (for example, if delivery to the
    ///   specified address is not possible)
    /// </param>
    /// <param name="ShippingOptions">
    ///   Required if <c>ok</c> is <c>True</c>. A JSON-serialized array of
    ///   available shipping options.
    /// </param>
    /// <param name="ErrorMessage">
    ///   Required if <c>ok</c> is <c>False</c>. Error message in human
    ///   readable form that explains why it is impossible to complete the
    ///   order (e.g. "Sorry, delivery to your desired address is
    ///   unavailable'). Telegram will display this message to the user.
    /// </param>
    /// <seealso href="https://core.telegram.org/bots/api#answershippingquery" />
    function AnswerShippingQuery(const ShippingQueryId: string; Ok: Boolean; ShippingOptions: TArray<TtgShippingOption>; const ErrorMessage: string): Boolean;
    /// <summary>
    ///   Once the user has confirmed their payment and shipping details, the
    ///   Bot API sends the final confirmation in the form of an <see cref="TelegAPi.Types|TtgUpdate">
    ///   Update</see> with the field PreCheckoutQueryId. Use this method to
    ///   respond to such pre-checkout queries.
    /// </summary>
    /// <param name="PreCheckoutQueryId">
    ///   Unique identifier for the query to be answered
    /// </param>
    /// <param name="Ok">
    ///   Specify <c>True</c> if everything is alright (goods are available,
    ///   etc.) and the bot is ready to proceed with the order. Use False if
    ///   there are any problems.
    /// </param>
    /// <param name="ErrorMessage">
    ///   Required if <c>ok</c> is <c>False</c>. Error message in human
    ///   readable form that explains the reason for failure to proceed with
    ///   the checkout (e.g. "Sorry, somebody just bought the last of our
    ///   amazing black T-shirts while you were busy filling out your payment
    ///   details. Please choose a different color or garment!"). Telegram will
    ///   display this message to the user.
    /// </param>
    /// <returns>
    ///   On success, True is returned.
    /// </returns>
    /// <remarks>
    ///   <b>Note</b>: The Bot API must receive an answer within 10 seconds
    ///   after the pre-checkout query was sent.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#answerprecheckoutquery" />
    function AnswerPreCheckoutQuery(const PreCheckoutQueryId: string; Ok: Boolean; const ErrorMessage: string = ''): Boolean;
{$ENDREGION}
{$REGION 'Games'}
    /// <summary>
    ///   Use this method to send a game.
    /// </summary>
    /// <param name="ChatId">
    ///   Unique identifier for the target chat <br />
    /// </param>
    /// <param name="GameShortName">
    ///   Short name of the game, serves as the unique identifier for the game.
    ///   Set up your games via Botfather. <br />
    /// </param>
    /// <param name="DisableNotification">
    ///   Sends the message silently. iOS users will not receive a
    ///   notification, Android users will receive a notification with no
    ///   sound. <br />
    /// </param>
    /// <param name="ReplyToMessageId">
    ///   If the message is a reply, ID of the original message <br />
    /// </param>
    /// <param name="ReplyMarkup">
    ///   A JSON-serialized object for an inline keyboard. If empty, one ‘Play
    ///   game_title’ button will be shown. If not empty, the first button must
    ///   launch the game. <br />
    /// </param>
    /// <returns>
    ///   On success, the sent Message is returned.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#sendgame" />
    function SendGame(ChatId: Integer; const GameShortName: string; DisableNotification: Boolean = False; ReplyToMessageId: Integer = 0; ReplyMarkup: TtgReplyKeyboardMarkup = nil): TtgMessage;
    /// <summary>
    ///   Use this method to set the score of the specified user in a game.
    /// </summary>
    /// <param name="UserId">
    ///   User identifier <br />
    /// </param>
    /// <param name="Score">
    ///   New score, must be non-negative <br />
    /// </param>
    /// <param name="Force">
    ///   Pass True, if the high score is allowed to decrease. This can be
    ///   useful when fixing mistakes or banning cheaters <br />
    /// </param>
    /// <param name="DisableEditMessage">
    ///   Pass True, if the game message should not be automatically edited to
    ///   include the current scoreboard <br />
    /// </param>
    /// <param name="ChatId">
    ///   Required if InlineMessageId is not specified. Unique identifier for <br />
    ///    the target chat <br />
    /// </param>
    /// <param name="MessageId">
    ///   Required if InlineMessageId is not specified. Identifier of the <br />
    ///    sent message <br />
    /// </param>
    /// <param name="InlineMessageId">
    ///   Required if ChatId and MessageId are not specified. Identifier of the
    ///   inline message <br />
    /// </param>
    /// <returns>
    ///   On success, if the message was sent by the bot, returns the edited
    ///   Message, otherwise returns True. Returns an error, if the new score
    ///   is not greater than the user's current score in the chat and force is
    ///   False.
    /// </returns>
    /// <seealso href="https://core.telegram.org/bots/api#setgamescore" />
    function SetGameScore(UserId: Integer; Score: Integer; Force: Boolean = False; DisableEditMessage: Boolean = False; ChatId: Integer = 0; MessageId: Integer = 0; const InlineMessageId: string = ''): TtgMessage;
    /// <summary>
    ///   Use this method to get data for high score tables. Will return the
    ///   score of the specified user and several of his neighbors in a game.
    /// </summary>
    /// <param name="UserId">
    ///   Target user id <br />
    /// </param>
    /// <param name="ChatId">
    ///   Required if InlineMessageId is not specified. Unique identifier for <br />
    ///    the target chat <br />
    /// </param>
    /// <param name="MessageId">
    ///   Required if InlineMessageId is not specified. Identifier of the <br />
    ///    sent message <br />
    /// </param>
    /// <param name="InlineMessageId">
    ///   Required if ChatId and MessageId are not specified. Identifier of <br />
    ///    the inline message <br />
    /// </param>
    /// <returns>
    ///   On success, returns an Array of <see cref="TelegAPi.Types|TtgGameHighScore">
    ///   GameHighScore</see> objects.
    /// </returns>
    /// <remarks>
    ///   This method will currently return scores for the target user, plus
    ///   two of his closest neighbors on each side. Will also return the top
    ///   three users if the user and his neighbors are not among them. Please
    ///   note that this behavior is subject to change.
    /// </remarks>
    /// <seealso href="https://core.telegram.org/bots/api#getgamehighscores">
    ///   Official API
    /// </seealso>
    function GetGameHighScores(UserId: Integer; ChatId: Integer = 0; MessageId: Integer = 0; const InlineMessageId: string = ''): TArray<TtgGameHighScore>;
{$ENDREGION}
  end;

implementation

uses
  TelegAPI.Helpers;

{ TTelegramBotCore }

{$REGION 'Core'}

constructor TTelegramBotCore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowedUpdates := UPDATES_ALLOWED_ALL;
  FIsReceiving := False;
  PollingTimeout := 1000;
  MessageOffset := 0;
end;

function TTelegramBotCore.GetVersionAPI: string;
begin
  Result := '3.0.2';
end;

procedure TtgRecesiver.OnUpdateReceived(AValue: TtgUpdate);
begin
  if Assigned(Bot.OnUpdate) then
    Bot.OnUpdate(Bot, AValue);
  case AValue.&type of
    TtgUpdateType.MessageUpdate:
      if Assigned(Bot.OnMessage) then
        Bot.OnMessage(Bot, AValue.Message);
    TtgUpdateType.InlineQueryUpdate:
      if Assigned(Bot.OnInlineQuery) then
        Bot.OnInlineQuery(Bot, AValue.InlineQuery);
    TtgUpdateType.ChosenInlineResultUpdate:
      if Assigned(Bot.OnInlineResultChosen) then
        Bot.OnInlineResultChosen(Bot, AValue.ChosenInlineResult);
    TtgUpdateType.CallbackQueryUpdate:
      if Assigned(Bot.OnCallbackQuery) then
        Bot.OnCallbackQuery(Bot, AValue.CallbackQuery);
    TtgUpdateType.EditedMessage:
      if Assigned(Bot.OnMessageEdited) then
        Bot.OnMessageEdited(Bot, AValue.EditedMessage);
  else
    raise ETelegramException.Create('Unknown update type');
  end
end;

function TTelegramBotCore.API<T>(const Method: string; Parameters: TDictionary<string, TValue>): T;
var
  LHttp: THTTPClient;
  LHttpResponse: IHTTPResponse;
  LApiResponse: TtgApiResponse<T>;
  LURL_TELEG: string;
  LParamToDate: TMultipartFormData;
begin
  if not Self.IsValidToken then
  begin
    if IsReceiving then
      IsReceiving := False;
    raise EArgumentException.Create('Invalid token format');
  end;
  LHttp := THTTPClient.Create;
  LApiResponse := nil;
  LParamToDate := nil;
  try
    LHttp.ProxySettings := FProxySettings;
    LURL_TELEG := 'https://api.telegram.org/bot' + FToken + '/' + Method;
    try
      if Assigned(Parameters) then
      begin
        LParamToDate := ParamsToFormData(Parameters);
        LHttpResponse := LHttp.Post(LURL_TELEG, LParamToDate);
      end
      else
      begin
        LHttpResponse := LHttp.Get(LURL_TELEG);
      end;
    except
      on E: Exception do
      begin
        Self.ErrorHandler(E);
        Result := Default(T);
        Exit;
      end;
    end;
    if Assigned(OnReceiveRawData) then
      OnReceiveRawData(Self, LHttpResponse.ContentAsString);
    LApiResponse := TtgApiResponse<T>.FromJSON(LHttpResponse.ContentAsString);
    if not LApiResponse.Ok then
    begin
      if Assigned(OnReceiveError) then
        TThread.Synchronize(FRecesiver,
          procedure
          begin
            OnReceiveError(Self, EApiRequestException.FromApiResponse<T>(LApiResponse, Parameters))
          end)
      else
        raise EApiRequestException.FromApiResponse<T>(LApiResponse, Parameters);
    end;
    Result := LApiResponse.ResultObject;
    LApiResponse.ResultObject := Default(T);
  finally
    FreeAndNil(LParamToDate);
    FreeAndNil(LHttp);
    FreeAndNil(LApiResponse);
  end;
end;

function TTelegramBotCore.ParamsToFormData(Parameters: TDictionary<string, TValue>): TMultipartFormData;
var
  Parameter: TPair<string, TValue>;
begin
  Result := TMultipartFormData.Create;
  for Parameter in Parameters do
  begin
    if Parameter.Value.IsType<string>then
    begin
      if not Parameter.Value.AsString.IsEmpty then
        Result.AddField(Parameter.Key, Parameter.Value.AsString);
    end
    else if Parameter.Value.IsType<Int64>then
    begin
      if Parameter.Value.AsInt64 <> 0 then
        Result.AddField(Parameter.Key, Parameter.Value.AsInt64.ToString);
    end
    else if Parameter.Value.IsType<Boolean>then
    begin
      if Parameter.Value.AsBoolean then
        Result.AddField(Parameter.Key, Parameter.Value.AsBoolean.ToString(TUseBoolStrs.True));
    end
    else if Parameter.Value.IsType<TtgFileToSend>then
    begin
      { TODO -oOwner -cGeneral : Отправка файлов }
      with Parameter.Value.AsType<TtgFileToSend>do
      begin
        if Assigned(Content) then
          Result.AddStream(Parameter.Key, Content)
        else
          Result.AddFile(Parameter.Key, FileName);
      end;
    end
    else if Parameter.Value.Kind = tkClass then
    begin
      { TODO -oOwner -cGeneral : Проверить че за херня тут твориться }
      if not Parameter.Value.IsEmpty then
        Result.AddField(Parameter.Key, Parameter.Value.AsObject.AsJSON);
    end
    else
      raise ETelegramUnknownData.Create('Check parametr type ' + Parameter.Value.ToString);
  end;
end;

procedure TTelegramBotCore.SetIsReceiving(const Value: Boolean);
begin
  if (csDesigning in ComponentState) then
    Exit;
  FIsReceiving := Value;
  if Value then
  begin
    FRecesiver := TtgRecesiver.Create(True);
    FRecesiver.FreeOnTerminate := False;
    FRecesiver.Bot := TTelegramBot(Self);
    FRecesiver.OnTerminate := DoDisconnect;
    FRecesiver.Start;
  end
  else
  begin
    FreeAndNil(FRecesiver);
  end;
end;

function TTelegramBotCore.ArrayToString<T>(LArray: TArray<T>): string;
var
  SA: ISuperArray;
  I: Integer;
  X: TtgInlineQueryResult;
begin
  SA := TSuperArray.Create();
  for I := Low(LArray) to High(LArray) do
    SA.Add(T(LArray[I]).AsJSONObject);
  Result := SA.AsJSON();
end;

procedure TTelegramBotCore.DoDisconnect(ASender: TObject);
begin
  if Assigned(OnDisconnect) then
    OnDisconnect(ASender);
end;

procedure TTelegramBotCore.ErrorHandler(AException: Exception);
begin
  if Assigned(OnReceiveGeneralError) then
  begin
    OnReceiveGeneralError(Self, AException);
    Exit;
  end
  else
  begin
    raise Exception.Create(AException.Message);
  end;
end;

destructor TTelegramBotCore.Destroy;
begin
  Self.PollingTimeout := 0;
  if IsReceiving then
    IsReceiving := False;
  inherited;
end;
{$ENDREGION}
{ TTelegram }
{$REGION 'Getting updates'}

procedure TTelegramBot.SetWebhook(const Url: string; Certificate: TtgFileToSend; MaxConnections: Integer; AllowedUpdates: TAllowedUpdates);
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('url', Url);
    Parameters.Add('certificate', Certificate);
    Parameters.Add('max_connections', MaxConnections);
    Parameters.Add('allowed_updates', AllowedUpdates.ToString);
    API<Boolean>('setWebhook', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetWebhookInfo: TtgWebhookInfo;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Result := API<TtgWebhookInfo>('getWebhookInfo', nil);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetUpdates(const Offset, Limit, Timeout: Integer; AllowedUpdates: TAllowedUpdates): TArray<TtgUpdate>;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('offset', Offset);
    Parameters.Add('limit', Limit);
    Parameters.Add('timeout', Timeout);
    Parameters.Add('allowed_updates', AllowedUpdates.ToString);
    Result := API<TArray<TtgUpdate>>('getUpdates', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.DeleteWebhook: Boolean;
begin
  Result := API<Boolean>('deleteWebhook', nil);
end;
{$ENDREGION}
{$REGION 'Basic methods'}

function TTelegramBot.UnbanChatMember(ChatId: TValue; UserId: Integer): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Result := API<Boolean>('unbanChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendLocation(ChatId: TValue; Location: TtgLocation; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('latitude', Location.Latitude);
    Parameters.Add('longitude', Location.Longitude);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendLocation', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendPhoto(ChatId, Photo: TValue; const Caption: string; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('photo', Photo);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendPhoto', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendSticker(ChatId, Sticker: TValue; const Caption: string; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('sticker', Sticker);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendSticker', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendMessage(const ChatId: TValue; const Text: string; ParseMode: TtgParseMode; DisableWebPagePreview, DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: IReplyMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('text', Text);
    Parameters.Add('parse_mode', ParseMode.ToString);
    Parameters.Add('disable_web_page_preview', DisableWebPagePreview);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', TInterfacedObject(ReplyMarkup));
    Result := API<TtgMessage>('sendMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendVenue(ChatId: TValue; Venue: TtgVenue; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('latitude', Venue.Location.Latitude);
    Parameters.Add('longitude', Venue.Location.Longitude);
    Parameters.Add('title', Venue.Title);
    Parameters.Add('address', Venue.Address);
    Parameters.Add('foursquare_id', Venue.FoursquareId);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendVenue', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendVideo(ChatId, Video: TValue; Duration, Width, Height: Integer; const Caption: string; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('video', Video);
    Parameters.Add('duration', Duration);
    Parameters.Add('width', Width);
    Parameters.Add('height', Height);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendVideo', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendVideoNote(ChatId, VideoNote: TValue; Duration, Length: Integer; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  LParameters: TDictionary<string, TValue>;
begin
  LParameters := TDictionary<string, TValue>.Create;
  try
    LParameters.Add('chat_id', ChatId);
    LParameters.Add('video_note', VideoNote);
    LParameters.Add('duration', Duration);
    LParameters.Add('length', Length);
    LParameters.Add('disable_notification', DisableNotification);
    LParameters.Add('reply_to_message_id', ReplyToMessageId);
    LParameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendVoice', LParameters);
  finally
    LParameters.Free;
  end;
end;

function TTelegramBot.SendVoice(ChatId, Voice: TValue; Duration: Integer; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('voice', Voice);
    Parameters.Add('duration', Duration);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendVoice', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendAudio(ChatId, Audio: TValue; Duration: Integer; const Performer, Title: string; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('audio', Audio);
    Parameters.Add('duration', Duration);
    Parameters.Add('performer', Performer);
    Parameters.Add('title', Title);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendAudio', Parameters);
  finally
    Parameters.Free;
  end;
end;

procedure TTelegramBot.SendChatAction(ChatId: TValue; const Action: TtgSendChatAction);
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('action', Action.ToString);
    API<Boolean>('sendChatAction', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendContact(ChatId: TValue; Contact: TtgContact; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('phone_number', Contact.PhoneNumber);
    Parameters.Add('first_name', Contact.FirstName);
    Parameters.Add('last_name', Contact.LastName);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendContact', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendDocument(ChatId, Document: TValue; const Caption: string; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('document', Document);
    Parameters.Add('caption', Caption);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendDocument', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.KickChatMember(ChatId: TValue; UserId: Integer): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Result := API<Boolean>('kickChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.LeaveChat(ChatId: TValue): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Result := API<Boolean>('leaveChat', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetUserProfilePhotos(ChatId: TValue; Offset, Limit: Integer): TtgUserProfilePhotos;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('offset', Offset);
    Parameters.Add('limit', Limit);
    Result := API<TtgUserProfilePhotos>('getUserProfilePhotos', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetMe: TtgUser;
begin
  Result := Self.API<TtgUser>('getMe', nil);
end;

function TTelegramBot.ForwardMessage(ChatId, FromChatId: TValue; DisableNotification: Boolean; MessageId: Integer): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('from_chat_id', FromChatId);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('message_id', MessageId);
    Result := API<TtgMessage>('forwardMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChat(const ChatId: TValue): TtgChat;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Result := Self.API<TtgChat>('getChat', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChatAdministrators(const ChatId: TValue): TArray<TtgChatMember>;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Result := Self.API<TArray<TtgChatMember>>('getChatAdministrators', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChatMember(ChatId: TValue; UserId: Integer): TtgChatMember;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('user_id', UserId);
    Result := Self.API<TtgChatMember>('getChatMember', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetChatMembersCount(const ChatId: TValue): Integer;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Result := Self.API<Integer>('getChatMembersCount', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetFile(const FileId: string): TtgFile;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('file_id', FileId);
    Result := Self.API<TtgFile>('getFile', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.AnswerCallbackQuery(const CallbackQueryId, Text: string; ShowAlert: Boolean; Url: string; CacheTime: Integer): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('callback_query_id', CallbackQueryId);
    Parameters.Add('text', Text);
    Parameters.Add('show_alert', ShowAlert);
    Parameters.Add('url', Url);
    Parameters.Add('cache_time', CacheTime);
    Result := API<Boolean>('answerCallbackQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}
{$REGION 'Updating messages'}

function TTelegramBot.EditMessageText(ChatId: TValue; MessageId: Integer; const InlineMessageId, Text: string; ParseMode: TtgParseMode; DisableWebPagePreview: Boolean; ReplyMarkup: TtgReplyKeyboardMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('text', Text);
    Parameters.Add('parse_mode', ParseMode.ToString);
    Parameters.Add('disable_web_page_preview', DisableWebPagePreview);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<Boolean>('editMessageText', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.DeleteMessage(ChatId: TValue; MessageId: Integer): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Result := API<Boolean>('deleteMessage', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageCaption(ChatId: TValue; MessageId: Integer; const InlineMessageId, Caption: string; ReplyMarkup: TtgReplyKeyboardMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('caption', Caption);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<Boolean>('editMessageCaption', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.EditMessageReplyMarkup(ChatId: TValue; MessageId: Integer; const InlineMessageId: string; ReplyMarkup: TtgReplyKeyboardMarkup): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<Boolean>('editMessageText', Parameters);
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}
{$REGION 'Inline mode'}

function TTelegramBot.AnswerInlineQuery(const InlineQueryId: string; Results: TArray<TtgInlineQueryResult>; CacheTime: Integer; IsPersonal: Boolean; const NextOffset, SwitchPmText, SwitchPmParameter: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('inline_query_id', InlineQueryId);
    Parameters.Add('results', ArrayToString<TtgInlineQueryResult>(Results));
    Parameters.Add('cache_time', CacheTime);
    Parameters.Add('is_personal', IsPersonal);
    Parameters.Add('next_offset', NextOffset);
    Parameters.Add('switch_pm_text', SwitchPmText);
    Parameters.Add('switch_pm_parameter', SwitchPmParameter);
    Result := API<Boolean>('answerInlineQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;
{$ENDREGION}
{$REGION 'Payments'}

function TTelegramBot.SendInvoice(ChatId: Integer; const Title: string; const Description: string; const Payload: string; const ProviderToken: string; const StartParameter: string; const Currency: string; Prices: TArray<TtgLabeledPrice>; const PhotoUrl: string; PhotoSize: Integer; PhotoWidth: Integer; PhotoHeight: Integer; NeedName: Boolean; NeedPhoneNumber: Boolean; NeedEmail: Boolean; NeedShippingAddress: Boolean; IsFlexible: Boolean; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  LParameters: TDictionary<string, TValue>;
begin
  LParameters := TDictionary<string, TValue>.Create;
  try
    LParameters.Add('chat_id', ChatId);
    LParameters.Add('title', Title);
    LParameters.Add('description', Description);
    LParameters.Add('payload', Payload);
    LParameters.Add('provider_token', ProviderToken);
    LParameters.Add('currency', Currency);
    LParameters.Add('prices', ArrayToString<TtgLabeledPrice>(Prices));
    LParameters.Add('photo_url', PhotoUrl);
    LParameters.Add('photo_size', PhotoSize);
    LParameters.Add('photo_width', PhotoWidth);
    LParameters.Add('photo_height', PhotoHeight);
    LParameters.Add('need_name', NeedName);
    LParameters.Add('need_phone_number', NeedPhoneNumber);
    LParameters.Add('need_email', NeedEmail);
    LParameters.Add('need_shipping_address', NeedShippingAddress);
    LParameters.Add('is_flexible', IsFlexible);
    LParameters.Add('disable_notification', DisableNotification);
    LParameters.Add('reply_to_message_id', ReplyToMessageId);
    LParameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendInvoice', LParameters);
  finally
    LParameters.Free;
  end;
end;

function TTelegramBot.AnswerPreCheckoutQuery(const PreCheckoutQueryId: string; Ok: Boolean; const ErrorMessage: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('Pre_checkout_query_id', PreCheckoutQueryId);
    Parameters.Add('Ok', Ok);
    Parameters.Add('Error_message', ErrorMessage);
    Result := API<Boolean>('AnswerPreCheckoutQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.AnswerShippingQuery(const ShippingQueryId: string; Ok: Boolean; ShippingOptions: TArray<TtgShippingOption>; const ErrorMessage: string): Boolean;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('Shipping_query_id', ShippingQueryId);
    Parameters.Add('Ok', Ok);
    Parameters.Add('Shipping_options', ArrayToString<TtgShippingOption>(ShippingOptions));
    Parameters.Add('Error_message', ErrorMessage);
    Result := API<Boolean>('answerShippingQuery', Parameters);
  finally
    Parameters.Free;
  end;
end;

{$ENDREGION}
{$REGION 'Games'}

function TTelegramBot.SetGameScore(UserId, Score: Integer; Force, DisableEditMessage: Boolean; ChatId, MessageId: Integer; const InlineMessageId: string): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('score', Score);
    Parameters.Add('force', Force);
    Parameters.Add('disable_edit_message', DisableEditMessage);
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    Result := API<TtgMessage>('setGameScore', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.SendGame(ChatId: Integer; const GameShortName: string; DisableNotification: Boolean; ReplyToMessageId: Integer; ReplyMarkup: TtgReplyKeyboardMarkup): TtgMessage;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('game_short_name', GameShortName);
    Parameters.Add('disable_notification', DisableNotification);
    Parameters.Add('reply_to_message_id', ReplyToMessageId);
    Parameters.Add('reply_markup', ReplyMarkup);
    Result := API<TtgMessage>('sendGame', Parameters);
  finally
    Parameters.Free;
  end;
end;

function TTelegramBot.GetGameHighScores(UserId, ChatId, MessageId: Integer; const InlineMessageId: string): TArray<TtgGameHighScore>;
var
  Parameters: TDictionary<string, TValue>;
begin
  Parameters := TDictionary<string, TValue>.Create;
  try
    Parameters.Add('user_id', UserId);
    Parameters.Add('chat_id', ChatId);
    Parameters.Add('message_id', MessageId);
    Parameters.Add('inline_message_id', InlineMessageId);
    Result := API<TArray<TtgGameHighScore>>('getGameHighScores', Parameters);
  finally
    Parameters.Free;
  end;
end;

{$ENDREGION}
{$REGION 'Async'}
{$IF Defined(NO_QUEUE)}   // для работы в dll"ках
procedure TtgRecesiver.Execute;
var
  LUpdates: TArray<TtgUpdate>;
  I: Integer;
begin
  if Assigned(Bot.OnConnect) then
    Bot.OnConnect(Bot);
  repeat
    Sleep(Bot.PollingTimeout);
    if (Terminated) or (not Bot.IsReceiving) then
      Break;
    LUpdates := FBot.GetUpdates(Bot.MessageOffset, 100, 0, UPDATES_ALLOWED_ALL);
    if Length(LUpdates) = 0 then
      Continue;
    Bot.MessageOffset := LUpdates[High(LUpdates)].Id + 1;
    for I := Low(LUpdates) to High(LUpdates) do
      Self.OnUpdateReceived(LUpdates[I]);
    if Assigned(LUpdates) then
    begin
      for I := Low(LUpdates) to High(LUpdates) do
        FreeAndNil(LUpdates[I]);
      LUpdates := nil;
    end;

  until (Terminated) or (not Bot.IsReceiving);
end;

{$ELSE}
procedure TtgRecesiver.Execute;
var
  LUpdates: TArray<TtgUpdate>;
begin
  if Assigned(Bot.OnConnect) then
    Bot.OnConnect(Bot);
  repeat
    Sleep(Bot.PollingTimeout);
    if (Terminated) or (not Bot.IsReceiving) then
      Break;
    try
      LUpdates := FBot.GetUpdates(Bot.MessageOffset, 100, 0, UPDATES_ALLOWED_ALL);
    except
      on E: Exception do
        FBot.ErrorHandler(E);
    end;
    if Length(LUpdates) = 0 then
      Continue;
    Bot.MessageOffset := LUpdates[High(LUpdates)].Id + 1;
    TThread.Synchronize(nil,
      procedure
      var
        I: Integer;
      begin
        for I := Low(LUpdates) to High(LUpdates) do
          Self.OnUpdateReceived(LUpdates[I]);
        if Assigned(LUpdates) then
        begin
          for I := Low(LUpdates) to High(LUpdates) do
            FreeAndNil(LUpdates[I]);
          LUpdates := nil;
        end;
      end);
  until (Terminated) or (not Bot.IsReceiving);
end;

{$ENDIF}
{$ENDREGION}

end.

